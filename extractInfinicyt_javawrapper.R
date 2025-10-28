library(flowCore)

extract_infinicyt <- function(filepath, 
                              java_executable_path = "C:\\Program Files\\Java\\jdk-25\\bin\\java",
                              java_class_files_path = "java",
                              remove_temp_dir = TRUE, 
                              verbose = TRUE) {
  tempdir <- paste0("temp_dir_", as.numeric(Sys.time()))
  if (verbose){message("Unzipping file to ", tempdir)}
  unzip(filepath, exdir = tempdir)
  
  # Read in the fcs file and the corresponding xml file
  if (verbose){ message("Loading XML and FCS") }
  fcs_filepath <- paste0(tempdir, "/", list.files(tempdir, pattern = ".fcs"))
  binary_filepath <- paste0(tempdir, "\\", list.files(tempdir, pattern = ".pr$"))
  
  pr_file <- normalizePath(binary_filepath, winslash = "\\", mustWork = TRUE)
  # java_class_files_path should point to the directory that contains the `fcs` folder
  classpath_root <- normalizePath(java_class_files_path, winslash = "\\", mustWork = FALSE)
  if (!file.exists(classpath_root)) {
    # If relative 'java' folder inside project, try relative path
    classpath_root <- file.path(getwd(), java_class_files_path)
  }

  o <- capture.output(ff_agg <- suppressWarnings(flowCore::read.FCS(fcs_filepath, truncate_max_range = FALSE)))
  
  flowlist <- extract_flowframes(ff_agg, verbose = verbose)
  names(flowlist) <- sapply(flowlist, function(x) x$name)
  
  # Run java with -cp <classpath_root> fcs.reader.PRReader "<pr_file>"
  java_args <- c("-cp", classpath_root, "fcs.reader.PRReader", pr_file)
  
  if (verbose) {
    message("Running Java: ", paste(shQuote(java_executable_path), paste(shQuote(java_args), collapse = " "), collapse = " "))
  }
  
  # use system2 to capture stdout and stderr
  java_res <- tryCatch({
    system2(java_executable_path, args = java_args, stdout = TRUE, stderr = TRUE)
  }, error = function(e){
    stop("Failed to run java: ", conditionMessage(e))
  })
  
  # Check if java produced anything or returned an error message
  if (length(java_res) == 0 || any(grepl("Exception|Error|Could not find or load main class|NoClassDefFoundError|ClassNotFoundException", java_res, ignore.case = TRUE))) {
    stop("Java PRReader failed. Captured output:\n", paste(java_res, collapse = "\n"))
  }

  empty_lines <- which(java_res == "")
  
  celllabel_lines <- (empty_lines[1]+3) : (empty_lines[2]-1)
  celllabels <- as.numeric(gsub(".*;", "", java_res[celllabel_lines]))
  
  hierarchy_lines <- (empty_lines[2]+2) : length(java_res)
  hierarchy <- java_res[hierarchy_lines]
  hierarchy <- data.frame(id = gsub(".*id:([^,]*), .*", "\\1", hierarchy),
                          name = gsub(".*name:([^,]*), .*", "\\1", hierarchy),
                          subpopulations = gsub(".*subpopulations: \\[(.*)\\] .*", "\\1", hierarchy))
  
  cellmatrix <- matrix(FALSE, nrow = length(celllabels), ncol = nrow(hierarchy),
                       dimnames = list(NULL, paste(hierarchy$id, hierarchy$name)))
  for(i in seq_len(nrow(hierarchy))){
    if (verbose) { message(i) }
    cellmatrix[celllabels == hierarchy$id[i], i] <- TRUE
  }
  for(i in rev(seq_len(nrow(hierarchy)))){
    if (verbose) { message(i) }
    populations <- strsplit(hierarchy$subpopulations[i], ",")[[1]]
    if(length(populations) > 0){
      population_columns <- sapply(populations, function(x) which(hierarchy$id == x))
      cellmatrix[,i] <- apply(cellmatrix[,c(i, population_columns), drop = FALSE], 1, any)
    }
  }
  
  colSums(cellmatrix)
  
  
  end <- cumsum(sapply(flowlist, function(x) flowCore::nrow(x$ff)))
  start <- c(1, end[1:(length(end)-1)]+1)
  
  for(i in seq_along(flowlist)){
    flowlist[[i]]$manual_matrix <- cellmatrix[start[i]:end[i],]
  }
  
  
  if (remove_temp_dir) {
    unlink(tempdir, recursive = T)
  }
  
  return(flowlist)
}


extract_flowframes <- function(ff_agg, verbose = TRUE){
  
  # If marker name in description, add to column name (as stored in gates, spillover, ...)
  # and remove numbers added by flowcore to create unique colnames
  if(!all(is.na(ff_agg@parameters@data[,"desc"]))){ 
    to_update <- which(!is.na(ff_agg@parameters@data[,"desc"]))
    colnames(ff_agg)[to_update] <- paste0(gsub("-[0-9]*$", "", colnames(ff_agg)[to_update]),
                                          ":",
                                          ff_agg@parameters@data[to_update,"desc"])
  }
  
  if(!is.null(ff_agg@description$INFCYT)){
    # Extract infinicyt aggregate information such as number of files, 
    # cell counts and filenames
    infcyt_keyword <- unlist(strsplit(ff_agg@description$INFCYT, ";"))
    n_files <- as.numeric(infcyt_keyword[3])
    cell_counts <- as.numeric(infcyt_keyword[4:(3+n_files)])
    filenames <- infcyt_keyword[grepl("\\.fcs|.cyt$", infcyt_keyword)]
    
    if(length(filenames) == 0) filenames <- paste("File", seq(n_files))
    
    # Derive file_ids from number of cells per file
    file_id <- rep(seq_len(n_files), times = cell_counts)
  } else { # Assume only one file
    filenames <- ff_agg@description$GUID
    file_id <- rep(1, nrow(ff_agg))
  }
  
  # Build list with all separate flow frames
  flowlist <- list()
  for(i in seq_along(filenames)){
    if(verbose) message("Extracting ", filenames[i])
    
    subset <- ff_agg[file_id == i,]
    
    # Remove fully missing columns (not present in panel)
    subset_cols <- apply(exprs(subset), 2, function(x)!all(is.na(x)))
    subset <- subset[, subset_cols]
    # Remove any rows which have an NA value
    subset_rows <- apply(exprs(subset), 1, function(x)!any(is.na(x)))
    subset <- subset[subset_rows, ]
    
    # Extract spillover matrix
    spill <- get_spillover(ff_agg, i)
    spill_cols <- intersect(names(which(subset_cols)), colnames(spill))
    spill_subset <- spill[spill_cols, spill_cols]
    keyword(subset)[["SPILL"]] <- spill_subset
    keyword(subset)[["GUID"]] <- filenames[i]
    
    # Compensate and transform
    new_ff_c <- flowCore::compensate(subset, spill_subset)
    new_ff_tfList <- tryCatch(
      {
        flowCore::estimateLogicle(new_ff_c, colnames(spill_subset))
      },
      error = function(e) {
        warning("Default logicle parameters for ", filenames[i])
        flowCore::transformList( colnames(spill_subset), 
                                 flowCore::logicleTransform())
      }
    )
    new_ff_t <- flowCore::transform(new_ff_c, new_ff_tfList)
    
    flowlist[[filenames[i]]] <-  list(
      "name" = filenames[[i]],
      "ff" = new_ff_t,
      "transform_list" = new_ff_tfList
    )
  }
  return(flowlist)
}


parse_infinispill <- function(str, start = 1){
  spill <- unlist(strsplit(str, ","))
  if(spill[start] == "SA") start <- start+1
  ncol <- as.numeric(spill[start])
  dimnames <- spill[(start+1):(start+ncol)]
  values <- as.numeric(spill[(start+ncol+1):length(spill)])
  
  spill <- matrix(values,
                  ncol = ncol,
                  dimnames = list(dimnames, dimnames),
                  byrow = TRUE)
}

get_spillover <- function(ff, file_id){
  
  if(paste0("INFINISPILL", "_", file_id) %in% names(keyword(ff))){
    
    spill <- parse_infinispill(ff@description[[paste0("INFINISPILL", "_", file_id)]],
                               start = 2)
    
  } else if("INFINISPILL" %in% names(keyword(ff))){
    
    spill <- parse_infinispill(ff@description$INFINISPILL)
    rownames(spill) <- colnames(spill) <- sapply(colnames(spill), function(col){
      stringr::str_split(col, "\\$INFCYTFL\\$")[[1]][file_id]
    })
    
    
  } else {
    
    spill_cols <- grep("SC\\-|Time", colnames(ff), invert = TRUE, value = TRUE)
    spill <- diag(nrow = length(spill_cols))
    dimnames(spill) <- list(spill_cols, spill_cols)
    warning("No spill")
  }
  
  return(spill)
}