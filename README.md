# ExtractInfinicyt
Script to extract fcs files and gating information from infinicyt .CYT files


## Usage instructions

- Install the Java Developers Kit (https://www.oracle.com/ae/java/technologies/downloads/)

- Compile the java files, by opening terminal, moving to the "java" folder of this github repo, and calling `javac .\fcs\reader\PRReader.java`. Example command line input on my Windows system:
```
 > D:
 > cd D:\Git_repos\ExtractInfinicyt\java\
 > javac .\fcs\reader\PRReader.java
```

- In R, source the extractInfinicyt_javawrapper.R

- Run the extract_infinicyt function, passing the path to your cyt file (and if necessary adapt the optional arguments with the paths to your java executable and the folder containing the compiled java files)

```
source("D:/Git_repos/ExtractInfinicyt/extractInfinicyt_javawrapper.R")

filepath <- "ExampleData/AML[RIO][2569].CYT"

infinicyt_res <- extract_infinicyt(filepath = filepath)

colSums(infinicyt_res$`File 1`$manual_matrix)
```

## Disclaimer

Java code provided by Juan Hernández, R wrapper written by Sofie Van Gassen. 
Might not work for all versions of infinicyt / different operating systems / ..., but can hopefully be a useful starting point for other researchers wanting access to manual infinicyt labels in a scripting environment.