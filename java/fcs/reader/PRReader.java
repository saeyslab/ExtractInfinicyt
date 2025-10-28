package fcs.reader;

import java.awt.Color;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import fcs.reader.XMLUtil.IElementXML;

public class PRReader {
  private static final String CLASS_NAME_XML = "resultOptions";

  private int populationEvents[] = null;
  private java.util.List<CPopulationData> cpopulations = new ArrayList<>();

  public static void main(String[] args) {
    ZipFile zip = null;
    try {
      if (args.length < 1) {
        System.err.println("usage: arg0 = 'file.cyt | .pr'");
        return;
      }

      File file = new File(args[0]);
      InputStream prFile = null;
      if (file.getName().toLowerCase().endsWith("cyt")) {
        zip = new ZipFile(file);
        ZipEntry entry = zip.getEntry("data.pr");
        if(entry != null) {
          prFile = zip.getInputStream(entry);
        }
      }
      if (prFile == null) {
        prFile = new FileInputStream(file);
      }

      PRReader reader = new PRReader(prFile);
      System.out.println("\nEVENTS TABLE");
      dumpToCSV(reader);

      System.out.println("\nPOPULATION TABLE");
      dumpPopulations(reader.cpopulations);
    } catch (Exception e) {
      e.printStackTrace();
    } finally {
      if(zip != null) {
        try {
          zip.close();
        } catch (IOException e) {
        }
      }
    }
  }

  public static void dumpToCSV(PRReader reader) {
    String separator = ";";
    System.out.println("event" + separator + "populationID");
    for (int i = 0; i < reader.populationEvents.length; i++) {
      System.out.println(i + separator + reader.populationEvents[i]);
    }
  }

  public static void dumpPopulations(List<CPopulationData> cpopulations) {
    for (CPopulationData pob : cpopulations) {
      StringBuilder tmp = new StringBuilder();
      pob.subpopulations.stream().forEach(p -> tmp.append(p.id + ","));
      if (tmp.length() > 0) {
        tmp.setLength(tmp.length() - 1);
      }
      System.out.println("{ id:" + pob.id + ", name:\"" + pob.name + "\", subpopulations: [" + tmp.toString() + "] }");
      dumpPopulations(pob.subpopulations);
    }
  }

  public PRReader() {
  }

  public PRReader(InputStream pr) throws Exception {
    this.read(pr);
  }

  public static class CPopulation {
    public static final String CLASS_NAME_XML = "population";
  }

  public static class CPopulationData implements IElementXML {
    public static final String CLASS_NAME_XML = "population";
    private static final String ID_XML = "id";
    private static final String NAME_XML = "name";
    private static final String COLOR_XML = "color";
    private static final String TYPE_XML = "type";
    private static final String SUBPOPULATIONS_XML = "subpopulations";

    private int id = 0;
    private String populationType = "population";
    private String name = null;
    private Color color = null;
    private List<CPopulationData> subpopulations = new ArrayList<>();

    public CPopulationData(DataInputStream in, int version) throws IOException {
      this.id = version > 1490 && version < 1494 ? in.readInt() : in.readShort();
      this.name = in.readUTF();
      this.color = new Color(in.readInt());
    }

    public CPopulationData(int id) {
      this.id = id;
      this.name = String.valueOf(id);
      this.color = Color.GRAY;
    }

    public CPopulationData(Element param) {
      try {
        this.readXML(param);
      } catch (Exception e) {
        e.printStackTrace();
      }
    }

    @Override
    public String getDefaultNodeName() {
      return CLASS_NAME_XML;
    }

    @Override
    public void readXML(Element param) {
      this.id = XMLUtil.getElementValueInt(param, ID_XML);
      this.name = XMLUtil.getElementValueString(param, NAME_XML);
      this.color = XMLUtil.getElementValueColor(param, COLOR_XML);
      this.populationType = XMLUtil.getElementValueString(param, TYPE_XML);

      Element subpopulations = XMLUtil.getElementByName(param, SUBPOPULATIONS_XML);
      if (subpopulations != null) {
        List<Element> listaSubpopulations = XMLUtil.getAllElementsByName(subpopulations, CPopulation.CLASS_NAME_XML);
        for (Element dato : listaSubpopulations) {
          this.subpopulations.add(new CPopulationData(dato));
        }
      }
    }

    public List<CPopulationData> getSubpopulations() {
      return this.subpopulations;
    }
  }

  public static String readLongUTF(DataInputStream in) throws IOException {
    if (in.readBoolean()) {
      int bytes = in.readInt();
      byte[] b = new byte[bytes];
      in.read(b);
      return new String(b, "UTF-8");
    }
    return null;
  }

  public void read(DataInputStream in) throws Exception {
    Map<Integer, List<Integer>> cpopulationsIListaEvents = new HashMap<>();
    int version = in.readShort();
    int numDatos = 0;
    if (version > 223) {
      numDatos = in.readInt();
      this.populationEvents = new int[numDatos];
      for (int i = 0; i < numDatos; i++) {
        this.populationEvents[i] = in.readInt();
      }
      int numpopulations = 0;
      if (version > 224) {
        numpopulations = in.readInt();
        for (int i = 0; i < numpopulations; i++) {
          CPopulationData datosPopulation = new CPopulationData(in, version);
          this.cpopulations.add(datosPopulation);
        }
      }
      try {
        String info = readLongUTF(in);
        if (info != null) {
          Document document = XMLUtil.getDocument(info);
          if (document != null) {
            this.cpopulations.clear();
            this.cpopulations.add(new CPopulationData(XMLUtil.getElementByName(document, CPopulationData.CLASS_NAME_XML)));
          }
        }
      } catch (EOFException ex) {
      }
    } else {
      try {
        for (; true; numDatos++) {
          Integer key = Integer.valueOf(in.readInt());
          List<Integer> events = cpopulationsIListaEvents.get(key);
          if (events == null) {
            events = new ArrayList<>();
            cpopulationsIListaEvents.put(key, events);
          }
          events.add(numDatos);
        }
      } catch (EOFException ex) {
      }
      this.populationEvents = new int[numDatos];
      for (Integer key : cpopulationsIListaEvents.keySet()) {
        List<Integer> events = cpopulationsIListaEvents.get(key);
        for (Integer element : events) {
          this.populationEvents[element] = key.intValue();
        }
      }
    }
    if (this.cpopulations.isEmpty()) {
      List<Integer> populationsSinRepeticion = new ArrayList<>();
      for (int populationEvento : this.populationEvents) {
        populationsSinRepeticion.add(populationEvento);
      }
      for (Integer pop : populationsSinRepeticion) {
        this.cpopulations.add(new CPopulationData(pop));
      }
    }
  }

  public void read(InputStream ficheroResultados) throws Exception {
    DataInputStream in = null;
    try {
      in = new DataInputStream(ficheroResultados);
      this.read(in);
    } catch (Exception ex) {
      ex.printStackTrace();
    } finally {
      in.close();
    }
  }

  public String getDefaultNodeName() {
    return CLASS_NAME_XML;
  }
}
