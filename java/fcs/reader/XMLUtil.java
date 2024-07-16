package fcs.reader;

import java.awt.Color;
import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class XMLUtil {
	public interface IElementXML {
		public static final String VERSION_XML = "version";

		public default void readXML(Element parent, String name) throws Exception {
			this.readXML(parent);
		}

		public void readXML(Element param) throws Exception;

		public String getDefaultNodeName(); // Siempre debe devolver un valor distinto de NULL
	}

	public static void readXML(Element param, IElementXML elemento, String elemenName, boolean returnNull)
			throws Exception {
		Element raiz = null;
		if (!elemenName.equals(param.getNodeName())) {
			raiz = getElementByName(param, elemenName);
			if (raiz != null) {
				param = raiz;
			} else if (returnNull) {
				param = null;
			}
		}
		elemento.readXML(param, elemenName);
	}

	public static Integer getElementValueInt(Element param, String name) {
		return getElementValueInt(param, name, 0);
	}

	public static Integer getElementValueInt(Element param, String name, Integer defaultValue) {
		Element elemento = checkElement(param, name);
		if (elemento != null) {
			return Integer.valueOf(elemento.getTextContent());
		}
		return defaultValue;
	}

	public static Long getElementValueLong(Element param, String name) {
		return getElementValueLong(param, name, new Long(0));
	}

	public static Long getElementValueLong(Element param, String name, Long defaultValue) {
		Element elemento = checkElement(param, name);
		if (elemento != null) {
			return Long.valueOf(elemento.getTextContent()).longValue();
		}
		return defaultValue;
	}

	public static Float getElementValueFloat(Element param, String name) {
		return getElementValueFloat(param, name, 0f);
	}

	public static Float getElementValueFloat(Element param, String name, Float defaultValue) {
		Element elemento = checkElement(param, name);
		if (elemento != null) {
			try {
				return Float.parseFloat(elemento.getTextContent());
			} catch (Exception e) {
				e.printStackTrace();
				return defaultValue;
			}
		}
		return defaultValue;
	}

	public static Double getElementValueDouble(Element param, String name) {
		return getElementValueDouble(param, name, 0.0);
	}

	public static Double getElementValueDouble(Element param, String name, Double defaultValue) {
		Element elemento = checkElement(param, name);
		if (elemento != null) {
			try {
				return Double.parseDouble(elemento.getTextContent());
			} catch (Exception e) {
				e.printStackTrace();
				return defaultValue;
			}
		}
		return defaultValue;
	}

	public static List<Element> getAllElementsByName(Element param, List<String> names) {
		return getAllElementsByName(param, names, false);
	}

	public static List<Element> getAllElementsByName(Element param, String name) {
		return getAllElementsByName(param, name, false);
	}

	public static List<Element> getAllElementsByName(Element param, String name, boolean recursive) {
		List<String> names = new ArrayList<>();
		names.add(name);
		return getAllElementsByName(param, names, recursive);
	}

	private static List<Element> getAllElementsByName(Element param, List<String> names, boolean recursive) {
		List<Element> listaElementos = new ArrayList<>();
		if (param != null) {
			if (!recursive) {
				NodeList nodes = param.getChildNodes();
				for (int i = 0; i < nodes.getLength(); i++) {
					Node node = nodes.item(i);
					if (node.getNodeType() == Node.ELEMENT_NODE && names.contains(((Element) node).getNodeName())) {
						listaElementos.add((Element) node);
					}
				}
			} else {
				NodeList nodesTmp = param.getChildNodes();
				for (int i = 0; i < nodesTmp.getLength(); i++) {
					if (nodesTmp.item(i) instanceof Element) {
						Element node = (Element) nodesTmp.item(i);
						if (names.contains(node.getTagName())) {
							listaElementos.add(node);
						}
						listaElementos.addAll(getAllElementsByName(node, names, recursive));
					}
				}
			}
		}
		return listaElementos;
	}

	public static Element getElementByName(Element param, String name) {
		if (param == null) {
			return null;
		}
		NodeList nodes = param.getChildNodes();
		for (int i = 0; i < nodes.getLength(); i++) {
			Node node = nodes.item(i);
			if (node.getNodeType() == Node.ELEMENT_NODE && name.equals(((Element) node).getNodeName())) {
				return (Element) node;
			}
		}
		return null;
	}

	public static Element getElementByName(Document doc, String name) {
		if (doc != null) {
			NodeList lista = doc.getElementsByTagName(name);
			if (lista.getLength() > 0) {
				return (Element) lista.item(0);
			}
		}
		return null;
	}

	public static List<Element> getAllElementsByName(Document doc, String name) {
		List<Element> listaElementos = new ArrayList<>();
		if (doc != null) {
			NodeList lista = doc.getElementsByTagName(name);
			if (lista.getLength() > 0) {
				for (int i = 0; i < lista.getLength(); i++) {
					listaElementos.add((Element) lista.item(i));
				}
			}
		}
		return listaElementos;
	}

	public static Element checkElement(Element param, String name) {
		if (name == null) {
			return param;
		}
		Element elemento = param;
		if (param != null && !param.getNodeName().equals(name)) {
			elemento = getElementByName(param, name);
			if (elemento == null) {
				return null;
			}
		}
		return elemento;
	}

	public static Element checkElement(Element param, String name, int index) {
		Element elemento = param;
		if (!param.getNodeName().equals(name)) {
			List<Element> elementos = getAllElementsByName(param, name, false);
			if (elementos == null || elementos.size() <= index) {
				return null;
			}
			elemento = elementos.get(index);
		}
		return elemento;
	}

	public static Document getDocument(String xml) throws Exception {
		if (xml != null && !xml.trim().equals("")) {
			return getNewDocumentBuilder().parse(new ByteArrayInputStream(xml.getBytes("UTF-8")));
		}
		return null;
	}

	private static DocumentBuilder getNewDocumentBuilder() throws ParserConfigurationException {
		DocumentBuilder builder;
		builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		return builder;
	}

	public static Document newDocument() throws ParserConfigurationException {
		return getNewDocumentBuilder().newDocument();
	}

	public static Color getElementValueColor(Element param, String name) {
		Element elemento = checkElement(param, name);
		if (elemento != null) {
			return obtainXMLColor(elemento.getTextContent());
		}
		return null;
	}

	public static Color obtainXMLColor(String colorXML) {
		if (colorXML == null || colorXML.trim().equals("")) {
			return null;
		}
		String colores = colorXML;
		if (colorXML.charAt(0) == '#') {
			colores = colorXML.substring(1); // quitamos el #
		}
		int red = Integer.decode("0x" + colores.substring(0, 2)).intValue();
		int green = Integer.decode("0x" + colores.substring(2, 4)).intValue();
		int blue = Integer.decode("0x" + colores.substring(4, 6)).intValue();

		return new Color(red, green, blue);
	}

	public static String getElementValueString(Element param, String name) {
		return getElementValueString(param, name, null);
	}

	public static final String NULL_ATTRIBUTE_XML = "null";

	public static String getElementValueString(Element param, String name, String defaultValue) {
		Element elemento = checkElement(param, name);
		if (elemento != null) {
			String value = elemento.getTextContent();
			if (value == null || value.trim().equals("")) {
				Boolean nullAtrib = getAttributeBoolean(elemento, NULL_ATTRIBUTE_XML);
				if (nullAtrib) {
					return null;
				}
			}
			return value;
		}
		return defaultValue;
	}

	public static Boolean getAttributeBoolean(Element param, String name) {
		return getAttributeBoolean(param, name, false);
	}

	public static Boolean getAttributeBoolean(Element param, String name, Boolean defaultValue) {
		if (!existAttribute(param, name)) {
			return defaultValue;
		}
		return new Boolean(param.getAttribute(name));
	}

	private static boolean existAttribute(Element param, String name) {
		return existAttribute(param, name, true);
	}

	private static boolean existAttribute(Element param, String name, boolean checkEmpty) {
		if (param == null || !param.hasAttribute(name) || param.getAttribute(name) == null
				|| checkEmpty && param.getAttribute(name).trim().equals("")) {
			return false;
		}
		return true;
	}
}
