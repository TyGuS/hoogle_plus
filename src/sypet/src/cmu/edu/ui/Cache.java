package cmu.edu.ui;

import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.gson.stream.JsonWriter;
import cmu.edu.parser.JarParser;
import cmu.edu.parser.JsonParser;
import cmu.edu.parser.MethodSignature;
import cmu.edu.parser.SyPetConfig;
import soot.Type;

public class Cache {

	private static List<MethodSignature> sigs;
	private Map<String, Set<String>> superclassMap;

	public Cache(List<String> packages, List<String> libs) {
		SyPetConfig jsonConfig = JsonParser.parseJsonConfig("config/config.json");
		Set<String> acceptableSuperClasses = new HashSet<>();
		acceptableSuperClasses.addAll(jsonConfig.localSuperClasses);

		JarParser parser = new JarParser(libs);
		Cache.sigs = parser.parseJar(libs, packages, jsonConfig.blacklist);
		this.superclassMap = JarParser.getSuperClasses(acceptableSuperClasses);
	}

	public void cacheMethods(String filename, String package_name) {

		JsonWriter writer;
		try {
			writer = new JsonWriter(new FileWriter(filename));
			writer.setIndent("  ");
			writer.beginObject();
			writer.name(package_name);
		    writer.beginArray();
			for (MethodSignature method : sigs) {
				writer.beginObject();
				writer.name("name").value(method.getName());
				writer.name("retType").value(method.getRetType().toString());
				for (Type type : method.getArgTypes()) {
					writer.name("argType").value(type.toString());
				}
				writer.name("isStatic").value(method.getIsStatic());
				writer.name("hostClass").value(method.getHostClass().toString());
				writer.name("isConstructor").value(method.getIsConstructor());
				writer.name("method").value(method.getMethod().toString());
				writer.endObject();
			}
			writer.endArray();
			writer.endObject();
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static void main(String[] args) {
		
		String package_name = "javax.swing";
		String library = "lib/rt8.jar";

		ArrayList<String> packages = new ArrayList<String>(Arrays.asList(package_name));
		ArrayList<String> libs = new ArrayList<String>(Arrays.asList(library));

		Cache cache = new Cache(packages, libs);
		cache.cacheMethods(package_name+".json", package_name);

	}

}
