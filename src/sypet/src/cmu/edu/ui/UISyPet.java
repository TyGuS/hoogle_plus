package cmu.edu.ui;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.io.output.ByteArrayOutputStream;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.sat4j.specs.TimeoutException;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.stream.JsonReader;

import cmu.edu.codeformer.CodeFormer;
import cmu.edu.compilation.Test;
import cmu.edu.parser.CacheMethodSignature;
import cmu.edu.parser.JarParser;
import cmu.edu.parser.JsonParser;
import cmu.edu.parser.MethodSignature;
import cmu.edu.parser.SyPetConfig;
import cmu.edu.parser.SyPetInput;
import cmu.edu.petrinet.BuildNet;
import cmu.edu.reachability.Encoding;
import cmu.edu.reachability.EncodingUtil;
import cmu.edu.reachability.SequentialEncoding;
import cmu.edu.reachability.Variable;
import uniol.apt.adt.pn.PetriNet;
import uniol.apt.adt.pn.Place;
import uniol.apt.adt.pn.Transition;

/**
 * This represents the UI for SyPet to be called by other applications.
 * 
 * @author Ruben Martins
 */
public class UISyPet {

	private List<MethodSignature> sigs;
	private Map<String, Set<String>> superclassMap;
	private Map<String, Set<String>> subclassMap;
	private List<String> packages;
	private List<String> libs;

	private PetriNet net;
	private Map<String, MethodSignature> signatureMap;

	private List<String> inputs;
	private String retType;
	private String testCode;
	private String methodName;
	private List<String> varNames;
	private List<String> hints;
	
	private BuildNet buildNet;
	
	// TODO: refactor this code
	public UISyPet(List<String> packages, List<String> libs, List<String> soot, List<String> hints) {
		
		buildNet = null;
		net = null;
		sigs = null;
		this.hints = hints;
					
		this.packages = packages;
		this.libs = libs;
		
		String configPath = "./config/config.json";
		List<List<String>> globalSuperClasses = new ArrayList<>();
		ArrayList<String> poly1 = new ArrayList<String>(Arrays.asList("java.lang.CharSequence","java.lang.String"));
		globalSuperClasses.add(poly1);
		SyPetConfig jsonConfig = new SyPetConfig(new ArrayList<>(), globalSuperClasses, new ArrayList<>(), new ArrayList<>());
		
		Path path = Paths.get(configPath);

		if (Files.exists(path))
			jsonConfig = JsonParser.parseJsonConfig(configPath);
		
		Set<String> localSuperClasses = new HashSet<>();
		localSuperClasses.addAll(jsonConfig.localSuperClasses);
		
		// suppress warnings from Soot
		PrintStream origOutput = System.out;
		PrintStream newOutput = new PrintStream(new ByteArrayOutputStream());
		System.setOut(newOutput);
		
		JarParser parser = new JarParser(libs, soot);
		long start = System.nanoTime();
		this.sigs = parser.parseJar(libs, packages, jsonConfig.blacklist);
		long end = System.nanoTime();
		System.out.println("Time for parsing jars: " + ((end - start)/100000000.0));
		this.superclassMap = JarParser.getSuperClasses(localSuperClasses);
		for (List<String> poly : jsonConfig.globalSuperClasses) {
			assert (poly.size() == 2);
			
			if (this.superclassMap.containsKey(poly.get(0))) {
				this.superclassMap.get(poly.get(0)).add(poly.get(1));
			} else {
				Set<String> subclass = new HashSet<>();
				subclass.add(poly.get(1));
				this.superclassMap.put(poly.get(0), subclass);
			}
			
		}
		this.subclassMap = new HashMap<>();
		for (String key : superclassMap.keySet()) {
			for (String value : superclassMap.get(key)) {
				if (!subclassMap.containsKey(value)) {
					subclassMap.put(value, new HashSet<String>());
				}
				subclassMap.get(value).add(key);
			}
		}

		System.setOut(origOutput);
		buildNet = new BuildNet(jsonConfig.noSideEffects);
		try {
			net = buildNet.build(sigs, superclassMap, subclassMap, new ArrayList<>(), true);
		} catch (IOException e) {
			e.printStackTrace();
		}
		signatureMap = BuildNet.dict;
		// System.out.println("c #Transitions = " + net.getTransitions().size());
		// System.out.println("c #Places = " + net.getPlaces().size());
		
	}

	public UISyPet(List<String> packages, List<String> libs, List<String> hints) {
		this.packages = packages;
		this.libs = libs;
		this.hints = hints;
		
		String configPath = "./config/config.json";
		List<List<String>> globalSuperClasses = new ArrayList<>();
		ArrayList<String> poly1 = new ArrayList<String>(Arrays.asList("java.lang.CharSequence","java.lang.String"));
		globalSuperClasses.add(poly1);
		SyPetConfig jsonConfig = new SyPetConfig(new ArrayList<>(), globalSuperClasses, new ArrayList<>(), new ArrayList<>());
		
		Path path = Paths.get(configPath);

		if (Files.exists(path))
			jsonConfig = JsonParser.parseJsonConfig(configPath);
		
		Set<String> localSuperClasses = new HashSet<>();
		localSuperClasses.addAll(jsonConfig.localSuperClasses);
		
		// suppress warnings from Soot
		PrintStream origOutput = System.out;
		// PrintStream newOutput = new PrintStream(new ByteArrayOutputStream());
		// System.setOut(newOutput);
		
		JarParser parser = new JarParser(libs);
		this.sigs = parser.parseJar(libs, packages, jsonConfig.blacklist);
		// System.out.println(sigs.toString());
		this.superclassMap = JarParser.getSuperClasses(localSuperClasses);

		for (List<String> poly : jsonConfig.globalSuperClasses) {
			assert (poly.size() == 2);
			
			if (this.superclassMap.containsKey(poly.get(0))) {
				this.superclassMap.get(poly.get(0)).add(poly.get(1));
			} else {
				Set<String> subclass = new HashSet<>();
				subclass.add(poly.get(1));
				this.superclassMap.put(poly.get(0), subclass);
			}
			
		}
		this.subclassMap = new HashMap<>();
		for (String key : superclassMap.keySet()) {
			for (String value : superclassMap.get(key)) {
				if (!subclassMap.containsKey(value)) {
					subclassMap.put(value, new HashSet<String>());
				}
				subclassMap.get(value).add(key);
			}
		}
		
		System.setOut(origOutput);
		buildNet = new BuildNet(jsonConfig.noSideEffects);
		try {
			net = buildNet.build(sigs, superclassMap, subclassMap, new ArrayList<>(), true);
			System.out.println("Places: " + net.getPlaces().toString());
			System.out.println("Transitions: " + net.getTransitions().toString());
		} catch (IOException e) {
			e.printStackTrace();
		}
		signatureMap = BuildNet.dict;
		System.out.println("c #Transitions = " + net.getTransitions().size());
		System.out.println("c #Places = " + net.getPlaces().size());
	}

	public void setSignature(String methodName, List<String> paramNames, List<String> srcTypes, String tgtType,
			String testCode) {

		this.inputs = srcTypes;
		this.retType = tgtType;
		this.testCode = testCode;
		this.varNames = paramNames;
		this.methodName = methodName;

		buildNet.setMaxTokens(srcTypes);
	}
	
	public String synthesize(int min_loc, int max_loc) {

		int loc = min_loc;
		boolean solution = false;
		String synthesizedCode = "";
		String code;
		int paths = 0;
		int programs = 0;

		while (!solution && loc <= max_loc) {
			// create a formula that has the same semantics as the petri-net
			Encoding encoding = new SequentialEncoding(net, loc);
			// set initial state and final state
			encoding.setState(EncodingUtil.setInitialState(net, inputs), 0);
			encoding.setState(EncodingUtil.setGoalState(net, retType), loc);
			encoding.setHints(hints);

			// 4. Perform reachability analysis

			// for each loc find all possible programs
			List<Variable> result = Encoding.solver.findPath(loc);
			paths++;
			while (!result.isEmpty() && !solution) {
				// System.out.println(result.toString());
				List<String> apis = new ArrayList<String>();
				// A list of method signatures
				List<MethodSignature> signatures = new ArrayList<>();
				for (Variable s : result) {
					apis.add(s.getName());
					MethodSignature sig = signatureMap.get(s.getName());
					if (sig != null) { // check if s is a line of a code
						signatures.add(sig);
					}
				}
				// 5. Convert a path to a program
				boolean sat = true;
				CodeFormer former = new CodeFormer(signatures, inputs, retType, varNames, methodName, subclassMap,
						superclassMap);
				while (sat) {
					try {
						code = former.solve();
						// System.out.println(code.toString());
						programs++;
					} catch (TimeoutException e) {
						sat = false;
						break;
					}
					sat = !former.isUnsat();
					// 6. Run the test cases
					boolean compre = false;
					// System.out.println("code = " + code);
					// System.out.println("testCode = " + testCode);
					try {
						compre = Test.runTest(code, testCode, libs);
					} catch (IOException e) {
						e.printStackTrace();
					}
					if (compre) {
						solution = true;
						synthesizedCode = code;
						break;
					}

				}
				// the current path did not result in a program that passes all test cases find the next path
				paths++;
				result = Encoding.solver.findPath(loc);
			}

			// we did not find a program of length = loc
			loc++;
		}
		System.out.println("c #Programs explored = " + programs);
		System.out.println("c #Paths explored = " + paths);
		return synthesizedCode;

	}

	public List<String> synthesizeAll(int max_loc) {
		ArrayList<String> allCode = new ArrayList<>();

		int loc = 1;
		boolean solution = false;
		String code = "";

		while (!solution && loc <= max_loc) {
			// create a formula that has the same semantics as the petri-net
			Encoding encoding = new SequentialEncoding(net, loc);
			// set initial state and final state
			encoding.setState(EncodingUtil.setInitialState(net, inputs), 0);
			encoding.setState(EncodingUtil.setGoalState(net, retType), loc);
			encoding.setHints(hints);

			// 4. Perform reachability analysis

			// for each loc find all possible programs
			List<Variable> result = Encoding.solver.findPath(loc);
			while (!result.isEmpty() && !solution) {
				List<String> apis = new ArrayList<String>();
				// A list of method signatures
				List<MethodSignature> signatures = new ArrayList<>();
				for (Variable s : result) {
					apis.add(s.getName());
					MethodSignature sig = signatureMap.get(s.getName());
					if (sig != null) { // check if s is a line of a code
						signatures.add(sig);
					}
				}
				// 5. Convert a path to a program
				boolean sat = true;
				CodeFormer former = new CodeFormer(signatures, inputs, retType, varNames, methodName, subclassMap,
						superclassMap);
				while (sat) {
					try {
						code = former.solve();
					} catch (TimeoutException e) {
						sat = false;
						break;
					}
					sat = !former.isUnsat();
					// 6. Run the test cases
					boolean compre = false;
					try {
						compre = Test.runTest(code, testCode, this.libs);
					} catch (IOException e) {
						e.printStackTrace();
					}

					if (compre) {
						allCode.add(code);
					}

					// the current path did not result in a program that passes all test cases find
					// the next path
					result = Encoding.solver.findPath(loc);
				}
			}

			// we did not find a program of length = loc
			loc++;
		}
		return allCode;
	}

	private void loadCache() {
		// CacheMethodSignature jobj = new
		// Gson().fromJson("./cache/cmu.symonster/cmu.symonster.json",
		// CacheMethodSignature.class);
		
		JsonReader jsonReader;
		try {
			jsonReader = new JsonReader(new FileReader("./cache/cmu.symonster/cmu.symonster.json"));

			jsonReader.beginObject();
			String name = jsonReader.nextName();
			jsonReader.beginArray();
			jsonReader.beginObject();
			int pos = 0;
			
			while (jsonReader.hasNext()) {
				
				if (pos == 0) {
					name = jsonReader.nextName();
					if (name.startsWith("is")) {
						//System.out.println("name= " + name);
						if(jsonReader.nextBoolean()) {
							name = "true";
						} else name = "false";
						pos = 0;
					} else pos++;
				} else if (pos == 1) {
					name = jsonReader.nextString();
					pos = 0;
				}

				//System.out.println("name= " + name);
				
			}

			jsonReader.endObject();
			jsonReader.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}

	}

}
