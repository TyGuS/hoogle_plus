package cmu.edu.utils;

import java.util.List;
import java.util.ArrayList;
import java.io.FileReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.StringReader;
import java.util.Map;
import java.util.HashMap;

import com.google.gson.Gson;
import com.google.gson.stream.JsonReader;
import java.lang.reflect.Type;
import com.google.gson.reflect.TypeToken;

import cmu.edu.petrinet.Function;
import cmu.edu.petrinet.Param;
import cmu.edu.petrinet.BuildNet;
import cmu.edu.reachability.Encoding;
import cmu.edu.reachability.SequentialEncoding;
import cmu.edu.reachability.EncodingUtil;
import cmu.edu.reachability.Variable;
import cmu.edu.codeformer.CodeFormer;
import org.sat4j.specs.TimeoutException;
import uniol.apt.adt.pn.Flow;
import uniol.apt.adt.pn.PetriNet;
import uniol.apt.adt.pn.Place;
import uniol.apt.adt.pn.Transition;

public class SynquidUtil {
	private static List<Function> functions;
	private static Map<String, Function> functionMap = new HashMap<>();
	private static BuildNet buildNet;
	private static PetriNet net;
	private static Encoding encoding;
	private static List<String> srcTypes;
	private static List<String> args;
	private static String tgtType;
	private static int loc = 1;

	public static void init(List<String> srcs, List<String> argNames, String tgt, String symbols)
		throws FileNotFoundException, IOException, TimeoutException {
		srcTypes = srcs;
		tgtType = tgt;
		args = argNames;
		functionMap.clear();
		getSigs(symbols);
		buildNet = new BuildNet(new ArrayList<>());
		net = buildNet.build(functions, srcTypes);
	}

	public static void buildNextEncoding() {
		// create a formula that has the same semantics as the petri-net
		encoding = new SequentialEncoding(net, loc);
		// set initial state and final state
		encoding.setState(EncodingUtil.setInitialState(net, srcTypes), 0);
        encoding.setState(EncodingUtil.setGoalState(net, tgtType), loc);
	}

	public static List<String> synthesize() {
        // Perform reachability analysis
        // System.out.println("Number of functions:"+functions.size());
        List<Variable> result = Encoding.solver.findPath(loc);
        List<Function> signatures = new ArrayList<>();

        // Increase a location until we have a result
        while (result.isEmpty()){
        	loc++;
        	buildNextEncoding();
        	result = Encoding.solver.findPath(loc);
        }

		for (Variable s : result) {
			Function sig = functionMap.get(s.getName());
			if (sig != null) { // check if s is a line of a code
				signatures.add(sig);
			}
		}
        CodeFormer former = new CodeFormer(signatures, srcTypes, tgtType, args);
        try { 
        	return former.solveHaskell();
        } catch (TimeoutException e) {
			return null;
		}
	}

	// process signatures and construct the Petri Net
	private static void getSigs(String symbols) throws FileNotFoundException  {
		// read all the function signatures from a JSON file
		// JsonReader reader = new JsonReader(new FileReader(sigPath));
		JsonReader reader = new JsonReader(new StringReader(symbols));
		Gson gson = new Gson();
		Type listFuncType = new TypeToken<ArrayList<Function>>(){}.getType();
		functions = gson.fromJson(reader, listFuncType);
		for(Function fun : functions) {
			// System.out.println(fun.getFunName());
			functionMap.put(fun.getFunName(), fun);
		}
		return;
	}

	private static void createPlace(PetriNet p, String pName) {
		if (!p.containsPlace(pName)) {
			p.createPlace(pName);
		}
	}

	private static void createTransition(PetriNet p, String pName) {
		if (!p.containsTransition(pName)) {
			p.createTransition(pName);
		}
	}
}