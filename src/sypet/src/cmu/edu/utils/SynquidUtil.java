package cmu.edu.utils;

import java.util.List;
import java.util.ArrayList;
import java.io.FileReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.StringReader;
import java.util.Map;
import java.util.Set;
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
import cmu.edu.codeformer.FormerResult;
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
    private static List<String> toExclude;
    private static int variableCounter = 0;

    public static void init(List<String> srcs, List<String> argNames, String tgt, String symbols, 
                            List<String> solutions, Integer startLoc)
        throws FileNotFoundException, IOException, TimeoutException {
        long start = System.nanoTime();
        srcTypes = srcs;
        tgtType = tgt;
        args = argNames;
        functionMap.clear();
        getSigs(symbols);
        buildNet = new BuildNet(new ArrayList<>());
        net = buildNet.build(functions, srcTypes);
        toExclude = solutions;
        loc = startLoc;
        long end = System.nanoTime();
        // System.out.println("Time for constructing the Petri Net: " + ((end-start)/1000000000.0));
    }

    public static void buildNextEncoding() {
        // create a formula that has the same semantics as the petri-net
        encoding = new SequentialEncoding(net, loc);
        // set initial state and final state
        encoding.setState(EncodingUtil.setInitialState(net, srcTypes), 0);
        encoding.setState(EncodingUtil.setGoalState(net, tgtType), loc);
    }

    public static String synthesize() {
        // Perform reachability analysis
        long start = System.nanoTime();
        // Increase a location until we have a result
        while (true){
            System.out.println("Current loc is:" + loc);
            List<Variable> result = Encoding.solver.findPath(loc);
            while (!result.isEmpty()){
                System.out.println(result.toString());
                List<Function> signatures = new ArrayList<>();
                for (Variable s : result) {
                    String name = s.getName();
                    // each time we see an entry point, add all the parameters as variables
                    if(name.contains("|entry")) {
                        Transition tr = net.getTransition(name);
                        for (Flow f : tr.getPostsetEdges()) {
                            Place p = f.getPlace();
                            if(!p.getId().contains("|")){
                                signatures.add(new Function(new ArrayList<String>(), new ArrayList<Function>(), p.getId(), "x"+variableCounter));
                                variableCounter += 1;
                            }
                        }
                    }
                    // remove the name suffix and get the higher-order function name
                    int suffixIdx = name.lastIndexOf('|');
                    if(suffixIdx != -1) {
                        name = name.substring(0, suffixIdx);
                    }
                    Function sig = functionMap.get(name);
                    if (sig != null) { // check if s is a line of a code
                        if (signatures.contains(sig)) {
                            signatures.remove(sig);
                        }
                        signatures.add(sig);
                    }
                }
                CodeFormer former = new CodeFormer(signatures, srcTypes, tgtType, args);
                boolean sat = true;
                while (sat) {
                    try {
                        FormerResult formerResult = former.solveHaskell();
                        if (toExclude.contains(formerResult.getCode())) {
                            throw new TimeoutException();
                        }
                        formerResult.setLoc(loc);
                        Gson gson = new Gson();
                        String res = gson.toJson(formerResult);
                        long end = System.nanoTime();
                        System.out.println(formerResult.getCode());
                        // System.out.println("Time for finding a solution: " + ((end-start)/1000000000.0));
                        return res;
                    } catch (TimeoutException e) {
                        sat = false;
                        break;
                    }
                    // sat = !former.isUnsat();
                }
                result = Encoding.solver.findPath(loc);
            }
            loc++;
            buildNextEncoding();
        }
        // return "";
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