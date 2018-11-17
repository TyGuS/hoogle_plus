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

    public static List<Function> getPath() {
        // System.err.println("Current loc is:" + loc);
        List<Variable> result = Encoding.solver.findPath(loc);
        while(result.isEmpty()) {
            loc++;
            buildNextEncoding();
            result = Encoding.solver.findPath(loc);
        }
        // System.err.println(result.toString());
        int variableCounter = 0;
        List<Function> signatures = new ArrayList<>();
        for (Variable s : result) {
            String name = s.getName();
            // remove the name suffix and get the higher-order function name
            int suffixIdx = name.lastIndexOf('|');
            if(suffixIdx != -1) {
                name = name.substring(0, suffixIdx);
            }
            Function sig = functionMap.get(name);
            if (sig != null) { // check if s is a line of a code
                if (signatures.contains(sig) && suffixIdx != -1) {
                    signatures.remove(sig);
                }
                signatures.add(sig);
            }
        }
        return signatures;
    }

    private static void addTypedTerm(Map<String, List<String>> typedTerms, String type, String term) {
        if(typedTerms.containsKey(type)) {
            typedTerms.get(type).add(term);
        } else{
            List<String> list = new ArrayList<>();
            list.add(term);
            typedTerms.put(type, list);
        }
    }

    private static void removeTypedTerm(Map<String, List<String>> typedTerms, String type, String term) {
        if(typedTerms.containsKey(type)) {
            typedTerms.get(type).remove(term);
        }
    }

    private static void addHOParams(Map<String, List<String>> typedTerms, Function function) {
        for(Function f : function.getHoParams()) {
            for(String p : f.getFunParams()) {
                addTypedTerm(typedTerms, p, "x" + xcounter);
                xcounter ++;
            }
            for(Function arg : f.getHoParams()) {
                addHOParams(typedTerms, arg);
            }
        }
    }

    private static List<String> buildFunction(Function function, List<Function> signatures, Map<String, List<String>> typedTerms) {
        List<String> funApplication = new ArrayList<>();
        String name = function.getFunName();
        int funNameEndAt = name.lastIndexOf('_') ;
        if(funNameEndAt != -1){
            name = name.substring(0, funNameEndAt);
        }
        funApplication.add(name);
        // System.err.println("Building function for " + name);
        for(String p : function.getFunParams()) {
            List<String> subterms = new ArrayList<>();
            if (p.charAt(0) == 'f') { // if this is a HO argument
                for(Function hof : function.getHoParams()) {
                    if(hof.getFunName().equals(p)) {
                        // System.err.println("Looking for higher order parameters for " + name);
                        String hoFunctionHeader = "\\";
                        for(String param : hof.getFunParams()) {
                            hoFunctionHeader += "x" + xcounter + " ";
                            xcounter++;
                        }
                        hoFunctionHeader += "-> ";
                        for(String t : typedTerms.get(hof.getFunReturn())) {
                            subterms.add("(" + hoFunctionHeader + t + ")");
                        }
                        break;
                    }
                }
            } else {
                subterms = typedTerms.get(p);
            }
            List<String> partialApplication = new ArrayList<>();
            if(subterms == null) {
                // System.err.println("Cannot find terms with type " + p);
                // System.err.println("Current typedTerms are");
                for(String t : typedTerms.keySet()) {
                    // System.err.println(t + "," + typedTerms.get(t).toString());
                }
                return new ArrayList<String>();
            } else {
                for(String subterm : subterms) {
                    for(String fun : funApplication) {
                        partialApplication.add(fun + " " + subterm);
                    }
                    // sat = !former.isUnsat();
                }
                funApplication = partialApplication;
            }
        }
        // System.err.println(funApplication.toString());
        // add new build terms into the map
        for(String app : funApplication) {
            addTypedTerm(typedTerms, function.getFunReturn(), "(" + app + ")");
        }
        
        return funApplication;
    }

    private static boolean hasAllFunctions(List<Function> signatures, String code) {
        for(Function f : signatures) {
            String name = f.getFunName();
            int funNameEndAt = name.lastIndexOf('_') ;
            if(funNameEndAt != -1){
                name = name.substring(0, funNameEndAt);
            }
            if(!code.contains(name)) {
                // System.err.println("Does not include " + name);
                return false;
            } else {
                code = code.replace(name, "");
                // System.err.println(code);
            }
        }
        for(String arg : args) {
            if(!code.contains(arg)) {
                // System.err.println("Does not include " + arg);
                return false;
            } else {
                code = code.replace(arg, "");
                // System.err.println(code);
            }
        }
        // for(String arg : hoArgs) {
        //     if(!code.contains(arg)) {
        //         System.err.println("Does not include " + arg + " in " + code);
        //         return false;
        //     } else {
        //         code = code.replace(arg, "");
        //         System.err.println(code);
        //     }
        // }
        return true;
    }

    private static List<String> fillSketch(List<Function> signatures, List<String> sources, List<String> arguments) {
        // start from the last one to fill the holes
        xcounter = 0;

        // build the type map
        Map<String, List<String>> typedTerms = new HashMap<>();
        for(int i = 0; i < sources.size(); i++) {
            addTypedTerm(typedTerms, sources.get(i), arguments.get(i));
        }
        for(Function f : signatures) {
            addHOParams(typedTerms, f);
        }

        // restore variable counter
        xcounter = 0;
        List<String> res = new ArrayList<>();
        //start from beginning, each hole can only be filled with terms before it
        for(Function f : signatures) {
            res = buildFunction(f, signatures.subList(0, signatures.indexOf(f)), typedTerms);
        }

        return res;
    }

    public static List<String> synthesize() {
        List<Function> signatures = getPath();
        List<String> res = fillSketch(signatures, srcTypes, args);
        List<String> resultCode = new ArrayList<>();
        for(String program : res) {
            // System.err.println(program);
            if(hasAllFunctions(signatures, program)) {
                resultCode.add(program);
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
            // fun.print();
        }
        // System.err.println("Total number of functions:" + functions.size());
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
