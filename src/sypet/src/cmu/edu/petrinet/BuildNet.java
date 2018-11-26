/**
 * BSD 3-Clause License
 *
 *
 *  Copyright (c) 2018, SyPet 2.0 - Ruben Martins, Yu Feng, Isil Dillig
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  * Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 *  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package cmu.edu.petrinet;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import soot.Type;

import cmu.edu.utils.SynquidUtil;

import cmu.edu.parser.MethodSignature;
import uniol.apt.adt.exception.NoSuchEdgeException;
import uniol.apt.adt.exception.NoSuchNodeException;
import uniol.apt.adt.pn.Flow;
import uniol.apt.adt.pn.PetriNet;
import uniol.apt.adt.pn.Place;
import uniol.apt.adt.pn.Transition;

/**
 * Build petri net from a set of libraries.
 *
 * @author Ruben Martins
 * @author Anlun Xu
 */
public class BuildNet {
    static public PetriNet petrinet = new PetriNet("net");
    // A map from transition name to a method signature
    static public Map<String, MethodSignature> dict = new HashMap<String, MethodSignature>();

    static private Map<String, List<String>> superDict = new HashMap<>();
    static private Map<String, List<String>> subDict = new HashMap<>();

    static private List<String> noSideEffects = new ArrayList<>();

    public BuildNet(List<String> noSideEffects) {
        petrinet = new PetriNet("net");
        dict = new HashMap<String, MethodSignature>();
        superDict = new HashMap<>();
        subDict = new HashMap<>();
        this.noSideEffects = noSideEffects;
    }

    private static void generatePolymophism(Transition t, int count, List<Place> inputs, Stack<Place> polyInputs) {
        if (inputs.size() == count) {
            boolean skip = true;
            for (int i = 0; i < inputs.size(); i++) {
                if (!inputs.get(i).equals(polyInputs.get(i))) {
                    skip = false;
                }
            }
            if (skip) {
                return;
            }

            String newTransitionName = t.getId() + "Poly:(";
            for (Place p : polyInputs) {
                newTransitionName += p.getId() + " ";
            }
            newTransitionName += ")";

            if (petrinet.containsTransition(newTransitionName)) {
                return;
            }

            boolean polymorphicOutput = false;
            for (Flow f : t.getPostsetEdges()) {
                Place p = f.getPlace();
                List<String> subClasses = subDict.get(p.getId());
                if (subClasses != null){
                    polymorphicOutput = true;
                    break;
                }
                if (polymorphicOutput)
                    break;
            }

            Transition newTransition = petrinet.createTransition(newTransitionName);
            for (Place p : polyInputs) {
                // NOTE: why is the weight of the flow restricted to 1?
                addFlow(p.getId(), newTransitionName, 1);
            }

            for (Flow f : t.getPostsetEdges()) {
                Place p = f.getPlace();
                int w = f.getWeight();
                petrinet.createFlow(newTransition, p, w);
            }
            dict.put(newTransitionName, dict.get(t.getId()));

            if (polymorphicOutput){

                for (Flow f : t.getPostsetEdges()) {
                    Place p = f.getPlace();
                    List<String> subClasses = subDict.get(p.getId());
                    for (String s : subClasses){
                        if (!petrinet.containsPlace(s))
                            continue;
                        String newPolyTransitionName = newTransitionName+"(" + s + ")";
                        assert (!petrinet.containsTransition(newPolyTransitionName));
                        newTransition = petrinet.createTransition(newPolyTransitionName);
                        for (Place p2 : polyInputs) {
                            addFlow(p2.getId(), newPolyTransitionName, 1);
                        }
                        int w = f.getWeight();
                        petrinet.createFlow(newTransition, petrinet.getPlace(s), w);
                        dict.put(newPolyTransitionName, dict.get(t.getId()));
                    }
                }

            }

        } else {
            Place p = inputs.get(count);
            List<String> subClasses = subDict.get(p.getId());
            if (subClasses == null) { // No possible polymophism
                polyInputs.push(p);
                generatePolymophism(t, count + 1, inputs, polyInputs);
                polyInputs.pop();
                return;
            } else {
                for (String subclass : subClasses) {
                    addPlace(subclass);
                    Place polyClass = petrinet.getPlace(subclass);
                    polyInputs.push(polyClass);
                    generatePolymophism(t, count + 1, inputs, polyInputs);
                    polyInputs.pop();
                }
                return;
            }
        }
    }

    private static void copyPolymorphism() {
        // Handles polymorphism by creating copies for each method that
        // has superclass as input type
        for (Transition t : petrinet.getTransitions()) {
            List<Place> inputs = new ArrayList<>();
            Set<Flow> inEdges = t.getPresetEdges();
            for (Flow f : inEdges) {
                for (int i = 0; i < f.getWeight(); i++) {
                    inputs.add(f.getPlace());
                }
            }
            Stack<Place> polyInputs = new Stack<>();
            generatePolymophism(t, 0, inputs, polyInputs);
        }
    }

    // This method handles polymorphism by creating methods that transforms each
    // subclass into its super class
    private static void normalPolymorphism() {
        for (String subClass : superDict.keySet()) {
            addPlace(subClass);
            for (String superClass : superDict.get(subClass)) {
                addPlace(superClass);
                String methodName = subClass + "IsPolymorphicTo" + superClass;
                petrinet.createTransition(methodName);
                petrinet.createFlow(subClass, methodName, 1);
                petrinet.createFlow(methodName, superClass, 1);
            }
        }
    }

    private static void getPolymorphismInformation(Map<String, Set<String>> superClassMap,
            Map<String, Set<String>> subClassMap) {
        for (String s : superClassMap.keySet()) {
            if (!petrinet.containsPlace(s))
                continue;

            Set<String> superClasses = superClassMap.get(s);
            if (superClasses.size() != 0) {
                List<String> superClassList = new ArrayList<>(superClasses);
                superDict.put(s, superClassList);
            }
        }
        for (String s : subClassMap.keySet()) {
            if (!petrinet.containsPlace(s))
                continue;
            
            Set<String> subClasses = subClassMap.get(s);
            if (subClasses.size() != 0) {
                List<String> subClassList = new ArrayList<>(subClasses);
                subDict.put(s, subClassList);
            }
        }
    }

    private static void addPlace(String placeID) {
        //placeID = placeID.replace("$", ".");
        try {
            petrinet.getPlace(placeID);
        } catch (NoSuchNodeException e) {
            petrinet.createPlace(placeID);
            // petrinet.createTransition(placeID + "Clone");
            //petrinet.getPlace(placeID).setMaxToken(2);
            // petrinet.createFlow(placeID, placeID + "Clone", 1);
            // petrinet.createFlow(placeID + "Clone", placeID, 2);
        }
    }

    private static void addFlow(String ID1, String ID2, int weight) {
        Flow f;
        try {
            f = petrinet.getFlow(ID1, ID2);
            f.setWeight(f.getWeight() + weight);
        } catch (NoSuchEdgeException e) {
            petrinet.createFlow(ID1, ID2, weight);
        }
    }

    // TODO: clean the code in this method
    private static void addVoidTransition(MethodSignature methodSig) {
        String methodname = methodSig.getName();
        boolean isStatic = methodSig.getIsStatic();
        boolean isConstructor = methodSig.getIsConstructor();
        String className = methodSig.getHostClass().getName();
        String transitionName = "(Void)";
        List<Type> args = methodSig.getArgTypes();

        if (isConstructor) {
            transitionName += methodname + "(Constructor)" + "(";
            for (Type t : args) {
                transitionName += t.toString() + " ";
            }
            transitionName += ")";
            transitionName += methodSig.getRetType();
            petrinet.createTransition(transitionName);
        } else if (isStatic) {
            transitionName += "(static)" + className + "." + methodname + "(";
            for (Type t : args) {
                transitionName += t.toString() + " ";
            }
            transitionName += ")";
            transitionName += methodSig.getRetType();
            petrinet.createTransition(transitionName);
        } else { // The method is not static, so it has an extra argument
            transitionName += className + "." + methodname + "(";
            transitionName += className + " ";
            for (Type t : args) {
                transitionName += t.toString() + " ";
            }
            transitionName += ")";
            transitionName += methodSig.getRetType();
            petrinet.createTransition(transitionName);

            addPlace(className);
            addFlow(className, transitionName, 1);
        }
        dict.put(transitionName, methodSig); // add signature into map

        for (Type t : args) {
            addPlace(t.toString());
            addFlow(t.toString(), transitionName, 1);
        }

        // add place for the return type
        Type retType = methodSig.getRetType();
        assert (retType.toString() == "void");
        if (retType.toString() != "void") {
            addPlace(retType.toString());
            addFlow(transitionName, retType.toString(), 1);
        } else {
            addPlace("void");
            addFlow(transitionName, "void", 1);
        }
    }

    private static void addTransition(MethodSignature methodSig) {
        String methodname = methodSig.getName();
        boolean isStatic = methodSig.getIsStatic();
        boolean isConstructor = methodSig.getIsConstructor();
        String className = methodSig.getHostClass().getName();
        String transitionName;
        List<Type> args = methodSig.getArgTypes();
        
        if (isConstructor) {
            transitionName = methodname + "(Constructor)" + "(";
            for (Type t : args) {
                transitionName += t.toString() + " ";
            }
            transitionName += ")";
            transitionName += methodSig.getRetType();
            // FIXME: fix this potential bug later; triggered in javax.mail
            if (!petrinet.containsTransition(transitionName))
                petrinet.createTransition(transitionName);
        } else if (isStatic) {
            transitionName = "(static)" + className + "." + methodname + "(";
            for (Type t : args) {
                transitionName += t.toString() + " ";
            }
            transitionName += ")";
            transitionName += methodSig.getRetType();
            // FIXME: fix this potential bug later; triggered in javax.mail
            if (!petrinet.containsTransition(transitionName))
                petrinet.createTransition(transitionName);
        } else { // The method is not static, so it has an extra argument
            transitionName = className + "." + methodname + "(";
            transitionName += className + " ";
            for (Type t : args) {
                transitionName += t.toString() + " ";
            }
            transitionName += ")";
            transitionName += methodSig.getRetType();
            // FIXME: fix this potential bug later; triggered in javax.mail
            if (!petrinet.containsTransition(transitionName)) {
                petrinet.createTransition(transitionName);

                addPlace(className);
                addFlow(className, transitionName, 1);
            }
        }
        dict.put(transitionName, methodSig); // add signature into map

        for (Type t : args) {
            addPlace(t.toString());
            addFlow(t.toString(), transitionName, 1);
        }

        // add place for the return type
        Type retType = methodSig.getRetType();
        if (retType.toString() != "void") {
            addPlace(retType.toString());
            addFlow(transitionName, retType.toString(), 1);
        } else {

            if (noSideEffects.contains(className)) {
                addVoidTransition(methodSig);
            }

            addPlace(className);
            addFlow(transitionName, className, 1);

        }
    }

    public void setMaxTokens(List<String> inputs) {
        Map<String, Integer> maxTokenMap = new HashMap<>();
        for (Place p : petrinet.getPlaces()) {
            maxTokenMap.put(p.getId(), 1);
        }

        // compute max.
        for (Transition t : petrinet.getTransitions()) {
            for (Flow flow : t.getPresetEdges()) {
                String argType = flow.getSource().getId();
                int num = flow.getWeight();
                assert maxTokenMap.containsKey(argType);
                int curr = maxTokenMap.get(argType);
                if (num > curr)
                    maxTokenMap.put(argType, num);
            }
        }
        for (Place p : petrinet.getPlaces()) {
            assert (maxTokenMap.containsKey(p.getId()));
            p.setMaxToken(maxTokenMap.get(p.getId())+1);
        }

        // Update the maxtoken for inputs
        HashMap<Place, Integer> count = new HashMap<Place, Integer>();
        for (String input : inputs) {
            Place p;
            p = petrinet.getPlace(input);
            if (count.containsKey(p)) {
                count.put(p, count.get(p) + 1);
            } else {
                count.put(p, 1);
            }
        }
        for (Place p : count.keySet()) {
            if (count.get(p) > p.getMaxToken()) {
                p.setMaxToken(count.get(p));
            }
        }

    }

    public PetriNet build(List<Function> functions, List<String> inputs) {
        for (Function sig : functions) {
            addHaskellTransition(sig);
        }
        setMaxTokens(inputs);
        return petrinet;
    }

    private void addHaskellTransition(Function sig) {
        if(sig.getHoParams().size() == 0) {
            addFirstOrderTransition(sig);
        } else {
            addHigherOrderTransition(sig);
        }
    }

    private void addHigherOrderTransition(Function sig) {
        for(int i = 0; i < sig.getHoParams().size(); i++) {
            // for each higher order parameter, there is an entry point and an exit point
            String entry = sig.getFunName() + "|entry" + i;
            String exit = sig.getFunName() + "|exit" + i;
            petrinet.createTransition(entry);
            petrinet.createTransition(exit);
            // all the first order arguments go into the entry point
            for(String param : sig.getFunParams()) {
                if(param.charAt(0) == 'f') continue; // skip high order parameters
                addPlace(param);
                addFlow(param, entry, 1);
            }

            //recursively add this argument as a transition
            Function hoParam = sig.getHoParams().get(i);
 
            // the return type of the HO argument goes into exit
            addPlace(hoParam.getFunReturn());
            addFlow(hoParam.getFunReturn(), exit, 1);

            // a special place f to ensure it goes out of our exit if enters
            String special = sig.getFunName() + "|" + i;
            addPlace(special);
            addFlow(entry, special, 1);
            addFlow(special, exit, 1);

            // entry assignes one token to the parameters of higher order argument
            for(String param : hoParam.getFunParams()) {
                addPlace(param);
                addFlow(entry, param, 1);

                // consume some ho arguments
                // addPlace("void");
                // String consume = param + "|consume";
                // if(!petrinet.containsTransition(consume)) {
                //     petrinet.createTransition(consume);
                // }
                // addFlow(param, consume, 1);
                // addFlow(special, consume, 1);
                // addFlow(consume, "void", 1);
                // addFlow(consume, special, 1);
            }

            // exit point goes to the return type of function
            addFlow(exit, sig.getFunReturn(), 1);
        }
    }

    private void addFirstOrderTransition(Function sig) {
        petrinet.createTransition(sig.getFunName());
        addPlace(sig.getFunReturn());
        if (sig.getFunParams().size() == 0) {
            addPlace("void");
            addFlow("void", sig.getFunName(), 1);
        } else {
            // create places for all function parameter types
            // and add edges from parameters to function transition
            for (String param : sig.getFunParams()) {
                addPlace(param);
                addFlow(param, sig.getFunName(), 1);
            }
        }
        // add edge from function transition to the return type
        addFlow(sig.getFunName(), sig.getFunReturn(),1);
    }

    public PetriNet build(List<MethodSignature> result, Map<String, Set<String>> superClassMap,
            Map<String, Set<String>> subClassMap, List<String> inputs, boolean copyPoly) throws java.io.IOException {
        for (MethodSignature k : result) {
            addTransition(k);
        }

        getPolymorphismInformation(superClassMap, subClassMap);

        if (copyPoly) {
            copyPolymorphism();
        } else {
            normalPolymorphism();
        }

        setMaxTokens(inputs);

        return petrinet;
    }
}
