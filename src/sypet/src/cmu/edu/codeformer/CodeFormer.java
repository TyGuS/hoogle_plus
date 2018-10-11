/**
 * BSD 3-Clause License
 *	
 *	
 *	Copyright (c) 2018, SyPet 2.0 - Ruben Martins, Yu Feng, Isil Dillig
 *	All rights reserved.
 *	
 *	Redistribution and use in source and binary forms, with or without
 *	modification, are permitted provided that the following conditions are met:
 *	
 *	* Redistributions of source code must retain the above copyright notice, this
 *	  list of conditions and the following disclaimer.
 *	
 *	* Redistributions in binary form must reproduce the above copyright notice,
 *	  this list of conditions and the following disclaimer in the documentation
 *	  and/or other materials provided with the distribution.
 *	
 *	* Neither the name of the copyright holder nor the names of its
 *	  contributors may be used to endorse or promote products derived from
 *	  this software without specific prior written permission.
 *	
 *	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *	AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *	IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *	DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 *	FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *	DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *	SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *	CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 *	OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *	OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package cmu.edu.codeformer;

import cmu.edu.parser.MethodSignature;
import cmu.edu.petrinet.Function;
import cmu.edu.petrinet.Param;
import org.sat4j.core.VecInt;
import org.sat4j.minisat.SolverFactory;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.ISolver;
import org.sat4j.specs.IVecInt;
import org.sat4j.specs.TimeoutException;
import soot.Type;

import java.util.*;

/**
 * Given a sequence of method calls, this class will produce a string containing the corresponding Java code.
 */
public class CodeFormer {
    private final List<MethodSignature> sigs;
    private final List<Function> functions;
    private int slotNumber = 0;
    private int retNumber = 0;
    private final VarTable slotTypes = new VarTable();
    private final VarTable returnedValTypes = new VarTable();
    private boolean unsat = false;
    private final List<String> inputTypes;
    private final String retType;
    private final List<String> varNames;
    private final String methodName;
    private final Map<Integer,Integer> lastValueOfSlot = new HashMap<>();
    private final Map<String,Set<String>> subclassMap;
    private final Map<String,Set<String>> superclassMap;
    private final Map<Integer, String> returnedValFuncs = new HashMap<>();
    ISolver solver = SolverFactory.newDefault();

    /**
     *
     * The initial setup for the class.
     * @param sigs requires a sequence of signatures in the expected order.
     * @param varNames parameter names of the method
     * @param methodName method name of the method
     */
    public CodeFormer(List<MethodSignature> sigs, List<String> inputTypes, String retType, List<String> varNames,
                      String methodName, Map<String, Set<String>> subclassMap, Map<String, Set<String>> superclassMap) {
        this.sigs = sigs;
        this.inputTypes = inputTypes;
        this.retType = retType;
        this.varNames = varNames;
        this.methodName = methodName;
        this.subclassMap = subclassMap;
        this.superclassMap = superclassMap;
        this.functions = null;
        //solver.setTimeout(1000000);
        //Setup
        //Add method input
        
        for (String input : inputTypes){
            returnedValTypes.addEntry(input,retNumber);
            retNumber += 1;
        }

        //Add slots and variables to the signatures table
        for (MethodSignature sig : sigs){
            if (sig.getIsConstructor()){

            }
            else if (!sig.getIsStatic()){
                slotTypes.addEntry(sig.getHostClass().getType().toString(),slotNumber);
                lastValueOfSlot.put(slotNumber,retNumber);
                slotNumber += 1;
            }
            for (Type type : sig.getArgTypes()){
                slotTypes.addEntry(type.toString(),slotNumber);
                lastValueOfSlot.put(slotNumber,retNumber);
                slotNumber += 1;
            }

            if (!sig.getRetType().toString().equals("void")){
                returnedValTypes.addEntry(sig.getRetType().toString(),retNumber);
                retNumber += 1;
            }
        }
        //Add method return value
        if (retType != null)
        {
            slotTypes.addEntry(retType,slotNumber);
            lastValueOfSlot.put(slotNumber,retNumber);
            slotNumber += 1;
        }

        //Setup constrains
        addSingleVariableConstrains();
        addAtLeastOneSlot();
    }

    public CodeFormer(List<Function> sigs, List<String> inputTypes, String retType) {
        this.inputTypes = inputTypes;
        this.retType = retType;
        this.sigs = null;
        this.functions = sigs;
        this.varNames = new ArrayList<String>();
        for(int i = 0; i < inputTypes.size(); i++) {
            varNames.add("arg"+i);
        }
        this.methodName = null;
        this.subclassMap = new HashMap<>();
        this.superclassMap = new HashMap<>();
        //Setup
        //Add method input
        // solver.setTimeout(1000000);
        
        for (String input : inputTypes){
            returnedValTypes.addEntry(input,retNumber);
            returnedValFuncs.put(retNumber, varNames.get(retNumber));
            retNumber += 1;
        }

        //Add slots and variables to the signatures table
        for (Function sig : functions){
            for (String type : sig.getFunParams()){
                slotTypes.addEntry(type,slotNumber);
                lastValueOfSlot.put(slotNumber,retNumber);
                slotNumber += 1;
            }
            returnedValTypes.addEntry(sig.getFunReturn(),retNumber);
            returnedValFuncs.put(retNumber, sig.getFunName());
            retNumber += 1;
        }
        //Add method return value
        if (retType != null)
        {
            slotTypes.addEntry(retType,slotNumber);
            lastValueOfSlot.put(slotNumber,retNumber);
            slotNumber += 1;
        }
        // System.out.println("slot number after adding constraints: " + slotNumber);
        //Setup constrains
        addSingleVariableConstrainsHaskell();
        addAtLeastOneSlotHaskell();
        // System.out.println(solver.toString());
    }

    /**
     * Each call to solve will produce one extra solution.
     * @return one solution to the programming (Java code)
     * @throws TimeoutException Iff there is no solution available
     */
    public String solve() throws TimeoutException {
        //Solve
        int[] satResult;
        try {
            if (solver.isSatisfiable()){
                satResult = solver.model();
            }
            else{
                unsat = true;
                throw new TimeoutException();
            }
        } catch (TimeoutException e) {
            unsat = true;
            throw new TimeoutException();
        }

        //A list only with filtered positive elements in the result.
        List<Integer> satList = new ArrayList<>();

        //Block this version, and filter the result with only positive ones.
        VecInt block = new VecInt();
        for (Integer id : satResult){
            block.push(-id);
            if (id > 0) satList.add(id);
        }
        try {
            solver.addClause(block);
        } catch (ContradictionException e) {
            unsat = true;
        }

        // System.out.println(satList.toString());

        // return satList;
        //formCode
        return formCode(satList);

    }

    public String solveHaskell() throws TimeoutException {
        //Solve
        int[] satResult;
        try {
            if (solver.isSatisfiable()){
                satResult = solver.model();
            }
            else{
                unsat = true;
                throw new TimeoutException();
            }
        } catch (TimeoutException e) {
            unsat = true;
            throw new TimeoutException();
        }

        //A list only with filtered positive elements in the result.
        List<Integer> satList = new ArrayList<>();

        //Block this version, and filter the result with only positive ones.
        VecInt block = new VecInt();
        for (Integer id : satResult){
            block.push(-id);
            if (id > 0) satList.add(id);
        }
        try {
            solver.addClause(block);
        } catch (ContradictionException e) {
            unsat = true;
        }

        //formCode
        return formHaskellCode(satList);

    }

    /**
     *
     * @return true iff the problem is no longer solvable.
     */
    public boolean isUnsat() {
        return unsat;
    }


    //Each slot only has variable
    private void addSingleVariableConstrains(){
        for (int slotValue = 0; slotValue < slotNumber ; slotValue += 1) {
            IVecInt vec = new VecInt();
            IVecInt vec0 = new VecInt();
            String slotType = slotTypes.getType(slotValue);
            List<String> possibleSlotTypes = new LinkedList<>();
            if (subclassMap.containsKey(slotType)) possibleSlotTypes.addAll(subclassMap.get(slotType));
            possibleSlotTypes.add(slotType);
            for (String curSlotType : possibleSlotTypes){
                for (int returnedValue : returnedValTypes.getEntries(curSlotType)) {
                    if (returnedValue < lastValueOfSlot.get(slotValue)) vec.push(calculateID(returnedValue,slotValue));
                    else vec0.push(calculateID(returnedValue,slotValue));
                }
            }
            try {
                solver.addExactly(vec,1);
                solver.addExactly(vec0,0);
            } catch (ContradictionException e) {
                unsat = true;
            }
        }
    }

    private void addSingleVariableConstrainsHaskell(){
        for (int slotValue = 0; slotValue < slotNumber ; slotValue += 1) {
            IVecInt vec = new VecInt();
            IVecInt vec0 = new VecInt();
            String slotType = slotTypes.getType(slotValue);
            List<String> possibleSlotTypes = new LinkedList<>();
            // if (subclassMap.containsKey(slotType)) possibleSlotTypes.addAll(subclassMap.get(slotType));
            possibleSlotTypes.add(slotType);
            for (String curSlotType : possibleSlotTypes){
                for (int returnedValue : returnedValTypes.getEntries(curSlotType)) {
                    if (returnedValue < lastValueOfSlot.get(slotValue)) vec.push(calculateID(returnedValue,slotValue));
                    else vec0.push(calculateID(returnedValue,slotValue));
                }
            }
            try {
                solver.addExactly(vec,1);
                solver.addExactly(vec0,0);
            } catch (ContradictionException e) {
                unsat = true;
            }
        }
    }

    //Each returned value used at least once
    private void addAtLeastOneSlot(){
        for (int returnedValue = 0; returnedValue < retNumber ; returnedValue += 1) {
            IVecInt vec = new VecInt();
            List<String> possibleSlotTypes = new LinkedList<>();
            String returnedType = returnedValTypes.getType(returnedValue);
            if (superclassMap.containsKey(returnedType)) possibleSlotTypes.addAll(superclassMap.get(returnedType));
            possibleSlotTypes.add(returnedType);
            for (String slotType : possibleSlotTypes){
                for (int slotValue : slotTypes.getEntries(slotType)) {
                    vec.push(calculateID(returnedValue,slotValue));
                }
            }
            try {
                solver.addAtLeast(vec,1);
            } catch (ContradictionException e) {
                unsat = true;
            }
        }
    }

    private void addAtLeastOneSlotHaskell(){
        for (int returnedValue = 0; returnedValue < retNumber ; returnedValue += 1) {
            IVecInt vec = new VecInt();
            List<String> possibleSlotTypes = new LinkedList<>();
            String returnedType = returnedValTypes.getType(returnedValue);
            // if (superclassMap.containsKey(returnedType)) possibleSlotTypes.addAll(superclassMap.get(returnedType));
            possibleSlotTypes.add(returnedType);
            for (String slotType : possibleSlotTypes){
                for (int slotValue : slotTypes.getEntries(slotType)) {
                    vec.push(calculateID(returnedValue,slotValue));
                }
            }
            try {
                solver.addAtLeast(vec,1);
            } catch (ContradictionException e) {
                unsat = true;
            }
        }
    }

    public String formHaskellCode(List<Integer> satResult) {
        String error = "";
        
        //FormCode
        StringBuilder resultBuilder = new StringBuilder();
        String returnExpr = "";
        int varCount = 0;
        int slotCount = 0;

        //Add method signature
        resultBuilder.append("f = ");
        for (int i = 0 ; i < inputTypes.size() ; i++){
            resultBuilder.append("\\");
            resultBuilder.append(convVarName(varCount));
            varCount += 1;
            resultBuilder.append(". ");
        }
        resultBuilder.append("\n");

        List<String[]> var2code = new ArrayList<>();
        for (Function sig : functions){
            StringBuilder builder = new StringBuilder();
            String [] v = new String[2];
            v[0] = convVarName(varCount);
            varCount += 1;
            // remove the number index for each funcation name here
            int funNameEndAt = sig.getFunName().lastIndexOf('_') ;
            if(funNameEndAt != -1){
                builder.append(sig.getFunName().substring(0, funNameEndAt).replace('.','_'));
            }
            else{
                builder.append(sig.getFunName().replace('.','_'));
            }
            builder.append(" ");
            for (int i = 0; i < sig.getFunParams().size() ; i++){
                if (slotCount >= satResult.size())
                    return error;
                int id = satResult.get(slotCount);
                slotCount ++;
                int returnedValue = calculateReturnedValue(id);
                int slotValue = calculateSlotValue(id);
                //assert (slotValue == slotCount);
                builder.append(convVarName(returnedValue));
                if (i != sig.getFunParams().size() - 1){
                    builder.append(" ");
                }
            }
            v[1] = builder.toString();
            var2code.add(v);
        }
        if (retType != null ){
            if (slotCount >= satResult.size())
                return error;
            int id = satResult.get(slotCount);
            slotCount ++;
            int returnedValue = calculateReturnedValue(id);
            int slotValue = calculateSlotValue(id);
            //assert (slotValue == slotCount);
            returnExpr = convVarName(returnedValue);
        }
        // System.out.println(returnExpr);
        // reverse the array to substitute all the temperary variables in the ANF expression
        // or maybe we can keep the ANF form
        Collections.reverse(var2code);
        for(String [] code : var2code) {
            // System.out.println(code[0]);
            // System.out.println(code[1]);
            if(returnExpr.contains(code[0])) {
                returnExpr = returnExpr.replace(code[0], "(" + code[1] + ")");
            }else{
                System.out.println("Not found");
            }
        }
        resultBuilder.append(returnExpr);
        return resultBuilder.toString();
    }

    private String formCode(List<Integer> satResult){
    	
    	// FIXME: check what is causing this bug
    	String error = "";
    	
    	//FormCode
        StringBuilder builder = new StringBuilder();
        int varCount = 0;
        int slotCount = 0;

        //Add method signature
        builder.append("public static ");
        if (retType != null){
            builder.append(retType.toString());
            builder.append(" ");
        }
        else{
            builder.append("void ");
        }
        builder.append(methodName);
        builder.append("(");
        for (int i = 0 ; i < inputTypes.size() ; i++){
            builder.append(inputTypes.get(i));
            builder.append(" ");
            builder.append(convVarName(varCount));
            varCount += 1;
            if (i != inputTypes.size() - 1) builder.append(", ");
        }
        builder.append(") throws Throwable{\n");

        for (MethodSignature sig : sigs){
        	
            if (!sig.getRetType().toString().equals("void")){
                builder.append(sig.getRetType().toString().replace('$','.'));
                builder.append(" ");
                builder.append(convVarName(varCount));
                varCount += 1;
                builder.append(" = ");
            }

            if (sig.getIsConstructor()){
                builder.append(" new ");
            }
            else if (sig.getIsStatic()){
                String hostclstr = sig.getHostClass().toString();
                builder.append(hostclstr.replace('$','.'));
                builder.append(".");
            }

            else{
            	if (slotCount >= satResult.size())
                	return error;
                int id = satResult.get(slotCount);
                slotCount ++;
                int returnedValue = calculateReturnedValue(id);
                builder.append(convVarName(returnedValue));
                builder.append(".");
            }

            builder.append(sig.getName().replace('$','.'));
            builder.append("(");
            for (int i = 0; i < sig.getArgTypes().size() ; i++){
            	if (slotCount >= satResult.size())
                	return error;
                int id = satResult.get(slotCount);
                slotCount ++;
                int returnedValue = calculateReturnedValue(id);
                int slotValue = calculateSlotValue(id);
                //assert (slotValue == slotCount);

                builder.append(convVarName(returnedValue));
                if (i != sig.getArgTypes().size() - 1){
                    builder.append(",");
                }
            }
            builder.append(");\n");
        }
        if (retType != null ){
            builder.append("return ");
            
            if (slotCount >= satResult.size())
            	return error;
            int id = satResult.get(slotCount);
            slotCount ++;
            int returnedValue = calculateReturnedValue(id);
            int slotValue = calculateSlotValue(id);
            //assert (slotValue == slotCount);
            builder.append(convVarName(returnedValue));
            builder.append(";\n");
        }
        builder.append("}");
        return builder.toString().replace('$','.');
    }


    private int calculateID(int returnedValue,int slotValue){
        return returnedValue + retNumber * slotValue + 1;
    }
    private int calculateReturnedValue(int id){
        return (id-1)%retNumber;
    }
    private int calculateSlotValue(int id){
        return (id-1)/retNumber;
    }

    private String convVarName(int val){
        if (val < varNames.size()) return varNames.get(val);
        else return "var_"+(val - varNames.size());
    }

}
