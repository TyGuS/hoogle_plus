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

package cmu.edu.parser;

import java.util.List;

import soot.SootClass;
import soot.SootMethod;
import soot.Type;
import soot.JastAddJ.TypeAccess;

/**
 * Data structure that describes a method signature, including
 * 1. Method name
 * 2. Return type
 * 3. The list of argument types in order.
 * 4. Whether the method is static.
 * 5. The host class of the method.
 * @author Kaige Liu
 * 
 */
public class MethodSignature {
    private final String name;
    private final Type retType;
    private final List<Type> argTypes;
    private final boolean isStatic;
    private final SootClass hostClass;
    private final boolean isConstructor;
    private final SootMethod method;

    public MethodSignature(String name, Type retType, List<Type> argTypes, boolean isStatic, SootClass hostClass, boolean isConstructor, SootMethod method){
        this.retType = retType;
        this.argTypes = argTypes;
        this.isStatic = isStatic;
        this.hostClass = hostClass;
        this.isConstructor = isConstructor;
        if (isConstructor) {
            this.name = hostClass.getName();
        }else{
            this.name = name;
        }
        this.method = method;
    }

    public String getName() {
        return name;
    }

    public Type getRetType() {
        return retType;
    }

    public List<Type> getArgTypes() {
        return argTypes;
    }

    public boolean getIsStatic(){
        return isStatic;
    }
    public boolean getIsConstructor(){
        return isConstructor;
    }
    public SootClass getHostClass() {
        return hostClass;
    }
    public SootMethod getMethod() {
        return method;
    }

    @Override
    public String toString(){
        String result =  retType + " " +  hostClass + "."+name + "(";
        if (isStatic) result = "static "+result;
        int i = 0;
        for (Type t : argTypes){
            if (i != argTypes.size()-1) result += t + ", ";
            else result += t;
            i += 1;
        }
        result += ")";
        return result;
    }

    @Override
    public boolean equals(Object o){
        if (!(o instanceof MethodSignature)) return false;
        MethodSignature sig = (MethodSignature)o;
        return sig.name.equals(name) && sig.hostClass.equals(hostClass) && sig.retType.equals(retType) &&
                sig.isStatic == isStatic && sig.argTypes.equals(argTypes) && sig.isConstructor == isConstructor;
    }

    @Override
    public int hashCode(){
        return method.hashCode();
    }
}
