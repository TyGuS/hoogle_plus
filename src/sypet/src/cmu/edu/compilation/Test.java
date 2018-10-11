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

package cmu.edu.compilation;

import java.io.IOException;
import java.util.List;


/**
 * Write code given the tests and classes.
 * 
 * @author Ruben Martins
 * @author Kaige Liu
 */
public class Test {
    private static final String CLASSNAME = "Target";

    /**
     * run test class based on the synthesized code and test code.
     * @param code synthesized code
     * @param testCode test code with name "test"
     * @return whether test pasted
     */
    public static boolean runTest(String code, String testCode, List<String> libs) throws IOException {
        //Create file;
        String classCode = writeCode(code,testCode);
        Compile compile = new Compile(CLASSNAME);
        boolean runResult = compile.runTest(classCode, libs);
        return runResult;
    }
    
    private static String writeCode(String code,String testCode) throws IOException {
        StringBuilder builder = new StringBuilder();
        builder.append("public class "+CLASSNAME  +" {\n");
        builder.append(code);
        // test cases need to be static for compilation to succeed
        if (!testCode.contains("public static"))
        	testCode = testCode.replace("public", "public static");
        
        String [] tokens = testCode.split(" ");
        for (int l = 0; l < tokens.length; l++) {
        	if (tokens[l].equals("static")) {
        		String name = tokens[l+2];
        		if (!name.equals("test()"))
        			testCode = testCode.replace(name, "test()");
        		break;
        	}
        }
        
        builder.append(testCode);
        builder.append("}\n");
        return builder.toString();
    }
}
