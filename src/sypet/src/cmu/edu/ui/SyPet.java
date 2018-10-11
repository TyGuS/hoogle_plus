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

package cmu.edu.ui;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import cmu.edu.parser.JsonParser;
import cmu.edu.parser.SyPetInput;

/**
 * This represents the main entry point to synthesis loop from SyPet.
 * 
 * @author Ruben Martins
 */
public class SyPet {

	public static void main(String[] args) throws IOException {

		if (args.length != 1) {
			System.out.println("Error: wrong number of arguments= " + args.length);
			System.out.println("Usage: ./sypet.sh <filename.json>");
			System.exit(0);
		}

		Path jsonFilePath = Paths.get(args[0]);
		if (!Files.exists(jsonFilePath)) {
			System.out.println("File does not exist= " + args[0]);
			System.exit(0);
		}

		SyPetInput jsonInput = JsonParser.parseJsonInput(args[0]);

		List<String> packages = jsonInput.packages;
		List<String> libs = jsonInput.libs;
		List<String> hints = new ArrayList<>();
		if (jsonInput.hints != null)
			hints = jsonInput.hints;
		long start = System.nanoTime();
		UISyPet sypet = new UISyPet(packages, libs, hints);
		long end = System.nanoTime();
		System.out.println("Time for construct a UISypet:" + ((end-start)/1000000000.0));
		String methodName = jsonInput.methodName;
		List<String> paramNames = jsonInput.paramNames;
		List<String> srcTypes = jsonInput.srcTypes;
		String tgtType = jsonInput.tgtType;

		// check if test case file exists
		if (!Files.exists(Paths.get(jsonInput.testPath))) {
			System.out.println("File does not exist= " + args[0]);
			System.exit(0);
		}

		File file = new File(jsonInput.testPath);
		BufferedReader br = new BufferedReader(new FileReader(file));
		StringBuilder fileContents = new StringBuilder();
		String line = br.readLine();
		while (line != null) {
			fileContents.append(line);
			line = br.readLine();
		}
		br.close();
		String testCode = fileContents.toString();
		
		sypet.setSignature(methodName, paramNames, srcTypes, tgtType, testCode);
		String code = sypet.synthesize(jsonInput.lb, jsonInput.ub);
		if (!code.equals(""))
			System.out.println("c Synthesized code:\n" + code);
		System.exit(0);
	}

}
