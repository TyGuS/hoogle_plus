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

import java.util.ArrayList;
import java.util.List;

/**
 * This represents the input for SyPet.
 * 
 * @author Kaige Liu
 * @author Ruben Martins
 * 
 */
public class SyPetInput {

	public SyPetInput(int id, int lb, int ub, String methodName, List<String> paramNames, List<String> srcTypes,
			String tgtType, List<String> packages, List<String> libs, String testPath, List<String> calls,
			List<String> types) {
		this.id = id;
		this.lb = lb;
		this.ub = ub;
		this.methodName = methodName;
		this.paramNames = paramNames;
		this.srcTypes = srcTypes;
		this.tgtType = tgtType;
		this.packages = packages;
		this.libs = libs;
		this.testPath = testPath;
		this.calls = calls;
		this.types = types;
		testBody = "";
		hints = new ArrayList<>();
	}
	
	public SyPetInput(int id, int lb, int ub, String methodName, List<String> paramNames, List<String> srcTypes,
			String tgtType, List<String> packages, List<String> libs, String testPath, List<String> calls,
			List<String> types, List<String> hints) {
		this.id = id;
		this.lb = lb;
		this.ub = ub;
		this.methodName = methodName;
		this.paramNames = paramNames;
		this.srcTypes = srcTypes;
		this.tgtType = tgtType;
		this.packages = packages;
		this.libs = libs;
		this.testPath = testPath;
		this.calls = calls;
		this.types = types;
		testBody = "";
		this.hints = hints;
	}

	public SyPetInput(int id, int lb, int ub, String methodName, List<String> paramNames, List<String> srcTypes,
			String tgtType, List<String> packages, List<String> libs, String testPath, List<String> calls,
			List<String> types, String testBody) {
		this.id = id;
		this.lb = lb;
		this.ub = ub;
		this.methodName = methodName;
		this.paramNames = paramNames;
		this.srcTypes = srcTypes;
		this.tgtType = tgtType;
		this.packages = packages;
		this.libs = libs;
		this.testPath = testPath;
		this.calls = calls;
		this.types = types;
		this.testBody = testBody;
		hints = new ArrayList<>();
	}
	
	public SyPetInput(int id, int lb, int ub, String methodName, List<String> paramNames, List<String> srcTypes,
			String tgtType, List<String> packages, List<String> libs, String testPath, List<String> calls,
			List<String> types, String testBody, List<String> hints) {
		this.id = id;
		this.lb = lb;
		this.ub = ub;
		this.methodName = methodName;
		this.paramNames = paramNames;
		this.srcTypes = srcTypes;
		this.tgtType = tgtType;
		this.packages = packages;
		this.libs = libs;
		this.testPath = testPath;
		this.calls = calls;
		this.types = types;
		this.testBody = testBody;
		this.hints = hints;
	}

	public int id;
	public String methodName;
	public List<String> paramNames;
	public List<String> srcTypes;
	public String tgtType;
	public List<String> packages;
	public List<String> libs;
	public String testPath;
	public List<String> calls;
	public List<String> types;
	public int lb;
	public int ub;
	public String testBody;
	public List<String> hints;
}
