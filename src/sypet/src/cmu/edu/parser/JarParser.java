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
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import soot.Scene;
import soot.SootClass;
import soot.SootMethod;
import soot.SourceLocator;
import cmu.edu.parser.MethodSignature;
import cmu.edu.utils.SootUtils;

/**
 * Parse a jar file to extract all methods and types.
 * 
 * @author Ruben Martins
 * @author Kaige Liu
 * @author Yu Feng
 */
public class JarParser {
	
	public static List<String> pkgs;
	
	public JarParser(List<String> libs){
		pkgs = new ArrayList<String>();
		SootUtils.initSoot(libs);
	}
	
	public JarParser(List<String> libs, List<String> soot) {
		pkgs = new ArrayList<String>();
		SootUtils.initSoot(soot);
	}
	
	/**
	 * Parse a list of given jar files, and produce a list of method signatures.
	 * 
	 * @param libs
	 *            physical addresses of libraries. e.g. "lib/hamcrest-core-1.3.jar"
	 * @return the list of method signature contained in the given libraries
	 */
	public List<MethodSignature> parseJar(List<String> libs, List<String> pkgss, List<String> blacklist) {
		pkgs = pkgss;
		List<MethodSignature> sigs = new LinkedList<>();
		for (String jarPath : libs) {
			List<String> cls = SourceLocator.v().getClassesUnder(jarPath);
			for (String cl : cls) {
				SootClass clazz = Scene.v().getSootClass(cl);
				List<SootMethod> methods = clazz.getMethods();
				for (SootMethod method : methods) {
					if (method.isPublic()) {
						boolean sat = false;
						for (String pkg : pkgs) {
							if (clazz.getName().startsWith(pkg)) {
								sat = true;
								break;
							}
						}
						for (String bl : blacklist) {
							if (method.getName().contains(bl)) {
								sat = false;
								break;
							}
						}
						if (method.getParameterTypes().size() > 5)
							sat = false;
						if (sat)
							sigs.add(getMethodSignature(method));
					}
				}
			}
		}
		return sigs;
	}

	/**
	 * A method that provides the super classes of all application classes.
	 * 
	 * @param acceptableSuperClasses
	 *            the set of classes that can be considered super classes. In order
	 *            to reduce the unnecessary super classes (e.g. Object).
	 * @return the map from each SootClass, to its corresponding set of super
	 *         classes.
	 */
	public static Map<String, Set<String>> getSuperClasses(Set<String> acceptableSuperClasses) {
		Map<String, Set<String>> result = new HashMap<>();
		for (SootClass cl : Scene.v().getClasses()) {
			for (String pkg : pkgs) {
				if (cl.getName().startsWith(pkg)) {
					result.put(cl.getName(), getSuperClassesOfClass(acceptableSuperClasses, cl));
					break;
				}
			}
		}
		return result;
	}
	
	public static Map<String, Set<String>> getSuperClasses(Set<String> acceptableSuperClasses, String pkg) {
		Map<String, Set<String>> result = new HashMap<>();
		for (SootClass cl : Scene.v().getClasses()) {
			if (cl.getName().startsWith(pkg)) {
				result.put(cl.getName(), getSuperClassesOfClass(acceptableSuperClasses, cl));
				break;
			}
		}
		return result;
	}

	private static Set<String> getSuperClassesOfClass(Set<String> acceptableSuperClasses, SootClass cl) {
		Set<String> res;
		if (cl.hasSuperclass()) {
			SootClass sup = cl.getSuperclass();
			res = getSuperClassesOfClass(acceptableSuperClasses, sup);
			if (acceptableSuperClasses.contains(sup.getName()))
				res.add(cl.getSuperclass().getName());
		} else {
			res = new HashSet<>();
		}
		for (SootClass interf : cl.getInterfaces()) {
			String name = interf.getName();
			if (acceptableSuperClasses.contains(name))
				res.add(name);
			res.addAll(getSuperClassesOfClass(acceptableSuperClasses, interf));
		}
		return res;
	}

	private static MethodSignature getMethodSignature(SootMethod method) {
		SootClass clazz = method.getDeclaringClass();
		if (method.getName().equals("<init>")) {
			MethodSignature sig = new MethodSignature(method.getName(), clazz.getType(), method.getParameterTypes(),
					method.isStatic(), clazz, true, method);
			return sig;
		} else {
			MethodSignature sig = new MethodSignature(method.getName(), method.getReturnType(),
					method.getParameterTypes(), method.isStatic(), clazz, false, method);
			return sig;
		}
	}

}
