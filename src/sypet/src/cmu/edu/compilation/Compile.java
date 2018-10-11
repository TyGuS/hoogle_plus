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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Writer;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticListener;
import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;

/**
 * This represents the compilation of the test cases synthesized by SyPet.
 * 
 * @author Ruben Martins
 * @author Yu Feng
 */
public class Compile {
	public static final boolean DISPLAY_ERROR = false;
	public static String CLASSNAME;
	protected static boolean mCompilationSuccess = true;
	
	Compile(String classname){
		Compile.CLASSNAME = classname;
	}
	
    public static class MyDiagnosticListener implements DiagnosticListener<JavaFileObject>
    {
        @Override
		public void report(Diagnostic<? extends JavaFileObject> diagnostic)
        {

        }
    }

	public boolean runTest(String code, List<String> libs) {
		Class<?> compiledClass = compileClass(code, libs);
		if (compiledClass == null) {
			return false;
		}
		boolean success = false;
		try{
			Method method = compiledClass.getMethod("test");
			success = (boolean) method.invoke(null);
		} catch (Exception e) {
			if (DISPLAY_ERROR) e.printStackTrace();
			success = false;
		}
		return success;
	}

	public static boolean isCompilationSuccess() {
		return mCompilationSuccess;
	}

	@SuppressWarnings("rawtypes")
	private static Class compileClass(String program, List<String> libs) {
		if (DISPLAY_ERROR) System.out.println(program);
		String classpath = genClassPath(libs);
		try{
			JavaCompiler javac = ToolProvider.getSystemJavaCompiler();

			MyDiagnosticListener c = new MyDiagnosticListener();
	        StandardJavaFileManager sjfm = javac.getStandardFileManager(c,
	                Locale.ENGLISH,
	                null);
			SpecialClassLoader cl = new SpecialClassLoader(libs);
			SpecialJavaFileManager fileManager = new SpecialJavaFileManager(sjfm, cl);

			List<String> options = new ArrayList<String>();
			options.add("-cp");
			options.add(classpath);
			List<MemorySource> compilationUnits = Arrays.asList(new MemorySource(CLASSNAME, program));
			Writer out = DISPLAY_ERROR ? new PrintWriter(System.err) : null;
			JavaCompiler.CompilationTask compile = javac.getTask(out, fileManager,
	                c, options, null, compilationUnits);
			mCompilationSuccess = compile.call();
			if (mCompilationSuccess) return cl.findClass(CLASSNAME);
		} catch (Exception e){
			if (DISPLAY_ERROR) e.printStackTrace();
		}
		return null;
	}

	private static String genClassPath(List<String> libs) {
		StringBuilder builder = new StringBuilder();
		for (String lib : libs) {
			builder.append(lib);
			builder.append(':');
		}
		builder.append('.');
		return builder.toString();
	}
}

class MemorySource extends SimpleJavaFileObject {
	private String src;
	public MemorySource(String name, String src) {
		super(URI.create("file:///" + name + ".java"), Kind.SOURCE);
		this.src = src;
	}
	public CharSequence getCharContent(boolean ignoreEncodingErrors) {
		return src;
	}
	public OutputStream openOutputStream() {
		throw new IllegalStateException();
	}
	public InputStream openInputStream() {
		return new ByteArrayInputStream(src.getBytes());
	}
}

class SpecialJavaFileManager extends ForwardingJavaFileManager {
	private SpecialClassLoader xcl;

	@SuppressWarnings("unchecked")
	public SpecialJavaFileManager(StandardJavaFileManager sjfm, SpecialClassLoader xcl) {
		super(sjfm);
		this.xcl = xcl;
	}

	public JavaFileObject getJavaFileForOutput(Location location, String name, JavaFileObject.Kind kind, FileObject sibling) throws IOException {
		MemoryByteCode mbc = new MemoryByteCode(name);
		xcl.addClass(name, mbc);
		return mbc;
	}

	public ClassLoader getClassLoader(Location location) {
		return xcl;
	}
}

class MemoryByteCode extends SimpleJavaFileObject {
	private ByteArrayOutputStream baos;

	public MemoryByteCode(String name) {
		super(URI.create("byte:///" + name + ".class"), Kind.CLASS);
	}

	public CharSequence getCharContent(boolean ignoreEncodingErrors) {
		throw new IllegalStateException();
	}

	public OutputStream openOutputStream() {
		baos = new ByteArrayOutputStream();
		return baos;
	}

	public InputStream openInputStream() {
		throw new IllegalStateException();
	}

	public byte[] getBytes() {
		return baos.toByteArray();
	}
}

class SpecialClassLoader extends ClassLoader {
	protected Map<String, MemoryByteCode> map = new HashMap<String, MemoryByteCode>();
	protected List<String> libs;
	protected URLClassLoader cl = null;

	public SpecialClassLoader(List<String> libs) {
		this.libs = libs;
	}

	@Override
	protected Class<?> findClass(String name) throws ClassNotFoundException {
		MemoryByteCode mbc = map.get(name);
		if (mbc == null){
			URL[] urls = getUrls(libs);
			if (cl == null) {
				cl = new URLClassLoader(urls, Thread.currentThread().getContextClassLoader());
			}
			return cl.loadClass(name);
		} else {
			return defineClass(name, mbc.getBytes(), 0, mbc.getBytes().length);
		}
	}

	public void addClass(String name, MemoryByteCode mbc) {
		map.put(name, mbc);
	}

	protected URL[] getUrls(List<String> libs) {
		URL[] urls = new URL[libs.size()];
		try {
			for (int i = 0; i < libs.size(); ++i) {
				urls[i] = new File(libs.get(i)).toURI().toURL();
			}
		} catch (MalformedURLException e) {
			e.printStackTrace();
		}
		return urls;
	}
}
