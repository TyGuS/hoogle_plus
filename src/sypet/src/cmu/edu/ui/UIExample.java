package cmu.edu.ui;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class UIExample {
	
	public static void main(String[] args) throws IOException {
		
		ArrayList<String> packages = new ArrayList<String>(
			    Arrays.asList("java.time"));
		ArrayList<String> libs = new ArrayList<String>(
				Arrays.asList("./lib/rt8.jar"));
		UISyPet sypet = new UISyPet(packages, libs, new ArrayList<String>());
		String methodName = "refactor";
		ArrayList<String> paramNames = new ArrayList<String>(
				Arrays.asList("sypet_arg0","sypet_arg1","sypet_arg2"));

		ArrayList<String> srcTypes = new ArrayList<String>(
				Arrays.asList("int","int","int"));
		
		String tgtType = "java.time.OffsetDateTime";
		
		// TODO: fix an issue if the test method is declared static
		// WARNING: the testCode cannot be static
		String testCode = 
				"public static boolean test() throws Throwable{\n"+
	        "java.util.Date date0 = new java.util.Date((-1), (-301), 0);\n"+
	        "int arg0 = date0.getYear();\n"+
	        "int arg1 = date0.getDate();\n"+
	        "int arg2 = date0.getMonth();\n"+
	        "java.time.OffsetDateTime offsetdatetime0 = refactor(arg0, arg1, arg2);\n"+
	        "boolean b0 = offsetdatetime0.getDayOfYear() == arg0;\n"+
	        "boolean b1 = offsetdatetime0.getHour() == arg1;\n"+
	        "boolean b2 = offsetdatetime0.hashCode() == arg2;\n"+
	        "return b0 && b1 && b2;\n"+
	    "}\n";
		
		sypet.setSignature(methodName, paramNames, srcTypes,tgtType, testCode);
		String code = sypet.synthesize(1,3);
		System.out.println("code = " + code);
		
//		
//		// Step 1. Create a list with the packages and libs
//		ArrayList<String> packages = new ArrayList<String>(
//			    Arrays.asList("java.util.Date","java.time"));
//		
//		ArrayList<String> libs = new ArrayList<String>(
//				Arrays.asList("lib/rt8.jar"));
//		
//		System.out.println("c parsing the libraries");
//		// Step 2. Create an UISYPet object
//		// Note: this step will take some time since Soot will parse the libraries
//		// TODO: cache previous seen libraries to speedup this analysis
//		UISyPet sypet = new UISyPet(packages, libs);
//		
//		// Step 3. Set a signature for searching
//		// WARNING: this step must be done before synthesizing code
//		// Note: this is similar to SyPet input via .json file
//		String methodName = "convert";
//
//		ArrayList<String> paramNames = new ArrayList<String>(
//				Arrays.asList("sypet_arg0"));
//
//		ArrayList<String> srcTypes = new ArrayList<String>(
//				Arrays.asList("int"));
//		
//		String tgtType = "java.time.LocalDate";
//		
////		// TODO: fix an issue if the test method is declared static
////		// WARNING: the testCode cannot be static
////		String testCodeYear = "	public boolean test() throws Throwable {\n" + 
////				"		java.util.Date date0 = new java.util.Date(1737, 252, 1876);\n" +
////				"       int year = date0.getYear();\n" +
////				"		java.time.LocalDate localdate0 = convert(year);\n" + 
////				"		return localdate0.getYear() == year;\n" + 
////				"		}";
////		
////		sypet.setSignature(methodName, paramNames, srcTypes,tgtType, testCodeYear);
////
////		System.out.println("c finding one solution to testYear");
////		// Step 4. Call SyPet to synthesize one snippet of code that passes the test cases
////		// Specify a maximum number of lines when calling the synthesize method
////		// Note: since SyPet uses clone edges, the maximum number of lines may be smaller than expected
////		String code = sypet.synthesize(3);
////		System.out.println("code = "  + code);
////		
////		System.out.println("c finding all solutions to testYear");
////		// If you want all solutions instead up to a given limit then you can use the synthesizeAll method
////		List<String> codeAll = sypet.synthesizeAll(3);
////		int solution = 1;
////		for (String c : codeAll) {
////			System.out.println("Solution #" + solution + " =");
////			System.out.println(c);
////			solution++;
////		}
//		
//		// If we want to issue a different query, we need to change the signature
//		// Assume we want to change the test case
//		String testCodeMonth = "public boolean test() throws Throwable {\n" + 
//				"		java.util.Date date0 = new java.util.Date(1737, 252, 1876);\n" +
//				"       int month = date0.getMonth();\n" + 
//				"		java.time.LocalDate localdate0 = convert(month);\n" + 
//				"		return localdate0.getMonthValue() == month;\n" + 
//				"		}";
//		
//		//System.out.println("c finding one solution to testMonth");
//		sypet.setSignature(methodName, paramNames, srcTypes, tgtType, testCodeMonth);
//		String code = sypet.synthesize(3);
//		System.out.println("code = "  + code);
//
//		// If you want all solutions instead up to a given limit then you can use the synthesizeAll method
//		System.out.println("c finding all solutions to testMonth");
//		List<String> codeAll = sypet.synthesizeAll(3);
//		int solution = 1;
//		for (String c : codeAll) {
//			System.out.println("Solution #" + solution + " =");
//			System.out.println(c);
//			solution++;
//		}	
//	
	}
	
}
