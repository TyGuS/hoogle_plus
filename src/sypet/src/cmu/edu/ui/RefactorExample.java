package cmu.edu.ui;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

public class RefactorExample {

	public static void main(String[] args) throws IOException {
		
		if (args.length != 1) {
			System.out.println("Error: wrong number of arguments= " + args.length);
			System.out.println("Usage: ant refactor -Dargs=\"<int>\"");
			System.out.println("int = {2,3,4} -> number of fixed loc");
			System.exit(0);
		}

		ArrayList<String> packages = new ArrayList<String>(Arrays.asList("java.time"));
		ArrayList<String> libs = new ArrayList<String>(Arrays.asList("./lib/rt8.jar"));
		UISyPet sypet = new UISyPet(packages, libs, new ArrayList<String>());
		String methodName = "refactor";
		ArrayList<String> paramNames = new ArrayList<String>(Arrays.asList("sypet_arg0", "sypet_arg1", "sypet_arg2", "sypet_arg3", "sypet_arg4"));
//		
//		ArrayList<String> srcTypes = new ArrayList<String>(
//				Arrays.asList("int","int","int"));
//		
//		String testCode = "	public static boolean test0(java.util.Date date0) throws Throwable{\n" + 
//				"        int arg0 = date0.getYear();\n" + 
//				"        int arg1 = date0.getDate();\n" + 
//				"        int arg2 = date0.getMonth();\n" + 
//				"        java.time.OffsetDateTime offsetdatetime0 = refactor(arg0, arg1, arg2);\n" + 
//				"        boolean b0 = offsetdatetime0.getYear() == arg0;\n" + 
//				"        boolean b1 = offsetdatetime0.getDayOfMonth() == arg1;\n" + 
//				"        boolean b2 = offsetdatetime0.getMonthValue() == arg2;\n" + 
//				"        return b0 && b1 && b2;\n" + 
//				"    }\n" + 
//				" public static boolean test() throws Throwable{\n" +
//				"        java.util.Date date0 = new java.util.Date(525, 525, 0);\n"+
//				"        java.util.Date date1 = new java.util.Date(1, 1, 1);\n"+
//				"        java.util.Date date2 = new java.util.Date(0, 0, 0);\n"+
//				"        java.util.Date date3 = new java.util.Date((-2934), (-2934), (-2934));\n"+
//				"        java.util.Date date4 = new java.util.Date(0, 1226, 1226);\n"+
//				"   return test0(date0) && test0(date1) && test0(date2) && test0(date3) && test0(date4);}\n";
//		
//		
//		String tgtType = "java.time.OffsetDateTime";

		ArrayList<String> srcTypes = new ArrayList<String>(
				Arrays.asList("java.lang.String", "java.lang.String", "long", "long", "long"));

		String tgtType = "java.lang.String";
		
		String testCode = "public static boolean test0() throws Throwable {\n"+
			"return (refactor(\"2015/10/21\", \"yyyy/MM/dd\",5,1,2).equals(\"2017/11/26\"));}\n"+
			"public static boolean test1() throws Throwable {\n"+
			"return (refactor(\"2013/04/28\", \"yyyy/MM/dd\",7,2,3).equals(\"2016/07/05\"));}\n"+
			"public static boolean test() throws Throwable {\n"+
			"return test0() && test1();}\n";

		sypet.setSignature(methodName, paramNames, srcTypes, tgtType, testCode);
		if (Integer.valueOf(args[0]) >= 2){
			System.out.println("c Fixing method= \"ofPattern\"");
			System.out.println("c Fixing method= \"parse\"");
			//sypet.addAtLeastK("(static)java.time.format.DateTimeFormatter.ofPattern(java.lang.String )java.time.format.DateTimeFormatter",1);
			//sypet.addAtLeastK("(static)java.time.LocalDate.parse(java.lang.CharSequence java.time.format.DateTimeFormatter )java.time.LocalDatePoly:(java.lang.String java.time.format.DateTimeFormatter )", 1);
		}
		if (Integer.valueOf(args[0]) >= 3){
			System.out.println("c Fixing method= \"format\"");
			//sypet.addAtLeastK("java.time.LocalDate.format(java.time.LocalDate java.time.format.DateTimeFormatter )java.lang.String", 1);	
		}
		if (Integer.valueOf(args[0]) >= 4){
			System.out.println("c Fixing method= \"plusDays\"");
			//sypet.addAtLeastK("java.time.LocalDate.plusDays(java.time.LocalDate long )java.time.LocalDate", 1);
		}
		
//		sypet.addAtLeastK("(static)java.time.format.DateTimeFormatter.ofPattern(java.lang.String )java.time.format.DateTimeFormatter",1);
//		sypet.addAtLeastK("(static)java.time.LocalDate.parse(java.lang.CharSequence java.time.format.DateTimeFormatter )java.time.LocalDatePoly:(java.lang.String java.time.format.DateTimeFormatter )", 1);
//		sypet.addAtLeastK("java.time.LocalDate.plusDays(java.time.LocalDate long )java.time.LocalDate", 1);
//		sypet.addAtLeastK("java.time.LocalDate.format(java.time.LocalDate java.time.format.DateTimeFormatter )java.lang.String", 1);
		String code = sypet.synthesize(1,7);
		System.out.println("code = " + code);

	}

}
