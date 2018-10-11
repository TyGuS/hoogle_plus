package cmu.edu.compilation;

import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.util.Calendar;
import java.util.Date;

public class Target {

//	public static boolean test0(java.util.Date date0) throws Throwable{
//        int arg0 = date0.getYear()+1900;
//        int arg1 = date0.getDate();
//        int arg2 = date0.getMonth()+1;
//        long arg3 = date0.getTime();
//        java.time.OffsetDateTime offsetdatetime0 = refactor(arg0, arg1, arg2);
//        boolean b0 = offsetdatetime0.getYear() == arg0;
//        boolean b1 = offsetdatetime0.getDayOfMonth() == arg1;
//        boolean b2 = offsetdatetime0.getMonthValue() == arg2;
//        return b0 && b1 && b2;
//    }
//	
//	public static boolean test() throws Throwable {
//		java.util.Date date0 = new java.util.Date(525, 525, 0);
//		java.util.Date date1 = new java.util.Date(1, 1, 1);
//		java.util.Date date2 = new java.util.Date(0, 0, 0);
//		java.util.Date date3 = new java.util.Date((-2934), (-2934), (-2934));
//		java.util.Date date4 = new java.util.Date(0, 1226, 1226);
//		return test0(date0) && test0(date1) && test0(date2) && test0(date3) && test0(date4);
//	}
//	
	public static String before(java.lang.String arg0, java.lang.String arg1, int arg2, int arg3, int arg4) throws ParseException {
		SimpleDateFormat sdf = new SimpleDateFormat(arg1);
		Date d = sdf.parse(arg0);
		Calendar c = Calendar.getInstance();
		c.setTime(d);
		c.add(Calendar.DATE, arg2);
		c.add(Calendar.YEAR, arg4);
		c.add(Calendar.MONTH, arg3);
		Date n = c.getTime();
		String output = sdf.format(n);
		return output;
	}
	
	
	public static java.time.OffsetDateTime refactor(int arg0, int arg1, int arg2){
		java.time.OffsetTime var_0 = java.time.OffsetTime.now();
		java.time.LocalDate var_1 = java.time.LocalDate.of(arg0, arg2, arg1);
		java.time.OffsetDateTime var_2 = var_0.atDate(var_1);
		return var_2;
	}
	
//	public static java.time.LocalDateTime refactor(long arg0){
//		java.time.Instant var_0 = Instant.ofEpochMilli(arg0);
//		java.time.LocalDateTime var_1 = LocalDateTime.ofInstant(var_0, ZoneOffset.UTC);
//		return var_1;
//	}
	

//	
//	public static java.lang.String refactor(java.lang.String sypet_arg0, java.lang.String sypet_arg1, long sypet_arg2, long sypet_arg3, long sypet_arg4) throws Throwable{
//	     java.time.format.DateTimeFormatter var_0 = java.time.format.DateTimeFormatter.ofPattern(sypet_arg1);
//	     java.time.LocalDate var_1 = java.time.LocalDate.parse(sypet_arg0,var_0);
//	     java.time.LocalDate var_2 = var_1.plusDays(sypet_arg2);
//	     java.time.LocalDate var_3 = var_2.plusMonths(sypet_arg3);
//	     java.time.LocalDate var_4 = var_3.plusYears(sypet_arg4);
//	     java.lang.String var_5 = var_4.format(var_0);
//	     return var_5;
//	}

	public static java.lang.String after(java.lang.String sypet_arg0, java.lang.String sypet_arg1, long sypet_arg2, long sypet_arg3, long sypet_arg4) throws Throwable{
	      java.time.format.DateTimeFormatter var_0 = java.time.format.DateTimeFormatter.ofPattern(sypet_arg1);
	      java.time.LocalDate var_1 = java.time.LocalDate.parse(sypet_arg0,var_0);
	      java.time.LocalDate var_2 = var_1.plusYears(sypet_arg4);
	      java.time.LocalDate var_3 = var_2.plusMonths(sypet_arg3);
	      java.time.LocalDate var_4 = var_3.plusDays(sypet_arg2);
	      java.lang.String var_5 = var_4.format(var_0);
	      return var_5;
	}
	
	public static boolean test0() throws Throwable {
		return (before("2015/10/21", "yyyy/MM/dd",5,1,2).equals("2017/11/26"));
	}

	public static boolean test1() throws Throwable {
		return (before("2013/04/28", "yyyy/MM/dd",7,2,3).equals("2016/07/05"));
	}
//
	public static boolean test() throws Throwable {
		return test0() && test1();
	}
	

	public static void main(String[] args) {
		try {
			System.out.println("test = " + Target.test());
		} catch (Throwable e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}