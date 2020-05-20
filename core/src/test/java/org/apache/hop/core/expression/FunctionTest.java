package org.apache.hop.core.expression;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.Currency;
import java.util.Locale;

import org.junit.Test;

public class FunctionTest extends ExpressionTest {

	@Test
	public void Coalesce() throws Exception {
		evalEquals("Coalesce(1,2,3)", 1);
		evalEquals("Coalesce(null,1,2)", 1);
		evalEquals("Coalesce(null,'TEST','BIDON')", "TEST");
		evalNull("Coalesce(null,null,null)");
		evalFails("Coalesce()");
	}

	@Test
	public void Iff() throws Exception {
		evalEquals("If(True,'True','False')", "True");
		evalEquals("If(False,'True','False')", "False");
		evalFails("If()");
		evalFails("If(true)");
		evalFails("If(true,2)");
	}

	@Test
	public void Nvl2() throws Exception {
		evalEquals("Nvl2(True,'ex1','ex2')", "ex1");
		evalEquals("Nvl2('test','ex1','ex2')", "ex1");
		evalEquals("Nvl2(NULL,'ex1','ex2')", "ex2");
		evalFails("Nvl2()");
		evalFails("Nvl2(true)");
		evalFails("Nvl2(true,2)");
	}

	@Test
	public void IfNull() throws Exception {
		evalEquals("IfNull(1,2)", 1);
		evalEquals("IfNull(null,1)", 1);
		evalEquals("IfNull(null,'TEST')", "TEST");
		evalFails("IfNull()");
		evalFails("IfNull(1)");
		evalFails("IfNull(1,2,3)");

		// Alias
		evalEquals("NVL(null,1)", 1);
	}

	@Test
	public void NullIf() throws Exception {
		evalEquals("NullIf(1,null)", 1);
		evalNull("NullIf(1,1)");
		evalNull("NullIf(NULL,1)");
		evalNull("NullIf('TEST','TEST')");
		evalNull("NullIf(Date '2019-01-01',Date '2019-01-01')");
		evalEquals("NullIf(1,2)", 1);
		evalEquals("NullIf('TEST','XXX')", "TEST");
		evalEquals("NullIf(Date '2019-01-01',Date '2018-12-31')", LocalDate.of(2019, Month.JANUARY, 1));
	}

	@Test
	public void Decode() throws Exception {
		evalEquals("Decode(1,1,'one',2,'two',Null,'<NULL>','other')", "one");
		evalEquals("Decode(2,1,'one',2,'two',Null,'<NULL>','other')", "two");
		evalEquals("Decode(NULL,1,'one',2,'two',Null,'<NULL>','other')", "<NULL>");
		evalEquals("Decode(9,1,'one',2,'two',Null,'<NULL>','other')", "other");
		evalNull("Decode(9,1,'one',2,'two',Null,'<NULL>')");
		evalFails("Decode()");
		evalFails("Decodo(1)");
		evalFails("Decode(1,2)");
	}

	@Test
	public void Pi() throws Exception {
		evalEquals("Pi()", Math.PI);
		evalFails("Pi(123)");
	}

	@Test
	public void CurrentDate() throws Exception {

//		evalEquals("Today()",now);
//		evalEquals("SysDate()",now);
//		evalEquals("CurDate()",now);
		evalEquals("Current_Date()", this.getContext().getCurrentDate());
		evalFails("Current_Date(123)");

	}

	@Test
	public void Date() throws Exception {
		evalEquals("Date(2019,01,01)", LocalDate.of(2019, Month.JANUARY, 1));
		evalEquals("Date(2020,02,27)", LocalDate.of(2020, Month.FEBRUARY, 27));
		evalFails("Date()");
		evalFails("Date(2020,13,22)");
	}

	@Test
	public void FirstDay() throws Exception {
		evalEquals("First_day(Date '2019-01-01')", LocalDate.of(2019, Month.JANUARY, 1));
		evalEquals("First_day(Date '2020-02-27')", LocalDate.of(2020, Month.FEBRUARY, 1));
		evalFails("First_day()");
		evalFails("First_day('test')");
	}

	@Test
	public void LastDay() throws Exception {
		evalEquals("Last_day(Date '2019-01-01')", LocalDate.of(2019, Month.JANUARY, 31));
		evalEquals("Last_day(Date '2020-02-27')", LocalDate.of(2020, Month.FEBRUARY, 29));
		evalFails("Last_day()");
		evalFails("Last_day('test')");
	}

	@Test
	public void NextDay() throws Exception {
		evalEquals("Next_day(Date '2020-02-28','monday')", LocalDate.of(2020, Month.MARCH, 2));
		
		evalNull("Next_day(null, 'monday')");
		evalNull("Next_day(Date '2020-02-28', null)");
		
		evalFails("Next_day()");
		evalFails("Next_day(Date '2020-02-28')");
	}
	
	
	@Test
	public void Upper() throws Exception {
		evalEquals("Upper('test')", "TEST");
		evalNull("Upper(NULL)");
		evalFails("Upper()");

		// Alias
		// evalEquals("UCase('test')", "TEST");
	}

	@Test
	public void InitCap() throws Exception {
		evalEquals("InitCap('hello the wORLD')", "Hello The World");
		evalNull("InitCap(NULL)");
	}

	@Test
	public void Instr() throws Exception {
		evalEquals("Instr('abcdefgh','abc')", 1);
		evalEquals("Instr('abcdefgh','ABC')", 0);
		evalEquals("Instr('abcdefgh','ef')", 5);
		evalEquals("Instr('abcdefgh','efa')", 0);
		evalEquals("Instr('abecdefgh','e',5)", 6);

		evalNull("Instr(NULL,'test')");
		evalNull("Instr('test',NULL)");
		evalNull("Instr(NULL,NULL)");
		evalFails("Instr()");
	}

	@Test
	public void RPad() throws Exception {
		evalEquals("RPad('test',7)", "test   ");
		evalEquals("RPad('test',7,'*')", "test***");
		evalEquals("RPad('test',4,'*')", "test");
		evalEquals("RPad('test',3,'*')", "tes");
		evalEquals("RPad('test',12,'')", "test");
		evalEquals("RPad('test',8,'ABC')", "testABCA");
		evalFails("RPad('test')");
		evalFails("RPad('test',-8)");
	}

	@Test
	public void LPad() throws Exception {
		evalEquals("LPad('test',6)", "  test");
		evalEquals("LPad('test',7,'*')", "***test");
		evalEquals("LPad('test',3,'*')", "tes");
		evalEquals("LPad('test',8,'ABC')", "ABCAtest");
		evalEquals("LPad('test',12,'')", "test");
		evalEquals("LPad('test',6,'ABC')", "ABtest");
		evalEquals("LPad('test',4,'ABC')", "test");
		evalFails("LPad('test')");
		evalFails("LPad('test',-8)");
	}

	@Test
	public void Year() throws Exception {
		evalEquals("Year(Date '2019-01-01')", 2019);
		evalNull("Year(null)");
		evalFails("Year()");
	}

	@Test
	public void MonthName() throws Exception {
		evalEquals("MonthName(Date '2019-01-01')", "January");
		evalEquals("MonthName(Date '2019-12-28')", "December");
		evalFails("MonthName()");
	}

	@Test
	public void DayName() throws Exception {
		evalEquals("DayName(Date '2019-01-01')", "Tuesday");
		evalEquals("DayName(Date '2019-12-28')", "Saturday");
		evalFails("DayName()");
	}

	@Test
	public void Month() throws Exception {
		evalEquals("Month(Date '2019-01-01')", 1);
		evalEquals("Month(Date '2020-02-23')", 2);
		evalEquals("Month(Date '2019-12-28')", 12);
		evalFails("Month()");
	}

	@Test
	public void Months_Between() throws Exception {
		evalEquals("Months_Between(Date '2005-01-01',Date '2005-02-02')", 1.032258064516129);
		evalEquals("Months_Between(Date '2007-11-09',Date '2003-12-28')", -45.54838709677419);
		// If the months and days are identical, the result is an integer.
		evalEquals("Months_Between(Date '2007-11-09',Date '2007-12-09')", 0.967741935483871);
	}

	@Test
	public void Years_Between() throws Exception {
		evalEquals("Years_Between(Timestamp '2001-01-01 12:00:00',Timestamp '2000-01-01 00:00:00')", -1);
	}

	@Test
	public void Minutes_Between() throws Exception {
		evalEquals("Minutes_Between(Timestamp '2019-01-01 15:00:59',Timestamp '2019-01-01 15:28:59')", 28);
		evalEquals("Minutes_Between(Timestamp '2019-01-01 15:00:59',Timestamp '2019-01-02 15:00:59')", 1440);
	}

	@Test
	public void Quarter() throws Exception {
		evalEquals("Quarter(Date '2019-01-01')", 1);
		evalEquals("Quarter(Date '2019-02-28')", 1);
		evalEquals("Quarter(Date '2019-04-28')", 2);
		evalEquals("Quarter(Date '2019-08-28')", 3);
		evalEquals("Quarter(Date '2019-12-28')", 4);
		evalFails("Quarter()");
	}

	@Test
	public void DayOfWeek() throws Exception {
		evalEquals("DayOfWeek(Date '2019-01-01')", 3);
		evalEquals("DayOfWeek(Date '2019-07-27')", 7);
		evalEquals("DayOfWeek(Date '2019-07-28')", 1);
		evalEquals("DayOfWeek(Date '2019-12-31')", 3);
		evalFails("DayOfWeek()");
	}

	@Test
	public void DayOfMonth() throws Exception {
		evalEquals("Day(Date '2019-01-01')", 1);
		evalEquals("Day(Date '2019-02-28')", 28);
		evalEquals("Day(Date '2019-12-28')", 28);
		evalFails("Day()");

		// Alias
		evalEquals("DayOfMonth(Date '2019-01-01')", 1);
	}

	@Test
	public void DayOfYear() throws Exception {
		evalEquals("DayOfYear(Date '2019-01-01')", 1);
		evalEquals("DayOfYear(Date '2019-02-02')", 33);
		evalEquals("DayOfYear(Date '2019-12-31')", 365);
	}

	@Test
	public void WeekOfYear() throws Exception {
		evalEquals("Week(Date '2019-01-01')", 1);
		evalEquals("Week(Date '2019-12-31')", 53);
	}

	@Test
	public void Add_Months() throws Exception {
		evalEquals("Add_Months(Date '2019-01-15',1)", LocalDate.of(2019, Month.FEBRUARY, 15));
		evalEquals("Add_Months(Date '2019-01-15',-2)", LocalDate.of(2018, Month.NOVEMBER, 15));
		evalEquals("Add_Months(Date '2019-11-15',3)", LocalDate.of(2020, Month.FEBRUARY, 15));
		// the resulting month has fewer days
		evalEquals("Add_Months(Date '2019-01-31',1)", LocalDate.of(2019, Month.FEBRUARY, 28));

		evalFails("Add_Months(Date '2019-01-15')");
	}

	@Test
	public void Hour() throws Exception {
		evalEquals("Hour(Timestamp '2019-01-01 15:28:59')", 15);
	}

	@Test
	public void Minute() throws Exception {
		evalEquals("Minute(Timestamp '2019-01-01 15:28:59')", 28);
	}

	@Test
	public void Second() throws Exception {
		evalEquals("Second(Timestamp '2019-01-01 15:28:59')", 59);
	}

	@Test
	public void Lower() throws Exception {
		evalEquals("Lower('TesT')", "test");
		evalNull("Lower(NULL)");
		evalFails("Lower()");
		evalFails("Lower('Test','Test')");

		// Alias
		evalEquals("LCase('TesT')", "test");
	}

	@Test
	public void Substring() throws Exception {
		evalEquals("Substring('TEST FROM',6)", "FROM");
		evalEquals("Substring('TEST FROM',6,2)", "FR");
		evalEquals("Substring('TEST FROM',1,4)", "TEST");
		
		// Alias
		evalEquals("Substr('TEST',5)", "");
		evalEquals("Mid('TEST',5)", "");
	}

	@Test
	public void Space() throws Exception {
		evalEquals("Space(4)", "    ");
		evalFails("Space()");
	}

	@Test
	public void Acos() throws Exception {
		evalEquals("Acos(0)", 1.5707963267948966);
		evalEquals("Acos(1)", 0);
		evalFails("Acos(10)");
		evalFails("Acos()");
		evalNull("Acos(NULL)");
	}

	@Test
	public void Acosh() throws Exception {
		evalEquals("Acosh(1)", 0);
		evalEquals("Acosh(3)", 1.762747174039086);
		evalFails("Acosh()");
		evalNull("Acosh(NULL)");
	}

	@Test
	public void Asinh() throws Exception {
		//evalEquals("Asinh(asin(0.5))", 0.5);
		evalFails("Asinh()");
		evalNull("Asinh(NULL)");
	}

	@Test
	public void Atanh() throws Exception {
		//evalEquals("Atanh(0.2)", 0.9);
		evalFails("Atanh()");
		evalNull("Atanh(NULL)");
	}

	
	@Test
	public void Cot() throws Exception {
		evalEquals("Cot(1)", 0.6420926159343306);
		evalFails("Cot(0)");
		evalFails("Cot()");
		evalNull("Cot(NULL)");
	}

	@Test
	public void Exp() throws Exception {
		evalEquals("Exp(2)", 7.38905609893065);
		evalNull("Exp(NULL)");
		evalFails("Exp()");
		evalFails("Exp(1,2)");
	}

	@Test
	public void Mod() throws Exception {
		evalEquals("Mod(15,4)", 3);
		evalNull("Mod(NULL,2)");
		evalFails("Mod()");
		evalFails("Mod(3)");
	}

	@Test
	public void Power() throws Exception {
		evalEquals("Power(3,2)", 9);
		evalEquals("Power(100,0.5)", 10);
		evalEquals("Power(0,0)", 1);
		evalEquals("Power(123,0)", 1);
		evalFails("Power()");
		evalFails("Power(3)");
		evalFails("Power(1,2,3)");
		evalNull("Power(NULL,2)");
		evalNull("Power(3,NULL)");
	}

	@Test
	public void Sign() throws Exception {
		evalEquals("Sign(0.3)", 1);
		evalEquals("Sign(0)", 0);
		evalEquals("Sign(-5)", -1);
		evalFails("Sign()");
		evalFails("Sign(1,2)");
		evalNull("Sign(NULL)");
	}

	@Test
	public void Cbrt() throws Exception {
		evalEquals("Cbrt(0)", 0);
		evalEquals("Cbrt(-343)", -7);
		evalFails("Cbrt()");
		evalNull("Cbrt(NULL)");
	}

	@Test
	public void Sqrt() throws Exception {
		evalEquals("Sqrt(10)", 3.1622776601683795);
		evalFails("Sqrt(-5)");
		evalFails("Sqrt()");
		evalNull("Sqrt(NULL)");
	}

	@Test
	public void Trim() throws Exception {
		evalEquals("Trim('a')", "a");
		evalEquals("Trim(' a ')", "a");
		evalEquals("Trim('  a b  ')", "a b");
		evalEquals("Trim('01ABC10 ', '012')", "ABC10 ");
		evalEquals("Trim(' 01ABC10 ', ' 012')", "ABC");

		evalNull("Trim(NULL)");
		evalFails("Trim()");
	}

	@Test
	public void LTrim() throws Exception {
		evalEquals("LTrim('a')", "a");
		evalEquals("LTrim(' a ')", "a ");
		evalEquals("LTrim(' a',NULL)", "a");
		evalEquals("LTrim('01ABC', '012')", "ABC");
		evalNull("LTrim(NULL)");
		evalFails("LTrim()");
	}

	@Test
	public void RTrim() throws Exception {
		evalEquals("RTrim('a')", "a");
		evalEquals("RTrim(' a ')", " a");
		evalEquals("RTrim('a ',NULL)", "a");
		evalEquals("Trim('210ABC10 ', '012')", "ABC10 ");
		evalNull("RTrim(NULL)");
		evalFails("RTrim()");
	}

	@Test
	public void Greatest() throws Exception {
		evalEquals("Greatest(5,2,null,9,4)", 9);
		evalEquals("Greatest('B','A','C')", "C");
	}

	@Test
	public void Least() throws Exception {
		evalEquals("Least(5,2,null,9,4)", 2);
	}

	@Test
	public void Length() throws Exception {
		evalEquals("Length('TEST')", 4);
	}

	@Test
	public void Bit_Length() throws Exception {
		// TODO: evalEquals("Bit_Length('TEST')", 64);
	}

	@Test
	public void Left() throws Exception {
		evalEquals("Left('TEST FROM',4)", "TEST");
		evalEquals("Left('',1)", "");
		evalEquals("Left('TEST',10)", "TEST");
		evalEquals("Left('TEST',-1)", "");
		evalNull("Left(NULL,4)");
		evalNull("Left('TEST',NULL)");
	}

	@Test
	public void Right() throws Exception {
		evalEquals("Right('TEST FROM',4)", "FROM");
		evalEquals("Right('',1)", "");
		evalEquals("Right('TEST',10)", "TEST");
		evalEquals("Right('TEST',-1)", "");
		evalNull("Right(NULL,4)");
		evalNull("Right('TEST',NULL)");
	}

	@Test
	public void Replace() throws Exception {
		evalEquals("Replace('ABCD','CD')", "AB");
		evalEquals("Replace('ABCDEFCD','CD','EF')", "ABEFEFEF");
		evalNull("Replace(NULL,'CD','EF')");
		evalNull("Replace('ABCD',NULL,'EF')");
	}

	@Test
	public void ToBoolean() throws Exception {
		evalTrue("To_Boolean('True')");
		evalTrue("To_Boolean('t')");
		evalTrue("To_Boolean('yes')");
		evalTrue("To_Boolean('on')");
		evalTrue("To_Boolean('1')");
		evalTrue("To_Boolean(5)");
		evalTrue("To_Boolean(-1)");

		evalFalse("To_Boolean('False')");
		evalFalse("To_Boolean('off')");
		evalFalse("To_Boolean('NO')");
		evalFalse("To_Boolean('F')");
		evalFalse("To_Boolean('n')");
		evalFalse("To_Boolean('0')");
		evalFalse("To_Boolean(0)");

		evalNull("To_Boolean(NULL)");
		evalFails("To_Boolean()");
		evalFails("To_Boolean(1,2,3)");
	}

	@Test
	public void ToChar() throws Exception {
		Currency currency = Currency.getInstance(Locale.getDefault());
		String cs = currency.getSymbol();

		// Text
		evalEquals("To_Char('abc')", "abc");
		evalNull("To_Char(NULL)");

		// Number
		evalEquals("To_Char(12,'99')", " 12");
		evalEquals("To_Char(12,'S99')", "+12");
		evalEquals("To_Char(12,'99S')", "12+");
		// evalEquals("To_Char(12,'MI99')", " 12");
		evalEquals("To_Char(12,'99MI')", "12 ");
		evalEquals("To_Char(12,'$99')", " " + cs + "12");

		evalEquals("To_Char(-7,'99')", " -7");
		// evalEquals("To_Char(485,'9 9 9')", " 4 8 5");

		evalEquals("To_Char(-7,'S99')", " -7");
		evalEquals("To_Char(-7,'99S')", " 7-");
		// evalEquals("To_Char(-7,'MI99')", "- 7");
		evalEquals("To_Char(-7,'99MI')", " 7-");
		evalEquals("To_Char(-7,'$99')", " -" + cs + "7");

		evalEquals("To_Char(0.45)", ".45");
		evalEquals("To_Char(12923)", "12923");
		evalEquals("To_Char(12923,'99999.99')", " 12923.00");
		evalEquals("To_Char(12,'9990999.9')", "    0012.0");
		// evalEquals("To_Char(12923,'FM99999.99')", "12923.00");
		evalEquals("To_Char(0.3,'00.99')", " 00.30");
		evalEquals("To_Char(0.3,'FM00.99')", "00.3");
		evalEquals("To_Char(0.3,'99.00000')", "   .30000");
		evalEquals("To_Char(12345,'9,999')", "######");
		evalEquals("To_Char(123.456,'9.9EEEE')", "  1.2E+02");
		evalEquals("To_Char(0,'9999999')", "       0");
		evalEquals("To_Char(11,'FMRN')", "XI");
		evalEquals("To_Char(123,'XX')", " 7B");
		// evalEquals("To_Char(123,'\"Number:\"999')", "Number: 123");

		// Date
		evalEquals("To_Char(Date '2019-07-23','AD')", "AD");
		evalEquals("To_Char(Date '2019-07-23','BC')", "AD");
		evalEquals("To_Char(Date '2019-07-23','Bc')", "Ad");
		evalEquals("To_Char(Date '2019-07-23','bc')", "ad");
		evalEquals("To_Char(Date '2019-07-23','A.D.')", "A.D.");
		evalEquals("To_Char(Date '2019-07-23','B.C.')", "A.D.");
		evalEquals("To_Char(Date '2019-07-23','B.c.')", "A.d.");
		evalEquals("To_Char(Date '2019-07-23','b.c.')", "a.d.");

		evalEquals("To_Char(Date '2019-07-23','CC')", "21");
		evalEquals("To_Char(Date '2000-07-23','CC')", "20");
		evalEquals("To_Char(Date '2019-07-23','SCC')", " 21");
		evalEquals("To_Char(Date '2000-07-23','SCC')", " 20");
		evalEquals("To_Char(Date '2000-07-23','FMSCC')", "20");
		evalEquals("To_Char(To_Date('-0200','SYYYY'),'SCC')", "-02");
		evalEquals("To_Char(To_Date('-0200','SYYYY'),'FMSCC')", "-2");
		
		evalEquals("To_Char(Date '2018-07-23','YEAR')", "TWO THOUSAND EIGHTEEN");
		evalEquals("To_Char(Date '2018-07-23','year')", "two thousand eighteen");
		evalEquals("To_Char(Date '2019-07-23','SYEAR')", " TWO THOUSAND NINETEEN");
		evalEquals("To_Char(Date '2019-07-23','YYYY')", "2019");
		evalEquals("To_Char(Date '0800-07-23','YYYY')", "0800"); 
		evalEquals("To_Char(Date '0800-07-23','FMYYYY')", "800"); // Year compact
		
		evalEquals("To_Char(Date '2019-07-23','YYY')", "019");
		evalEquals("To_Char(Date '2019-07-23','YY')", "19");
		evalEquals("To_Char(Date '2019-07-23','Y')", "9");
		evalEquals("To_Char(Date '2019-07-23','SYYYY')", " 2019");
		evalEquals("To_Char(To_Date('-2000','SYYYY'),'YYYY BC')", "2000 BC");
		evalEquals("To_Char(To_Date('-800','SYYYY'),'SYYYY')", "-0800"); // Negative signed year 
		evalEquals("To_Char(To_Date('-800','SYYYY'),'YYYY BC')", "0800 BC");
		evalEquals("To_Char(Date '0800-07-23','FMSYYYY')", "800"); // Signed year compact
		evalEquals("To_Char(To_Date('-800','SYYYY'),'FMSYYYY BC')", "-800 BC"); // Negative signed year compact
		 				
		evalEquals("To_Char(Date '2019-07-23','Q')", "3");

		evalEquals("To_Char(Date '2019-07-23','MM')", "07"); // Month number
		evalEquals("To_Char(Date '2019-07-23','FMMM')", "7"); // Month number compact
		evalEquals("To_Char(Date '2019-07-23','Month')", "July     ");
		evalEquals("To_Char(Date '2019-07-23','MONTH')", "JULY     ");
		evalEquals("To_Char(Date '2019-07-23','FMMONTH')", "JULY");
		evalEquals("To_Char(Date '2019-09-23','month')", "september");
		evalEquals("To_Char(Date '2019-09-23','MON')", "SEP");
		evalEquals("To_Char(Date '2019-09-23','Mon')", "Sep");
		evalEquals("To_Char(Date '2019-09-23','mon')", "sep");

		evalEquals("To_Char(Date '2019-07-23','WW')", "30"); // Week of year
		evalEquals("To_Char(Date '2019-07-23','IW')", "30"); // Iso Week of year

		evalEquals("To_Char(Date '2019-07-21','D')", "1"); // Day of week
		evalEquals("To_Char(Date '2019-07-23','D')", "3"); // Day of week
		evalEquals("To_Char(Date '2019-07-08','DD')", "08"); // Day of month
		evalEquals("To_Char(Date '2019-07-08','FMDD')", "8"); // Day of month compact
		evalEquals("To_Char(Date '2019-07-23','DD')", "23"); // Day of month
		evalEquals("To_Char(Date '2019-02-23','DDD')", "054"); // Day of year
		evalEquals("To_Char(Date '2019-02-23','FMDDD')", "54"); // Day of year compact
		
		evalEquals("To_Char(Date '2019-07-23','DAY')", "TUESDAY  "); // Day name
		evalEquals("To_Char(Date '2019-07-23','Day')", "Tuesday  "); // Day name
		evalEquals("To_Char(Date '2019-07-23','day')", "tuesday  "); // Day name
		evalEquals("To_Char(Date '2019-07-23','fmDay')", "Tuesday"); // Day name compact
		evalEquals("To_Char(Date '2019-07-23','DY')", "TUE"); // Day name
		evalEquals("To_Char(Date '2019-07-23','Dy')", "Tue"); // Day name
		evalEquals("To_Char(Date '2019-07-23','dy')", "tue"); // Day name

		evalEquals("To_Char(Date '2019-07-23','DS')", "07/23/2019"); // Date short
		// evalEquals("To_Char(Date '2019-07-23','DL')", "07/23/2019"); // Date long
		// evalEquals("To_Char(Date '2019-07-23','TS')", "07/23/2019"); // Time short

		evalEquals("To_Char(Date '2019-07-23','J')", "2458688");
		
		evalEquals("To_Char(Date '2019-07-23','$(FMMONTH)!')", "$(JULY)!"); // Special char
		
	}

	@Test
	public void ToDate() throws Exception {
		evalEquals("To_Date('2019-02-13','YYYY-MM-DD')", LocalDate.of(2019, 2, 13));
		evalEquals("To_Date('2020148','YYYYDDD')", LocalDate.of(2020, 5, 27));
		evalEquals("To_Date('2020-08','YYYY-MM')", LocalDate.of(2020, 8, 1));
		evalEquals("To_Date('2020-MarCH','YYYY-MONTH')", LocalDate.of(2020, 3, 1));
		evalEquals("To_Date('2020,feb,25','YYYY,MON,DD')", LocalDate.of(2020, 2, 25));		
		evalEquals("To_Date('2019-02-13 15:34:56','YYYY-MM-DD HH24:MI:SS')", LocalDateTime.of(2019, 2, 13, 15, 34, 56));
		evalEquals("To_Date('01/02/2020','DD/MM/YYYY')", LocalDate.of(2020, 2, 1));
		evalEquals("To_Date('01/II/2020','DD/RM/YYYY')", LocalDate.of(2020, 2, 1));

		evalEquals("To_Date('01/02/-100','DD/MM/SYYYY')", LocalDate.of(-100, 2, 1));

		evalEquals("To_Date('01/2/0001','DD/MM/RRRR')", LocalDate.of(2001, 2, 1));
		evalEquals("To_Date('01/2/52','DD/MM/RRRR')", LocalDate.of(1952, 2, 1));
		evalEquals("To_Date('01/2/0923','DD/MM/RRRR')", LocalDate.of(923, 2, 1));

		// Rule to try alternate format MM -> MON and MONTH
		evalEquals("To_Date('01/Feb/2020','DD/MM/YYYY')", LocalDate.of(2020, 2, 1));
		// Rule to try alternate format MM -> MON and MONTH
		evalEquals("To_Date('01/February/2020','DD/MM/YYYY')", LocalDate.of(2020, 2, 1));
		// Rule to try alternate format MON ->  MONTH
		evalEquals("To_Date('01/February/2020','DD/MON/YYYY')", LocalDate.of(2020, 2, 1));
		// Rule to try alternate format MONTH ->  MON
		evalEquals("To_Date('01/Feb/2020','DD/MONTH/YYYY')", LocalDate.of(2020, 2, 1));
		

		// '12-02-2008' is 2454803 in julian,
		evalEquals("To_Date('2454803','J')", LocalDate.of(2008, 12, 2));
		

		
				
		// Is interpreted as 10 February 2003
		//evalEquals("To_Date('06-2003-MON','WW-YYYY-DY')", LocalDate.of(2003, 2, 10));

		// Is interpreted as 31 December 2003, 12:59:33
		evalEquals("To_Date('12:59:33 365-2003', 'HH24:MI:SS DDD-YYYY')", LocalDateTime.of(2003, 12, 31, 12, 59, 33));

		// Is interpreted as 24 December 2009, 23:00:00
		evalEquals("To_Date('2009-12-24 11:00:00 PM','YYYY-MM-DD HH12:MI:SS AM')", LocalDateTime.of(2009, 12, 24, 23, 0, 0));

		// Is interpreted as 12 May 2003, 00:00:10.123
		//evalEquals("To_Date('2000_MAY_12 10.123','YYYY_MONTH_DD SS.FF3');
		
		
		// evalEquals("To_Date('15:30:40','hh24:mi:ss')",LocalDateTime.of(1970,1,1,11,30,40));
	}

	@Test
	public void Reverse() throws Exception {
		evalEquals("Reverse('Hello, world!')", "!dlrow ,olleH");
	}

	@Test
	public void Soundex() throws Exception {
		evalEquals("Soundex('I LOVE ROCKS.')", "I416");
		evalEquals("Soundex('I LOVE ROCK AND ROLL MUSIC.')", "I416");
	}

	@Test
	public void Translate() throws Exception {
		evalEquals("Translate('Hello, world!','eo','EO')", "HEllO, wOrld!");
		evalNull("Translate('Hello, world!',NULL,'EO')");
		evalNull("Translate(NULL,'eo','EO')");
	}

	@Test
	public void Extract() throws Exception {
		evalEquals("Extract(MILLENNIUM from Timestamp '2020-05-25 23:48:59')", 3);
		evalEquals("Extract(CENTURY from Timestamp '2000-12-25 23:48:59')", 20);
		evalEquals("Extract(CENTURY from Timestamp '2020-05-25 23:48:59')", 21);
		evalEquals("Extract(CENTURY from Date '0001-01-01')", 1);
		evalEquals("Extract(DECADE from Timestamp '1999-02-16 20:38:40')", 199);
		evalEquals("Extract(YEAR from Timestamp '2020-05-25 23:48:59')", 2020);
		evalEquals("Extract(QUARTER from Timestamp '2020-05-25 23:48:59')", 2);
		evalEquals("Extract(MONTH from Timestamp '2020-05-25 23:48:59')", 5);
		evalEquals("Extract(WEEK from Timestamp '2020-05-25 23:48:59')", 21);
		evalEquals("Extract(WEEK_ISO from Date '2010-01-03')", 53);
		evalEquals("Extract(WEEK_ISO from Date '2010-01-04')", 1);
		evalEquals("Extract(DAY from Timestamp '2020-05-25 23:48:59')", 25);
		evalEquals("Extract(DD from Timestamp '2020-05-25 23:48:59')", 25);
		evalEquals("Extract(DAYOFWEEK from Timestamp '2020-05-25 23:48:59')", 2);
		evalEquals("Extract(DAYOFWEEK_ISO from Date '2003-12-28')", 7);
		evalEquals("Extract(HOUR from Timestamp '2020-05-25 23:48:59')", 23);
		evalEquals("Extract(MINUTE from Timestamp '2020-05-25 23:48:59')", 48);
		evalEquals("Extract(SECOND from Timestamp '2020-05-25 23:48:59')", 59);
		evalEquals("Extract(millisecond from Time '00:00:01.1234567')", 123);
		evalEquals("Extract(microsecond from Time '00:00:01.1234567')", 123456);
		evalEquals("Extract(nanosecond from Time '00:00:01.1234567')", 123456700);

		// evalEquals("Date_Part(nanosecond,Time '00:00:01.1234567')", 123456700);

		evalNull("Extract(SECOND from NULL)");
	}

	@Test
	public void Truncate() throws Exception {

		// Truncate numeric
		evalEquals("Truncate(-975.975,-1)", -970);
		evalEquals("Truncate(-975.975,0)", -975);
		evalEquals("Truncate(-975.975,2)", -975.97);
		evalEquals("Truncate(-975.975,3)", -975.975);
		evalEquals("Truncate(-975.975,50)", -975.975);
		evalEquals("Truncate(123.456,-2)", 100);
		evalNull("Truncate(-975.975,Null)");

		// Truncate date time
		evalEquals("Truncate(DATE '2020-05-08','year')", LocalDate.of(2020, Month.JANUARY, 1));
		evalEquals("Truncate(DATE '2020-05-08','MONTH')", LocalDate.of(2020, Month.MAY, 1));
		evalEquals("Truncate(DATE '2020-05-25','DAY')", LocalDate.of(2020, Month.MAY, 25));
		evalEquals("Truncate(Timestamp '2020-05-25 23:59:59','DAY')", LocalDate.of(2020, Month.MAY, 25));
		evalEquals("Truncate(Timestamp '2020-05-25 23:59:59','HOUR')",
				LocalDateTime.of(2020, Month.MAY, 25, 23, 0, 0, 0));
		evalEquals("Truncate(Timestamp '2020-05-25 23:59:59','MINUTE')",
				LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 0, 0));
		evalEquals("Truncate(Timestamp '2020-05-25 23:59:59','SeCoNd')",
				LocalDateTime.of(2020, Month.MAY, 25, 23, 59, 59, 0));

		evalNull("Truncate(NULL,2)");
	}

	@Test
	public void StartsWith() throws Exception {
		evalTrue("StartsWith('TEST FROM','TES')");
		evalFalse("StartsWith('-TEST FROM','TES')");
	}

	@Test
	public void EndsWith() throws Exception {
		evalTrue("EndsWith('TEST FROM','ROM')");
		evalFalse("EndsWith('TEST FROM','ROMA')");
	}

	@Test
	public void Regexp_Like() throws Exception {
		evalTrue("Regexp_Like('12345TEST','123[:alnum:]*')");
		evalTrue("Regexp_Like('ABcdf987','[:xdigit:]*')");
	}

	@Test
	public void EqualsNull() throws Exception {
		evalFalse("Equal_Null(1,null)");
		evalFalse("Equal_Null(null,true)");
		evalTrue("Equal_Null(null,null)");
		evalFails("Equal_Null(NOM)");
		evalTrue("Equal_Null(Date '2019-01-01',Date '2019-01-01')");
		evalFalse("Equal_Null(Date '2019-01-01',Date '2018-01-01')");
	}

	@Test
	public void Concat() throws Exception {
		evalEquals("'tes'||'t'", "test");
		evalEquals("Concat('tes','t')", "test");
		evalEquals("Concat('a','b','c')", "abc");
		evalEquals("Concat(NULL,'a')", "a");
		evalEquals("Concat('a',NULL)", "a");
		evalNull("Concat(NULL,NULL)");
	}

	@Test
	public void Chr() throws Exception {
		evalEquals("Chr(83)", "S");
		evalEquals("Chr(115)", "s");
		evalEquals("Chr(233)", "é");
		evalEquals("Chr(945)", "α");
		evalEquals("Chr(8364)", "€");
		evalEquals("Chr(33288)", "興");
		evalNull("Chr(NULL)");
		evalFails("Chr()");
		evalFails("Chr(-1)");
		evalFails("Chr(999999999999)");
	}

	@Test
	public void Ascii() throws Exception {
		evalEquals("Ascii('ABC')", 65);
		evalEquals("Ascii('é')", 233);
		evalEquals("Ascii('€')", 8364);
		evalEquals("Ascii('興')", 33288);
		evalEquals("Ascii('')", 0);
		evalNull("Ascii(NULL)");
		evalFails("Ascii()");
	}

	@Test
	public void Unicode() throws Exception {
		evalEquals("Unicode('SSSS')", 83);
		evalEquals("Unicode('é')", 233);
		evalEquals("Unicode('€')", 8364);
		evalEquals("Unicode('')", 0);
		evalNull("Unicode(NULL)");
		evalFails("Unicode()");
	}

	@Test
	public void StringEncode() throws Exception {
		evalEquals("StringEncode('	')", "\\t");
	}

	@Test
	public void StringDecode() throws Exception {
		evalEquals("StringDecode('\t')", "\t");
	}

	@Test
	public void UrlEncode() throws Exception {
		evalEquals("UrlEncode('a b')", "a+b");
		evalEquals("UrlEncode('a+b')", "a%2Bb");
	}

	@Test
	public void UrlDecode() throws Exception {
		evalEquals("UrlDecode('a+b')", "a b");
		evalEquals("UrlDecode('a%2Bb')", "a+b");
	}

	@Test
	public void Ceil() throws Exception {
		evalEquals("Ceil(1)", 1);
		evalEquals("Ceil(125.9)", 126);
		evalEquals("Ceil(0.4873)", 1.0);
		evalEquals("Ceil(-0.65)", 0);
		evalEquals("Ceil(-42.8)", -42);
		evalNull("Ceil(null)");
		evalFails("Ceil()");
		evalFails("Ceil(1,2,3)");
		evalFails("Ceil('x')");
	}

	@Test
	public void Floor() throws Exception {
		evalEquals("Floor(1)", 1);
		evalEquals("Floor(125.9)", 125);
		evalEquals("Floor(0.4873)", 0.0);
		evalEquals("Floor(-0.65)", -1);
		evalNull("Floor(null)");
		evalFails("Floor()");
		evalFails("Floor(1,2,3)");
		evalFails("Floor('x')");
	}

	@Test
	public void Round() throws Exception {
		evalEquals("Round(1)", 1);
		evalEquals("Round(125.9)", 126);
		evalEquals("Round(0.4873)", 0.0);
		evalEquals("Round(-0.65)", -1);
		evalNull("Round(null)");
		evalFails("Round()");
		evalFails("Round(1,2,3)");
		evalFails("Round('x')");
	}

	@Test
	public void Ln() throws Exception {
		evalEquals("Ln(1)", 0);
		evalEquals("Exp(Ln(2))", 2);
		evalEquals("Ln(10)", 2.302585092994046);
		evalNull("Ln(null)");
		evalFails("Ln(0)");
		evalFails("Ln()");
	}

	@Test
	public void Log() throws Exception {
		evalEquals("Log(10,100)", 2);
		evalNull("Log(10,null)");
		evalNull("Log(null,1)");
		evalFails("Log(1)");
	}

	@Test
	public void Degrees() throws Exception {
		evalEquals("Degrees(Pi())", 180);
		evalFails("Degrees()");
		evalFails("Degrees(1,2)");
	}

	@Test
	public void Radians() throws Exception {
		evalEquals("Radians(180)", 3.141592653589793);
		evalFails("Radians()");
		evalFails("Radians(1,2)");
	}

	@Test
	public void MD5() throws Exception {
		evalEquals("MD5('Test')", "0cbc6611f5540bd0809a388dc95a615b");
	}

	@Test
	public void SHA1() throws Exception {
		evalEquals("SHA1('Test')", "640ab2bae07bedc4c163f679a746f7ab7fb5d1fa");
	}

	@Test
	public void SHA256() throws Exception {
		evalEquals("SHA256('Test')", "532eaabd9574880dbf76b9b8cc00832c20a6ec113d682299550d7a6e0f345e25");
	}

	@Test
	public void SHA384() throws Exception {
		evalEquals("SHA384('Test')",
				"7b8f4654076b80eb963911f19cfad1aaf4285ed48e826f6cde1b01a79aa73fadb5446e667fc4f90417782c91270540f3");
	}

	@Test
	public void SHA512() throws Exception {
		evalEquals("SHA512('Test')",
				"c6ee9e33cf5c6715a1d148fd73f7318884b41adcb916021e2bc0e800a5c5dd97f5142178f6ae88c8fdd98e1afb0ce4c8d2c54b5f37b30b7da1997bb33b0b8a31");
	}

	@Test
	public void Rand() throws Exception {
		evalEquals("Rand(180)", 0.7406425740713104);
	}

}
