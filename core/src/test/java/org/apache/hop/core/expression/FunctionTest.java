package org.apache.hop.core.expression;

import java.time.LocalDate;
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
		evalEquals("Iff(True,'True','False')", "True");
		evalEquals("Iff(False,'True','False')", "False");
		evalFails("Iff()");
		evalFails("Iff(true)");
		evalFails("Iff(true,2)");
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
		//evalEquals("NVL(null,1)", 1);
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
		evalEquals("Decode(NULL,1,'one',2,'two',Null,'<NULL>','other')","<NULL>");
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

//		LocalDateTime now = LocalDate.now().atStartOfDay();

//		evalEquals("Today()",now);
//		evalEquals("SysDate()",now);
//		evalEquals("CurDate()",now);
//		evalEquals("Current_Date()",now);
		evalFails("Current_Date(123)");

	}

	@Test
	public void LastDay() throws Exception {
		evalEquals("Last_day(Date '2019-01-01')", LocalDate.of(2019, Month.JANUARY, 31));
		evalEquals("Last_day(Date '2020-02-27')", LocalDate.of(2020, Month.FEBRUARY, 29));
		evalFails("Last_day()");
		evalFails("Last_day('test')");
	}

	@Test
	public void Upper() throws Exception {
		evalEquals("Upper('test')", "TEST");
		evalNull("Upper(NULL)");
		evalFails("Upper()");

		// Alias
		//evalEquals("UCase('test')", "TEST");
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
		evalEquals("RPad('test',7,'*')", "test***");
		evalEquals("RPad('test',4,'*')", "test");
		evalEquals("RPad('test',3,'*')", "tes");
		evalEquals("RPad('test',12,'')", "test");
		evalEquals("RPad('test',8,'ABC')", "testABCA");
	}

	@Test
	public void LPad() throws Exception {
		evalEquals("LPad('test',7,'*')", "***test");
		evalEquals("LPad('test',3,'*')", "tes");
		evalEquals("LPad('test',8,'ABC')", "ABCAtest");
		evalEquals("LPad('test',12,'')", "test");
		evalEquals("LPad('test',6,'ABC')", "ABtest");
		evalEquals("LPad('test',4,'ABC')", "test");
	}

	@Test
	public void Year() throws Exception {
		evalEquals("Year(Date '2019-01-01')", 2019);
		evalFails("Year()");
		
	}

	@Test
	public void MonthName() throws Exception {
		evalEquals("Month_Name(Date '2019-01-01')", "January");
		evalEquals("Month_Name(Date '2019-12-28')", "December");
		evalFails("Month_Name()");
	}

	@Test
	public void DayName() throws Exception {
		evalEquals("Day_Name(Date '2019-01-01')", "Tuesday");
		evalEquals("Day_Name(Date '2019-12-28')", "Saturday");
		evalFails("Day_Name()");
	}

	@Test
	public void Month() throws Exception {
		evalEquals("Month(Date '2019-01-01')", 1);
		evalEquals("Month(Date '2020-02-23')", 2);
		evalEquals("Month(Date '2019-12-28')", 12);
		evalFails("Month()");
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
		evalEquals("Day_Of_Week(Date '2019-01-01')", 3);
		evalEquals("Day_Of_Week(Date '2019-07-27')", 7);
		evalEquals("Day_Of_Week(Date '2019-07-28')", 1);
		evalEquals("Day_Of_Week(Date '2019-12-31')", 3);
		evalFails("Day_Of_Week()");
	}

	@Test
	public void DayOfMonth() throws Exception {
		evalEquals("Day_Of_Month(Date '2019-01-01')", 1);
		evalEquals("Day_Of_Month(Date '2019-02-28')", 28);
		evalEquals("Day_Of_Month(Date '2019-12-28')", 28);
		evalFails("Day_Of_Month()");

		// Alias
		evalEquals("Day(Date '2019-01-01')", 1);
	}

	@Test
	public void DayOfYear() throws Exception {
		evalEquals("Day_Of_Year(Date '2019-01-01')", 1);
		evalEquals("Day_Of_Year(Date '2019-02-02')", 33);
		evalEquals("Day_Of_Year(Date '2019-12-31')", 365);
	}

	@Test
	public void WeekOfYear() throws Exception {
		evalEquals("Week(Date '2019-01-01')", 1);
    	evalEquals("Week(Date '2019-12-31')", 53);
	}

	@Test
	public void AddMonths() throws Exception {
		evalEquals("Add_Months(Date '2019-01-15',1)", LocalDate.of(2019, Month.FEBRUARY, 15));
		evalEquals("Add_Months(Date '2019-01-15',-2)", LocalDate.of(2018, Month.NOVEMBER, 15));
		evalFails("Add_Months(Date '2019-01-15')");
	}

	@Test
	public void Lower() throws Exception {
		evalEquals("Lower('TesT')", "test");
		evalNull("Lower(NULL)");
		evalFails("Lower()");
		evalFails("Lower('Test','Test')");

		// Alias
		//evalEquals("LCase('TesT')", "test");
	}

	@Test
	public void Substr() throws Exception {
		evalEquals("Substr('TEST FROM',6)", "FROM");
		evalEquals("Substring('TEST FROM',6,2)", "FR");
		evalEquals("Substring('TEST FROM',1,4)", "TEST");
		evalEquals("Substring('TEST',5)", "");
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
	public void Cot() throws Exception {
		evalEquals("Cot(1)",0.6420926159343306);
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
		evalEquals("Mod(15,4)",3);
		evalNull("Mod(NULL,2)");
		evalFails("Mod()");
		evalFails("Mod(3)");
	}
	
	@Test
	public void Power() throws Exception {
		evalEquals("Power(3,2)",9);
		evalEquals("Power(100,0.5)",10);
		evalEquals("Power(0,0)",1);
		evalEquals("Power(123,0)",1);
		evalFails("Power()");
		evalFails("Power(3)");
		evalFails("Power(1,2,3)");
		evalNull("Power(NULL,2)");
		evalNull("Power(3,NULL)");
	}
	
	@Test
	public void Sign() throws Exception {
		evalEquals("Sign(0.3)",1);
		evalEquals("Sign(0)",0);
		evalEquals("Sign(-5)",-1);
		evalFails("Sign()");
		evalFails("Sign(1,2)");
		evalNull("Sign(NULL)");
	}
	
	@Test
	public void Cbrt() throws Exception {
		evalEquals("Cbrt(0)", 0);
		evalEquals("Cbrt(-343)",-7);
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
		
		// Alias
		//evalEquals("Char_Length('TEST')", 4);
		//evalEquals("Len('TEST')", 4);
	}
	
	@Test
	public void Bit_Length() throws Exception {
		// TODO: evalEquals("Bit_Length('TEST')", 64);
	}
	
	@Test
	public void Octet_Length() throws Exception {
		// TODO: evalEquals("Octet_Length('TEST')",4 );
		//evalEquals("Octet_Length(Unicode(392))", 2);			
	}
	
	@Test
	public void Left() throws Exception {
		evalEquals("Left('TEST FROM',4)", "TEST");
		evalEquals("Left('',1)", "");
		evalEquals("Left('TEST',10)", "TEST");
		evalFails("Left('TEST',-1)");
		evalNull("Left(NULL,4)");
		evalNull("Left('TEST',NULL)");
	}

	@Test
	public void Right() throws Exception {
		evalEquals("Right('TEST FROM',4)", "FROM");
		evalEquals("Right('',1)", "");
		evalEquals("Right('TEST',10)", "TEST");
		evalFails("Right('TEST',-1)");
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
		// TODO: evalEquals("To_Char(Date '-400-07-23','AD')", "BC");
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

		evalEquals("To_Char(Date '2018-07-23','YEAR')", "TWO THOUSAND EIGHTEEN");
		evalEquals("To_Char(Date '2018-07-23','year')", "two thousand eighteen");
		evalEquals("To_Char(Date '2019-07-23','SYEAR')", " TWO THOUSAND NINETEEN");
		// evalEquals("To_Char(Date '-19-07-23','SYEAR')", "");
		evalEquals("To_Char(Date '2019-07-23','YYYY')", "2019");
		evalEquals("To_Char(Date '2019-07-23','YYY')", "019");
		evalEquals("To_Char(Date '2019-07-23','YY')", "19");
		evalEquals("To_Char(Date '2019-07-23','Y')", "9");

		evalEquals("To_Char(Date '2019-07-23','Q')", "3");

		evalEquals("To_Char(Date '2019-07-23','MM')", "07");
		evalEquals("To_Char(Date '2019-07-23','Month')", "July     ");
		evalEquals("To_Char(Date '2019-07-23','MONTH')", "JULY     ");
		evalEquals("To_Char(Date '2019-09-23','month')", "september");
		evalEquals("To_Char(Date '2019-09-23','MON')", "SEP");
		evalEquals("To_Char(Date '2019-09-23','Mon')", "Sep");
		evalEquals("To_Char(Date '2019-09-23','mon')", "sep");

		evalEquals("To_Char(Date '2019-07-23','WW')", "30"); // Week of year
		evalEquals("To_Char(Date '2019-07-23','IW')", "30"); // Iso Week of year

		evalEquals("To_Char(Date '2019-07-21','D')", "1"); // Day of week
		evalEquals("To_Char(Date '2019-07-23','D')", "3"); // Day of week
		evalEquals("To_Char(Date '2019-07-08','DD')", "08"); // Day of month
		evalEquals("To_Char(Date '2019-07-23','DD')", "23"); // Day of month
		evalEquals("To_Char(Date '2019-07-23','DDD')", "204"); // Day of year
		evalEquals("To_Char(Date '2019-07-23','DAY')", "TUESDAY  "); // Day name
		evalEquals("To_Char(Date '2019-07-23','Day')", "Tuesday  "); // Day name
		evalEquals("To_Char(Date '2019-07-23','day')", "tuesday  "); // Day name
		evalEquals("To_Char(Date '2019-07-23','fmDay')", "Tuesday"); // Day name
		evalEquals("To_Char(Date '2019-07-23','DY')", "TUE"); // Day name
		evalEquals("To_Char(Date '2019-07-23','Dy')", "Tue"); // Day name
		evalEquals("To_Char(Date '2019-07-23','dy')", "tue"); // Day name

		evalEquals("To_Char(Date '2019-07-23','DS')", "07/23/2019"); // Date short
		// evalEquals("To_Char(Date '2019-07-23','DL')", "07/23/2019"); // Date long
		// evalEquals("To_Char(Date '2019-07-23','TS')", "07/23/2019"); // Time short

		evalEquals("To_Char(Date '2019-07-23','J')", "2458688");
	}

	@Test
	public void ToDate() throws Exception {
		//evalEquals("To_Date('2019-02-13','YYYY-MM-DD')", LocalDate.of(2019, 2, 13));
		//evalEquals("To_Date('11:30:40','hh:mi:ss')",LocalDateTime.of(1970,1,1,11,30,40)); 
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
		evalEquals("Ln(10)", 2.302585092994046);
		evalNull("Ln(null)");
		evalFails("Ln()");
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
		evalEquals("SHA384('Test')", "7b8f4654076b80eb963911f19cfad1aaf4285ed48e826f6cde1b01a79aa73fadb5446e667fc4f90417782c91270540f3");
	}
	
	
	@Test
	public void SHA512() throws Exception {
		evalEquals("SHA512('Test')", "c6ee9e33cf5c6715a1d148fd73f7318884b41adcb916021e2bc0e800a5c5dd97f5142178f6ae88c8fdd98e1afb0ce4c8d2c54b5f37b30b7da1997bb33b0b8a31");
	}
		
	@Test
	public void Rand() throws Exception {
		evalEquals("Rand(180)", 0.7406425740713104);
	}

}
