package org.apache.hop.core.expression;

import java.math.BigDecimal;

import org.junit.Test;

public class OperatorTest extends ExpressionTest {

	@Test
	public void precedence() throws Exception {
		evalEquals("3*5/2", ( ( 3 * 5 ) / 2 ));
		evalTrue("( 3 > 5 ) IS FALSE");
	}

	@Test
	public void Equals() throws Exception {
		evalTrue("NAME = 'TEST'");
		evalTrue("Age = 40");
		evalTrue("FLAg = true");
		evalTrue("2.000 = 2");
		evalTrue("2.000 = 2.00");
		evalTrue("-1.4e-10 = -1.4e-10");
		evalTrue("15 = 0xF");
		// TODO: fixme evalTrue("'0.0' = 0");
		evalTrue("15.0 = '15'");
		evalTrue("'.01' = 0.01");

		evalTrue("true = true");
		evalTrue("true = 1");
		evalTrue("true = 'Y'");
		evalTrue("true = 'Yes'");
		evalTrue("true = 'True'");

		evalTrue("false = false");
		evalFalse("true = false");
		evalFalse("false = true");

		evalNull("1 = null");
		evalNull("null = true");
		evalNull("null = null");
		evalFails("NOM = ");
		evalTrue("Date '2019-01-01' = Date '2019-01-01'");
		evalFalse("Date '2019-01-01' = Date '2018-01-01'");
	}

	@Test
	public void NotEquals() throws Exception {
		evalTrue("'bar' != 'foo'");		
		evalTrue("NAME <> 'tEST'");
		evalFalse("Age <> 40");
		evalFalse("Age != 40");
		evalTrue("1 <> 2");
		evalTrue("10 <> 0x10");
		evalFalse("1 <> '1'");

		evalTrue("true <> false");
		evalTrue("false <> true");
		evalFalse("true <> true");
		evalFalse("false <> false");

		evalTrue("null <> 'bar'");
		evalFalse("null <> null");
		evalFalse("2 <> 2.000");
		evalFalse("2.000 <> 2.00");
		evalFalse("true <> true");
		evalTrue("Date '2019-01-01' <> Date '2018-01-01'");
		evalFalse("Date '2019-01-01' <> Date '2019-01-01'");
	}

	@Test
	public void GreaterThan() throws Exception {
		evalTrue("9>5");
		evalTrue("9.4>9.358");
		evalTrue("(4+2)>10-9");
		evalTrue("Age>10");
		evalFalse("5>5");

		evalFalse("false > true");
		evalFalse("false > false");
		evalFalse("true > true");
		evalTrue("true > false");

		evalFalse("'bar' > 'foo'");
		evalFalse("'foo' > 'foo'");
		evalTrue("'foo' > 'bar'");

		evalTrue("Date '2019-02-01' > Date '2019-01-01'");
		evalFalse("Date '2019-01-01' > Date '2019-01-01'");
		evalFalse("Date '2018-01-01' > Date '2019-01-01'");
	}

	@Test
	public void GreaterThanOrEquals() throws Exception {
		evalTrue("9 >= 5");
		evalTrue("9.4 >= 9.358");
		evalTrue("(4+2) >= 10-9");
		evalTrue("Age >= 10");
		evalTrue("5 >= 5");

		evalFalse("false >= true");
		evalTrue("false >= false");
		evalTrue("true >= true");
		evalTrue("true >= false");

		evalFalse("'bar' >= 'foo'");
		evalTrue("'foo' >= 'foo'");
		evalTrue("'foo' >= 'bar'");

		evalTrue("Date '2019-02-01' >= Date '2019-01-01'");
		evalTrue("Date '2019-01-01' >= Date '2019-01-01'");
		evalFalse("Date '2018-01-01' >= Date '2019-01-01'");
	}

	@Test
	public void LessThan() throws Exception {
		evalTrue("5 < 9");
		evalTrue("9.358 < 9.4");
		evalTrue("10-9 < (4+2)");
		evalTrue("Age < 100");
		evalFalse("5 < 5");

		evalFalse("true < false");
		evalTrue("false < true");
		evalFalse("false < false");
		evalFalse("true < true");

		evalTrue("'bar' < 'foo'");
		evalFalse("'foo' < 'foo'");
		evalFalse("'foo' < 'bar'");

		evalTrue("Date '2019-01-01' < Date '2019-02-01'");
		evalFalse("Date '2019-01-01' < Date '2019-01-01'");
		evalFalse("Date '2019-01-01' < Date '2018-01-01'");

	}

	@Test
	public void LessThanOrEquals() throws Exception {
		evalTrue("5 <= 9");
		evalTrue("9.358 <= 9.4");
		evalTrue("10-9 <= (4+2)");
		evalTrue("Age <= 100");
		evalTrue("5 <= 5");

		evalTrue("false <= false");
		evalTrue("true <= true");
		evalTrue("false <= true");
		evalFalse("true <= false");

		evalTrue("'foo' <= 'foo'");
		evalTrue("'bar' <= 'foo'");
		evalFalse("'foo' <= 'bar'");

		evalTrue("Date '2019-01-01' <= Date '2019-02-01'");
		evalTrue("Date '2019-01-01' <= Date '2019-01-01'");
		evalFalse("Date '2019-01-01' <= Date '2018-01-01'");
	}

	@Test
	public void In() throws Exception {
		evalTrue("SEX in ('?','F','RM')");
		evalTrue("SEX not in ('?','-','!')");
		evalTrue("2 in (1,2,3)");
		evalTrue("2.5 IN (1,2.5,3)");
		evalTrue("'2' in (1,2,3)");
		evalTrue("Date '2019-01-01' in (Date '2019-04-01',Date '2019-01-01',Date '2019-03-06')");
		evalFalse("2 in (1,2.5,3)");
		evalFails("2 in (1,2.5,)");
		evalFails("2 in ()");
	}

	@Test
	public void Is() throws Exception {
		evalTrue("True IS True");
		evalTrue("True IS NOT False");
		evalTrue("FLAG is True");
		evalFalse("True IS False");
		evalTrue("False IS False");
		evalFalse("False IS Null");
		evalFalse("Null is True");
		evalFalse("Null IS False");
		evalTrue("Null IS NULL");
	}

	@Test
	public void Arithmetic() throws Exception {
		evalEquals("10*2+1", 21);
		evalEquals("1+10*2", 21);
		evalEquals("10*(2+1)", 30);
		evalEquals("30/(5+5)", 3);
		evalEquals("42%(3+2)", 2);
		evalEquals("1-2+3*4/5/6-7", (((1 - 2) + (((3 * 4) / 5) / 6)) - 7));
		
		evalEquals("Age-(10+3*10+50-2*25)", 0);
		//evalEquals("10**2+5", 105);
		//evalEquals("5+10**2", 105);
		//evalEquals("3*10**2", 300);
		//evalEquals("10**2*3", 300);
		
		// evalEquals("2*'1.23'",2.46); // TODO: Should be casted to number and not
		// integer
	}	
	
	@Test
	public void Between() throws Exception {
		evalTrue("3 between 1 and 5");
		evalTrue("3 between 3 and 5");
		evalTrue("5 between 3 and 5");
		evalTrue("'CH' between 'A' and 'E'");
		evalFalse("1 between 3 and 5");
		evalTrue("Age between 39.999 and 40.0001");
		evalTrue("Age not between 10 and 20");
		evalFails("Age between 10 and");

		evalTrue("Date '2019-02-28' between Date '2019-01-01' and Date '2019-12-31'");

		// precedence of BETWEEN is higher than AND and OR, but lower than '+'
		//evalTrue("10 between 5 and 8 + 2 or False and True");
	}

	@Test
	public void Cast() throws Exception {
		// String to boolean
		evalTrue("Cast('Yes' as Boolean)");
		evalFalse("Cast('No' as Boolean)");
		evalTrue("Cast('Y' as Boolean)");	
		evalFalse("Cast('n' as Boolean)");		
		evalTrue("Cast('On' as Boolean)");
		evalFalse("Cast('Off' as Boolean)");
		evalTrue("Cast('True' as Boolean)");
		evalFalse("Cast('False' as Boolean)");

		// Number to boolean
		evalTrue("Cast(1 as Boolean)");
		evalTrue("Cast(-12.1 as Boolean)");
		evalFalse("Cast(0 as Boolean)");	
		

		evalEquals("Cast(true as String)", "TRUE");
		evalEquals("Cast(123.6 as String)", "123.6");

		 
		evalEquals("Cast('1234' as Number)", new Double(1234));
		evalEquals("Cast('1234.567' as Integer)", new Long(1234));
		// TODO: evalEquals("Cast('0x123' as Integer)", new Long(291));
		
		evalEquals("Cast(12345678901234567890123456789012345678901234567890 as BigNumber)", new BigDecimal("12345678901234567890123456789012345678901234567890"));
		
		evalFails("Cast(123 as Nill)");
	}

	@Test
	public void Negate() throws Exception {
		evalEquals("-40", -40);
		evalEquals("-Age", -40);
		evalNull("-null");
		evalEquals("+40", 40);

	}

	@Test
	public void Multiply() throws Exception {
		evalEquals("4*10", 40);
		evalEquals("-4*-1", 4);
		evalEquals("2*-2", -4);
		evalNull("null*1");
	}

	@Test
	public void Divide() throws Exception {
		evalEquals("40/10", 4);
		evalEquals("-40/-10", 4);
		evalNull("null/1");
		evalFails("40/0");
	}

    // TODO: remove or implement power operator
	public void Power() throws Exception {		
		evalEquals("4**2", 16);
		evalEquals("-4**2", -16);
		evalNull("null**1");
		evalEquals("4**0", 1);
		evalEquals("-4**0", -1);
		evalEquals("(-2)**0", 1);
		evalEquals("2**(3**2)",512);
		evalEquals("2**3**2",512);
		//-2**2=-(2**2)=-4
		evalEquals("-2**2", -4);
	}

	@Test
	public void BitwiseNot() throws Exception {
		//evalEquals("~1", -2);
		//evalEquals("~0", -1);
		//evalEquals("~0xFF", 0x1);
	}

	@Test
	public void BitwiseAnd() throws Exception {
		evalEquals("3 & 2", 2);
		evalEquals("100 & 2", 0);
	}
	
	@Test
	public void BitwiseOr() throws Exception {
		evalEquals("100 | 2", 102);
		evalEquals("3 | 2", 3);
	}

	@Test
	public void BitwiseXor() throws Exception {
		evalEquals("2 ^ 2", 0);
		evalEquals("2 ^1", 3);
		evalEquals("100 ^ 2", 102);
	}
	
	@Test
	public void LogicalNot() throws Exception {

		evalTrue("FLAG is not false");
		evalTrue("NULLIS is null");
		evalTrue("NOT (NULLIS is not null)");
		evalFalse("NOT 1");
		evalTrue("NOT 0");
		evalNull("NOT NULL");
		evalFails("FLAG is ");
	}

	@Test
	public void LogicalOr() throws Exception {
		evalTrue("true OR true");
		evalTrue("true OR false");
		evalTrue("false OR true");
		evalFalse("false OR false");
		evalTrue("true OR null");
		evalTrue("null OR true");
		evalFalse("false OR null");
		evalFalse("null OR false");
		evalNull("null OR null");
	}

	@Test
	public void LogicalAnd() throws Exception {
		evalTrue("true AND true");
		evalFalse("true AND false");
		evalFalse("false AND true");
		evalFalse("false AND false");
		evalNull("true AND null");
		evalNull("null AND true");
		evalNull("false AND null");
		evalNull("null AND false");
		evalNull("null AND null");
	}

	@Test
	public void LogicalXor() throws Exception {
		evalTrue("false XOR true");
		evalTrue("true XOR false");
		evalFalse("false XOR false");
		evalFalse("true XOR true");
		evalNull("true XOR null");
		evalNull("null XOR true");
		evalNull("null XOR null");
	}

	@Test
	public void Like() throws Exception {
		evalTrue("NAME like 'TES%'");
		evalTrue("NAME not like 'X%'");
		evalFalse("NAME like 'X%'");
		evalTrue("'Tuesday' like '%es%'");

		// values that starts with "a" and ends with "o"
		evalTrue("'amigo' like 'a%o'");
		evalTrue("'ao' like 'a%o'");
		// values that starts with "a" and are at least 3 characters in length
		evalTrue("'ami' like 'a_%_%'");
		evalTrue("'amigo' like 'a_%_%'");
		evalTrue("'Friday' like '___day'");
		evalFalse("'am' like 'a_%_%'");

		evalTrue("'100%' like '100^%' escape '^'");
		evalFalse("'100' like '100^%' escape '^'");


		// evalTrue("'Amigo' like '[A-C]%'");

		// NULL does not match NULL
		evalFalse("NULL like NULL");
	}

	@Test
	public void Concat() throws Exception {
		evalEquals("CONCAT('TES','T')", "TEST");
		evalTrue("NAME='TES'||'T'");
		evalTrue("NAME='TES'||NULLIS||'T'");
		evalEquals("'TEST'||null", "TEST");
		evalEquals("null||'TEST'", "TEST");
		evalNull("null||null");
	}

	@Test
	public void Contains() throws Exception {
		evalTrue("CONTAINS(NAME,'ES')");
		evalTrue("NAME =~ 'ES'");
		evalTrue("NAME =~ 'TEST'");
	}

	@Test
	public void CaseWhen() throws Exception {
		evalEquals("case when Age=40 then 10 else 50 end", 10);
		evalEquals("case when Age=10+20 then 1*5  when Age=20+20 then 2*5 else 50 end", 10);
		evalEquals("case Age when 10 then 10 when 40 then 40 else 50 end", 40);
	}

}
