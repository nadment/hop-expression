/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.core.expression;

import static org.junit.Assert.assertEquals;
import org.apache.hop.expression.Identifier;
import org.junit.Test;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

public class OperatorTest extends BaseExpressionTest {

  @Test
  public void Comment() throws Exception {
    evalTrue(" // Test line comment \n  true ");
    evalTrue(" /* Test block comment */  true ");
    evalTrue(" true /* Test block comment */");
    evalTrue("/*\n * Comment on multi line\n *\n */ True");
    evalTrue("/*\n * Comment on multi line \n  with nesting: /* nested block comment */ *\n */   True");

    // Single line comment
    evalTrue("// Single line comment\nTrue");
    evalTrue("-- Single line comment\nTrue");
    
    // Multi line comment
    evalTrue("/* Line 1\n * Line 2 */ True");

    // Empty
    evalFails("-- Single line comment\n");    
    evalFails(" /");
    evalFails("/*   True");
    evalFails("/   True");
    evalFails("/*   True*");
    evalFails("/* /* nested block comment */    True");
  }

  @Test
  public void Identifier() throws Exception {

    Identifier identifier = new Identifier("NAME");
    assertEquals("NAME", identifier.getName());
    assertEquals(new Identifier("NAME"), identifier);
    assertEquals(new Identifier("NAME").hashCode(), identifier.hashCode());

    evalEquals("Age%2", 0);
    evalEquals(" \t\n\"Age\"%2", 0);
    evalEquals("\"IDENTIFIER SPACE\"", "SPACE");
    evalEquals("\"IDENTIFIER_UNDERSCORE\"", "UNDERSCORE");
    evalEquals("\"IDENTIFIER lower\"", "lower");

    evalFails("\"");
    evalFails(" \" ");    
    evalFails(" \"");
    evalFails("\"\"");

    writeEquals("IDENTIFIER");
    // Reserved word
    writeEquals("\"CASE\"");
    writeEquals("\"LIKE\"");
    // Data type name
    writeEquals("\"NUMBER\"");
    // Date part name
    writeEquals("\"CENTURY\"");
    // Function name
    writeEquals("\"YEAR\"");
    writeEquals("\"UPPER\"");
    // Contains space
    writeEquals("\"IDENTIFIER SPACE\"+1");
  }


  @Test
  public void CoercionBoolean() throws Exception {

    // Coercion Number to Boolean
    evalTrue("true = 1");
    evalTrue("false = 0");

    // Coercion String to Boolean
    evalTrue("'1'=true");
    evalTrue("'On'=true");
    evalTrue("'Y'=true");
    evalTrue("true = 'Y'");
    evalTrue("'Yes'=true");
    evalTrue("true = 'Yes'");
    evalTrue("'T'=true");
    evalTrue("'TRUE'=true");
    evalTrue("true = 'True'");

    evalTrue("'0'=false");
    evalTrue("'N'=false");
    evalTrue("'NO'=false");
    evalTrue("'OFF'=false");
    evalTrue("'F'=false");
    evalTrue("'FALSE'=false");
  }

  @Test
  public void ReservedWord() throws Exception {
    evalEquals("Upper(\"FROM\")", "PARIS");
  }

  @Test
  public void SyntaxError() throws Exception {
    evalFails("'T'||'T");
    evalFails("\"T\"||\"T");
    evalFails("9!7");
    evalFails("9+(");
    evalFails("9+*(");
    evalFails("Year(");
    evalFails("Year)");
    evalFails("Year()");
    evalFails("Year(()");
    evalFails("Year())");
    evalFails("Year(1,2)");
    evalFails("TRUE AND");
    evalFails("5 BETWEEN 4 AND");
    evalFails("5 BETWEEN 4 OR");
    evalFails("case when 1=1 then 1 else 0");
    evalFails("case when 1=1 then 1 else  end ");
    evalFails("case 1 when 1  else 0 end");
    evalFails("Cast(3 as NILL)");
    evalFails("1 in ()    ");
    evalFails("1 in (,2,3)");
    evalFails("1 in (1,2,3");
    evalFails("1 in (1,,3)");
    evalFails("1 in (1,2,)");
    evalFails("Date '2020-20-28'");
  }

  @Test
  public void precedenceAndAssociativity() throws Exception {

    // Arithmetic
    evalEquals("3*5/2", ((3 * 5) / 2d));
    evalEquals("9/3*3", (9 / 3) * 3);
    evalEquals("1 + 2 * 3 * 4 + 5", ((1 + ((2 * 3) * 4)) + 5));
    evalEquals("1-2+3*4/5/6-7", 1 - 2 + 3 * 4d / 5d / 6d - 7);
    evalEquals("10*2+1", 21);
    evalEquals("1+10*2", 21);
    evalEquals("10*(2+1)", 30);
    evalEquals("30/(5+5)", 3);
    evalEquals("42%(3+2)", 2);
    evalEquals("1-2+3*4/5/6-7", (((1d - 2d) + (((3d * 4d) / 5d) / 6d)) - 7d));
    evalEquals("Age-(10+3*10+50-2*25)", 0);

    evalEquals("2*'1.23'", 2.46);
    // integer

    // NOT has higher precedence than AND, which has higher precedence than OR
    evalTrue("NOT false AND NOT false");
    evalTrue("NOT 5 = 5 OR NOT 'Test' = 'X' AND NOT 5 = 4");

    // Equals (=) has higher precedence than NOT "NOT (1=1)"
    evalTrue("NOT 2 = 1");

    // IS NULL has higher precedence than NOT
    evalFalse("NOT \"NULLIS\" IS NULL");

    // IS NULL has lower precedence than comparison (1 = 1) IS NULL
    evalFalse("1 = 1 is null");
    evalTrue(" 3 > 5 IS FALSE");

    // BETWEEN, IN, LIKE have higher precedence than comparison
    // evalTrue("6 = 6 between 4 = 4 and 5 = 5");
  }


  @Test
  public void EqualTo() throws Exception {
    evalTrue("NAME = 'TEST'");
    evalTrue("Age = 40");
    evalTrue("FLAg = true");
    evalTrue("2.000 = 2");
    evalTrue("2.000 = 2.00");
    evalTrue("-1.4e-10 = -1.4e-10");

    // Integer and Binary
    evalTrue("15 = 0xF");
    evalTrue("'0.0' = 0");
    evalTrue("0.0 = '0.000'");
    evalTrue("15.0 = '15'");
    evalTrue("'.01' = 0.01");

    // Boolean
    evalTrue("true = true");
    evalTrue("false = false");
    evalFalse("true = false");
    evalFalse("false = true");

    // String
    evalTrue("'ABC' = 'ABC'");
    evalFalse("'ABC' = 'abc'");

    // Date
    evalTrue("Date '2019-01-01' = Date '2019-01-01'");
    evalFalse("Date '2019-01-01' = Date '2018-01-01'");

    // Null
    evalNull("1 = null");
    evalNull("null = true");
    evalNull("null = false");
    // NULL is not equal ( = ) to anything—not even to another NULL.
    evalNull("null = null");

    evalFails("NOM=");
    evalFails("NOM = ");

    writeEquals("AGE=40");
  }

  @Test
  public void NotEqualTo() throws Exception {
    evalTrue("'bar' != 'foo'");
    evalTrue("NAME <> 'tEST'");
    evalFalse("Age != 40");
    evalFalse("Age <> 40");

    evalTrue("1 <> 2");
    evalTrue("10 <> 0x10");
    evalFalse("1 <> '1'");

    evalTrue("true <> false");
    evalTrue("false <> true");
    evalFalse("true <> true");
    evalFalse("false <> false");

    evalFalse("2 <> 2.000");
    evalFalse("2.000 <> 2.00");
    evalFalse("true <> true");
    evalTrue("Date '2019-01-01' <> Date '2018-01-01'");
    evalFalse("Date '2019-01-01' <> Date '2019-01-01'");

    evalNull("null <> 'bar'");
    evalNull("'bar' <> null");
    evalNull("null <> null");

    evalFails("NOM<>");
    evalFails("NOM <> ");
    evalFails("NOM!");
    evalFails("NOM ! ");

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

    evalNull("null > 0");
    evalNull("1 > null");

    evalFails("NOM>");
    evalFails("NOM > ");
  }

  @Test
  public void GreaterThanOrEqualTo() throws Exception {
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

    evalNull("null >= 0");
    evalNull("1 >= null");

    evalFails("NOM>=");
    evalFails("NOM >=");
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

    evalNull("null < 1");
    evalNull("0 < null");

    evalFails("NOM<");
    evalFails("NOM < ");
  }

  @Test
  public void LessThanOrEqualTo() throws Exception {
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

    evalNull("null <= 1");
    evalNull("0 <= null");

    evalFails("NOM<=");
    evalFails("NOM <=");
  }

  @Test
  public void In() throws Exception {
    evalTrue("SEX in ('?','F','RM')");
    evalTrue("SEX not in ('?','-','!')");
    evalTrue("2 in (1,2,3)");
    evalTrue("2.5 IN (1,2.5,3)");
    evalTrue("'2' in (null,1,2,3)");
    evalTrue("Date '2019-01-01' in (Date '2019-04-01',Date '2019-01-01',Date '2019-03-06')");
    evalFalse("2 in (1,2.5,3)");
    evalTrue("2 in (null,1,2,3)");
    evalFalse("2 in (null,null,null)");
    evalFalse("1 not in (null,1)");
    evalNull("NULL in (1,2,3)");
    evalNull("NULL in (null)");

    evalFails("2 in (1,2.5,)");
    evalFails("2 in ()");

    writeEquals("AGE IN (10,20,30,40)");
  }

  @Test
  public void Is() throws Exception {
    evalTrue("True IS True");
    evalTrue("True IS NOT False");
    evalTrue("FLAG is True");
    evalFalse("True IS False");
    evalFalse("True IS Null");
    evalTrue("False IS False");
    evalFalse("False IS Null");
    evalFalse("Null is True");
    evalFalse("Null IS False");
    evalTrue("Null IS NULL");

    writeEquals("FIELD IS TRUE");
  }

  @Test
  public void Addition() throws Exception {
    evalEquals("Add(10,-0.5)", 9.5);
    evalEquals("0xF+0", 15);
    evalEquals("0b00011+0", 3);
    evalEquals("-24.7+0.5+24.7+0.5E-2", 0.505);
    evalEquals("PRICE+PRICE", -10.24);
    evalEquals("AMOUNT+1", 123456.789 + 1);

    evalEquals("Date '2019-02-25'+1", LocalDate.of(2019, 2, 26));
    evalEquals("Date '2019-02-25'+1.5", LocalDateTime.of(2019, 2, 26, 12, 0, 0));
    evalEquals("Date '2019-02-25'+5/(60*24)", LocalDateTime.of(2019, 2, 25, 0, 5, 0));

    evalNull("5+NULL+5");
    evalNull("+NULL+5");
    evalFails("5+");
    evalFails("TRUE+FALSE");
  }

  @Test
  public void Subtract() throws Exception {
    evalEquals("Subtract(10,-0.5)", 10.5);
    evalEquals("Age-0.5", 39.5);
    evalEquals("Date '2019-02-25'-1", LocalDate.of(2019, 2, 24));
    evalEquals("Date '2019-02-25'-28", LocalDate.of(2019, 1, 28));
    evalEquals("Date '2019-02-25'-0.5", LocalDateTime.of(2019, 2, 24, 12, 0, 0));
    evalEquals("Date '2019-02-25'-5/(60*24)", LocalDateTime.of(2019, 2, 24, 23, 55, 0));

    evalEquals("Date '2019-02-25'-Date '2019-02-23'", 2);
    evalEquals("Date '2019-02-23'-Date '2019-02-25'", -2);
    evalEquals("Date '2019-02-25'-to_Date('2019-02-23 12:00','YYYY-MM-DD HH24:MI')", 1.5);

    evalNull("5-NULL");
    evalNull("NULL-5");

    evalFails("5-");
    evalFails("TRUE-FALSE");
  }

  @Test
  public void Between() throws Exception {
    evalTrue("3 between 1 and 5");
    evalTrue("3 between 3 and 5");
    evalTrue("5 between 3 and 5");
    evalTrue("-1 between -3+1 and 5");
    evalTrue("'the' between 'that' and 'then'");
    evalFalse("1 between 3 and 5");
    evalTrue("Age between 39.999 and 40.0001");
    evalTrue("Age not between 10 and 20");
    evalTrue("Age not between 10 and 20 and 'Test' is not null");

    evalTrue("Date '2019-02-28' between Date '2019-01-01' and Date '2019-12-31'");

    evalNull("NULL between -10 and 20");
    evalNull("1 between NULL and 20");
    evalNull("1 between -10 and NULL");

    evalFails("Age between 10 and");
    evalFails("Age between and 10");
    evalFails("Age between and ");

    writeEquals("FIELD BETWEEN 10 AND 20");
  }

  @Test
  public void Cast() throws Exception {

    // String to Boolean
    evalTrue("'Yes'::Boolean");
    evalTrue("'Yes' :: Boolean");
    evalTrue("CAST('YES' as Boolean)");
    evalTrue("CAST('yes' as Boolean)");
    evalTrue("CAST('Yes' as Boolean)");
    evalFalse("CAST('No' as Boolean)");
    evalFalse("CAST('no' as Boolean)");
    evalFalse("CAST('nO' as Boolean)");
    evalTrue("CAST('Y' as Boolean)");
    evalFalse("CAST('n' as Boolean)");
    evalTrue("CAST('ON' as Boolean)");
    evalTrue("CAST('on' as Boolean)");
    evalTrue("CAST('On' as Boolean)");
    evalTrue("CAST('oN' as Boolean)");
    evalFalse("CAST('OFF' as Boolean)");
    evalFalse("CAST('off' as Boolean)");
    evalFalse("CAST('Off' as Boolean)");
    evalTrue("CAST('TRUE' as Boolean)");
    evalTrue("CAST('true' as Boolean)");
    evalTrue("CAST('True' as Boolean)");
    evalFalse("CAST('FALSE' as Boolean)");
    evalFalse("CAST('false' as Boolean)");
    evalFalse("CAST('False' as Boolean)");


    // Number to Boolean
    evalTrue("CAST(1 as Boolean)");
    evalTrue("1::Boolean");
    evalTrue("CAST(-12.1 as Boolean)");
    evalFalse("CAST(0 as Boolean)");

    // Number to Integer
    evalEquals("CAST(1.25 as Integer)", 1);
    // Oracle truncate to 1 and Snowflake round it to 2
    // TODO: evalEquals("CAST(1.75 as Integer)",2);

    // Boolean to String
    evalEquals("CAST(true as String)", "TRUE");
    evalEquals("true::String", "TRUE");


    // Date to String
    evalEquals("CAST(Date '2019-02-25' AS String)", "2019-02-25");
    evalEquals("CAST(Date '2019-02-25' AS String FORMAT 'DD/MM/YYYY')", "25/02/2019");

    // evalEquals("Cast(Time '23:48:59' as String)", "23:48:59");

    // evalEquals("Timestamp '1900-01-04 12:00'::Number", 3.5);

    // evalEquals("Time '23:48:59'+1.5", "23:48:59");

    // Integer to String
    evalEquals("CAST(12923 AS STRING)", "12923");
    evalEquals("CAST(-1234 AS STRING FORMAT '9999MI')", "1234-");

    // Number to String
    evalEquals("CAST(123.6 as String)", "123.6");
    evalEquals("123.6::String", "123.6");
    evalEquals("CAST(0.45 AS STRING)", ".45");
    evalEquals("CAST(0.45 AS STRING FORMAT 'FM000.00')", "000.45");
    evalEquals("CAST(1234.56 AS STRING FORMAT '9999MI')", "1234 ");

    // String to Integer
    evalEquals("CAST('1234' as Integer)", 1234L);
    evalEquals("'1234'::Integer+5", 1239L);
    evalEquals("CAST('1234.567' as Integer)", 1235L);

    // String to Number
    evalEquals("'1234'::Number", 1234d);
    evalEquals("CAST('1234' as Number)", 1234d);
    evalEquals("CAST('1234.567' as Number)", 1234.567d);
    evalEquals("CAST('  -1e-37  ' as Number)", -1e-37d);

    // String to Date
    evalEquals("CAST('2020-march' as DATE FORMAT 'YYYY-MONTH')", LocalDate.of(2020, 3, 1));
    evalEquals("CAST('2020-01-19 11:23:44' as DATE FORMAT 'YYYY-MM-DD HH:MI:SS')", LocalDateTime.of(2020, 1, 19, 11,23,44));
    
    // Binary to Integer
    evalEquals("CAST(0x123 as Integer)", 291L);


    evalEquals("TO_NUMBER('123','000')::INTEGER+1", 124);


    evalEquals("CAST(12345678901234567890123456789012345678 as BigNumber)",
        new BigDecimal("12345678901234567890123456789012345678"));

    evalNull("CAST(Null as Binary)");
    evalNull("CAST(Null as Boolean)");
    evalNull("CAST(Null as String)");
    evalNull("CAST(Null as Integer)");
    evalNull("CAST(Null as Number)");
    evalNull("CAST(Null as BigNumber)");

    // Unsupported conversion
    evalFails("CAST(Date '2019-02-25' AS INTEGER)");
    evalFails("CAST(Date '2019-02-25' AS NUMBER)");
    evalFails("CAST(TRUE AS DATE)");
    evalFails("CAST(Date '2019-02-25' AS BOOLEAN )");
    evalFails("CAST(Date '2019-02-25' AS BOOLEAN)");

    // Bad syntax
    evalFails("'1234':");
    evalFails("'1234':NUMBER");
    evalFails("'1234'::");
    evalFails("CAST('bad' AS)");
    evalFails("CAST('bad' AS NULL)");
    evalFails("CAST('2020-01-01' AS NULL)");
    evalFails("CAST(1234 AS STRING FORMAT )");
    evalFails("CAST(Date '2019-02-25' AS String FORMAT )");
    evalFails("CAST(Date '2019-02-25' AS String FORMAT NULL)");


    // Bad data type
    evalFails("Cast(123 as Nill)");

    // FIXME:  writeEquals("CAST(NULL AS BINARY)", "CAST(NULL AS BINARY)");
    // FIXME: writeEquals("CAST('1234' AS NUMBER)", "CAST('1234' AS NUMBER)");
    // FIXME:  writeEquals("CAST('2020-12-15' AS DATE FORMAT 'YYYY-MM-DD')");
    // FIXME: writeEquals("'1234'::NUMBER", "CAST('1234' AS NUMBER)");
  }

  @Test
  public void Positive() throws Exception {
    evalEquals("+(40)", 40);
    evalEquals("+(Age)", 40);
    evalEquals("+40", 40);
    evalEquals("1+ +2", 3);
    evalNull("+null");
  }

  @Test
  public void Negative() throws Exception {
    evalEquals("-(1+2)", -3);
    evalEquals("-40", -40);
    evalEquals("-Age", -40);
    evalEquals("+40", 40);
    evalEquals("1+ -2", -1);
    evalNull("-null");
  }

  @Test
  public void Multiply() throws Exception {
    evalEquals("Multiply(2.5,10)", 25D);
    evalEquals("4*10", 40D);
    evalEquals("-4*-1", 4D);
    evalEquals("2*-2", -4D);
    evalEquals("1.23456::BigNumber*-2.987654", -3.68843812224);
    evalNull("null*1");
    evalNull("1*null");
  }

  @Test
  public void Divide() throws Exception {
    evalEquals("Divide(10,4)", 2.5);
    evalEquals("10/4", 2.5);
    evalEquals("40/-10", -4D);
    evalEquals("-40/-10", 4D);
    evalEquals("5/2", 2.5);
    evalNull("null/1");
    evalNull("null/0");
    evalNull("1/null");
    evalFails("40/0");
  }

  @Test
  public void BitNot() throws Exception {
    evalEquals("~1", -2);
    evalEquals("~ 1", -2);
    evalEquals("~0", -1);
    evalEquals("~4", -5);
    evalEquals("~65504", -65505);
    evalNull("~NULL");
    evalFails("~");
    evalFails("~ ");

    // Alias
    evalEquals("BITNOT(1)", -2);
  }

  @Test
  public void BitAnd() throws Exception {
    evalEquals("BITAND(3,2)", 2);
    evalEquals("3 & 2", 2);
    evalEquals("100 & 2", 0);
    evalNull("100 & null");
    evalNull("NULL & 100");
    evalFails("100&");
    evalFails("100 & ");
  }

  @Test
  public void BitOr() throws Exception {
    evalEquals("BITOR(100,2)", 102);
    evalEquals("100 | 2", 102);
    evalEquals("3 | 2", 3);
    evalNull("100 | null");
    evalNull("NULL | 100");
    evalFails("3|");
    evalFails("3 | ");
  }

  @Test
  public void BitXor() throws Exception {
    evalEquals("BITXOR(2,2)", 0);
    evalEquals("2 ^ 1", 3);
    evalEquals("100 ^ 2", 102);
    evalNull("100 ^ null");
    evalNull("NULL ^ 100");
    evalFails("100^");
    evalFails("100 ^ ");
  }

  @Test
  public void BoolNot() throws Exception {
    evalTrue("FLAG is not false");
    evalTrue("NULLIS is null");
    evalTrue("NOT (NULLIS is not null)");
    evalFalse("NOT 1");
    evalTrue("NOT 0");
    evalTrue("NOT NOT True");
    evalNull("NOT NULL");
    evalFails("FLAG is ");
    evalFails("NOT");
    writeEquals("FIELD IS NOT TRUE", "NOT FIELD IS TRUE");
  }

  @Test
  public void BoolOr() throws Exception {
    evalTrue("true OR true");
    evalTrue("true OR false");
    evalTrue("false OR true");
    evalFalse("false OR false");
    evalTrue("true OR null");
    evalTrue("null OR true");
    evalFalse("false OR null");
    evalFalse("null OR false");
    evalNull("null OR null");
    evalFails("false OR");

    writeEquals("FIELD1 OR FIELD2");
  }

  @Test
  public void BoolAnd() throws Exception {
    evalTrue("true AND true");
    evalFalse("true AND false");
    evalFalse("false AND true");
    evalFalse("false AND false");
    evalNull("true AND null");
    evalNull("null AND true");
    evalNull("false AND null");
    evalNull("null AND false");
    evalNull("null AND null");
    writeEquals("FIELD1 AND FIELD2");
  }

  @Test
  public void ILike() throws Exception {
    evalTrue("'test' ILIKE '%t%'");
    evalTrue("'test' ILIKE '%T%'");

    // Escape with other char
    evalTrue("'Result 100% value' ilike 'RESULT%100^%%' escape '^'");

    evalNull("'test' ILIKE NULL");
    evalNull("'test' ILIKE 'TEST' escape NULL");
    evalNull("NULL ILIKE '%T%'");
  }

  @Test
  public void Like() throws Exception {
    evalTrue("NAME like 'TES%'");
    evalTrue("NAME not like 'X%'");
    evalFalse("NAME like 'X%'");
    evalTrue("'Tuesday' like '%es%'");
    evalTrue("'...Tuesday....' like '%es%'");

    // Test one char
    evalTrue("'A' like '_'");
    // evalFalse("'AA' like '_'");

    // Test empty
    evalTrue("'' like '%'");
    evalFalse("'' like '_'");

    // values that starts with "a" and ends with "o"
    evalTrue("'amigo' like 'a%o'");
    evalTrue("'ao' like 'a%o'");
    // values that starts with "a" and are at least 3 characters in length
    evalTrue("'ami' like 'a_%_%'");
    evalTrue("'amigo' like 'a_%_%'");
    evalTrue("'Friday' like '___day'");
    evalFalse("'am' like 'a_%_%'");
    evalTrue("'AA' like '__'");
    evalFalse("'AA' like '___'");


    // New line
    evalTrue("'AA\nA' like 'AA%'");
    evalTrue("'AA\nA' like 'AA_A'");
    evalTrue("'AA\nA' like 'AA%A'");
    evalFalse("'AA\nA' like 'AA_'");

    // Escape with other char
    evalTrue("'Result 100% value' like '%100^%%' escape '^'");

    // Double escape char
    evalFalse("'^100% milles' like '^^100^%%' escape '^'");

    // Escape with Regexp special char
    evalTrue("'give me 30% discount' like '%30!%%' escape '!'");
    evalTrue("'ADD_MONTHS' like '%ADD!_%' escape '!'");

    // TODO: evalTrue("'Amigo' like '[A-C]%'");

    // Optimizable
    evalTrue("'ABCDEFG' like 'ABCDEFG'");
    evalTrue("'ABCDEFG' like 'ABCDE%'");
    evalTrue("'ABCDEFG' like '%DEFG'");
    evalTrue("'ABCDEFG' like '%CDE%'");


    evalNull("NULL like 'NULL'");
    evalNull("'test' LIKE NULL");
    evalNull("'test' LIKE 'TEST' escape NULL");

    // NULL does not match NULL
    evalNull("NULL like NULL");

    
    evalFails("'give me 30% discount' like '%30!%%' escape '!!'");
    
    writeEquals("FIELD1 LIKE 'ADD%'");
    writeEquals("FIELD1 LIKE '%ADD!_%' ESCAPE '!'");
  }

  @Test
  public void Concat() throws Exception {
    evalEquals("CONCAT('TES','T')", "TEST");
    evalTrue("NAME='TES'||'T'");
    evalTrue("NAME='TES'||NULLIS||'T'");
    evalEquals("'TEST'||null", "TEST");
    evalEquals("null||'TEST'", "TEST");
    evalNull("null||null");

    writeEquals("FIELD1||'TEST'");
  }

  @Test
  public void CaseWhen() throws Exception {

    // implicit ELSE NULL case
    evalNull("case when Age=10 then 10 end");
    evalEquals("case when Age=40 then 10 end", 10L);

    // explicit ELSE case
    evalEquals("case when Age=40 then 10 else 50 end", 10L);
    evalEquals("case when Age>80 then 'A' else 'B' end", "B");
    evalNull("case when Age>80 then 'A' end");

    // Search CASE WHEN
    evalEquals("case when Age=10+20 then 1*5 when Age=20+20 then 2*5 else 50 end", 10L);

    
    
    // Simple CASE
    evalEquals("case Age when 10 then 10 when 40 then 40 else 50 end", 40L);
    evalEquals("case Age when 10 then 10 when 20 then 20 else -1 end", -1L);
    evalNull("case Age when 10 then 10 when 20 then 20 end");

    // Missing 'END'
    evalFails("case when Age=40 then 10 else 50");

    // Implicit ELSE NULL
    writeEquals("CASE WHEN AGE=40 THEN 10 END", "CASE WHEN AGE=40 THEN 10 ELSE NULL END");

    writeEquals("CASE WHEN AGE=40 THEN TRUE ELSE FALSE END");
    writeEquals("CASE AGE WHEN 40 THEN 'A' WHEN 20 THEN 'B' ELSE 'C' END");
  }

}


