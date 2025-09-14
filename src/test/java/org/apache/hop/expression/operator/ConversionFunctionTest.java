/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression.operator;

import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Locale;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionTest;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.IntervalType;
import org.apache.hop.expression.type.JsonType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.util.JsonConversion;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.parallel.Execution;

@TestInstance(PER_CLASS)
@Execution(CONCURRENT)
class ConversionFunctionTest extends ExpressionTest {

  @Test
  void To_Binary() throws Exception {
    evalEquals("TO_BINARY(HEX_ENCODE('Apache Hop'),'HEX')", "Apache Hop".getBytes());
    evalEquals("TO_BINARY('41706163686520486f70','HEX')", "Apache Hop".getBytes());

    evalEquals("TO_BINARY(BASE64_ENCODE('Apache Hop'),'BASE64')", "Apache Hop".getBytes());
    evalEquals("TO_BINARY('QXBhY2hlIEhvcA==','BASE64')", "Apache Hop".getBytes());

    evalEquals("TO_BINARY('Apache Hop','UtF-8')", "Apache Hop".getBytes(StandardCharsets.UTF_8));
    evalEquals("TO_BINARY('Apache Hop','UtF8')", "Apache Hop".getBytes(StandardCharsets.UTF_8));

    // Null handling
    evalNull("TO_BINARY(NULL_STRING)");

    // Check operands
    evalFails("TO_BINARY()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("TO_BINARY('Apache Hop',NULL_STRING)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void To_Boolean() throws Exception {
    evalTrue("To_Boolean('True')").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalTrue("To_Boolean('t')");
    evalTrue("To_Boolean('yes')");
    evalTrue("To_Boolean('on')");
    evalTrue("To_Boolean('1')");
    evalTrue("To_Boolean(5)");
    evalTrue("To_Boolean(-1)");
    evalTrue("To_Boolean(2.3)");
    evalTrue("To_Boolean(-2.3)");
    evalTrue("To_Boolean(0.5)");
    evalFalse("To_Boolean('False')");
    evalFalse("To_Boolean('off')");
    evalFalse("To_Boolean('NO')");
    evalFalse("To_Boolean('F')");
    evalFalse("To_Boolean('n')");
    evalFalse("To_Boolean('0')");
    evalFalse("To_Boolean(0)");
    evalFalse("To_Boolean(0.0000)");

    // Null handling
    evalNull("To_Boolean(NULL_STRING)").returnType(BooleanType.BOOLEAN);

    // Check operands
    evalFails("To_Boolean()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("To_Boolean(1,2,3)", ErrorCode.TOO_MANY_ARGUMENT);

    // TODO: Enforce field date conversion
    // evalFails("To_Boolean(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);

    evalFails("To_Boolean('falsee')", ErrorCode.CONVERSION_ERROR);
  }

  @Test
  void To_Number() throws Exception {

    // No precision/scale and no format
    evalEquals("TO_NUMBER('1000')", 1000D);
    evalEquals("TO_NUMBER('12.3456')", 12.3456D);
    evalEquals("TO_NUMBER('98.76546')", 98.76546D);
    evalEquals("TO_NUMBER('1.2E3')", 1200D);
    evalEquals("TO_NUMBER('1.2E-3')", 0.0012D);

    // Underscore
    evalEquals("TO_NUMBER('1_000')", 1000D);
    evalEquals("TO_NUMBER('1_000.000_1')", 1000.0001D);
    evalEquals("TO_NUMBER('1_2.3_4E1_0')", 12.34E10D);
    evalEquals("TO_NUMBER('1_234.2E-3')", 1.2342D);

    // Format with Decimals
    evalEquals("TO_NUMBER('5467.12', '999999.99')", 5467.12D);
    evalEquals("TO_NUMBER('1234.5','09999.99')", 1234.5D);
    Locale.setDefault(new Locale("en", "EN"));
    evalEquals("TO_NUMBER('5467.12', '999999D99')", 5467.12D);
    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals("TO_NUMBER('5467,12', '999999D99')", 5467.12D);

    // Format No Decimals
    evalEquals("TO_NUMBER('4687841', '9999999')", 4687841D);

    // Trailing space
    evalEquals("TO_NUMBER('   5467.12', '999999.99')", 5467.12D);

    // No sign
    evalEquals("TO_NUMBER('+0.1','99.99')", 0.1D);
    evalEquals("TO_NUMBER('-0.2','99.99')", -0.2D);
    evalEquals("TO_NUMBER(' -0.2','99.99')", -0.2D);
    evalEquals("TO_NUMBER(' .2','99.99')", 0.2D);

    // Sign S_ and _S
    evalEquals("TO_NUMBER('-0.2','S99.99')", -0.2D);
    evalEquals("TO_NUMBER('0.3-','99.99S')", -0.3D);
    evalEquals("TO_NUMBER('0.3-','99.99s')", -0.3D);

    // Sign MI_ and _MI
    evalEquals("TO_NUMBER('0.4-','99.99MI')", -0.4D);
    evalEquals("TO_NUMBER('0.4-','99.99mi')", -0.4D);
    evalEquals("TO_NUMBER('0.4 -','99.99mi')", -0.4D);
    evalEquals("TO_NUMBER(' 0.4 -','99.99mi')", -0.4D);
    evalEquals("TO_NUMBER('-   4','MI9999')", -4D);
    evalEquals("TO_NUMBER('-4','MI9999')", -4D);

    // Sign PR (format element can appear only in the last position of a number format model.)
    evalEquals("TO_NUMBER(' 0.5 ','99.99PR')", 0.5D);
    evalEquals("TO_NUMBER('<0.5>','99.99PR')", -0.5D);
    evalFails("TO_NUMBER('-5','PR9999')", ErrorCode.INVALID_NUMBER_FORMAT);

    // Format with a Thousand Group Markers
    Locale.setDefault(new Locale("en", "US"));
    evalEquals("TO_NUMBER('12,345,678', '999,999,999')", 12_345_678D);
    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals("TO_NUMBER('12 345 678', '999G999G999')", 12_345_678D);
    evalEquals("TO_NUMBER('12 345 678,123', '999G999G999D000')", 12_345_678.123D);
    Locale.setDefault(new Locale("de", "DE"));
    evalEquals("TO_NUMBER('12.345.678', '999G999G999')", 12_345_678D);

    // Format with Currency dollar
    Locale.setDefault(new Locale("en", "US"));
    evalEquals("TO_NUMBER('$65.169', '$99.999')", 65.169D);
    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals("TO_NUMBER('$65.169', '$99.999')", 65.169D);

    // Format with Currency symbol
    Locale.setDefault(new Locale("en", "US"));
    evalEquals("TO_NUMBER('$65.169', 'L99.999')", 65.169D);
    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals("TO_NUMBER('€65.169', 'L99.999')", 65.169D);
    evalEquals("TO_NUMBER('65.16€', '99.999L')", 65.16D);

    // Format with Currency code
    Locale.setDefault(new Locale("en", "US"));
    evalEquals("TO_NUMBER('USD65.169', 'C99.999')", 65.169D);
    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals("TO_NUMBER('EUR65.169', 'C99.999')", 65.169D);
    evalEquals("TO_NUMBER('65.16EUR', '99.999C')", 65.16D);

    // Format Hex
    evalEquals("TO_NUMBER('ABCD','FMXXXX')", 43981D);

    // Format Roman numeral
    evalEquals("TO_NUMBER('DXV','RN')", 515D);
    evalEquals("TO_NUMBER('CDLXXXV','RN')", 485D);

    evalEquals("TO_NUMBER('MCMXCIX','rn')", 1999D);
    evalEquals("TO_NUMBER('MMMDCCXXIV','rn')", 3724D);

    // Parse multi format
    evalEquals("TO_NUMBER('1234-','MI9999|9999MI')", -1234D);

    // Null handling
    evalNull("TO_NUMBER(NULL_STRING)");

    // Date to number epoch
    evalEquals("TO_NUMBER(Date '1970-01-01')", BigDecimal.ZERO);
    evalEquals("TO_NUMBER(Date '2019-02-25')", 1551052800D);
    evalEquals("TO_NUMBER(Date '1800-01-01')", -5364662400D);
    evalEquals("TO_NUMBER(Timestamp '2010-09-13 04:32:03.123')", new BigDecimal("1284352323.123"));
    evalEquals(
        "TO_NUMBER(Timestamp '2010-09-13 04:32:03.123000000')", new BigDecimal("1284352323.123"));
    evalEquals(
        "TO_NUMBER(Timestamp '2010-09-13 04:32:03.123456789')",
        new BigDecimal("1284352323.123456789"));
    evalNull("TO_NUMBER(NULL_DATE)");

    // You can specify only one decimal separator in a number format model.
    evalFails("TO_NUMBER('123.456','9D999D9')", ErrorCode.INVALID_NUMBER_FORMAT);
    evalFails("TO_NUMBER('123.456','9.999.9')", ErrorCode.INVALID_NUMBER_FORMAT);

    // A group separator cannot appear to the right of a decimal character or period in a number
    // format model.
    evalFails("TO_NUMBER('-0.2','999.999G99')", ErrorCode.INVALID_NUMBER_FORMAT);
    evalFails("TO_NUMBER('-0.2','999.999,99')", ErrorCode.INVALID_NUMBER_FORMAT);
  }

  @Test
  void To_Char() throws Exception {
    // Null handling
    evalNull("TO_CHAR(NULL_NUMBER)").returnType(StringType.STRING);
    evalNull("TO_CHAR(NULL_DATE)").returnType(StringType.STRING);
    evalNull("TO_CHAR(NULL_BINARY)").returnType(StringType.STRING);
    evalNull("TO_CHAR(NULL_BOOLEAN)").returnType(StringType.STRING);

    // Default format
    evalEquals("TO_CHAR(0.45)", "0.45").returnType(StringType.STRING_NOT_NULL);
    evalEquals("TO_CHAR(12923)", "12923").returnType(StringType.STRING_NOT_NULL);

    // Format fixed length with decimal
    Locale.setDefault(Locale.ENGLISH);
    evalEquals("TO_CHAR(0.1,'90.99')", "  0.1 ").returnType(StringType.STRING_NOT_NULL);
    evalEquals("TO_CHAR(-0.2,'90.90')", " -0.20");
    evalEquals("TO_CHAR(0,'90.99')", "  0.  ");
    evalEquals("TO_CHAR(0,'90D99')", "  0.  ");
    evalEquals("TO_CHAR(0,'90d99')", "  0.  ");
    evalEquals("TO_CHAR(0,'99.99')", "  0.  ");
    evalEquals("TO_CHAR(0,'9999')", "    0");
    evalEquals("TO_CHAR(0,'9999999')", "       0");
    evalEquals("TO_CHAR(0,'0999')", " 0000");
    evalEquals("TO_CHAR(-0.5, '90.99')", " -0.5 ");
    evalEquals("TO_CHAR(0.003,'0.999')", " 0.003");
    evalEquals("TO_CHAR(12,'99')", " 12");
    evalEquals("TO_CHAR(-7,'99')", " -7");
    evalEquals("TO_CHAR(12923,'99,999.00')", " 12,923.00");
    evalEquals("TO_CHAR(12,'9990999.9')", "    0012. ");
    evalEquals("TO_CHAR(0.3,'99.00000')", "   .30000");
    evalEquals("TO_CHAR(0.3,'00.00')", " 00.30");
    evalEquals("TO_CHAR(12923,'FM99999.99')", "12923.");
    evalEquals("TO_CHAR(12923,'FM9,9,9,9,9')", "1,2,9,2,3");
    evalEquals("TO_CHAR(0.3,'FM00.99')", "00.3");

    // Blanks for the integer part of a fixed-point number when the integer part is zero
    evalEquals("TO_CHAR(-0.2,'99.90')", "  -.20");
    evalEquals("TO_CHAR(-0.2,'99.99')", "  -.2 ");

    // TODO: Quoted text is reproduced in the result
    // evalEquals("TO_CHAR(485.8, '\"Pre:\"999\" Post:\" .999')", "Pre: 485 Post: .800");

    evalEquals("TO_CHAR(12345.567,'9,999')", "######");
    evalEquals("TO_CHAR(1234.94,'9999MI')", "1234 ");
    evalEquals("TO_CHAR(555.0, 'FM999.009')", "555.00");

    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals("TO_CHAR(0,'90.99')", "  0.  ");
    evalEquals("TO_CHAR(0,'90D99')", "  0,  ");
    evalEquals("TO_CHAR(0,'90d00')", "  0,00");

    // Format fixed length with grouping
    Locale.setDefault(new Locale("en", "EN"));
    evalEquals("TO_CHAR(1485,'9,999')", " 1,485");
    Locale.setDefault(new Locale("fr", "FR"));
    evalEquals("TO_CHAR(3148.5, '9G999D999')", " 3 148,5  ");
    evalEquals("TO_CHAR(3148.5, '9g999d990')", " 3 148,500");
    Locale.setDefault(new Locale("de", "DE"));
    evalEquals("TO_CHAR(3148.5, '9G999D999')", " 3.148,5  ");

    // Sign
    evalEquals("TO_CHAR(12,'S99')", "+12");
    evalEquals("TO_CHAR(12,'99S')", "12+");
    evalEquals("TO_CHAR(-7,'S99')", " -7");
    evalEquals("TO_CHAR(-7,'99S')", " 7-");
    evalEquals("TO_CHAR(-7,'99s')", " 7-");

    evalEquals("TO_CHAR(12,'99MI')", "12 ");
    evalEquals("TO_CHAR(7,'99MI')", " 7 ");
    evalEquals("TO_CHAR(-7,'99MI')", " 7-");
    evalEquals("TO_CHAR(-7,'MI99')", "- 7");
    // FM affect the trailing blank added by the MI suffix.
    evalEquals("TO_CHAR(485,'FMMI999')", "485");
    evalEquals("TO_CHAR(485,'FM999MI')", "485");

    evalEquals("TO_CHAR(7,'9999pr')", "    7 ");
    evalEquals("TO_CHAR(-7,'9999PR')", "   <7>");
    evalEquals("TO_CHAR(7,'FM9999PR')", "7");

    evalFails("TO_CHAR(-7,'PR9999')", ErrorCode.INVALID_NUMBER_FORMAT);

    // Currency dollar
    evalEquals("TO_CHAR(12,'$99')", " $12");
    evalEquals("TO_CHAR(-7,'$99')", " -$7");
    evalEquals("TO_CHAR(-7,'99$')", " -$7");
    evalEquals("TO_CHAR(124,'$99')", "####");
    evalEquals("TO_CHAR(124,'FM$99')", "###");

    // Currency code ISO 4217
    Locale.setDefault(new Locale("en", "GB"));
    evalEquals("TO_CHAR(12,'C99')", " GBP12");
    Locale.setDefault(new Locale("en", "US"));
    evalEquals("TO_CHAR(-7,'C99')", " -USD7");
    Locale.setDefault(new Locale("fr", "FR"));
    evalEquals("TO_CHAR(-77,'99C')", "-77EUR");
    Locale.setDefault(new Locale("et", "EE"));
    evalEquals("TO_CHAR(-7,'99C')", " -7EUR");
    // Only currency ISO code
    evalEquals("TO_CHAR(0,'FMC')", "EUR");

    // Currency symbol
    Locale.setDefault(new Locale("en", "GB"));
    evalEquals("TO_CHAR(12,'FML99')", "£12");
    Locale.setDefault(new Locale("fr", "FR"));
    evalEquals("TO_CHAR(-7,'L99')", " -€7");
    evalEquals("TO_CHAR(-7,'99L')", " -7€");
    evalEquals("TO_CHAR(123.45,'L999.99')", " €123.45");
    evalEquals("TO_CHAR(123.45,'FML999.99')", "€123.45");
    evalEquals("TO_CHAR(0,'FML')", "€"); // Only symbol

    // Text minimum
    evalEquals("TO_CHAR(123.456,'TM')", "123.456");
    evalEquals("TO_CHAR(123.456,'tm')", "123.456");
    evalEquals("TO_CHAR(123.456,'tm9')", "123.456");
    evalEquals("TO_CHAR(123.456,'TME')", "1.23456E+02");
    evalEquals("TO_CHAR(123.456,'tMe')", "1.23456e+02");
    evalEquals("TO_CHAR(123.456,'tMe')", "1.23456e+02");

    // Scientific
    evalEquals("TO_CHAR(123.456,'9.9EEEE')", "  1.2E+02");

    // Roman
    evalEquals("TO_CHAR(11,'FMRN')", "XI");
    evalEquals("TO_CHAR(11,'rn')", "             xi");
    evalEquals("TO_CHAR(5.2, 'FMRN')", "V");
    evalEquals("TO_CHAR(485, 'FMRN')", "CDLXXXV");
    evalEquals("TO_CHAR(515, 'RN')", "            DXV");
    evalFails("TO_CHAR(0, 'RN')", ErrorCode.CALL_FUNCTION_ERROR); // Must be > 0
    evalFails("TO_CHAR(4000, 'RN')", ErrorCode.CALL_FUNCTION_ERROR); // Must be < 4000

    // Hex
    evalEquals("TO_CHAR(123,'XX')", " 7B");
    evalEquals("TO_CHAR(123,'xx')", " 7b");
    evalEquals("TO_CHAR(123,'0XXX')", " 007B");
    evalEquals("TO_CHAR(123,'FM0XXX')", "007B");
    evalEquals("TO_CHAR(9234,'xx')", "###");

    // No space
    evalFails("TO_CHAR(485,'9 9 9')", ErrorCode.INVALID_NUMBER_FORMAT);

    // Date
    evalEquals("To_Char(DATE '2019-07-23','AD')", "AD").returnType(StringType.STRING_NOT_NULL);
    evalEquals("To_Char(DATE '2019-07-23','BC')", "AD");
    evalEquals("To_Char(DATE '2019-07-23','Bc')", "Ad");
    evalEquals("To_Char(DATE '2019-07-23','bc')", "ad");
    evalEquals("To_Char(DATE '2019-07-23','A.D.')", "A.D.");
    evalEquals("To_Char(DATE '2019-07-23','B.C.')", "A.D.");
    evalEquals("To_Char(DATE '2019-07-23','B.c.')", "A.d.");
    evalEquals("To_Char(DATE '2019-07-23','b.c.')", "a.d.");
    evalEquals("To_Char(MAKE_DATE(-10,07,23),'b.c.')", "b.c.");

    // Punctuation is reproduced in the result
    evalEquals("To_Char(DATE '2019-07-23','dd/mm/yyyy')", "23/07/2019");
    evalEquals("To_Char(DATE '2019-07-23','dd.mm.yyyy')", "23.07.2019");
    evalEquals("To_Char(DATE '2019-07-23',':dd,mm;yyyy')", ":23,07;2019");

    // Quoted text is reproduced in the result
    evalEquals("To_Char(DATE '2019-07-23','dd\"text\"mmyyyy')", "23text072019");

    // Century
    evalEquals("To_Char(DATE '2019-07-23','CC')", "21");
    evalEquals("To_Char(DATE '2000-07-23','CC')", "20");
    evalEquals("To_Char(DATE '2019-07-23','SCC')", " 21");
    evalEquals("To_Char(DATE '2000-07-23','SCC')", " 20");
    evalEquals("To_Char(DATE '2000-07-23','FMSCC')", "20");
    evalEquals("To_Char(To_Date('-0200','SYYYY'),'SCC')", "-02");
    evalEquals("To_Char(To_Date('-0200','SYYYY'),'FMSCC')", "-2");

    evalEquals("To_Char(DATE '2122-01-01','YEAR')", "TWENTY-ONE TWENTY-TWO");
    evalEquals("To_Char(DATE '2021-01-01','YEAR')", "TWENTY TWENTY-ONE");
    evalEquals("To_Char(DATE '2020-01-01','YEAR')", "TWENTY TWENTY");
    evalEquals("To_Char(DATE '1999-01-01','YEAR')", "NINETEEN NINETY-NINE");
    evalEquals("To_Char(DATE '1900-01-01','YEAR')", "NINETEEN HUNDRED");
    evalEquals("To_Char(DATE '1830-01-01','YEAR')", "EIGHTEEN THIRTY");
    evalEquals("To_Char(DATE '2020-01-01','year')", "twenty twenty");
    evalEquals("To_Char(DATE '2020-01-01','syear')", " twenty twenty");
    evalEquals("To_Char(DATE '2020-01-01','FMsyear')", "twenty twenty");
    evalEquals("To_Char(To_Date('-1999','SYYYY'),'SYEAR')", "-NINETEEN NINETY-NINE");
    evalEquals("To_Char(To_Date('-0228','SYYYY'),'SYEAR')", "-TWO TWENTY-EIGHT");

    // Year
    evalEquals("To_Char(DATE '2019-01-01','YYYY')", "2019");
    evalEquals("To_Char(DATE '0800-01-01','YYYY')", "0800");
    evalEquals("To_Char(DATE '0800-01-01','FMYYYY')", "800"); // Year compact
    evalEquals("To_Char(DATE '2019-01-01','SYYYY')", " 2019");
    evalEquals("To_Char(To_Date('-2000','SYYYY'),'YYYY BC')", "2000 BC");
    evalEquals("To_Char(To_Date('-800','SYYYY'),'SYYYY')", "-0800"); // Negative signed year
    evalEquals("To_Char(To_Date('-800','SYYYY'),'YYYY BC')", "0800 BC");
    evalEquals("To_Char(DATE '0800-07-23','FMSYYYY')", "800"); // Signed year compact
    evalEquals("To_Char(To_Date('-800','SYYYY'),'FMSYYYY BC')", "-800 BC");
    evalEquals("To_Char(DATE '2019-01-01','YYY')", "019");
    evalEquals("To_Char(DATE '2019-01-01','YY')", "19");
    evalEquals("To_Char(DATE '2019-01-01','Y')", "9");

    // ISO Year
    evalEquals("To_Char(DATE '2019-12-28','IYYY')", "2019");
    evalEquals("To_Char(DATE '2019-12-28','IYY')", "019");
    evalEquals("To_Char(DATE '2019-12-28','IY')", "19");
    evalEquals("To_Char(DATE '2019-12-28','I')", "9");
    evalEquals("To_Char(DATE '2019-12-31','IYYY')", "2020");
    evalEquals("To_Char(DATE '2019-12-31','IYY')", "020");
    evalEquals("To_Char(DATE '2019-12-31','IY')", "20");
    evalEquals("To_Char(DATE '2019-12-31','I')", "0");

    // Quarter
    evalEquals("To_Char(DATE '2019-07-23','Q')", "3");

    // Month number
    evalEquals("To_Char(DATE '2019-07-23','MM')", "07");
    evalEquals("To_Char(DATE '2019-07-23','FMMM')", "7");

    // Full month name
    evalEquals("To_Char(DATE '2019-07-23','Month')", "July     ");
    evalEquals("To_Char(DATE '2019-07-23','MONTH')", "JULY     ");
    evalEquals("To_Char(DATE '2019-07-23','FMMONTH')", "JULY");
    evalEquals("To_Char(DATE '2019-09-23','month')", "september");

    // Short month name
    evalEquals("To_Char(DATE '2019-09-23','MON')", "SEP");
    evalEquals("To_Char(DATE '2019-09-23','Mon')", "Sep");
    evalEquals("To_Char(DATE '2019-09-23','mon')", "sep");

    // Roman numeral month
    evalEquals("To_Char(DATE '2019-09-23','RM')", "IX");
    evalEquals("To_Char(DATE '2019-06-23','rm')", "vi");

    // Aligned week of month
    evalEquals("To_Char(DATE '2015-12-31','\"W=\"W')", "W=5");
    evalEquals("To_Char(DATE '2015-02-05','\"W=\"W')", "W=1");

    // Aligned week of year and ISO Week of year (The first week of the ISO year is the week that
    // contains January 4.)
    evalEquals(
        "To_Char(DATE '2015-12-31','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2015-12-31 thu IYYY=2015 IW=53 WW=53");
    evalEquals(
        "To_Char(DATE '2016-01-01','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-01 fri IYYY=2015 IW=53 WW=01");
    evalEquals(
        "To_Char(DATE '2016-01-02','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-02 sat IYYY=2015 IW=53 WW=01");
    evalEquals(
        "To_Char(DATE '2016-01-03','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-03 sun IYYY=2015 IW=53 WW=01");
    evalEquals(
        "To_Char(DATE '2016-01-04','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-04 mon IYYY=2016 IW=01 WW=01");
    evalEquals(
        "To_Char(DATE '2016-01-05','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-05 tue IYYY=2016 IW=01 WW=01");
    evalEquals(
        "To_Char(DATE '2016-01-06','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-06 wed IYYY=2016 IW=01 WW=01");
    evalEquals(
        "To_Char(DATE '2016-01-07','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-07 thu IYYY=2016 IW=01 WW=01");
    evalEquals(
        "To_Char(DATE '2016-01-08','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2016-01-08 fri IYYY=2016 IW=01 WW=02");
    evalEquals(
        "To_Char(DATE '2042-12-31','YYYY-MM-DD dy \"IYYY=\"IYYY \"IW=\"IW \"WW=\"WW')",
        "2042-12-31 wed IYYY=2043 IW=01 WW=53");
    evalEquals("To_Char(DATE '2016-01-04','FM\"IW=\"IW \"WW=\"WW')", "IW=1 WW=1"); // Compact

    // Day of week
    evalEquals("To_Char(DATE '2019-07-21','D')", "1");
    evalEquals("To_Char(DATE '2019-07-23','D')", "3");

    // Day of month
    evalEquals("To_Char(DATE '2019-07-08','DD')", "08");
    evalEquals("To_Char(DATE '2019-07-08','FMDD')", "8");
    evalEquals("To_Char(DATE '2019-07-23','DD')", "23");

    // Day of year
    evalEquals("To_Char(DATE '2019-02-23','DDD')", "054");
    evalEquals("To_Char(DATE '2019-02-23','FMDDD')", "54");

    // Julian day
    evalEquals("To_Char(DATE '2019-07-23','J')", "2458688");
    evalEquals("To_Char(DATE '0001-01-01','J')", "1721426");

    // Long day name
    evalEquals("To_Char(DATE '2019-07-23','DAY')", "TUESDAY  ");
    evalEquals("To_Char(DATE '2019-07-23','Day')", "Tuesday  ");
    evalEquals("To_Char(DATE '2019-07-23','day')", "tuesday  ");
    evalEquals("To_Char(DATE '2019-07-23','fmDay')", "Tuesday");

    // Short day name
    evalEquals("To_Char(DATE '2019-07-23','DY')", "TUE");
    evalEquals("To_Char(DATE '2019-07-23','Dy')", "Tue");
    evalEquals("To_Char(DATE '2019-07-23','dy')", "tue");

    // Time Zone Region
    evalEquals("To_Char(DATE '2019-07-23','TZR')", "Z");
    evalEquals(
        "To_Char(TIMESTAMP '2021-01-01 15:28:59.123456789' AT TIME ZONE 'Europe/Paris','TZR')",
        "Europe/Paris");

    // Time zone region abbreviated with Daylight Saving Time
    evalEquals("To_Char(TO_DATE('2019-07-23 Europe/Paris','YYYY-MM-DD TZR'),'TZD')", "CEST");
    evalEquals(
        "To_Char(TIMESTAMP '2021-01-01 15:28:59.123456789' AT TIME ZONE 'Europe/Paris','TZD')",
        "CET");
    evalEquals(
        "To_Char(TIMESTAMP '2021-01-01 15:28:59.123456789' AT TIME ZONE 'America/New_York','TZD')",
        "EST");

    // Time Zone Hour:Minute
    evalEquals("To_Char(DATE '2019-07-23','TZH:TZM')", "+00:00");
    evalEquals(
        "To_Char(To_Date('2019-02-13 15:34:56 -06:00','YYYY-MM-DD HH24:MI:SS TZH:TZM'),'TZH:TZM')",
        "-06:00");
    evalEquals(
        "To_Char(To_Date('2019-02-13 15:34:56 +8:00','YYYY-MM-DD HH24:MI:SS TZH:TZM'),'TZH:TZM')",
        "+08:00");

    // Time
    evalEquals("To_Char(TIMESTAMP '2019-02-13 15:34:56','HH:MI:SS')", "03:34:56");

    // Time 12 hours
    evalEquals("To_Char(TIMESTAMP '2019-02-13 03:34:56','HH12:MI:SS AM')", "03:34:56 AM");
    evalEquals("To_Char(TIMESTAMP '2019-02-13 15:34:56','HH12:MI:SS AM')", "03:34:56 PM");

    // Time 24 hours
    evalEquals("To_Char(TIMESTAMP '2019-02-13 15:34:56.123','HH24:MI:SS')", "15:34:56");

    // Time fraction
    evalEquals("To_Char(TIMESTAMP '2019-02-13 15:34:56.123','HH24:MI:SS.FF3')", "15:34:56.123");
    evalEquals(
        "To_Char(TIMESTAMP '2019-02-13 15:34:56.123456','HH24:MI:SS.FF6')", "15:34:56.123456");
    evalEquals(
        "To_Char(TIMESTAMP '2019-02-13 15:34:56.123456789','HH24:MI:SS.FF9')",
        "15:34:56.123456789");

    // Seconds of day
    evalEquals("To_Char(TIMESTAMP '2019-02-13 03:34:56','SSSSS')", "12896");

    // AM PM
    evalEquals("To_Char(TIMESTAMP '2019-02-13 03:34:56','HH12:MI:SS Am')", "03:34:56 Am");
    evalEquals("To_Char(TIMESTAMP '2019-02-13 15:34:56','HH12:MI:SS Pm')", "03:34:56 Pm");
    evalEquals("To_Char(TIMESTAMP '2019-02-13 03:34:56','HH12:MI:SS A.M.')", "03:34:56 A.M.");
    evalEquals("To_Char(TIMESTAMP '2019-02-13 15:34:56','HH12:MI:SS p.m.')", "03:34:56 p.m.");

    // Short date and time
    Locale.setDefault(new Locale("en", "US"));
    evalEquals("To_Char(TIMESTAMP '2019-07-23 14:52:00','DS TS')", "Jul 23, 2019 2:52:00 PM");
    Locale.setDefault(new Locale("fr", "FR"));
    evalEquals("To_Char(TIMESTAMP '2019-07-23 14:52:00','DS TS')", "Jul 23, 2019 2:52:00 PM");

    // Long date
    Locale.setDefault(new Locale("fr", "BE"));
    evalEquals(
        "To_Char(TIMESTAMP '2019-07-23 14:52:00','DL')",
        "mardi 23 juillet 2019 à 14 h 52 min 00 s Z");

    // Local radix character
    Locale.setDefault(new Locale("en", "GB"));
    // evalEquals("To_Char(TIMESTAMP '2019-07-23 14:52:00','HH:MI:SSXFF')", "02:52:00,000000");
    Locale.setDefault(new Locale("fr", "BE"));
    // evalEquals("To_Char(TIMESTAMP '2019-07-23 14:52:00','HH:MI:SSXFF')", "02:52:00,000000");

    // Special char
    evalEquals("To_Char(DATE '2019-07-23',':;.,=-/(FMMONTH)')", ":;.,=-/(JULY)");

    // Check operands
    evalFails("To_Char(DATE '2019-07-23','*')", ErrorCode.INVALID_DATE_FORMAT);
    evalFails("To_Char(DATE '2019-07-23','Â£')", ErrorCode.INVALID_DATE_FORMAT);
    evalFails("To_Char(DATE '2019-07-23','{}[]')", ErrorCode.INVALID_DATE_FORMAT);

    // Full case
    evalEquals(
        "To_Char(TIMESTAMP '2020-12-03 01:02:03.123456','yyyy-mm-dd hh:mi:ss.FF')",
        "2020-12-03 01:02:03.123456");

    // Boolean
    evalEquals("TO_CHAR(FIELD_BOOLEAN_TRUE)", "TRUE");
    optimize("TO_CHAR(TRUE)", "'TRUE'");

    // Binary
    evalEquals("TO_CHAR(BINARY '41706163686520486f70','HEX')", "41706163686520486f70");
    evalEquals("TO_CHAR(BINARY '41706163686520486f70','BASE64')", "QXBhY2hlIEhvcA==");
    evalEquals("TO_CHAR(BINARY '41706163686520486f70','UTF8')", "Apache Hop");
    evalEquals("TO_CHAR(BINARY '41706163686520486f70','UTF-8')", "Apache Hop");

    // String
    evalEquals("TO_CHAR('Apache Hop')", "Apache Hop");
  }

  @Test
  void To_Date() throws Exception {
    evalEquals("To_Date('2019-02-13','YYYY-MM-DD')", LocalDate.of(2019, 2, 13));
    evalEquals("To_Date('2020:148','YYYY:DDD')", LocalDate.of(2020, 5, 27));
    evalEquals("To_Date('2020-08','YYYY-MM')", LocalDate.of(2020, 8, 1));
    evalEquals("To_Date('2020-MarCH','YYYY-MONTH')", LocalDate.of(2020, Month.MARCH, 1));
    evalEquals("To_Date('2020,feb,25','YYYY,MON,DD')", LocalDate.of(2020, Month.FEBRUARY, 25));
    evalEquals(
        "To_Date('2019-02-13 15:34:56','YYYY-MM-DD HH:MI:SS')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 3, 34, 56));
    evalEquals(
        "To_Date('2019-02-13 15:34:56','YYYY-MM-DD HH12:MI:SS')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 3, 34, 56));
    evalEquals(
        "To_Date('2019-02-13 15:34:56','YYYY-MM-DD HH24:MI:SS')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 15, 34, 56));

    // Separator T
    evalEquals(
        "To_Date('2019-02-13T15:34:56','YYYY-MM-DD HH24:MI:SS')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 15, 34, 56));
    evalEquals(
        "To_Date('2019-02-13T15:34:56','YYYY-MM-DD\"T\"HH24:MI:SS')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 15, 34, 56));

    evalEquals("To_Date('01/02/2020','DD/MM/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));
    evalEquals("To_Date(' 01/02/2020','DD/MM/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));
    evalEquals("To_Date('01/02/2020 ','DD/MM/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));
    evalEquals("To_Date('01/ 02/2020 ','DD/MM/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));
    evalEquals("To_Date('01/ 2/2020 ','DD/MM/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));

    evalEquals("To_Date('01/II/2020','DD/RM/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));
    evalEquals("To_Date('01/VII/2020','DD/RM/YYYY')", LocalDate.of(2020, Month.JULY, 1));

    evalEquals("To_Date('01/02/-100','DD/MM/SYYYY')", LocalDate.of(-100, 2, 1));

    evalEquals("To_Date('01/02/10','DD/MM/YY')", LocalDate.of(2010, 2, 1));
    evalEquals("To_Date('01/02/50','DD/MM/YY')", LocalDate.of(2050, 2, 1));
    evalEquals("To_Date('01/02/80','DD/MM/YY')", LocalDate.of(1980, 2, 1));

    IVariables variables = new Variables();
    variables.setVariable(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START, "1970");
    IExpressionContext context = createExpressionContext(variables);
    evalEquals(context, "To_Date('01/02/69','DD/MM/YY')", LocalDate.of(2069, 2, 1));
    evalEquals(context, "To_Date('01/02/70','DD/MM/YY')", LocalDate.of(1970, 2, 1));

    variables.setVariable(ExpressionContext.EXPRESSION_TWO_DIGIT_YEAR_START, "2000");
    context = createExpressionContext(variables);
    evalEquals(context, "To_Date('01/02/80','DD/MM/YY')", LocalDate.of(2080, 2, 1));

    // TO VERIFY
    evalEquals("To_Date('01-jan-4710bc','dd-mon-yyyybc')", LocalDate.of(-4709, 1, 1));

    // Time zone offset
    evalEquals(
        "To_Date('2019-02-13 15:34:56 +08:00','YYYY-MM-DD HH24:MI:SS TZH:TZM')",
        ZonedDateTime.of(2019, 2, 13, 15, 34, 56, 0, ZoneOffset.ofHours(8)));
    evalEquals(
        "To_Date('2019-02-13 15:34:56 +8:00','YYYY-MM-DD HH24:MI:SS TZH:TZM')",
        ZonedDateTime.of(2019, 2, 13, 15, 34, 56, 0, ZoneOffset.ofHours(8)));
    evalEquals(
        "To_Date('2019-02-13 15:34:56 -04:00','YYYY-MM-DD HH24:MI:SS TZH:TZM')",
        ZonedDateTime.of(2019, 2, 13, 15, 34, 56, 0, ZoneOffset.ofHours(-4)));

    // Time zone region
    evalEquals(
        "To_Date('2019-02-13 15:34:56 US/Pacific','YYYY-MM-DD HH24:MI:SS TZR')",
        ZonedDateTime.of(2019, 2, 13, 15, 34, 56, 0, ZoneId.of("US/Pacific")));
    evalEquals(
        "To_Date('Europe/Paris 2019-02-13 15:34:56','TZR YYYY-MM-DD HH24:MI:SS')",
        ZonedDateTime.of(2019, 2, 13, 15, 34, 56, 0, ZoneId.of("Europe/Paris")));

    // Trailing space
    evalEquals("To_Date('  2020-08','YYYY-MM')", LocalDate.of(2020, 8, 1));
    evalEquals("To_Date(' 08- 2020','MM-SYYYY')", LocalDate.of(2020, 8, 1));

    evalEquals("To_Date('01/2/0001','DD/MM/RRRR')", LocalDate.of(2001, Month.FEBRUARY, 1));
    evalEquals("To_Date('01/2/52','DD/MM/RRRR')", LocalDate.of(1952, Month.FEBRUARY, 1));
    evalEquals("To_Date('01/2/0923','DD/MM/RRRR')", LocalDate.of(923, Month.FEBRUARY, 1));

    // Month and day shorter than format
    evalEquals("To_Date('2020/2/1','YYYY/MM/DD')", LocalDate.of(2020, Month.FEBRUARY, 1));

    // If single elements are omitted, then their minimum values are assumed
    evalEquals("To_Date('2020-12','YYYY-MM')", LocalDate.of(2020, Month.DECEMBER, 1));
    evalEquals("To_Date('2020-02','YYYY-DD')", LocalDate.of(2020, Month.JANUARY, 2));
    evalEquals("To_Date('12-02','MM-DD')", LocalDate.of(1970, Month.DECEMBER, 2));
    evalEquals(
        "To_Date('2019-02-13','YYYY-MM-DD HH24:MI:SS')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 0, 0, 0));

    // Fractional seconds FF3 (milliseconds), FF or FF6 (microseconds), FF9 (nanoseconds).
    evalEquals(
        "To_Date('2019-02-13 19:34:56.123456','YYYY-MM-DD HH24:MI:SS.FF')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 19, 34, 56, 123456000));
    evalEquals(
        "To_Date('2019-02-13 19:34:56.123','YYYY-MM-DD HH24:MI:SS.FF3')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 19, 34, 56, 123000000));
    evalEquals(
        "To_Date('2019-02-13 19:34:56.123456','YYYY-MM-DD HH24:MI:SS.FF6')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 19, 34, 56, 123456000));
    evalEquals(
        "To_Date('2019-02-13 19:34:56.123456789','YYYY-MM-DD HH24:MI:SS.FF9')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 19, 34, 56, 123456789));

    // Rule to try alternate format MM -> MON and MONTH
    evalEquals("To_Date('01/Feb/2020','DD/MM/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));
    // Rule to try alternate format MM -> MON and MONTH
    evalEquals("To_Date('01/February/2020','DD/MM/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));
    // Rule to try alternate format MON -> MONTH
    evalEquals("To_Date('01/February/2020','DD/MON/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));
    // Rule to try alternate format MONTH -> MON
    evalEquals("To_Date('01/Feb/2020','DD/MONTH/YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));

    // Julian date
    evalEquals("To_Date('2454803','J')", LocalDate.of(2008, 12, 2));
    evalEquals("To_Date('1721426','J')", LocalDate.of(1, 1, 1));
    evalEquals("To_Date('1001426','J')", LocalDate.of(-1971, 9, 16));

    // FX
    evalFails("To_Date('15/ Feb /2020','FXDD/MM/YYYY')", ErrorCode.UNPARSABLE_DATE_WITH_FORMAT);
    evalFails("To_Date('1-02-2020','FXDD/MM/YYYY')", ErrorCode.UNPARSABLE_DATE_WITH_FORMAT);
    evalFails("To_Date('1/02/2020','FXDD/MM/YYYY')", ErrorCode.UNPARSABLE_DATE_WITH_FORMAT);
    // evalEquals("To_Date('1/02/2020','FXFMDD-MON-YYYY')", LocalDate.of(2020, Month.FEBRUARY, 1));

    // Is interpreted as 10 February 2003
    // evalEquals("To_Date('06-2003-MON','WW-YYYY-DY')", LocalDate.of(2003, 2, 10));

    // Is interpreted as 31 December 2003, 12:59:33
    evalEquals(
        "To_Date('12:59:33 365-2003', 'HH24:MI:SS DDD-YYYY')",
        LocalDateTime.of(2003, 12, 31, 12, 59, 33));

    // Is interpreted as 24 December 2009, 23:00:00
    evalEquals(
        "To_Date('2009-12-24 11:00:00 PM','YYYY-MM-DD HH:MI:SS AM')",
        LocalDateTime.of(2009, 12, 24, 23, 0, 0));
    evalEquals(
        "To_Date('2009-12-24 11:00:00 PM','YYYY-MM-DD HH12:MI:SS AM')",
        LocalDateTime.of(2009, 12, 24, 23, 0, 0));

    // Is interpreted as 12 May 2003, 00:00:10.123
    evalEquals(
        "To_Date('2000_MAY_12 10.123','YYYY_MONTH_DD SS.FF3')",
        LocalDateTime.of(2000, 5, 12, 0, 0, 10, 123000000));

    evalEquals("To_Date('15:30:40','hh24:mi:ss')", LocalDateTime.of(1970, 1, 1, 15, 30, 40));

    // Integer Unix Epoch in seconds
    evalEquals("TO_DATE(0)", LocalDateTime.of(1970, 1, 1, 0, 0, 0));
    evalEquals("TO_DATE(1551052800)", LocalDate.of(2019, 2, 25));
    evalEquals("TO_DATE(-5364662400)", LocalDate.of(1800, 1, 1));
    evalEquals("TO_DATE(1284352323)", LocalDateTime.of(2010, 9, 13, 4, 32, 3));

    // Number Unix Epoch in seconds with fractional
    evalEquals("TO_DATE(1551052800.000000000)", LocalDate.of(2019, 2, 25));
    evalEquals("TO_DATE(-5364662400.000000000)", LocalDate.of(1800, 1, 1));
    evalEquals("TO_DATE(1284352323.1)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 100000000));
    evalEquals("TO_DATE(1284352323.12)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 120000000));
    evalEquals("TO_DATE(1284352323.123)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 123000000));
    evalEquals("TO_DATE(1284352323.123456789)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 123456789));

    // Explicit AUTO format ISO 8601
    evalEquals(
        "TO_DATE('2024-12-01 3:05:06.123456789','AUTO')",
        LocalDateTime.of(2024, 12, 1, 3, 5, 6, 123456789));
    evalEquals(
        "TO_DATE('2024-12-01 12:05:06.123456789','AUTO')",
        LocalDateTime.of(2024, 12, 1, 12, 5, 6, 123456789));
    evalEquals(
        "TO_DATE('2024-12-01 3:05:06.123456789+02:00','AUTO')",
        LocalDateTime.of(2024, 12, 1, 3, 5, 6, 123456789));
    evalEquals(
        "TO_DATE('2024-12-01 3:05:06.123456789 +02:00','AUTO')",
        LocalDateTime.of(2024, 12, 1, 3, 5, 6, 123456789));

    evalEquals("TO_DATE('2024-12-01 23:05','AUTO')", LocalDateTime.of(2024, 12, 1, 23, 5, 0));

    // AUTO format date only
    evalEquals("TO_DATE(' 2024-02-25 ')", LocalDate.of(2024, 2, 25));
    evalEquals("TO_DATE('  2024-02-25  ')", LocalDate.of(2024, 2, 25));
    evalEquals("TO_DATE('2024-02-25')", LocalDate.of(2024, 2, 25));
    evalEquals("TO_DATE('2024-2-25')", LocalDate.of(2024, 2, 25));
    evalEquals("TO_DATE('2024-2-5')", LocalDate.of(2024, 2, 5));
    evalEquals("TO_DATE('2024-02-5')", LocalDate.of(2024, 2, 5));

    // AUTO format date with time part
    evalEquals("TO_DATE('2024-2-5 3')", LocalDateTime.of(2024, 2, 5, 3, 0, 0));
    evalEquals("TO_DATE('2024-2-5 13')", LocalDateTime.of(2024, 2, 5, 13, 0, 0));
    evalEquals("TO_DATE('2024-2-5 13:5')", LocalDateTime.of(2024, 2, 5, 13, 5, 0));
    evalEquals("TO_DATE('2024-2-5 13:05')", LocalDateTime.of(2024, 2, 5, 13, 5, 0));
    evalEquals("TO_DATE('2024-2-5T13:05')", LocalDateTime.of(2024, 2, 5, 13, 5, 0));
    evalEquals("TO_DATE('2024-2-5 13:05:59')", LocalDateTime.of(2024, 2, 5, 13, 5, 59));
    evalEquals("TO_DATE('2024-2-5 13:5:9')", LocalDateTime.of(2024, 2, 5, 13, 5, 9));

    // AUTO format date with time part and optional nanosecond
    evalEquals("TO_DATE('2024-2-5 13:5:9.123')", LocalDateTime.of(2024, 2, 5, 13, 5, 9, 123000000));
    evalEquals(
        "TO_DATE('2024-2-5 13:5:9.123456')", LocalDateTime.of(2024, 2, 5, 13, 5, 9, 123456000));
    evalEquals(
        "TO_DATE('2024-2-5 13:5:9.123456789')", LocalDateTime.of(2024, 2, 5, 13, 5, 9, 123456789));
    evalEquals(
        "TO_DATE('2024-2-5 13:5:9,123456789')", LocalDateTime.of(2024, 2, 5, 13, 5, 9, 123456789));

    // AUTO format date with time part and time zone offset
    evalEquals(
        "TO_DATE('2024-2-5 13:05+02:00')",
        OffsetDateTime.of(2024, 2, 5, 13, 5, 0, 0, ZoneOffset.ofHours(2)));
    evalEquals(
        "TO_DATE('2024-2-5 13:05+0200')",
        OffsetDateTime.of(2024, 2, 5, 13, 5, 0, 0, ZoneOffset.ofHours(2)));
    evalEquals(
        "TO_DATE('2024-2-5 13:05+02')",
        OffsetDateTime.of(2024, 2, 5, 13, 5, 0, 0, ZoneOffset.ofHours(2)));
    evalEquals(
        "TO_DATE('2024-2-5 13:05-02:00')",
        OffsetDateTime.of(2024, 2, 5, 13, 5, 0, 0, ZoneOffset.ofHours(-2)));
    evalEquals(
        "TO_DATE('2024-2-5 13:05-0200')",
        OffsetDateTime.of(2024, 2, 5, 13, 5, 0, 0, ZoneOffset.ofHours(-2)));
    evalEquals(
        "TO_DATE('2024-2-5 13:05-02')",
        OffsetDateTime.of(2024, 2, 5, 13, 5, 0, 0, ZoneOffset.ofHours(-2)));
    evalEquals(
        "TO_DATE('2024-2-5 13:05 -4:00')",
        OffsetDateTime.of(2024, 2, 5, 13, 5, 0, 0, ZoneOffset.ofHours(-4)));
    evalEquals(
        "TO_DATE('2024-2-5 13:05Z')", OffsetDateTime.of(2024, 2, 5, 13, 5, 0, 0, ZoneOffset.UTC));

    evalEquals(
        "TO_DATE('2024-2-5 13:5:9.123456789+02:00')",
        OffsetDateTime.of(2024, 2, 5, 13, 5, 9, 123456789, ZoneOffset.ofHours(2)));
    evalEquals(
        "TO_DATE('2024-2-5 13:5:9.123456789 +02:00')",
        OffsetDateTime.of(2024, 2, 5, 13, 5, 9, 123456789, ZoneOffset.ofHours(2)));

    // AUTO format short date and time
    evalEquals("TO_DATE('20240205')", LocalDate.of(2024, 2, 5));
    evalEquals("TO_DATE('20240205T13')", LocalDateTime.of(2024, 2, 5, 13, 0));
    evalEquals("TO_DATE('20240205T1305')", LocalDateTime.of(2024, 2, 5, 13, 5));
    evalEquals("TO_DATE('20240205T130510')", LocalDateTime.of(2024, 2, 5, 13, 5, 10));
    evalEquals(
        "TO_DATE('20240205T130510,123')", LocalDateTime.of(2024, 2, 5, 13, 5, 10, 123000000));
    evalEquals(
        "TO_DATE('20240205T130510.123')", LocalDateTime.of(2024, 2, 5, 13, 5, 10, 123000000));
    evalEquals(
        "TO_DATE('20240205T130510.123456')", LocalDateTime.of(2024, 2, 5, 13, 5, 10, 123456000));
    evalEquals(
        "TO_DATE('20240205T130510.123456789')", LocalDateTime.of(2024, 2, 5, 13, 5, 10, 123456789));
    evalEquals(
        "TO_DATE('20240205T130510.123456789-0200')",
        OffsetDateTime.of(2024, 2, 5, 13, 5, 10, 123456789, ZoneOffset.ofHours(-2)));

    evalNull("To_Date(NULL_STRING,'FXDD/MM/YYYY')");
  }

  @Test
  void To_Interval() throws Exception {
    evalEquals("TO_INTERVAL('0-0 45 22:30:58')", Interval.of(0, 0, 45, 22, 30, 58))
        .returnType(IntervalType.INTERVAL_NOT_NULL);
    evalEquals("TO_INTERVAL('+0-0 45 22:30:58')", Interval.of(0, 0, 45, 22, 30, 58));
    evalEquals(
        "TO_INTERVAL('45 days 22 hours 30 minutes 58 seconds')", Interval.of(0, 0, 45, 22, 30, 58));
    evalEquals("TO_INTERVAL('-0-0 45 22:30:58')", Interval.of(0, 0, 45, 22, 30, 58).negate());
    evalNull("TO_INTERVAL(NULL_STRING)");

    optimize("TO_INTERVAL('-0-0 45 22:30:58')", "INTERVAL '-45 22:30:58' DAY TO SECOND");
  }

  @Test
  void To_Json() throws Exception {

    evalEquals(
            "To_Json('{\"name\":\"Smith\", \"age\":29}')",
            JsonConversion.convert("{\"name\":\"Smith\",\"age\":29}"))
        .returnType(JsonType.JSON_NOT_NULL);
    evalEquals("To_Json('true')", JsonConversion.convert("true"));
    evalEquals("To_Json('null')", JsonConversion.convert("null"));

    evalNull("To_Json(NULL_STRING)").returnType(JsonType.JSON);

    // Check operands
    evalFails("To_Json()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    evalFails("To_Json(FIELD_BOOLEAN_TRUE)", ErrorCode.CONVERSION_ERROR);
    evalFails("To_Json('{\"name\":\"Smith\"; \"age\":29}')", ErrorCode.CONVERSION_ERROR);
  }

  @Test
  void Try_Cast() throws Exception {

    // String to Boolean
    evalTrue("TRY_CAST('Yes' as Boolean)").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalFalse("TRY_CAST('False' as Boolean)");
    evalNull("TRY_CAST('Fake' as Boolean)");

    // String to Number
    evalEquals("TRY_CAST('-12.5' as Number)", -12.5D).returnType(NumberType.NUMBER_NOT_NULL);
    evalNull("TRY_CAST('X12.5' as Number)").returnType(NumberType.NUMBER);

    // String to Date
    evalEquals("TRY_CAST('2024' as DATE format 'YYYY')", LocalDate.of(2024, 1, 1))
        .returnType(DateType.DATE_NOT_NULL);
    evalNull("TRY_CAST('XXXX' as DATE format 'YYYY')").returnType(DateType.DATE);

    // String to JSON
    evalEquals(
            "TRY_CAST(FIELD_STRING_JSON as JSON)",
            JsonConversion.convert("{id:\"01\",name:\"John\",age:29}"))
        .returnType(JsonType.JSON);

    // Number to Boolean
    evalTrue("TRY_CAST(1 as Boolean)").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalTrue("TRY_CAST(-12.1 as Boolean)");
    evalNull("TRY_CAST('test' as Boolean)");

    // Boolean to String
    evalEquals("TRY_CAST(True as String)", "TRUE").returnType(StringType.STRING_NOT_NULL);

    // Date to String
    evalEquals("TRY_CAST(DATE '2019-02-25' AS STRING FORMAT 'DD/MM/YYYY')", "25/02/2019")
        .returnType(StringType.STRING_NOT_NULL);
    evalNull("TRY_CAST('2019-99-25' AS DATE)").returnType(DateType.DATE);
    evalNull("TRY_CAST('2019-99-25' AS DATE FORMAT 'YYYY-MM-DD')");
    evalNull("TRY_CAST(NULL_STRING AS DATE)");

    // Date to String
    evalEquals("TRY_CAST(FIELD_INET as STRING)", "10.10.10.1").returnType(StringType.STRING);

    // Bad syntax
    evalFails(
        "TRY_CAST('2020-01-021' AS DATE FORMAT NULL_STRING)", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("TRY_CAST('bad' AS)", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("TRY_CAST(1234 AS STRING FORMAT )", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("TRY_CAST(DATE '2019-02-25' AS String FORMAT )", ErrorCode.SYNTAX_ERROR_FUNCTION);

    // Bad data type
    evalFails("TRY_CAST('2020-01-021' AS NULL)", ErrorCode.INVALID_TYPE);
    evalFails("Try_Cast(123 as Nill)", ErrorCode.INVALID_TYPE);

    // Bad format
    evalFails("TRY_CAST('2020-01-021' AS DATE FORMAT 'OOOO-MM-DD')", ErrorCode.INVALID_DATE_FORMAT);

    optimize("TRY_CAST(FIELD_STRING AS BINARY)", "TRY(CAST(FIELD_STRING AS BINARY))");
    optimize("TRY_CAST(FIELD_INTEGER AS NUMBER)", "TRY(CAST(FIELD_INTEGER AS NUMBER))");
    optimize(
        "TRY_CAST(FIELD_STRING AS DATE FORMAT 'YYYY-MM-DD')",
        "TRY(CAST(FIELD_STRING AS DATE FORMAT 'YYYY-MM-DD'))");
  }

  @Test
  void Try_To_Binary() throws Exception {
    evalEquals("TRY_TO_BINARY(HEX_ENCODE('Apache Hop'),'HEX')", "Apache Hop".getBytes())
        .returnType(BinaryType.BINARY_NOT_NULL);

    evalEquals("TRY_TO_BINARY('41706163686520486f70','HEX')", "Apache Hop".getBytes());
    evalNull("TRY_TO_BINARY('Z4','HEX')").returnType(BinaryType.BINARY);

    evalEquals("TRY_TO_BINARY(BASE64_ENCODE('Apache Hop'),'BASE64')", "Apache Hop".getBytes());
    evalEquals("TRY_TO_BINARY('QXBhY2hlIEhvcA==','BASE64')", "Apache Hop".getBytes());

    evalEquals(
        "TRY_TO_BINARY('Apache Hop','UtF-8')", "Apache Hop".getBytes(StandardCharsets.UTF_8));

    evalNull("TRY_TO_BINARY(NULL_STRING)");

    // Failed if the format is null
    evalFails("TRY_TO_BINARY('Apache Hop',NULL_STRING)", ErrorCode.ILLEGAL_ARGUMENT);

    // Failed if the format is bad
    evalFails("TRY_TO_BINARY('Apache Hop','ZZZ')", ErrorCode.INVALID_BINARY_FORMAT);

    // Check operands
    evalFails("TRY_TO_BINARY()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("TRY_TO_BINARY('te','t','s')", ErrorCode.TOO_MANY_ARGUMENT);

    optimize("TRY_TO_BINARY(FIELD_STRING)", "TRY(TO_BINARY(FIELD_STRING))");
  }

  @Test
  void Try_To_Boolean() throws Exception {
    evalTrue("TRY_TO_BOOLEAN('True')").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalFalse("TRY_TO_BOOLEAN('falSE')").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalFalse("TRY_TO_BOOLEAN(0)").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalFalse("TRY_TO_BOOLEAN(-0.00)").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalTrue("TRY_TO_BOOLEAN(1)").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalTrue("TRY_TO_BOOLEAN(1.2)").returnType(BooleanType.BOOLEAN_NOT_NULL);

    evalNull("TRY_TO_BOOLEAN('Bad')").returnType(BooleanType.BOOLEAN);
    evalNull("TRY_TO_BOOLEAN(NULL_STRING)").returnType(BooleanType.BOOLEAN);

    // Check operands
    evalFails("TRY_TO_BOOLEAN()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("TRY_TO_BOOLEAN('yes',3)", ErrorCode.TOO_MANY_ARGUMENT);

    optimize("TRY_TO_BOOLEAN(FIELD_STRING)", "TRY(TO_BOOLEAN(FIELD_STRING))");
  }

  @Test
  void Try_To_Number() throws Exception {
    evalEquals("TRY_TO_NUMBER('5467.12', '999999.99')", 5467.12D)
        .returnType(NumberType.NUMBER_NOT_NULL);

    // Return NULL if parsing failed
    evalNull("TRY_TO_NUMBER('54Z67z12', '999999D99')");

    evalNull("TRY_TO_NUMBER(NULL_STRING)");

    // Check operands
    evalFails("TRY_TO_NUMBER()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    // Failed if the format is bad
    evalFails("TRY_TO_NUMBER('5467.12', 'ZZZ')", ErrorCode.INVALID_NUMBER_FORMAT);

    // Date to Epoch
    evalEquals("TRY_TO_NUMBER(Date '1970-01-01')", 0D);
    evalEquals("TRY_TO_NUMBER(Date '2019-02-25')", 1551052800D);
    evalEquals(
        "TRY_TO_NUMBER(Timestamp '2010-09-13 04:32:03.123456789')",
        new BigDecimal("1284352323.123456789"));
    evalNull("TRY_TO_NUMBER(NULL_DATE)");

    optimize("TRY_TO_NUMBER(FIELD_STRING)", "TRY(TO_NUMBER(FIELD_STRING))");
  }

  @Test
  void Try_To_Date() throws Exception {
    evalEquals("TRY_TO_DATE('2019-02-13','YYYY-MM-DD')", LocalDate.of(2019, 2, 13))
        .returnType(DateType.DATE_NOT_NULL);

    // Return NULL if parsing failed
    evalNull("TRY_TO_DATE('2019-01-42','YYYY-MM-DD')").returnType(DateType.DATE);
    evalNull("TRY_TO_DATE('2019-01-0x','YYYY-MM-DD')");
    evalNull("TRY_TO_DATE('2019-13-13','YYYY-MM-DD')");
    evalNull("TRY_TO_DATE('20x9-13-13','YYYY-MM-DD')");

    // Null handling
    evalNull("TRY_TO_DATE(NULL_STRING,'FXDD/MM/YYYY')");

    // Check operands
    evalFails("TRY_TO_DATE()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    // Failed if the format is bad
    evalFails("TRY_TO_DATE('2019-12-01','OOOO-MM-DD')", ErrorCode.INVALID_DATE_FORMAT);

    // Integer Unix Epoch in seconds
    evalEquals("TRY_TO_DATE(0)", LocalDateTime.of(1970, 1, 1, 0, 0, 0));
    evalEquals("TRY_TO_DATE(1551052800)", LocalDate.of(2019, 2, 25));
    evalEquals("TRY_TO_DATE(-5364662400)", LocalDate.of(1800, 1, 1));
    evalEquals("TRY_TO_DATE(1284352323)", LocalDateTime.of(2010, 9, 13, 4, 32, 3));

    // Number Unix Epoch in seconds with fractional
    evalEquals("TRY_TO_DATE(1551052800.000000000)", LocalDate.of(2019, 2, 25));
    evalEquals("TRY_TO_DATE(-5364662400.000000000)", LocalDate.of(1800, 1, 1));
    evalEquals("TRY_TO_DATE(1284352323.1)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 100000000));
    evalEquals("TRY_TO_DATE(1284352323.12)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 120000000));
    evalEquals("TRY_TO_DATE(1284352323.123)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 123000000));
    evalEquals(
        "TRY_TO_DATE(1284352323.123456789)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 123456789));

    optimize("TRY_TO_DATE(FIELD_STRING)", "TRY(TO_DATE(FIELD_STRING))");
  }

  @Test
  void Try_To_Json() throws Exception {
    evalEquals(
            "Try_To_Json('{\"name\":\"Smith\", \"age\":29}')",
            JsonConversion.convert("{\"name\":\"Smith\",\"age\":29}"))
        .returnType(JsonType.JSON_NOT_NULL);
    evalEquals("Try_To_Json('true')", JsonConversion.convert("true"));
    evalEquals("Try_To_Json('null')", JsonConversion.convert("null"));

    // Null handling
    evalNull("Try_To_Json(NULL_STRING)");
    evalNull("Try_To_Json('BAD JSON ;')");
    evalNull("Try_To_Json('{\"name\":\"Smith\"; \"age\":29}')");

    // Check operands
    evalFails("Try_To_Json()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Try_To_Json(FIELD_JSON, NULL_JSON)", ErrorCode.TOO_MANY_ARGUMENT);

    optimize("TRY_TO_JSON(FIELD_STRING)", "TRY(TO_JSON(FIELD_STRING))");
  }
}
