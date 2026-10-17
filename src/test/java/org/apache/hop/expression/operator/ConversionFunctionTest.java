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
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ResourceLock;
import org.junit.jupiter.api.parallel.Resources;

@TestInstance(PER_CLASS)
@Execution(CONCURRENT)
@ResourceLock(Resources.LOCALE)
class ConversionFunctionTest extends ExpressionTest {

  private Locale defaultLocale = Locale.getDefault();

  @BeforeAll
  void saveLocale() {
    defaultLocale = Locale.getDefault();
  }

  /** Restore the default locale changed by a test, so that a test does not depend on another. */
  @AfterEach
  void restoreLocale() {
    Locale.setDefault(defaultLocale);
  }

  @Test
  void To_Binary() throws Exception {
    evalEquals("TO_BINARY(HEX_ENCODE('Apache Hop'),'HEX')", "Apache Hop".getBytes());
    evalEquals("TO_BINARY('41706163686520486f70','HEX')", "Apache Hop".getBytes());

    evalEquals("TO_BINARY(BASE64_ENCODE('Apache Hop'),'BASE64')", "Apache Hop".getBytes());
    evalEquals("TO_BINARY('QXBhY2hlIEhvcA==','BASE64')", "Apache Hop".getBytes());

    evalEquals("TO_BINARY('Apache Hop','UtF-8')", "Apache Hop".getBytes(StandardCharsets.UTF_8));
    evalEquals("TO_BINARY('Apache Hop','UtF8')", "Apache Hop".getBytes(StandardCharsets.UTF_8));

    // Default format is HEX and format is case-insensitive
    evalEquals("TO_BINARY('41706163686520486f70')", "Apache Hop".getBytes());
    evalEquals("TO_BINARY('QXBhY2hlIEhvcA==','base64')", "Apache Hop".getBytes());

    // Null handling
    evalNull("TO_BINARY(NULL_STRING)");

    // Check operands
    evalFails("TO_BINARY()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("TO_BINARY('Apache Hop',NULL_STRING)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);

    evalFails("TO_BINARY('Z4','HEX')", ErrorCode.UNPARSABLE_BINARY_WITH_FORMAT);
  }

  @Test
  void To_Boolean() throws Exception {
    evalTrue("To_Boolean('True')").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalTrue("To_Boolean(' True  ')");
    evalFalse("To_Boolean(' off')");
    evalTrue("To_Boolean('t')");
    evalTrue("To_Boolean('yes')");
    evalTrue("To_Boolean('y')");
    evalTrue("To_Boolean('on')");
    evalTrue("To_Boolean('1')");
    evalTrue("To_Boolean(2)");
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
    evalFails("To_Boolean()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("To_Boolean(1,2,3)", ErrorCode.TOO_MANY_ARGUMENTS);

    // TODO: Enforce field date conversion
    // evalFails("To_Boolean(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);

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
    evalEquals("TO_NUMBER('1.2E+02')", 1.2E+02D);

    // Underscore
    evalEquals("TO_NUMBER('1_000')", 1000D);
    evalEquals("TO_NUMBER('1_000.000_1')", 1000.0001D);
    evalEquals("TO_NUMBER('1_2.3_4E1_0')", 12.34E10D);
    evalEquals("TO_NUMBER('1_234.2E-3')", 1.2342D);

    // Format with Decimals
    evalEquals("TO_NUMBER('5467.12', '999999.99')", 5467.12D);
    evalEquals("TO_NUMBER('1234.5','09999.99')", 1234.5D);
    Locale.setDefault(Locale.US);
    evalEquals("TO_NUMBER('5467.12', '999999D99')", 5467.12D);
    Locale.setDefault(Locale.FRANCE);
    evalEquals("TO_NUMBER('5467,12', '999999D99')", 5467.12D);

    // Format No Decimals
    evalEquals("TO_NUMBER('4687841', '9999999')", 4687841D);

    // Leading space
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
    evalEquals("TO_NUMBER('+0.2','S99.99')", 0.2D);
    evalEquals("TO_NUMBER('0.3+','99.99S')", 0.3D);

    // A missing sign is a positive value
    evalEquals("TO_NUMBER('0.2','S99.99')", 0.2D);
    evalEquals("TO_NUMBER('0.3','99.99S')", 0.3D);

    // Scientific notation
    evalEquals("TO_NUMBER('1.2E+03','9.9EEEE')", 1200D);
    evalEquals("TO_NUMBER('1.2e-03','9.9eeee')", 0.0012D);
    evalEquals("TO_NUMBER('-1.2E+03','9.9EEEE')", -1200D);
    evalEquals("TO_NUMBER('  1.2E+02','9.9EEEE')", 120D);
    evalEquals("TO_NUMBER('12E2','99EEEE')", 1200D);
    evalEquals("TO_NUMBER(TO_CHAR(123.456,'9.99EEEE'),'9.99EEEE')", 123D);
    evalFails("TO_NUMBER('1.2','9.9EEEE')", ErrorCode.UNPARSABLE_NUMBER_WITH_FORMAT);
    evalFails("TO_NUMBER('1.2E+','9.9EEEE')", ErrorCode.UNPARSABLE_NUMBER_WITH_FORMAT);

    // Alternative formats
    evalEquals("TO_NUMBER('1234','S9999|9999S')", 1234D);
    evalEquals("TO_NUMBER('-1234','S9999|9999S')", -1234D);
    evalEquals("TO_NUMBER('1234-','S9999|9999S')", -1234D);

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
    Locale.setDefault(Locale.US);
    evalEquals("TO_NUMBER('12,345,678', '999,999,999')", 12_345_678D);
    Locale.setDefault(Locale.FRANCE);
    evalEquals("TO_NUMBER('12 345 678', '999G999G999')", 12_345_678D);
    evalEquals("TO_NUMBER('12 345 678,123', '999G999G999D000')", 12_345_678.123D);
    // Without a decimal separator in the format, the comma is a grouping separator
    evalEquals("TO_NUMBER('12,345', '99G999')", 12_345D);
    Locale.setDefault(Locale.GERMANY);
    evalEquals("TO_NUMBER('12.345.678', '999G999G999')", 12_345_678D);

    // Format with Currency dollar
    Locale.setDefault(Locale.US);
    evalEquals("TO_NUMBER('$65.169', '$99.999')", 65.169D);
    Locale.setDefault(Locale.FRANCE);
    evalEquals("TO_NUMBER('$65.169', '$99.999')", 65.169D);

    // Format with Currency symbol
    Locale.setDefault(Locale.US);
    evalEquals("TO_NUMBER('$65.169', 'L99.999')", 65.169D);
    Locale.setDefault(Locale.FRANCE);
    evalEquals("TO_NUMBER('€65.169', 'L99.999')", 65.169D);
    evalEquals("TO_NUMBER('65.16€', '99.999L')", 65.16D);

    // Format with Currency code
    Locale.setDefault(Locale.US);
    evalEquals("TO_NUMBER('USD65.169', 'C99.999')", 65.169D);
    Locale.setDefault(Locale.FRANCE);
    evalEquals("TO_NUMBER('EUR65.169', 'C99.999')", 65.169D);
    evalEquals("TO_NUMBER('65.16EUR', '99.999C')", 65.16D);

    // Format Hex
    evalEquals("TO_NUMBER('ABCD','FMXXXX')", 43981D);
    evalEquals("TO_NUMBER('abcd','xxxx')", 43981D);
    evalEquals("TO_NUMBER('007B','0XXX')", 123D);

    // Format Roman numeral
    evalEquals("TO_NUMBER('DXV','RN')", 515D);
    evalEquals("TO_NUMBER('CDLXXXV','RN')", 485D);

    evalEquals("TO_NUMBER('MCMXCIX','rn')", 1999D);
    evalEquals("TO_NUMBER('MMMDCCXXIV','rn')", 3724D);
    evalEquals("TO_NUMBER('xiv','rn')", 14D);
    evalEquals("TO_NUMBER(TO_CHAR(515, 'RN'),'RN')", 515D);
    evalFails("TO_NUMBER('ABC','RN')", ErrorCode.UNPARSABLE_NUMBER_WITH_FORMAT);

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

    evalEquals("TO_NUMBER(TIMESTAMP '2010-09-13 04:32:03')", new BigDecimal("1284352323"));
    evalEquals("TO_NUMBER('12','B99')", 12D);
    evalEquals("TO_NUMBER('12.5','TM')", 12.5D);
    evalEquals("TO_NUMBER('12.5','FM99.99')", 12.5D);
    evalEquals("TO_NUMBER('xiv','RN')", 14D);
    evalFails("TO_NUMBER('12a')", ErrorCode.UNPARSABLE_NUMBER_WITH_FORMAT);

    // Literals are always preserved as-is at their position and must be present
    evalEquals("TO_NUMBER('(12)','(99)')", 12D);
    evalEquals("TO_NUMBER(' ( 12)','(999)')", 12D);
    evalEquals("TO_NUMBER('-(12)','(99)')", -12D);
    evalEquals("TO_NUMBER('12/34','99/99')", 1234D);
    evalEquals("TO_NUMBER('1-234','999-999')", 1234D);
    evalEquals("TO_NUMBER('=12;','=99;')", 12D);
    evalEquals("TO_NUMBER('12.3:4','99.9:9')", 12.34D);
    evalFails("TO_NUMBER('1234','99/99')", ErrorCode.UNPARSABLE_NUMBER_WITH_FORMAT);
    evalFails("TO_NUMBER('1 2','99')", ErrorCode.UNPARSABLE_NUMBER_WITH_FORMAT);

    // Hexadecimal keeps the sign
    evalEquals("TO_NUMBER('-FF','XX')", -255D);

    // An empty format
    evalFails("TO_NUMBER('1','')", ErrorCode.INVALID_NUMBER_FORMAT);
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
    evalEquals("TO_CHAR(0.1,'90.99')", "  0.10").returnType(StringType.STRING_NOT_NULL);
    evalEquals("TO_CHAR(-0.2,'90.90')", " -0.20");
    evalEquals("TO_CHAR(0,'90.99')", "  0.00");
    evalEquals("TO_CHAR(0,'90D99')", "  0.00");
    evalEquals("TO_CHAR(0,'90d99')", "  0.00");
    evalEquals("TO_CHAR(0,'99.99')", "   .00");
    evalEquals("TO_CHAR(0,'9999')", "    0");
    evalEquals("TO_CHAR(0,'9999999')", "       0");
    evalEquals("TO_CHAR(0,'0999')", " 0000");

    // Blank mode returns blanks when the value is zero
    evalEquals("TO_CHAR(0,'B9999')", "     ");
    evalEquals("TO_CHAR(0,'B90.99')", "      ");
    evalEquals("TO_CHAR(0.5,'B0.99')", " 0.50");
    evalEquals("TO_CHAR(-0.5,'B0.99')", "-0.50");
    evalEquals("TO_CHAR(12,'B0999')", " 0012");
    evalEquals("TO_CHAR(12,'FMB0999')", "0012");

    evalEquals("TO_CHAR(-0.5, '90.99')", " -0.50");
    evalEquals("TO_CHAR(0.003,'0.999')", " 0.003");
    evalEquals("TO_CHAR(12,'99')", " 12");
    evalEquals("TO_CHAR(-7,'99')", " -7");
    evalEquals("TO_CHAR(12923,'99,999.00')", " 12,923.00");
    evalEquals("TO_CHAR(12,'9990999.9')", "    0012.0");
    evalEquals("TO_CHAR(0.3,'99.00000')", "   .30000");
    evalEquals("TO_CHAR(0.3,'00.00')", " 00.30");
    evalEquals("TO_CHAR(12923,'FM99999.99')", "12923.");
    evalEquals("TO_CHAR(12923,'FM9,9,9,9,9')", "1,2,9,2,3");
    evalEquals("TO_CHAR(0.3,'FM00.99')", "00.3");

    // Blanks for the integer part of a fixed-point number when the integer part is zero
    evalEquals("TO_CHAR(-0.2,'99.90')", "  -.20");
    evalEquals("TO_CHAR(-0.2,'99.99')", "  -.20");

    // TODO: Quoted text is reproduced in the result
    // evalEquals("TO_CHAR(485.8, '\"Pre:\"999\" Post:\" .999')", "Pre: 485 Post: .800");

    evalEquals("TO_CHAR(12345.567,'9,999')", "######");
    evalEquals("TO_CHAR(1234.94,'9999MI')", "1235 ");
    evalEquals("TO_CHAR(1.999,'9.99')", " 2.00");
    evalEquals("TO_CHAR(-1.995,'9.99')", "-2.00");
    evalEquals("TO_CHAR(1.994,'9.99')", " 1.99");
    evalEquals("TO_CHAR(9.99,'9.9')", "####");
    evalEquals("TO_CHAR(555.0, 'FM999.009')", "555.00");

    Locale.setDefault(Locale.FRANCE);
    evalEquals("TO_CHAR(0,'90.99')", "  0.00");
    evalEquals("TO_CHAR(0,'90D99')", "  0,00");
    evalEquals("TO_CHAR(0,'90d00')", "  0,00");

    // Format fixed length with grouping
    Locale.setDefault(Locale.US);
    evalEquals("TO_CHAR(1485,'9,999')", " 1,485");
    evalEquals("TO_CHAR(3148.5, '9G999D999')", " 3,148.500");
    Locale.setDefault(Locale.FRANCE);
    evalEquals("TO_CHAR(3148.5, '9G999D999')", " 3 148,500");
    evalEquals("TO_CHAR(3148.5, '9g999d990')", " 3 148,500");
    Locale.setDefault(Locale.GERMANY);
    evalEquals("TO_CHAR(3148.5, '9G999D999')", " 3.148,500");

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
    Locale.setDefault(Locale.UK);
    evalEquals("TO_CHAR(12,'C99')", " GBP12");
    Locale.setDefault(Locale.US);
    evalEquals("TO_CHAR(-7,'C99')", " -USD7");
    Locale.setDefault(Locale.FRANCE);
    evalEquals("TO_CHAR(-77,'99C')", "-77EUR");
    Locale.setDefault(Locale.GERMANY);
    evalEquals("TO_CHAR(-7,'99C')", " -7EUR");
    // Only currency ISO code
    evalEquals("TO_CHAR(0,'FMC')", "EUR");

    // Currency symbol
    Locale.setDefault(Locale.UK);
    evalEquals("TO_CHAR(12,'FML99')", "£12");
    Locale.setDefault(Locale.FRANCE);
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
    evalEquals("TO_CHAR(1.50,'TME')", "1.5E+00");
    evalEquals("TO_CHAR(1500,'TME')", "1.5E+03");
    evalEquals("TO_CHAR(123.456,'tMe')", "1.23456e+02");

    // Scientific, the mantissa is rounded after the normalization of the exponent
    evalEquals("TO_CHAR(123.456,'9.9EEEE')", "  1.2E+02");
    evalEquals("TO_CHAR(125,'9.9EEEE')", "  1.3E+02");
    evalEquals("TO_CHAR(-125,'9.9EEEE')", " -1.3E+02");
    evalEquals("TO_CHAR(999,'9.9EEEE')", "  1.0E+03");
    evalEquals("TO_CHAR(0.00123,'9.9EEEE')", "  1.2E-03");
    evalEquals("TO_CHAR(0,'9.9EEEE')", "  0.0E+00");

    // Shifted by V digits
    evalEquals("TO_CHAR(12.3,'99V9')", " 123");
    evalEquals("TO_CHAR(12,'99V9')", " 120");
    evalEquals("TO_CHAR(999,'99V9')", "####");

    // Roman
    evalEquals("TO_CHAR(11,'FMRN')", "XI");
    evalEquals("TO_CHAR(11,'rn')", "             xi");
    evalEquals("TO_CHAR(5.2, 'FMRN')", "V");
    // A value with a fractional part is rounded
    evalEquals("TO_CHAR(5.7, 'FMRN')", "VI");
    evalEquals("TO_CHAR(0.6, 'FMRN')", "I");
    evalFails("TO_CHAR(3999.5, 'RN')", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    // A value out of the integer range is not truncated to its low-order bits
    evalFails("TO_CHAR(4294967297, 'RN')", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalEquals("TO_CHAR(485, 'FMRN')", "CDLXXXV");
    evalEquals("TO_CHAR(515, 'RN')", "            DXV");
    evalFails("TO_CHAR(0, 'RN')", ErrorCode.ARGUMENT_OUT_OF_RANGE); // Must be > 0
    evalFails("TO_CHAR(4000, 'RN')", ErrorCode.ARGUMENT_OUT_OF_RANGE); // Must be < 4000

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
    evalEquals("To_Char(To_Date('-2000','YYYY'),'YYYY BC')", "2000 BC");
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
    evalEquals("To_Char(DATE '2019-12-28','I-MM')", "9-12");

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
    evalEquals("To_Char(DATE '2019-07-27','D')", "7");

    // Week of month, week of year
    evalEquals("To_Char(DATE '2019-07-23','W/MM')", "4/07");
    evalEquals("To_Char(DATE '2019-07-23','FMWW')", "30");
    evalEquals("To_Char(DATE '2019-07-23','FMIW')", "30");

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
        "To_Char(To_Date('2019-02-13 15:34:56 -00:30','YYYY-MM-DD HH24:MI:SS TZH:TZM'),'TZH:TZM')",
        "-00:30");
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
    Locale.setDefault(Locale.US);
    // Java 21 use character NARROW NO-BREAK SPACE (NNBSP)
    evalEquals("To_Char(TIMESTAMP '2019-07-23 14:52:00','DS TS')", "Jul 23, 2019 2:52:00\u202fPM");
    Locale.setDefault(Locale.UK);
    evalEquals("To_Char(TIMESTAMP '2019-07-23 14:52:00','DS TS')", "23 Jul 2019 14:52:00");
    Locale.setDefault(Locale.FRANCE);
    evalEquals("To_Char(TIMESTAMP '2019-07-23 14:52:00','DS TS')", "23 juil. 2019 14:52:00");

    // Long date
    Locale.setDefault(Locale.US);
    evalEquals("To_Char(TIMESTAMP '2019-07-23 14:52:00','DL')", "Tuesday, July 23, 2019");
    Locale.setDefault(Locale.FRANCE);
    evalEquals("To_Char(TIMESTAMP '2019-07-23 14:52:00','DL')", "mardi 23 juillet 2019");

    // Local radix character
    Locale.setDefault(Locale.UK);
    evalEquals("To_Char(TIMESTAMP '2019-07-23 14:52:00','HH:MI:SSXFF')", "02:52:00.000000");
    Locale.setDefault(Locale.FRANCE);
    evalEquals("To_Char(TIMESTAMP '2019-07-23 14:52:00','HH:MI:SSXFF')", "02:52:00,000000");

    // Special char
    evalEquals("To_Char(DATE '2019-07-23',':;.,=-/(FMMONTH)')", ":;.,=-/(JULY)");

    // Check operands
    evalFails("To_Char(DATE '2019-07-23','*')", ErrorCode.INVALID_DATE_FORMAT);
    evalFails("To_Char(DATE '2019-07-23','£')", ErrorCode.INVALID_DATE_FORMAT);
    // A string with a format, which could be a number or a date
    evalFails("To_Char('12','99')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("To_Char(DATE '2019-07-23','{}[]')", ErrorCode.INVALID_DATE_FORMAT);

    // Full case
    evalEquals(
        "To_Char(TIMESTAMP '2020-12-03 01:02:03.123456','yyyy-mm-dd hh:mi:ss.FF')",
        "2020-12-03 01:02:03.123456");

    // Boolean
    evalEquals("TO_CHAR(FIELD_BOOLEAN_TRUE)", "TRUE");
    evalEquals("TO_CHAR(FALSE)", "FALSE");
    optimize("TO_CHAR(TRUE)", "'TRUE'");

    // Binary
    evalEquals("TO_CHAR(BINARY '41706163686520486f70')", "41706163686520486f70");
    evalEquals("TO_CHAR(BINARY '41706163686520486f70','HEX')", "41706163686520486f70");
    evalEquals("TO_CHAR(BINARY '41706163686520486f70','base64')", "QXBhY2hlIEhvcA==");
    evalEquals("TO_CHAR(BINARY '41706163686520486f70','BASE64')", "QXBhY2hlIEhvcA==");
    evalEquals("TO_CHAR(BINARY '41706163686520486f70','UTF8')", "Apache Hop");
    evalEquals("TO_CHAR(BINARY '41706163686520486f70','UTF-8')", "Apache Hop");

    // String
    evalEquals("TO_CHAR('Apache Hop')", "Apache Hop");

    // Default date format
    evalEquals("TO_CHAR(DATE '2019-07-23')", "2019-07-23");

    // Date
    evalEquals("To_Char(DATE '2019-07-23','RRRR')", "2019");
    evalEquals("To_Char(DATE '2019-07-23','Y,YYY')", "2,019");
    evalEquals("To_Char(TIMESTAMP '2019-02-13 15:34:56.123','HH24:MI:SSFF0')", "15:34:56");
    evalEquals("To_Char(TIMESTAMP '2019-02-13 15:34:56.123','HH24:MI:SS.FF0')", "15:34:56.");
    evalEquals("To_Char(DATE '0800-07-23','Y,YYY')", "0,800");
    evalEquals("To_Char(DATE '2019-07-23','DD/MM/Y,YYY')", "23/07/2,019");
    evalEquals("To_Char(DATE '2019-07-23','RR')", "19");
    evalEquals("To_Char(TIMESTAMP '2019-02-13 00:34:56','HH12')", "12");

    // Date format examples
    String timestamp = "TIMESTAMP '2021-12-21 16:01:07.123456' AT TIME ZONE 'Europe/Paris'";
    evalEquals(
        "To_Char(" + timestamp + ",'Dy, DD Mon YYYY HH12:MI:SS AM')",
        "Tue, 21 Dec 2021 04:01:07 PM");
    evalEquals(
        "To_Char(" + timestamp + ",'DY, DD MON YYYY HH12:MI:SS AM')",
        "TUE, 21 DEC 2021 04:01:07 PM");
    evalEquals(
        "To_Char(" + timestamp + ",'YYYY-MM-DD\"T\"HH24:MI:SS.FF6TZH:TZM')",
        "2021-12-21T16:01:07.123456+01:00");
    evalEquals(
        "To_Char(" + timestamp + ",'DD/MM/YYYY HH24:MI TZR')", "21/12/2021 16:01 Europe/Paris");

    // Number
    evalEquals("TO_CHAR(3999,'FMRN')", "MMMCMXCIX");
    evalEquals("TO_CHAR(14,'FMrn')", "xiv");
    evalEquals("TO_CHAR(123.5,'XX')", " 7C");
    evalEquals("TO_CHAR(255,'xx')", " ff");
    evalEquals("TO_CHAR(123.456,'9.9eeee')", "  1.2E+02");
    evalFails("TO_CHAR(1,'9.9.9')", ErrorCode.INVALID_NUMBER_FORMAT);

    // Literals are always preserved as-is at their position
    evalEquals("TO_CHAR(12,'(99)')", " (12)");
    evalEquals("TO_CHAR(-12,'(99)')", "-(12)");
    evalEquals("TO_CHAR(12,'(999)')", " ( 12)");
    evalEquals("TO_CHAR(12,'FM(999)')", "(12)");
    evalEquals("TO_CHAR(1234,'99/99')", " 12/34");
    evalEquals("TO_CHAR(1234,'999-999')", "   1-234");
    evalEquals("TO_CHAR(12,'000-000')", " 000-012");
    evalEquals("TO_CHAR(12,'=99;')", " =12;");
    evalEquals("TO_CHAR(12.34,'99.9:9')", " 12.3:4");
    evalEquals("TO_CHAR(123456,'99/99')", "######");
  }

  @Test
  void To_Date() throws Exception {
    evalEquals("To_Date('2019-02-13','YYYY-MM-DD')", LocalDate.of(2019, 2, 13));
    evalEquals("To_Date('2020:148','YYYY:DDD')", LocalDate.of(2020, 5, 27));
    evalEquals("To_Date('2020-08','YYYY-MM')", LocalDate.of(2020, 8, 1));
    evalEquals("To_Date('2020-MarCH','YYYY-MONTH')", LocalDate.of(2020, Month.MARCH, 1));
    evalEquals("To_Date('2020,feb,25','YYYY,MON,DD')", LocalDate.of(2020, Month.FEBRUARY, 25));
    // The hour in AM/PM must be between 1 and 12
    evalEquals(
        "To_Date('2019-02-13 03:34:56','YYYY-MM-DD HH:MI:SS')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 3, 34, 56));
    evalEquals(
        "To_Date('2019-02-13 12:34:56 AM','YYYY-MM-DD HH12:MI:SS AM')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 0, 34, 56));
    evalEquals(
        "To_Date('2019-02-13 12:34:56 PM','YYYY-MM-DD HH12:MI:SS AM')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 12, 34, 56));
    evalFails("To_Date('2019-02-13 15:34:56','YYYY-MM-DD HH:MI:SS')", ErrorCode.INVALID_HOUR_12);
    evalFails("To_Date('2019-02-13 15:34:56','YYYY-MM-DD HH12:MI:SS')", ErrorCode.INVALID_HOUR_12);
    evalFails("To_Date('2019-02-13 00:34:56','YYYY-MM-DD HH12:MI:SS')", ErrorCode.INVALID_HOUR_12);
    evalNull("TRY_TO_DATE('2019-02-13 15:34:56','YYYY-MM-DD HH:MI:SS')");
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
    evalEquals("To_Date('2020/XII','YYYY/RM')", LocalDate.of(2020, Month.DECEMBER, 1));
    evalFails("To_Date('2020/','YYYY/RM')", ErrorCode.UNPARSABLE_DATE_WITH_FORMAT);

    evalEquals("To_Date('01/02/-100','DD/MM/SYYYY')", LocalDate.of(-100, 2, 1));
    evalEquals("To_Date('01/02/-100','DD/MM/YYYY')", LocalDate.of(-100, 2, 1));
    evalEquals("To_Date('0800 BC','YYYY BC')", LocalDate.of(-800, 1, 1));
    evalEquals("To_Date('-800 BC','FMSYYYY BC')", LocalDate.of(-800, 1, 1));

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

    evalEquals("To_Date('01-jan-4710bc','dd-mon-yyyybc')", LocalDate.of(-4710, 1, 1));

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
    evalEquals(
        "To_Date('2019-02-13 15:34:56 -05:30','YYYY-MM-DD HH24:MI:SS TZH:TZM')",
        ZonedDateTime.of(2019, 2, 13, 15, 34, 56, 0, ZoneOffset.ofHoursMinutes(-5, -30)));
    evalEquals(
        "To_Date('2019-02-13 15:34:56 -00:30','YYYY-MM-DD HH24:MI:SS TZH:TZM')",
        ZonedDateTime.of(2019, 2, 13, 15, 34, 56, 0, ZoneOffset.ofHoursMinutes(0, -30)));

    // Time zone region
    evalEquals(
        "To_Date('2019-02-13 15:34:56 US/Pacific','YYYY-MM-DD HH24:MI:SS TZR')",
        ZonedDateTime.of(2019, 2, 13, 15, 34, 56, 0, ZoneId.of("US/Pacific")));
    evalEquals(
        "To_Date('Europe/Paris 2019-02-13 15:34:56','TZR YYYY-MM-DD HH24:MI:SS')",
        ZonedDateTime.of(2019, 2, 13, 15, 34, 56, 0, ZoneId.of("Europe/Paris")));
    evalEquals(
        "To_Date('2019-02-13 15:34:56 America/New_York','YYYY-MM-DD HH24:MI:SS TZR')",
        ZonedDateTime.of(2019, 2, 13, 15, 34, 56, 0, ZoneId.of("America/New_York")));
    evalFails(
        "To_Date('2019-02-13 Unknown/Zone','YYYY-MM-DD TZR')",
        ErrorCode.UNPARSABLE_DATE_WITH_FORMAT);
    evalFails("To_Date('2019-02-13 XYZ','YYYY-MM-DD TZD')", ErrorCode.UNPARSABLE_DATE_WITH_FORMAT);

    // An omitted time zone hour at the end of the text
    evalEquals(
        "To_Date('2019-02-13','YYYY-MM-DD TZH')",
        ZonedDateTime.of(2019, 2, 13, 0, 0, 0, 0, ZoneOffset.UTC));

    // The era does not reset the day of year or the Julian day
    evalEquals("To_Date('2020-100 AD','YYYY-DDD AD')", LocalDate.of(2020, 4, 9));
    evalEquals("To_Date('2454803 AD','J AD')", LocalDate.of(2008, 12, 2));

    // Trailing space
    evalEquals("To_Date('  2020-08','YYYY-MM')", LocalDate.of(2020, 8, 1));
    evalEquals("To_Date(' 08- 2020','MM-SYYYY')", LocalDate.of(2020, 8, 1));

    // Only a year of 1 or 2 digits is rounded
    evalEquals("To_Date('01/2/0001','DD/MM/RRRR')", LocalDate.of(1, Month.FEBRUARY, 1));
    evalEquals("To_Date('01/2/0049','DD/MM/RRRR')", LocalDate.of(49, Month.FEBRUARY, 1));
    evalEquals("To_Date('01/2/1','DD/MM/RRRR')", LocalDate.of(2001, Month.FEBRUARY, 1));
    evalEquals("To_Date('01/2/49','DD/MM/RRRR')", LocalDate.of(2049, Month.FEBRUARY, 1));
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
        "To_Date('2019-02-13 19:34:56.5','YYYY-MM-DD HH24:MI:SS.FF3')",
        LocalDateTime.of(2019, Month.FEBRUARY, 13, 19, 34, 56, 500000000));
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
    // A 2-digit year is completed with a century, also in exact mode
    evalEquals("To_Date('20-05-24','FXYY-MM-DD')", LocalDate.of(2020, 5, 24));
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

    // Is interpreted as 12 May 2000, 00:00:10.123
    evalEquals(
        "To_Date('2000_MAY_12 10.123','YYYY_MONTH_DD SS.FF3')",
        LocalDateTime.of(2000, 5, 12, 0, 0, 10, 123000000));

    evalEquals("To_Date('15:30:40','hh24:mi:ss')", LocalDateTime.of(1970, 1, 1, 15, 30, 40));

    // Integer Unix Epoch in seconds, in the UTC time zone
    evalEquals("TO_DATE(0)", ZonedDateTime.of(1970, 1, 1, 0, 0, 0, 0, ZoneOffset.UTC));
    evalEquals("TO_DATE(1551052800)", ZonedDateTime.of(2019, 2, 25, 0, 0, 0, 0, ZoneOffset.UTC));
    evalEquals("TO_DATE(-5364662400)", ZonedDateTime.of(1800, 1, 1, 0, 0, 0, 0, ZoneOffset.UTC));
    evalEquals("TO_DATE(1284352323)", ZonedDateTime.of(2010, 9, 13, 4, 32, 3, 0, ZoneOffset.UTC));

    // Number Unix Epoch in seconds with fractional
    evalEquals(
        "TO_DATE(1551052800.000000000)", ZonedDateTime.of(2019, 2, 25, 0, 0, 0, 0, ZoneOffset.UTC));
    evalEquals(
        "TO_DATE(-5364662400.000000000)", ZonedDateTime.of(1800, 1, 1, 0, 0, 0, 0, ZoneOffset.UTC));
    evalEquals(
        "TO_DATE(-1.5)", ZonedDateTime.of(1969, 12, 31, 23, 59, 58, 500000000, ZoneOffset.UTC));
    evalEquals("TO_DATE(1284352323.1)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 100000000));
    evalEquals("TO_DATE(1284352323.12)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 120000000));
    evalEquals("TO_DATE(1284352323.123)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 123000000));
    evalEquals(
        "TO_DATE(1284352323.123456789)",
        ZonedDateTime.of(2010, 9, 13, 4, 32, 3, 123456789, ZoneOffset.UTC));

    // An epoch out of the range of dates raises an error instead of wrapping around
    evalFails("TO_DATE(9223372036854775807)", ErrorCode.CONVERSION_ERROR);
    evalFails("TO_DATE(18446744073709551616)", ErrorCode.CONVERSION_ERROR);

    // Explicit AUTO format ISO 8601
    evalEquals(
        "TO_DATE('2024-12-01 3:05:06.123456789','AUTO')",
        LocalDateTime.of(2024, 12, 1, 3, 5, 6, 123456789));
    evalEquals(
        "TO_DATE('2024-12-01 12:05:06.123456789','AUTO')",
        LocalDateTime.of(2024, 12, 1, 12, 5, 6, 123456789));
    evalEquals(
        "TO_DATE('2024-12-01 3:05:06.123456789+02:00','AUTO')",
        ZonedDateTime.of(2024, 12, 1, 3, 5, 6, 123456789, ZoneOffset.ofHours(2)));
    evalEquals(
        "TO_DATE('2024-12-01 3:05:06.123456789 +02:00','AUTO')",
        ZonedDateTime.of(2024, 12, 1, 3, 5, 6, 123456789, ZoneOffset.ofHours(2)));
    evalEquals(
        "TO_DATE('2024-12-01 3:05:06-05:30','AUTO')",
        ZonedDateTime.of(2024, 12, 1, 3, 5, 6, 0, ZoneOffset.ofHoursMinutes(-5, -30)));
    evalEquals(
        "TO_DATE('2024-12-01 3:05:06-00:30','AUTO')",
        ZonedDateTime.of(2024, 12, 1, 3, 5, 6, 0, ZoneOffset.ofHoursMinutes(0, -30)));
    evalEquals("TO_DATE('20241201230506','AUTO')", LocalDateTime.of(2024, 12, 1, 23, 5, 6));
    evalEquals("TO_DATE('20241201T230506','AUTO')", LocalDateTime.of(2024, 12, 1, 23, 5, 6));

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
    evalNull("TO_DATE(NULL_NUMBER)");

    // Documentation examples
    evalEquals("TO_DATE('1999-12', 'YYYY-MM')", LocalDate.of(1999, 12, 1));
    evalEquals("TO_DATE(1284352323.000000123)", LocalDateTime.of(2010, 9, 13, 4, 32, 3, 123));

    // Time zones are case-sensitive
    evalFails(
        "TO_DATE('2019-02-13 15:34:56 europe/london','YYYY-MM-DD HH24:MI:SS TZR')",
        ErrorCode.UNPARSABLE_DATE_WITH_FORMAT);

    // Era, meridian indicator, RR year and ; separator
    evalEquals("TO_DATE('0800 B.C.','YYYY B.C.')", LocalDate.of(-800, 1, 1));
    evalEquals(
        "TO_DATE('2009-12-24 11:00:00 A.M.','YYYY-MM-DD HH:MI:SS A.M.')",
        LocalDateTime.of(2009, 12, 24, 11, 0, 0));
    evalEquals("TO_DATE('01/02/52','DD/MM/RR')", LocalDate.of(1952, 2, 1));
    evalEquals("TO_DATE('2,019-07-23','Y,YYY-MM-DD')", LocalDate.of(2019, 7, 23));
    evalEquals(
        "TO_DATE('2019-02-13 15:34:56','YYYY-MM-DD HH24:MI:SSFF0')",
        LocalDateTime.of(2019, 2, 13, 15, 34, 56));
    evalFails(
        "TO_DATE('2019-02-13 15:34:56.123','YYYY-MM-DD HH24:MI:SSFF0')",
        ErrorCode.UNPARSABLE_DATE_WITH_FORMAT);
    evalFails("TO_DATE('2019-07-23','Y,YYY-MM-DD')", ErrorCode.UNPARSABLE_DATE_WITH_FORMAT);
    evalEquals("TO_DATE('2019;07;23','YYYY;MM;DD')", LocalDate.of(2019, 7, 23));

    // Alternative formats
    evalEquals("TO_DATE('2019-07-23','YYYY-MM-DD|DD/MM/YYYY')", LocalDate.of(2019, 7, 23));
    evalEquals("TO_DATE('23/07/2019','YYYY-MM-DD|DD/MM/YYYY')", LocalDate.of(2019, 7, 23));

    // An element can only appear once, but separators can be repeated
    evalFails("TO_DATE('2020-2021','YYYY-YYYY')", ErrorCode.INVALID_DATE_FORMAT);
    evalFails("TO_DATE('2020 20','YYYY RR')", ErrorCode.INVALID_DATE_FORMAT);
    evalFails("TO_DATE('02 FEB','MM MON')", ErrorCode.INVALID_DATE_FORMAT);
    evalFails("TO_DATE('10:10','HH24:HH')", ErrorCode.INVALID_DATE_FORMAT);
    evalFails("TRY_TO_DATE('2020-2021','YYYY-YYYY')", ErrorCode.INVALID_DATE_FORMAT);
    evalEquals("TO_DATE('2019--07--23','YYYY--MM--DD')", LocalDate.of(2019, 7, 23));
    evalEquals("To_Char(DATE '2019-07-23','YYYY YYYY')", "2019 2019");

    // Output only elements
    evalFails("TO_DATE('2019 3','YYYY Q')", ErrorCode.UNPARSABLE_DATE_WITH_FORMAT);
    evalFails("TO_DATE('2019 TUE','YYYY DY')", ErrorCode.UNPARSABLE_DATE_WITH_FORMAT);
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

    // Documentation examples
    evalEquals("TO_INTERVAL('1 year 6 months 12 hours')", Interval.of(1, 6, 0, 12, 0, 0));
    evalEquals("TO_INTERVAL('1-6 0 12:00:00')", Interval.of(1, 6, 0, 12, 0, 0));

    evalFails("TO_INTERVAL('abc')", ErrorCode.INVALID_INTERVAL);
    evalFails("TO_INTERVAL('')", ErrorCode.INVALID_INTERVAL);
    evalFails("TO_INTERVAL('   ')", ErrorCode.INVALID_INTERVAL);

    // In the verbose syntax, each quantity has its own sign
    evalEquals("TO_INTERVAL('-1 year 6 months')", Interval.of(0, -6));
    evalEquals("TO_INTERVAL('1 month -1 day')", Interval.of(0, 1, -1));

    // At most 9 digits of fractional seconds
    evalEquals("TO_INTERVAL('0-0 0 00:00:00.123456789')", Interval.of(0, 0, 0, 0, 0, 0, 123456789));
    evalFails("TO_INTERVAL('0-0 0 00:00:00.1234567891')", ErrorCode.INVALID_INTERVAL);

    // A field or a quantity out of range
    evalFails("TO_INTERVAL('3000000000-0 0 00:00:00')", ErrorCode.INVALID_INTERVAL);
    evalFails("TO_INTERVAL('99999999999999999999 days')", ErrorCode.INVALID_INTERVAL);
    evalFails("TO_INTERVAL('9223372036854775807 weeks')", ErrorCode.INVALID_INTERVAL);

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
    evalFails("To_Json()", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    evalFails("To_Json(FIELD_BOOLEAN_TRUE)", ErrorCode.CONVERSION_ERROR);
    evalFails("To_Json('{\"name\":\"Smith\"; \"age\":29}')", ErrorCode.CONVERSION_ERROR);
    evalFails("To_Json('')", ErrorCode.CONVERSION_ERROR);
    evalFails("To_Json('   ')", ErrorCode.CONVERSION_ERROR);
    // Content after the JSON value is not ignored
    evalFails("To_Json('{a:1} x')", ErrorCode.CONVERSION_ERROR);
    evalFails("To_Json('[1,2]]')", ErrorCode.CONVERSION_ERROR);
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

    // Number to Date, an epoch out of the range of dates
    evalEquals(
        "TRY_CAST(1551052800 AS DATE)", ZonedDateTime.of(2019, 2, 25, 0, 0, 0, 0, ZoneOffset.UTC));
    evalNull("TRY_CAST(9223372036854775807 AS DATE)");

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
    evalFails("TRY_CAST('2020-01-021' AS NULL)", ErrorCode.INVALID_DATA_TYPE);
    evalFails("Try_Cast(123 as Nill)", ErrorCode.INVALID_DATA_TYPE);

    // Bad format
    evalFails("TRY_CAST('2020-01-021' AS DATE FORMAT 'OOOO-MM-DD')", ErrorCode.INVALID_DATE_FORMAT);

    // Documentation examples
    evalEquals("TRY_CAST('-1234.56' AS INTEGER FORMAT '9999.99')", -1235L);
    evalEquals("CAST('1,234.56' AS NUMBER FORMAT '9,999.99')", 1234.56D);

    // A string converted to a number keeps its decimals, with or without format
    evalEquals("CAST('-1234.56' AS NUMBER)", -1234.56D);
    evalEquals("CAST('-1234.56' AS NUMBER FORMAT '9999.99')", -1234.56D);
    evalEquals("'-1234.56'::NUMBER", -1234.56D);
    evalEquals("CAST(FIELD_STRING_NUMBER AS NUMBER FORMAT '99.99')", -12.56D);
    evalEquals("CAST('1,234' AS INTEGER FORMAT '9,999')", 1234L);
    evalFails(
        "CAST('1,234.56' AS NUMBER FORMAT '9999.99')", ErrorCode.UNPARSABLE_NUMBER_WITH_FORMAT);
    evalFails("CAST('1234' AS NUMBER FORMAT '###0')", ErrorCode.INVALID_NUMBER_FORMAT);

    // A number converted to a number is truncated to the scale of the type
    evalEquals("1.23456::NUMBER", 1D);
    evalEquals("TRY_CAST(MAKE_DATE(2020,5,24) AS STRING FORMAT 'yyyy-q')", "2020-2");
    evalEquals(
        "CAST('24/05/2020 00:00:00' AS DATE FORMAT 'DD/MM/YYYY HH24:MI:SS')",
        LocalDate.of(2020, 5, 24));
    evalFalse("TRY_CAST(0 AS BOOLEAN)");

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
    evalFails("TRY_TO_BINARY('Apache Hop',NULL_STRING)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);

    // Failed if the format is bad
    evalFails("TRY_TO_BINARY('Apache Hop','ZZZ')", ErrorCode.INVALID_BINARY_FORMAT);

    // Check operands
    evalFails("TRY_TO_BINARY()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("TRY_TO_BINARY('te','t','s')", ErrorCode.TOO_MANY_ARGUMENTS);

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
    evalFails("TRY_TO_BOOLEAN()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("TRY_TO_BOOLEAN('yes',3)", ErrorCode.TOO_MANY_ARGUMENTS);

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
    evalFails("TRY_TO_NUMBER()", ErrorCode.NOT_ENOUGH_ARGUMENTS);

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
    evalFails("TRY_TO_DATE()", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Failed if the format is bad
    evalFails("TRY_TO_DATE('2019-12-01','OOOO-MM-DD')", ErrorCode.INVALID_DATE_FORMAT);

    // An epoch out of the range of dates
    evalNull("TRY_TO_DATE(9223372036854775807)");
    evalNull("TRY_TO_DATE(18446744073709551616)");

    // An unknown time zone abbreviation
    evalNull("TRY_TO_DATE('2019-02-13 XYZ','YYYY-MM-DD TZD')");

    // Integer Unix Epoch in seconds, in the UTC time zone
    evalEquals("TRY_TO_DATE(0)", ZonedDateTime.of(1970, 1, 1, 0, 0, 0, 0, ZoneOffset.UTC));
    evalEquals(
        "TRY_TO_DATE(1551052800)", ZonedDateTime.of(2019, 2, 25, 0, 0, 0, 0, ZoneOffset.UTC));
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
    evalNull("Try_To_Json('')");
    evalNull("Try_To_Json('   ')");

    // Check operands
    evalFails("Try_To_Json()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Try_To_Json(FIELD_JSON, NULL_JSON)", ErrorCode.TOO_MANY_ARGUMENTS);

    optimize("TRY_TO_JSON(FIELD_STRING)", "TRY(TO_JSON(FIELD_STRING))");
  }
}
