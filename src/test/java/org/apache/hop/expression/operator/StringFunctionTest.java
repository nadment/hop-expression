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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

import org.apache.hop.expression.Array;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionTest;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.StringType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.parallel.Execution;

@TestInstance(PER_CLASS)
@Execution(CONCURRENT)
public class StringFunctionTest extends ExpressionTest {

  @Test
  void ParseUrl() throws Exception {
    evalEquals("Parse_Url('https://hop.apache.org:80/path?query=1','PROTOCOL')", "https")
        .returnType(StringType.STRING_NOT_NULL);
    evalEquals("Parse_Url('https://hop.apache.org:80/path?query=1','HOST')", "hop.apache.org");
    evalEquals("Parse_Url('https://hop.apache.org:80/path?query=1','PORT')", "80");
    evalEquals(
        "Parse_Url('https://user:password@hop.apache.org:80/path?query=1','USERINFO')",
        "user:password");
    evalEquals(
        "Parse_Url('https://user:password@hop.apache.org:80/path?query=1','AUTHORITY')",
        "user:password@hop.apache.org:80");

    evalEquals("Parse_Url('https://hop.apache.org:80/path?query=1','PATH')", "/path");
    evalEquals(
        "Parse_Url('https://hop.apache.org:80/path?query=1#fragment','FILE')", "/path?query=1");
    evalEquals(
        "Parse_Url('https://hop.apache.org:80/path?query=1&lang=fr','QUERY')", "query=1&lang=fr");
    evalEquals("Parse_Url('https://hop.apache.org:80/path?query=1&id=2','QUERY','id')", "2");
    evalEquals("Parse_Url('https://hop.apache.org:80/path?query=1#fragment', 'REF')", "fragment");

    // Null handling
    evalNull("Parse_Url(NULL_STRING,'PATH')");
    evalNull("Parse_Url('https://hop.apache.org:80',NULL_STRING)");
    evalNull("Parse_Url('https://hop.apache.org:80','PATH')");
    evalNull("Parse_Url('https://hop.apache.org/path?query=1','PORT')");
    evalNull("Parse_Url('https://hop.apache.org/path','QUERY')");
    evalNull("Parse_Url('https://hop.apache.org/path?query=1','QUERY','xxx')");
    evalNull("Parse_Url('https://hop.apache.org/path?query=1','QUERY',NULL_STRING)");
  }

  @Test
  void Normalize() throws Exception {
    evalEquals("Normalize('\u00ea')", "ê").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Normalize('\u0065\u0302')", "ê");
    evalEquals("Normalize('Jane\u2004Doe', 'NFKC')", "Jane Doe");
    evalEquals("Normalize('Jane\u2006Doe', 'NFKC')", "Jane Doe");
    evalEquals("Normalize('¼', 'NFKC')", "1⁄4");
    evalEquals("Normalize('i⁹', 'NFKC')", "i9");

    // Null handling
    evalNull("Normalize(NULL_STRING)");

    // Check operands
    evalFails("Normalize()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Normalize('ê','BAD')", ErrorCode.INVALID_ARGUMENT);
  }

  @Test
  void Unaccent() throws Exception {
    evalEquals("Unaccent('ÁÀÂÃÄÅĀĄàáâãäåāą')", "AAAAAAAAaaaaaaaa")
        .returnType(StringType.STRING_NOT_NULL);
    evalEquals("Unaccent('ÇĆČçćč')", "CCCccc");
    evalEquals("Unaccent('ĎḌḒďḍḓ')", "DDDddd");
    evalEquals("Unaccent('ÈÉÊËĚĒĘèéêëěēę')", "EEEEEEEeeeeeee");
    evalEquals("Unaccent('ÌÍÎÏĪìíîïī')", "IIIIIiiiii");
    evalEquals("Unaccent('Łł')", "Ll");
    evalEquals("Unaccent('ÑŇŃñňń')", "NNNnnn");
    evalEquals("Unaccent('ÒÓÔÕÕÖŌòóôõöō')", "OOOOOOOoooooo");
    evalEquals("Unaccent('ÙÚÛÜŮŪùúûüůū')", "UUUUUUuuuuuu");
    evalEquals("Unaccent('Řř')", "Rr");
    evalEquals("Unaccent('ŠŚšś')", "SSss");
    evalEquals("Unaccent('Ťť')", "Tt");
    evalEquals("Unaccent('ÝŸÿý')", "YYyy");
    evalEquals("Unaccent('ŽŻŹžżź')", "ZZZzzz");
    evalEquals("Unaccent('άώὨ')", "αωΩ");

    // Keep ligature and special char
    evalEquals("Unaccent('ÆæØøµß¢©')", "ÆæØøµß¢©");

    // Null handling
    evalNull("Unaccent(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("Unaccent()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    // Function repetition
    optimize("Unaccent(Unaccent(FIELD_STRING))", "UNACCENT(FIELD_STRING)");
  }

  @Test
  void Upper() throws Exception {
    evalEquals("Upper('test')", "TEST").returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("Upper(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("Upper()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    // Function repetition
    optimize("UPPER(UPPER(FIELD_STRING))", "UPPER(FIELD_STRING)");
    optimize("UPPER(LOWER(FIELD_STRING))", "UPPER(FIELD_STRING)");
    optimize("UPPER(INITCAP(FIELD_STRING))", "UPPER(FIELD_STRING)");
  }

  @Test
  void InitCap() throws Exception {
    evalEquals("InitCap('hello the wORLD')", "Hello The World")
        .returnType(StringType.STRING_NOT_NULL);
    evalEquals("InitCap('tRy a littlE  ')", "Try A Little  ");
    evalEquals("InitCap('won''t it?no')", "Won'T It?No");
    evalEquals("InitCap('ÉéÀàè]çÂâ ÊêÎÔô ÛûËÏ ïÜŸÇç ŒœÆæ')", "Ééààè]Çââ Êêîôô Ûûëï Ïüÿçç Œœææ");

    // Null handling
    evalNull("InitCap(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("InitCap()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    // Function repetition
    optimize("INITCAP(LOWER(FIELD_STRING))", "INITCAP(FIELD_STRING)");
    optimize("INITCAP(UPPER(FIELD_STRING))", "INITCAP(FIELD_STRING)");
    optimize("INITCAP(INITCAP(FIELD_STRING))", "INITCAP(FIELD_STRING)");
  }

  @Test
  void Instr() throws Exception {
    evalEquals("Instr('CORPORATE FLOOR','OR')", 2L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Instr('CORPORATE FLOOR','or')", 0L);
    evalEquals("Instr('CORPORATE FLOOR','ORA')", 5L);
    evalEquals("Instr('CORPORATE FLOOR','ORA',6)", 0L);
    evalEquals("Instr('CORPORATE FLOOR','OR',5)", 5L);
    evalEquals("Instr('CORPORATE FLOOR','OR',6)", 14L);
    evalEquals("Instr('CORPORATE FLOOR','OR',3)", 5L);
    evalEquals("Instr('CORPORATE FLOOR','OR',3, 1)", 5L);
    evalEquals("Instr('CORPORATE FLOOR','OR',3, 2)", 14L);
    evalEquals("Instr('CORPORATE FLOOR','OR',3, 3)", 0L);

    evalEquals("Instr('CORPORATE FLOOR','O',-1)", 14L);
    evalEquals("Instr('CORPORATE FLOOR','O',-2)", 13L);
    evalEquals("Instr('CORPORATE FLOOR','O',-3)", 5L);
    evalEquals("Instr('CORPORATE FLOOR','O',-4)", 5L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-1)", 14L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-3)", 5L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-12)", 2L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-3, 1)", 5L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-3, 2)", 2L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-3, 3)", 0L);

    // Null handling
    evalNull("Instr(NULL_STRING,'test')");
    evalNull("Instr('test',NULL_STRING)");
    evalNull("Instr(NULL_STRING,NULL_STRING)");

    // Check operands
    evalFails("Instr()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Instr('CORPORATE FLOOR','OR',-3, 0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Instr('CORPORATE FLOOR','OR',0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
  }

  @Test
  void RPad() throws Exception {
    evalEquals("RPad('test',7)", "test   ").returnType(StringType.STRING_NOT_NULL);
    evalEquals("RPad('test',7,'*')", "test***");
    evalEquals("RPad('test',4,'*')", "test");
    evalEquals("RPad('test',3,'*')", "tes");
    evalEquals("RPad('test',4,'ABC')", "test");
    evalEquals("RPad('test',6,'ABC')", "testAB");
    evalEquals("RPad('test',7,'ABC')", "testABC");
    evalEquals("RPad('test',8,'ABC')", "testABCA");

    evalEquals("RPad(BINARY '1A2B3C',2,BINARY '4D5E6F')", new byte[] {0x1A, 0x2B})
        .returnType(BinaryType.BINARY_NOT_NULL);
    evalEquals("RPad(BINARY '1A2B3C',3,BINARY '4D5E6F')", new byte[] {0x1A, 0x2B, 0x3C});
    evalEquals("RPad(BINARY '1A2B3C',4,BINARY '4D5E6F')", new byte[] {0x1A, 0x2B, 0x3C, 0x4D});
    evalEquals(
        "RPad(BINARY '1A2B3C',5,BINARY '4D5E6F')", new byte[] {0x1A, 0x2B, 0x3C, 0x4D, 0x5E});
    evalEquals(
        "RPad(BINARY '1A2B3C',6,BINARY '4D5E6F')", new byte[] {0x1A, 0x2B, 0x3C, 0x4D, 0x5E, 0x6F});
    evalEquals(
        "RPad(BINARY '1A2B3C',7,BINARY '4D5E6F')",
        new byte[] {0x1A, 0x2B, 0x3C, 0x4D, 0x5E, 0x6F, 0x4D});

    // Empty padding
    evalEquals("RPad('test',5,'')", "test");
    evalEquals("RPad(BINARY '2A3B4C',5,BINARY '')", new byte[] {0x2A, 0x3B, 0x4C});

    // If length is a negative number, the result of the function is an empty string or binary.
    evalEquals("RPad('test',-8)", "");
    evalEquals("RPad(FIELD_BINARY,-8)", new byte[0]);

    // Null handling
    evalNull("RPad(NULL_STRING,2)").returnType(StringType.STRING);
    evalNull("RPad(NULL_BINARY,2)").returnType(BinaryType.BINARY);
    evalNull("RPad(NULL_STRING,-8)");
    evalNull("RPad(NULL_BINARY,-8)");

    // Check operands
    evalFails("RPad('test')", ErrorCode.NOT_ENOUGH_ARGUMENT);

    // Check padding length exceeds the maximum limit
    evalFails("RPad('test',10000,'t')", ErrorCode.PADDING_LENGTH_EXCEEDS_MAXIMUM_LIMIT);
    evalFails(
        "RPad(FIELD_BINARY,10000, FIELD_BINARY)", ErrorCode.PADDING_LENGTH_EXCEEDS_MAXIMUM_LIMIT);
  }

  @Test
  void LPad() throws Exception {
    evalEquals("LPad('test',6)", "  test").returnType(StringType.STRING_NOT_NULL);
    evalEquals("LPad('test',7,'*')", "***test");
    evalEquals("LPad('test',3,'*')", "tes");
    evalEquals("LPad('test',4,'ABC')", "test");
    evalEquals("LPad('test',6,'ABC')", "ABtest");
    evalEquals("LPad('test',7,'ABC')", "ABCtest");
    evalEquals("LPad('test',8,'ABC')", "ABCAtest");

    evalEquals("LPad(BINARY '1A2B3C',2,BINARY '4D5E6F')", new byte[] {0x1A, 0x2B})
        .returnType(BinaryType.BINARY_NOT_NULL);
    evalEquals("LPad(BINARY '1A2B3C',3,BINARY '4D5E6F')", new byte[] {0x1A, 0x2B, 0x3C});
    evalEquals("LPad(BINARY '1A2B3C',4,BINARY '4D5E6F')", new byte[] {0x4D, 0x1A, 0x2B, 0x3C});
    evalEquals(
        "LPad(BINARY '1A2B3C',5,BINARY '4D5E6F')", new byte[] {0x4D, 0x5E, 0x1A, 0x2B, 0x3C});
    evalEquals(
        "LPad(BINARY '1A2B3C',6,BINARY '4D5E6F')", new byte[] {0x4D, 0x5E, 0x6F, 0x1A, 0x2B, 0x3C});
    evalEquals(
        "LPad(BINARY '1A2B3C',7,BINARY '4D5E6F')",
        new byte[] {0x4D, 0x5E, 0x6F, 0x4D, 0x1A, 0x2B, 0x3C});

    // Empty padding
    evalEquals("LPad('test',5,'')", "test");
    evalEquals("LPad(BINARY '2A3B4C',5,BINARY '')", new byte[] {0x2A, 0x3B, 0x4C});

    // If length is a negative number, the result of the function is an empty string or binary.
    evalEquals("LPad('test',-8)", "");

    // Null handling
    evalNull("LPad(NULL_STRING,2)").returnType(StringType.STRING);
    evalNull("LPad(NULL_STRING,-8)");
    evalNull("LPad(NULL_BINARY,2)").returnType(BinaryType.BINARY);
    evalNull("LPad(NULL_BINARY,-8)");

    // Missing arguments
    evalFails("LPad('test')", ErrorCode.NOT_ENOUGH_ARGUMENT);

    // Check padding length exceeds the maximum limit
    evalFails("LPad('test',10000,'t')", ErrorCode.PADDING_LENGTH_EXCEEDS_MAXIMUM_LIMIT);
    evalFails(
        "LPad(FIELD_BINARY,10000, FIELD_BINARY)", ErrorCode.PADDING_LENGTH_EXCEEDS_MAXIMUM_LIMIT);
  }

  @Test
  void Lower() throws Exception {
    evalEquals("Lower('TesT')", "test").returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("Lower(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("Lower()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Lower('Test','Test')", ErrorCode.TOO_MANY_ARGUMENT);

    // Function repetition
    optimize("LOWER(LOWER(FIELD_STRING))", "LOWER(FIELD_STRING)");
    optimize("LOWER(UPPER(FIELD_STRING))", "LOWER(FIELD_STRING)");
    optimize("LOWER(INITCAP(FIELD_STRING))", "LOWER(FIELD_STRING)");
  }

  @Test
  void Squeeze() throws Exception {
    evalEquals("SQUEEZE('   Tes T      ')", "Tes T").returnType(StringType.STRING_NOT_NULL);
    evalEquals("SQUEEZE('')", "");
    evalEquals("SQUEEZE('  ')", "");
    evalEquals("SQUEEZE(' T')", "T");
    evalEquals("SQUEEZE('T ')", "T");
    evalEquals("SQUEEZE(' T  es T ')", "T es T");
    evalEquals("SQUEEZE('T\t es T ')", "T es T");
    evalEquals("SQUEEZE('T \t es T')", "T es T");
    evalEquals("SQUEEZE('T \t es T\n\r')", "T es T");

    // Null handling
    evalNull("SQUEEZE(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("SQUEEZE()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("SQUEEZE('Test','Test')", ErrorCode.TOO_MANY_ARGUMENT);

    // Function repetition
    optimize("SQUEEZE(Squeeze(FIELD_STRING))", "SQUEEZE(FIELD_STRING)");
  }

  @Test
  void Substring() throws Exception {
    evalEquals("Substring('TEST FROM',6)", "FROM").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Substring('TEST FROM',6,2)", "FR");
    evalEquals("Substring('TEST FROM',1,4)", "TEST");
    evalEquals("Substring('TEST FROM',-4)", "FROM");
    evalEquals("Substring('ABCDEFG',1,1)", "A");

    // TODO: is this really useful ?
    // System.setProperty(Const.HOP_EMPTY_STRING_DIFFERS_FROM_NULL, "Y");
    // evalEquals("Substring('TEST',1,0)","").returnType(StringType.STRING);
    // System.setProperty(Const.HOP_EMPTY_STRING_DIFFERS_FROM_NULL, "N");
    evalNull("Substring('TEST',1,0)").returnType(StringType.STRING);

    // Null handling
    evalNull("Substring(NULL_STRING,1,1)").returnType(StringType.STRING);

    // Compatibility mode
    evalEquals("Substring('ABCDEFG',0,1)", "A");

    // Alias
    evalEquals("Substr('TEST',5)", "");
  }

  @Test
  void Split_part() throws Exception {
    evalEquals("Split_Part('127.1.2.3','.',1)", "127").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Split_Part('127.1.2.3','.',2)", "1");
    evalEquals("Split_Part('127.1.2.3','.',4)", "3");
    evalEquals("Split_Part('127.1.2.3','.',-1)", "3");
    evalEquals("Split_Part('127.1.2.3','.',-2)", "2");
    evalEquals("SPLIT_PART('user@hop.apache.org', '.', -2)", "apache");
    evalEquals("Split_Part('AAA-@-BBB-BBB-@-CCC','-@-',2)", "BBB-BBB");

    // No split if delimiter is empty
    evalEquals("Split_Part('127.1.2.3','',1)", "127.1.2.3");

    // If the part index is out of range, the returned value is an empty string.
    evalEquals("Split_Part('127.1.2.3','.',5)", "");

    // Null handling
    evalNull("Split_Part(NULL_STRING,'.',5)");
    evalNull("Split_Part('127.1.2.3',NULL_STRING,5)");
    evalNull("Split_Part('127.1.2.3','.',NULL_INTEGER)");

    // Check operands
    evalFails("Split_Part('127.1.2.3','.')", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void Strtok() throws Exception {
    evalEquals("Strtok('127.1-2-3','.-:',1)", "127").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Strtok('127.1-2-3','.-:',2)", "1");
    evalEquals("Strtok('127.1-2-3','.-:',4)", "3");
    evalEquals("Strtok('127.1-2-3','.-:',-1)", "3");
    evalEquals("Strtok('127.1-2-3','.-:',-2)", "2");
    evalEquals("Strtok('AAA.BBB:CCC-DDD','.-:',2)", "BBB");

    // Optional arguments
    evalEquals("Strtok('AAA BBB CCC DDD')", "AAA");
    evalEquals("Strtok('AAA BBB CCC DDD',3)", "CCC");
    evalEquals("Strtok('AAA:BBB-CCC DDD','-: ')", "AAA");

    // If the string starts or is terminated with the delimiter
    evalEquals("Strtok(' AAA:BBB-CCC DDD', 1)", "");
    evalEquals("Strtok('AAA:BBB-CCC DDD ', 3)", "");

    // Adjacent delimiters are treated as delimiters for empty tokens.
    evalEquals("Strtok('AAA  BBB CCC', 3)", "BBB");

    // No split if delimiter is empty
    evalEquals("Strtok('127.1.2.3','',1)", "127.1.2.3");

    // If the part index is out of range, the returned value is null.
    evalNull("Strtok('127.1.2.3','.',5)").returnType(StringType.STRING);
    evalNull("Strtok('','',1)");

    // If one operand is null
    evalNull("Strtok(NULL_STRING,'.',5)").returnType(StringType.STRING);
    evalNull("Strtok('127.1.2.3',NULL_STRING,5)").returnType(StringType.STRING);
    evalNull("Strtok('127.1.2.3','.',NULL_INTEGER)").returnType(StringType.STRING);

    // Check operands
    evalFails("Strtok()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Strtok('127.1.2.3','.',5,5)", ErrorCode.TOO_MANY_ARGUMENT);
  }

  @Test
  void Space() throws Exception {
    evalEquals("Space(4)", "    ").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Space(0)", "");

    // Null handling
    evalNull("Space(-3)");
    evalNull("Space(NULL_INTEGER)");

    // Check operands
    evalFails("Space()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Space(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Space(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("Space('str')", ErrorCode.ILLEGAL_ARGUMENT);

    // Simplify
    optimize("SPACE(4)", "'    '");
  }

  @Test
  void Trim() throws Exception {
    evalEquals("Trim('a')", "a").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Trim(' a ')", "a");
    evalEquals("Trim('  a b  ')", "a b");
    evalEquals("Trim('01ABC10 ', '012')", "ABC10 ");
    evalEquals("Trim(' 01ABC10 ', ' 012')", "ABC");

    // Null handling
    evalNull("Trim(NULL_STRING)").returnType(StringType.STRING);
    evalNull("Trim(' 01ABC012 ',NULL_STRING)");

    // Check operands
    evalFails("Trim()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    optimize("Trim(' test ')", "'test'");
    optimizeTrue("Trim(' test ')='test'");

    // Function repetition
    optimize("TRIM(TRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
    optimize("TRIM(LTRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
    optimize("TRIM(RTRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
  }

  @Test
  void LTrim() throws Exception {
    evalEquals("LTrim('a')", "a").returnType(StringType.STRING_NOT_NULL);
    evalEquals("LTrim(' a ')", "a ");
    evalEquals("LTrim('01ABC012', '012')", "ABC012");
    evalNull("LTrim(NULL_STRING)").returnType(StringType.STRING);
    evalNull("LTrim('01ABC012',NULL_STRING)");

    // Check operands
    evalFails("LTrim()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    // Function repetition
    optimize("LTRIM(LTRIM(FIELD_STRING))", "LTRIM(FIELD_STRING)");
    optimize("LTRIM(TRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
  }

  @Test
  void RTrim() throws Exception {
    evalEquals("RTrim('a')", "a").returnType(StringType.STRING_NOT_NULL);
    evalEquals("RTrim(' a ')", " a");
    evalEquals("RTrim('012ABC10', '012')", "012ABC");

    // Null handling
    evalNull("RTrim(NULL_STRING)").returnType(StringType.STRING);
    evalNull("RTrim('01ABC012',NULL_STRING)");

    // Check operands
    evalFails("RTrim()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    // Function repetition
    optimize("RTRIM(RTRIM(FIELD_STRING))", "RTRIM(FIELD_STRING)");
    optimize("RTRIM(TRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
  }

  @Test
  void Length() throws Exception {
    // String
    evalEquals("Length('TEST')", 4L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Len('TEST')", 4L).returnType(IntegerType.INTEGER_NOT_NULL);

    // Binary
    evalEquals("Length(BINARY 'F0FA')", 2L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Len(BINARY 'F0FA')", 2L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Length(BINARY '0F0FA')", 3L).returnType(IntegerType.INTEGER_NOT_NULL);

    // Null handling
    evalNull("Length(NULL_STRING)").returnType(IntegerType.INTEGER);
    evalNull("Length(NULL_BINARY)");

    // Implicit conversion from Boolean to String
    evalEquals("Length(true)", 4L);
    evalEquals("Length(FALSE)", 5L);

    // Implicit conversion from Integer to String
    evalEquals("Length(5)", 1L);
    evalEquals("Length(-5)", 2L);

    // Implicit conversion from Number to String
    evalEquals("Length(-5.3)", 4L);

    // Implicit conversion from Date to String
    evalEquals("Length(Date '2023-01-01')", 10L);

    // Check operands
    evalFails("Length()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Length('test','z')", ErrorCode.TOO_MANY_ARGUMENT);
  }

  @Test
  void Left() throws Exception {
    // String
    evalEquals("Left('TEST FROM',4)", "TEST").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Left('',1)", "");
    evalEquals("Left('TEST',10)", "TEST");
    evalEquals("Left('TEST',-1)", "");

    // Binary
    evalEquals("Left(BINARY '12345678', 4)", new byte[] {0x12, 0x34, 0x56, 0x78})
        .returnType(BinaryType.BINARY_NOT_NULL);
    evalEquals("Left(BINARY '12345678', 2)", new byte[] {0x12, 0x34});
    evalEquals("Left(BINARY '12345678', -2)", new byte[] {});

    // Null handling
    evalNull("Left(NULL_STRING,4)");
    evalNull("Left(FIELD_STRING,NULL_INTEGER)");
    evalNull("Left(NULL_BINARY,4)");
    evalNull("Left(FIELD_BINARY,NULL_INTEGER)");

    // Coercion to String
    evalEquals("Left(FIELD_BOOLEAN_TRUE,1)", "T");
    evalEquals("Left(FIELD_DATE,4)", "1981");

    // Check operands
    evalFails("Left('Test')", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Left('TEST',10,0)", ErrorCode.TOO_MANY_ARGUMENT);
  }

  @Test
  void Insert() throws Exception {
    // String
    evalEquals("Insert('abcd', 1, 0, 'QW')", "QWabcd").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Insert('abcd', 2, 1, 'QW')", "aQWcd");
    evalEquals("Insert('abcd', 2, 2, 'QW')", "aQWd");
    evalEquals("Insert('abcd', 5, 0, 'QW')", "abcdQW");

    // evalEquals("Insert('abcdefg', 1, 9, 'zy')", "zy");

    // Null handling
    evalNull("Insert(NULL_STRING, 2, 1, 'qw')");
    evalNull("Insert('abcd', NULL_INTEGER, 1, 'qw')");
    evalNull("Insert('abcd', 2, NULL_INTEGER, 'qw')");
    evalNull("Insert('abcd', 2, 1, NULL_STRING)");

    // Binary
    evalEquals("Insert(BINARY '1234', 1, 0, BINARY '56')", new byte[] {0x56, 0x12, 0x34})
        .returnType(BinaryType.BINARY_NOT_NULL);
    evalEquals("Insert(BINARY '1234', 2, 0, BINARY '56')", new byte[] {0x12, 0x56, 0x34});
    evalEquals("Insert(BINARY '1234', 3, 0, BINARY '56')", new byte[] {0x12, 0x34, 0x56});
    evalEquals("Insert(BINARY '1234', 1, 1, BINARY '56')", new byte[] {0x56, 0x34});
    evalNull("Insert(NULL_BINARY, 1, 0, BINARY '56')");

    // Check operands
    evalFails("Insert()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Insert(BINARY '1234', 0, 0, BINARY '56')", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Insert(BINARY '1234', 4, 0, BINARY '56')", ErrorCode.ARGUMENT_OUT_OF_RANGE);
  }

  @Test
  void Right() throws Exception {
    // String
    evalEquals("Right('TEST FROM',4)", "FROM").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Right('',1)", "");
    evalEquals("Right('TEST',10)", "TEST");
    evalEquals("Right('TEST',-1)", "");

    // Null handling
    evalNull("Right(NULL_STRING,4)");
    evalNull("Right('TEST',NULL_INTEGER)");

    // Binary
    evalEquals("Right(BINARY '12345678', 2)", new byte[] {0x56, 0x78})
        .returnType(BinaryType.BINARY_NOT_NULL);
    evalEquals("Right(BINARY '12345678', 4)", new byte[] {0x12, 0x34, 0x56, 0x78});
    evalEquals("Right(BINARY '12345678', -2)", new byte[] {});

    evalNull("Right(NULL_BINARY,4)");
    evalNull("Right(FIELD_BINARY,NULL_INTEGER)");
    evalNull("Right(FIELD_BINARY,NULL_INTEGER)");

    // Coercion to String
    evalEquals("Right(FIELD_BOOLEAN_TRUE,1)", "E");
    evalEquals("Right(FIELD_DATE,8)", "21:44:58");

    // Check operands
    evalFails("Right()", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void Repeat() throws Exception {
    // String
    evalEquals("Repeat('ABC',0)", "").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Repeat('ABC',1)", "ABC").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Repeat('ABC',2)", "ABCABC").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Repeat('ABC',3)", "ABCABCABC").returnType(StringType.STRING_NOT_NULL);

    // Binary
    evalEquals("Repeat(BINARY '1234',0)", new byte[] {}).returnType(BinaryType.BINARY_NOT_NULL);
    evalEquals("Repeat(BINARY '1234',1)", new byte[] {0x12, 0x34})
        .returnType(BinaryType.BINARY_NOT_NULL);
    evalEquals("Repeat(BINARY '1234',2)", new byte[] {0x12, 0x34, 0x12, 0x34});
    evalEquals("Repeat(BINARY '1234',3)", new byte[] {0x12, 0x34, 0x12, 0x34, 0x12, 0x34});

    // Null handling
    evalNull("Repeat(NULL_STRING,2)").returnType(StringType.STRING);
    evalNull("Repeat('ABC',NULL_INTEGER)");
    evalNull("Repeat(NULL_BINARY,2)").returnType(BinaryType.BINARY);
    evalNull("Repeat(FIELD_BINARY,NULL_INTEGER)");

    // Check operands
    evalFails("Repeat()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    // Result size to large
    evalFails("Repeat('ABC',9999999999999)", ErrorCode.RESULT_SIZE_TOO_LARGE);
    evalFails("Repeat(BINARY '1234',9999999999999)", ErrorCode.RESULT_SIZE_TOO_LARGE);
  }

  @Test
  void Replace() throws Exception {
    evalEquals("Replace('ABCD','CD')", "AB").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Replace('ABCDEFCD','CD','EF')", "ABEFEFEF");

    // Null handling
    evalNull("Replace(NULL_STRING,'CD','EF')");
    evalNull("Replace('ABCD',NULL_STRING,'EF')");

    // Check operands
    evalFails("Replace()", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void HexEncode() throws Exception {
    evalEquals("HEX_ENCODE('Apache Hop')", "41706163686520486f70");

    // Null handling
    evalNull("HEX_ENCODE(NULL_STRING)");

    // Check operands
    evalFails("HEX_ENCODE()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("HEX_ENCODE(0x01,0x02)", ErrorCode.TOO_MANY_ARGUMENT);
  }

  @Test
  void HexDecode() throws Exception {
    evalEquals("HEX_DECODE('41706163686520486f70')", "Apache Hop");

    evalNull("HEX_DECODE(NULL_STRING)");

    // Check operands
    evalFails("HEX_DECODE()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("HEX_DECODE('p1','p2')", ErrorCode.TOO_MANY_ARGUMENT);
  }

  @Test
  void Overlay() throws Exception {
    evalEquals("Overlay('Apxxxe','ach',3)", "Apache").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Overlay('Apxxxe','ache Hop',3)", "Apache Hop");
    evalEquals("Overlay('Apxxxe','ach',5)", "Apxxach");

    // Empty value
    evalEquals("Overlay('','Apache',3)", "Apache");
    evalEquals("Overlay('','Apache',3,0)", "Apache");
    evalEquals("Overlay('','Apache',3,20)", "Apache");

    // Empty replace
    evalEquals("Overlay('Apache','',2)", "Apache");
    evalEquals("Overlay('Apache','',2,3)", "Ahe");
    evalEquals("Overlay('Apache','',2,10)", "A");

    // Number of characters to replace
    evalEquals("Overlay('Apxxxe','ach',3,2)", "Apacxe");

    // Exact replacement length
    evalEquals("Overlay('Apxxxe','ach',3,3)", "Apache");
    evalEquals("Overlay('Apxxxe','ache',3,4)", "Apache");

    // More characters to replace than available in replacing String
    evalEquals("Overlay('Apxxxe','ach',3,4)", "Apach");
    evalEquals("Overlay('Apxxxe','pl',3,3)", "Apple");

    // Null handling
    evalNull("Overlay(NULL_STRING,'ach',3)").returnType(StringType.STRING);
    evalNull("Overlay(FIELD_STRING,NULL_STRING,3)").returnType(StringType.STRING);

    // Check operands
    evalFails("Overlay()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Overlay(FIELD_DATE)", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void Reverse() throws Exception {
    evalEquals("Reverse('Hello, world!')", "!dlrow ,olleH").returnType(StringType.of(13, false));
    evalEquals("Reverse(BINARY '2A3B4C')", new byte[] {0x4C, 0x3B, 0x2A})
        .returnType(BinaryType.of(3, false));

    // Null handling
    evalNull("Reverse(NULL_STRING)").returnType(StringType.STRING);
    evalNull("Reverse(NULL_BINARY)").returnType(BinaryType.BINARY);

    // Check operands
    evalFails("Reverse()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Reverse('str','bad')", ErrorCode.TOO_MANY_ARGUMENT);
  }

  @Test
  void Soundex() throws Exception {
    evalEquals("Soundex('Wikipedia')", "W213").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Soundex('I LOVE ROCKS.')", "I416");
    evalEquals("Soundex('I LOVE ROCK AND ROLL MUSIC.')", "I416");
    evalEquals("Soundex('123456')", "");

    // Null handling
    evalNull("Soundex(NULL_STRING)").returnType(StringType.STRING);

    // Coercion to string
    evalEquals("Soundex(FIELD_DATE)", "");

    // Check operands
    evalFails("Soundex()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Soundex('str','bad')", ErrorCode.TOO_MANY_ARGUMENT);
  }

  @Test
  void Difference() throws Exception {
    evalEquals("Difference('Perfect', 'Perfect')", 4L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Difference('Juice', 'Jucy')", 4L);
    evalEquals("Difference('Apple', 'Orange')", 0L);

    // Null handling
    evalNull("Difference(NULL_STRING,NULL_STRING)");
    evalNull("Difference(NULL_STRING,'Jucy')");
    evalNull("Difference('Juice',NULL_STRING)");

    evalFails("Difference()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Difference('str')", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Difference('str','bad','xx')", ErrorCode.TOO_MANY_ARGUMENT);
  }

  @Test
  void Levenshtein() throws Exception {
    evalEquals("Levenshtein('Superman', 'Superman')", 0L);
    evalEquals("Levenshtein('kitten', 'sitting')", 3L);

    // Null handling
    evalNull("Levenshtein(NULL_STRING,NULL_STRING)");
    evalNull("Levenshtein(NULL_STRING,'Superman')");
    evalNull("Levenshtein('Superman',NULL_STRING)");
  }

  @Test
  void JaroWinkler() throws Exception {
    evalEquals("JaroWinkler('Superman', 'Superman')", 100L);
    evalEquals("JaroWinkler('Peter Parker', 'Pete Parker')", 88L);
    evalEquals("JaroWinkler('elephant', 'hippo')", 44L);
    evalEquals("JaroWinkler('święta', 'swieta')", 78L);
    evalEquals("JaroWinkler('Ich weiß nicht', 'Ich weiss nicht')", 93L);

    // Null handling
    evalNull("JaroWinkler(NULL_STRING,NULL_STRING)");
    evalNull("JaroWinkler(NULL_STRING,'Superman')");
    evalNull("JaroWinkler('Superman',NULL_STRING)");
  }

  @Test
  void Translate() throws Exception {
    evalEquals("Translate('Hello, world!','eo','EO')", "HEllO, wOrld!");
    evalEquals("Translate('Hello, wOrld!','eol', 'E')", "HE, wOrd!");
    evalEquals("Translate('Hello, world!','oel,! ', '')", "Hwrd");
    evalEquals("Translate('Hello, world!','eo',NULL_STRING)", "Hll, wrld!");

    // Null handling
    evalNull("Translate(NULL_STRING,'eo','EO')");
    evalNull("Translate('Hello, world!',NULL_STRING,'EO')");
  }

  @Test
  void Regexp_Like() throws Exception {
    evalTrue("Regexp_Like('aaa','a{2,4}')").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalTrue("Regexp_Like('Erdbeere','Erd[a[:SPACE:]b]eere')");
    evalTrue("Regexp_Like('12345TEST','123[:alnum:]*')");
    evalTrue("Regexp_Like('ABcdf987','[:xdigit:]*')");
    evalTrue("Regexp_Like('ABcdf987','[:xdigit:]*')");
    evalTrue("Regexp_Like('A','[a-z]','i')");
    evalFalse("Regexp_Like('A','[a-z]','c')");

    // Null handling
    evalNull("Regexp_Like(NULL_STRING,'A')").returnType(BooleanType.BOOLEAN);
    evalNull("Regexp_Like('A', NULL_STRING)").returnType(BooleanType.BOOLEAN);

    // An empty pattern '' matches nothing
    evalFalse("Regexp_Like('','')");
    evalFalse("Regexp_Like('ABC','')");

    // Check operands
    evalFails("Regexp_Like()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Regexp_Like('A')", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Regexp_Like('A','[a-z]','z')", ErrorCode.INVALID_ARGUMENT);
  }

  @Test
  void Regexp_Replace() throws Exception {
    evalEquals("Regexp_Replace('A1.2.3.4','[^0-9]')", "1234")
        .returnType(StringType.STRING_NOT_NULL);
    evalEquals("Regexp_Replace('A1.2.3.4','[^0-9]', '', 1, 0)", "1234");
    evalEquals("Regexp_Replace('ABC, ABC, ABC','ABC', 'EFG', 1, 2)", "ABC, EFG, ABC");
    evalEquals("Regexp_Replace('AAA BBB CCC', '[:space:]+', '-')", "AAA BBB CCC");
    evalEquals("Regexp_Replace('expressssion', 's+','ss')", "expression");
    evalEquals(
        "Regexp_Replace('This line    contains    more      than one   spacing      between      words', '( ){2,}', ' ')",
        "This line contains more than one spacing between words");
    evalEquals("Regexp_Replace('ABCEFG', 'A..','WXYZ')", "WXYZEFG");
    evalEquals("Regexp_Replace('ABCEFG', '[A-Z]','',1,1)", "BCEFG");
    evalEquals("Regexp_Replace('abc', '(b|c)', 'X')", "aXX");
    evalEquals("Regexp_Replace('abc', '(.*)c', '\\1e')", "abe");
    evalEquals("Regexp_Replace('abc', '(a)(b)', '\\2\\1')", "bac");

    // An empty pattern matches nothing
    evalEquals("Regexp_Replace('ABCDEEEEEEFG', '','E')", "ABCDEEEEEEFG");

    // Back reference
    evalEquals(
        "Regexp_Replace('FIRSTNAME MIDDLENAME LASTNAME','(.*) (.*) (.*)','\\3, \\1 \\2')",
        "LASTNAME, FIRSTNAME MIDDLENAME");

    // Null handling
    evalNull("Regexp_Replace(NULL_STRING,'A')").returnType(StringType.STRING);
    evalNull("Regexp_Replace('A', NULL_STRING)");

    // Check operands
    evalFails("Regexp_Replace()", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void Regexp_Count() throws Exception {
    evalEquals("Regexp_Count('An apple costs 50 cents, a banana costs 10 cents.', '\\d+')", 2L)
        .returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Regexp_Count('An apple costs 50 cents, a banana costs 10 cents.', '\\d+', 20)", 1L);
    evalEquals(
        "Regexp_Count('An apple costs 50 cents, a banana costs 10 cents.', 'CENTS', 1, 'i')", 2L);
  }

  @Test
  void Regexp_Instr() throws Exception {
    evalEquals("Regexp_Instr('email@apache.org', '@[^.]*')", 6L)
        .returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Regexp_Instr('hello to YOU', '(.o).', 1, 3, 1,'i')", 13L);
    evalEquals(
        "Regexp_Instr('REGEXP_INSTR is an advanced extension of the INSTR function','[:a-z]{3,8}', 3, 2, 1)",
        37L);

    // An empty pattern matches nothing
    evalEquals("Regexp_Instr('email@apache.org', '')", 0L);
  }

  @Test
  void Regexp_Substr() throws Exception {
    evalEquals("regexp_substr('email@apache.org', '@[^.]*')", "@apache")
        .returnType(StringType.STRING_NOT_NULL);
    evalEquals(
        "regexp_substr('This is a regexp_substr demo', '[a-zA-Z0-9_]+', 1, 4)", "regexp_substr");

    evalEquals(
        "regexp_substr('It was the best of times, it was the worst of times.', 'the\\W+\\w+', 1, 1)",
        "the best");
    evalEquals(
        "regexp_substr('It was the best of times, it was the worst of times.', 'the\\W+\\w+', 1, 2)",
        "the worst");

    evalNull("regexp_substr('abc', 'z')");
    evalEquals("regexp_substr('abc', '.b.', 0)", "abc");
    evalNull("regexp_substr('abc', '.b.', 1)");
    // evalEquals("regexp_substr('abc', '([a-z])(b)', 1)", "a");
    // evalEquals("regexp_substr('abc', '([a-z])(b)', 2)", "b");

    // An empty pattern matches nothing
    evalNull("regexp_substr('email@apache.org', '')");

    evalNull("regexp_substr(NULL_STRING,  '@[^.]*')");
    evalNull("regexp_substr('email@apache.org', NULL_STRING)");
  }

  @Test
  void EqualNull() throws Exception {
    evalFalse("Equal_Null(1,NULL_INTEGER)").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalTrue("Equal_Null(NULL_STRING,NULL_STRING)").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalTrue("Equal_Null(NULL_INTEGER,NULL_NUMBER)");
    evalTrue("Equal_Null(DATE '2019-01-01',DATE '2019-01-01')");
    evalFalse("Equal_Null(DATE '2019-01-01',DATE '2018-01-01')")
        .returnType(BooleanType.BOOLEAN_NOT_NULL);

    evalFails("Equal_Null()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Equal_Null(NULL_INTEGER)", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Equal_Null(NULL_INTEGER, NULL_NUMBER, NULL_INTEGER)", ErrorCode.TOO_MANY_ARGUMENT);

    // Same operands always true
    optimizeTrue("EQUAL_NULL(NULL_STRING, NULL_STRING)");
    optimizeTrue("EQUAL_NULL(FIELD_STRING, FIELD_STRING)");
    optimizeTrue("EQUAL_NULL(FIELD_INTEGER, FIELD_INTEGER)");
    optimizeTrue("EQUAL_NULL(FIELD_DATE, FIELD_DATE)");

    optimize("EQUAL_NULL(FIELD_INTEGER,NULLIF(1,1))", "FIELD_INTEGER IS NULL");
    optimize("EQUAL_NULL(NULLIF(1,1),FIELD_INTEGER)", "FIELD_INTEGER IS NULL");
    // optimize("EQUAL_NULL(NULLIF(1,1),1+FIELD_INTEGER)", "(1+FIELD_INTEGER) IS NULL");
    optimize("EQUAL_NULL(TRUE,FIELD_BOOLEAN_TRUE)", "FIELD_BOOLEAN_TRUE IS TRUE");
    optimize("EQUAL_NULL(FIELD_BOOLEAN_TRUE,TRUE)", "FIELD_BOOLEAN_TRUE IS TRUE");
    optimize("EQUAL_NULL(FALSE,FIELD_BOOLEAN_TRUE)", "FIELD_BOOLEAN_TRUE IS FALSE");
    optimize("EQUAL_NULL(FIELD_BOOLEAN_TRUE,FALSE)", "FIELD_BOOLEAN_TRUE IS FALSE");
  }

  @Test
  void Concat() throws Exception {
    // String
    evalEquals("CONCAT('TES','T')", "TEST").returnType(StringType.of(4, false));
    evalEquals("FIELD_STRING||'t'", "TESTt").returnType(StringType.of(1001, false));
    evalTrue("FIELD_STRING='TES'||'T'");
    evalTrue("FIELD_STRING='TES'||NULL_STRING||'T'");
    evalEquals("Concat(NULL_STRING,'a')", "a").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Concat('a',NULL_STRING)", "a").returnType(StringType.STRING_NOT_NULL);

    evalEquals(
            "concat(cast('a' as string(2)), cast('b' as string(3)),cast('c' as string(2)))", "abc")
        .returnType(StringType.of(7, false));
    evalNull("NULL_STRING||NULL_STRING").returnType(StringType.STRING);

    // Binary
    evalEquals("Concat(BINARY '1F',BINARY '2A3B')", new byte[] {0x1F, 0x2A, 0x3B})
        .returnType(BinaryType.of(3, false));
    evalEquals("BINARY '1F' || NULL_BINARY || BINARY '2A3B'", new byte[] {0x1F, 0x2A, 0x3B})
        .returnType(BinaryType.BINARY_NOT_NULL);
    evalEquals("NULL_BINARY || BINARY '1F' || BINARY '2A3B'", new byte[] {0x1F, 0x2A, 0x3B})
        .returnType(BinaryType.BINARY_NOT_NULL);
    evalEquals("BINARY '1F' || BINARY '2A3B' || NULL_BINARY", new byte[] {0x1F, 0x2A, 0x3B})
        .returnType(BinaryType.BINARY_NOT_NULL);
    evalNull("Concat(NULL_BINARY,NULL_BINARY)").returnType(BinaryType.BINARY);

    evalFails("Concat()", ErrorCode.NOT_ENOUGH_ARGUMENT);

    // Coercion to string
    evalEquals("4 || 2", "42").returnType(StringType.of(2, false));
    evalEquals("4 || '2'", "42").returnType(StringType.of(2, false));
    evalEquals("Concat(FIELD_STRING, FIELD_INTEGER)", "TEST40");
    evalEquals("Concat(FIELD_INET, FIELD_STRING)", "10.10.10.1TEST");

    // TODO:Mix String and Binary

    // Array to array concatenation
    evalEquals("CARDINALITY([1,2,3] || [4,5])", 5L);
    optimize("[1,2,3] || [4,5] || [6,7]", "[1,2,3,4,5,6,7]");

    // optimize("[1,2,3] || [[4,5,6],[7,8,9]]", "[[1,2,3],[4,5,6],[7,8,9]]");

    // TODO: Element to array concatenation
    // optimize("1 || [2,3,4]", "[1,2,3,4]");
    // TODO: Array to element concatenation
    // optimize("[1,2,3] || 4", "[1,2,3,4]");

    // Check syntax
    evalFails("||'text'", ErrorCode.SYNTAX_ERROR);
    evalFails("'text'||", ErrorCode.SYNTAX_ERROR);

    // Literal
    optimize("CONCAT('TES','T')", "'TEST'");
    optimize("'A'||'B'", "'AB'");
    optimize("12||'34'", "'1234'");

    // Combine and flatten concats (Same syntax but cost reduced)
    optimize("'A'||FIELD_STRING||FIELD_STRING||'C'", "'A'||FIELD_STRING||FIELD_STRING||'C'");
    optimize("'A'||FIELD_STRING||NULL_STRING||'C'", "'A'||FIELD_STRING||NULL_STRING||'C'");
    optimize(
        "CONCAT('A',CONCAT(FIELD_STRING,CONCAT(FIELD_STRING,'C')||'D'))",
        "'A'||FIELD_STRING||FIELD_STRING||'C'||'D'");
  }

  @Test
  void ConcatWs() throws Exception {

    // String
    evalEquals("CONCAT_WS(',','ONE','TWO','THREE')", "ONE,TWO,THREE");
    evalEquals("CONCAT_WS('---','b','c')", "b---c").returnType(StringType.of(5, false));
    evalEquals("CONCAT_WS('--','one')", "one");
    evalEquals("CONCAT_WS(',','a',NULL_STRING,'b')", "a,b");

    // Binary
    evalEquals(
            "CONCAT_WS(BINARY '1F',BINARY '2A3B',BINARY '4D',BINARY '5E')",
            new byte[] {0x2A, 0x3B, 0x1F, 0x4D, 0x1F, 0x5E})
        .returnType(BinaryType.of(6, false));

    // Number
    evalEquals("CONCAT_WS(':',4,2)", "4:2").returnType(StringType.of(3, false));

    // Null handling
    evalNull("CONCAT_WS(NULL_STRING,'FIRST')").returnType(StringType.of(5));
    evalNull("CONCAT_WS('a',NULL_STRING)");
    evalNull("CONCAT_WS(BINARY '1F',NULL_STRING,NULL_STRING)").returnType(BinaryType.BINARY);

    evalFails("CONCAT_WS()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("CONCAT_WS(',')", ErrorCode.NOT_ENOUGH_ARGUMENT);

    // Mix String and Binary
    // TODO: evalFails("CONCAT_WS(FIELD_STRING,0x2A3B)");
  }

  @Test
  void Chr() throws Exception {
    evalEquals("Chr(83)", "S").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Chr(115)", "s");
    evalEquals("Chr(233)", "é");
    evalEquals("Chr(945)", "α");
    evalEquals("Chr(8364)", "€");
    evalEquals("Chr(33288)", "興");

    // Null handling
    evalNull("Chr(NULL_INTEGER)");

    // Check operands
    evalFails("Chr()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Chr('bad')", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("Chr(-1)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Chr(999999999999)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
  }

  @Test
  void Ascii() throws Exception {
    evalEquals("Ascii('ABC')", 65L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Ascii('é')", 233L);
    evalEquals("Ascii('€')", 8364L);
    evalEquals("Ascii('興')", 33288L);
    evalEquals("Ascii('')", 0L);

    // Null handling
    evalNull("Ascii(NULL_STRING)");

    // Check operands
    evalFails("Ascii()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Ascii('a','b')", ErrorCode.TOO_MANY_ARGUMENT);
  }

  @Test
  void Unicode() throws Exception {
    evalEquals("Unicode('SSSS')", 83L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Unicode('é')", 233L);
    evalEquals("Unicode('€')", 8364L);
    evalEquals("Unicode('')", 0L);

    // Null handling
    evalNull("Unicode(NULL_STRING)");

    // Check operands
    evalFails("Unicode()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Unicode('a','b')", ErrorCode.TOO_MANY_ARGUMENT);
  }

  @Test
  void String_To_Array() throws Exception {
    evalEquals(
        "String_To_Array('Green|Yellow|Blue','|')",
        new Array(Literal.of("Green"), Literal.of("Yellow"), Literal.of("Blue")));
    evalEquals("String_To_Array('AB|CD','')", new Array(Literal.of("AB|CD")));

    // Null handling
    evalNull("String_To_Array('Green|Yellow|Blue',NULL_STRING)");
    evalNull("String_To_Array(NULL_STRING,',')");

    // Check operands
    evalFails("String_To_Array()", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void String_Encode() throws Exception {
    evalEquals("String_Encode('\t\r\n\f\b\"')", "\\t\\r\\n\\f\\b\\\"")
        .returnType(StringType.STRING_NOT_NULL);

    // Encode 16 bit unicode
    evalEquals("String_Encode('€')", "\\u20AC");

    // Null handling
    evalNull("String_Encode(NULL_STRING)").returnType(StringType.STRING);
  }

  @Test
  void String_Decode() throws Exception {
    evalEquals("String_Decode('\\t\\r\\n\\f\\b\\\"')", "\t\r\n\f\b\"")
        .returnType(StringType.STRING_NOT_NULL);

    // Decode 16 bits unicode
    evalEquals("String_Decode('\\u20AC')", "€");
    // Decode octal
    evalEquals("String_Decode('\366\344\374')", "öäü");

    // Null handling
    evalNull("String_Decode(NULL_STRING)").returnType(StringType.STRING);
  }

  @Test
  void Html_Encode() throws Exception {
    evalEquals("Html_Encode('18€ & <test> ™')", "18&euro; &amp; &lt;test&gt; &trade;")
        .returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("Html_Encode(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("Html_Encode()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Html_Encode('x','y')", ErrorCode.TOO_MANY_ARGUMENT);
  }

  @Test
  void Html_Decode() throws Exception {
    evalEquals("Html_Decode('18&euro; &amp; &lt;test&gt; &#8482;')", "18€ & <test> ™");

    // Null handling
    evalNull("Html_Decode(NULL_STRING)");

    // Check operands
    evalFails("Html_Decode()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Html_Decode('x','y')", ErrorCode.TOO_MANY_ARGUMENT);
  }

  @Test
  void Url_Encode() throws Exception {
    evalEquals("Url_Encode('a b')", "a+b").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Url_Encode('a+b')", "a%2Bb");
    evalEquals("Url_Encode('âéè')", "%C3%A2%C3%A9%C3%A8");

    // Null handling
    evalNull("Url_Encode(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("Url_Encode()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Url_Encode('x','y')", ErrorCode.TOO_MANY_ARGUMENT);
  }

  @Test
  void Url_Decode() throws Exception {
    evalEquals("Url_Decode('a+b')", "a b").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Url_Decode('a%2Bb')", "a+b");
    evalEquals("Url_Decode('%C3%A2%C3%A9%C3%A8')", "âéè");

    // Null handling
    evalNull("Url_Decode(NULL_STRING)");

    // Check operands
    evalFails("Url_Decode()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Url_Decode('x','y')", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Url_Decode('a%%2Bb')", ErrorCode.CALL_FUNCTION_ERROR);
  }

  @Test
  void Base64_Encode() throws Exception {
    evalEquals("Base64_Encode('Apache Hop')", "QXBhY2hlIEhvcA==")
        .returnType(StringType.STRING_NOT_NULL);
    evalEquals("Base64_Encode('Apache Hop'::Binary)", "QXBhY2hlIEhvcA==")
        .returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("Base64_Encode(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("Base64_Encode()", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void Base64_Decode() throws Exception {
    evalEquals("Base64_Decode('QXBhY2hlIEhvcA==')", "Apache Hop")
        .returnType(StringType.STRING_NOT_NULL);
    evalEquals("Base64_Decode('QXBhY2hlIEhvcA=='::Binary)", "Apache Hop")
        .returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("Base64_Decode(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("Base64_Decode()", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void Base32_Encode() throws Exception {
    evalEquals("Base32_Encode('Apache Hop')", "IFYGCY3IMUQEQ33Q")
        .returnType(StringType.STRING_NOT_NULL);
    evalEquals("Base32_Encode('Apache Hop'::Binary)", "IFYGCY3IMUQEQ33Q")
        .returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("Base32_Encode(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("Base32_Encode()", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void Base32_Decode() throws Exception {
    evalEquals("Base32_Decode('IFYGCY3IMUQEQ33Q')", "Apache Hop")
        .returnType(StringType.STRING_NOT_NULL);
    evalEquals("Base32_Decode('IFYGCY3IMUQEQ33Q'::Binary)", "Apache Hop")
        .returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("Base32_Decode(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("Base32_Decode()", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void Uuid() throws Exception {
    assertFalse(FunctionRegistry.getFunction("UUID").isDeterministic());

    evalEquals("Length(Uuid())", 36L);
    evalEquals("Substr(Uuid(),15,1)", "7");

    optimize("UUID()").returnType(StringType.STRING_NOT_NULL);

    // Check operands
    evalFails("UUID(1)", ErrorCode.TOO_MANY_ARGUMENT);
  }

  @Test
  void Compress() throws Exception {
    evalEquals("Decompress(Compress('Test'::BINARY))::STRING", "Test");

    // Null handling
    evalNull("Compress(NULL_BINARY)").returnType(BinaryType.BINARY);
  }

  @Test
  void Decompress() throws Exception {
    evalEquals("Decompress(Compress('Test'::BINARY))::STRING", "Test");

    // Null handling
    evalNull("Decompress(NULL_BINARY)").returnType(BinaryType.BINARY);
  }

  @Test
  void Position() throws Exception {
    evalEquals("Position('abc' IN 'abcdefgh')", 1L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Position('XYZ' IN 'abcdefgh')", 0L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Position('def' IN 'abcdefgh')", 4L).returnType(IntegerType.INTEGER_NOT_NULL);

    // Null handling
    evalNull("Position(NULL_STRING IN 'abcdefgh')").returnType(IntegerType.INTEGER);
    evalNull("Position('abc' IN NULL_STRING)").returnType(IntegerType.INTEGER);

    // Check operands
    evalFails("Position()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Position('test')", ErrorCode.NOT_ENOUGH_ARGUMENT);

    // Check syntax
    evalFails("Position( ", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("Position('abc' ", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("Position('abc' IN ", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("Position('abc' IN 'abcd'", ErrorCode.MISSING_RIGHT_PARENTHESIS);
    evalFails("Position( IN 'fsd'", ErrorCode.SYNTAX_ERROR);

    optimize("POSITION('abc' IN FIELD_STRING)");
  }
}
