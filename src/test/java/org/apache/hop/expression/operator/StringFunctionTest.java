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
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

import org.apache.hop.expression.Array;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionTest;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
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
    // Documentation example
    evalEquals(
        "PARSE_URL('https://hop.apache.org:80/path?id=1&lang=fr','FILE')", "/path?id=1&lang=fr");
    // An invalid or lower case part, or an invalid URL, returns NULL
    evalNull("PARSE_URL('https://hop.apache.org/path','XXX')");
    evalNull("PARSE_URL('https://hop.apache.org/path','host')");
    evalNull("PARSE_URL('https://hop apache.org/path','HOST')");
    // The key is case-sensitive and only used with the QUERY part
    evalNull("PARSE_URL('https://hop.apache.org/path?id=1','QUERY','ID')");
    evalNull("PARSE_URL('https://hop.apache.org/path?id=1','HOST','id')");
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
    evalEquals("Parse_Url('https://hop.apache.org:80/path?query=1&id=2','QUERY','query')", "1");
    evalEquals("Parse_Url('https://hop.apache.org/path?xid=1&id=2&id=3','QUERY','id')", "2");
    evalEquals("Parse_Url('https://hop.apache.org/path?id=&lang=fr','QUERY','id')", "");
    // The key is not a regular expression
    evalNull("Parse_Url('https://hop.apache.org/path?axb=1','QUERY','a.b')");
    evalEquals("Parse_Url('https://hop.apache.org/path?axb=1&a.b=2','QUERY','a.b')", "2");
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
    evalEquals("LENGTH(NORMALIZE(CHR(234),'NFD'))", 2L);
    evalEquals("NORMALIZE(CHR(64257),'NFKD')", "fi");
    evalEquals("Normalize('\u00ea')", "ê").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Normalize('\u0065\u0302')", "ê");
    evalEquals("Normalize('Jane\u2004Doe', 'NFKC')", "Jane Doe");
    evalEquals("Normalize('Jane\u2006Doe', 'NFKC')", "Jane Doe");
    evalEquals("Normalize('¼', 'NFKC')", "1⁄4");
    evalEquals("Normalize('i⁹', 'NFKC')", "i9");

    // Null handling
    evalNull("Normalize(NULL_STRING)");
    evalNull("Normalize(NULL_STRING, 'NFKC')");
    evalNull("Normalize('ê', NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("Normalize()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
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

    // Keep modifier letters and symbols, they are not accents of a letter
    evalEquals("Unaccent('2^3 `x` ¨ ˜')", "2^3 `x` ¨ ˜");
    evalEquals("Unaccent('ラーメン')", "ラーメン");
    evalEquals("Unaccent('Café ラーメン')", "Cafe ラーメン");

    // Null handling
    evalNull("Unaccent(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("Unaccent()", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Function repetition
    optimize("Unaccent(Unaccent(FIELD_STRING))", "UNACCENT(FIELD_STRING)");
  }

  @Test
  void Upper() throws Exception {
    evalEquals("Upper('test')", "TEST").returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("Upper(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("Upper()", ErrorCode.NOT_ENOUGH_ARGUMENTS);

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
    evalFails("InitCap()", ErrorCode.NOT_ENOUGH_ARGUMENTS);

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
    evalEquals("Instr('CORPORATE FLOOR','O',-2)", 14L);
    evalEquals("Instr('CORPORATE FLOOR','O',-3)", 13L);
    evalEquals("Instr('CORPORATE FLOOR','O',-4)", 5L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-1)", 14L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-3)", 5L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-12)", 2L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-3, 1)", 5L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-3, 2)", 2L);
    evalEquals("Instr('CORPORATE FLOOR','OR',-3, 3)", 0L);

    // Match at the first character, with several occurrences
    evalEquals("Instr('ABAB','AB')", 1L);
    evalEquals("Instr('ABAB','AB',1,2)", 3L);
    evalEquals("Instr('ABAB','AB',1,3)", 0L);
    evalEquals("Instr('ABAB','AB',-1,2)", 1L);
    evalEquals("Instr('ABAB','AB',-1,3)", 0L);
    // Overlapping occurrences are counted
    evalEquals("Instr('AAA','AA',1,2)", 2L);
    evalEquals("Instr('AAA','AA',1,3)", 0L);
    evalEquals("Instr('ABABA','ABA',1,2)", 3L);
    evalEquals("Instr('AAA','AA',-1,1)", 2L);
    evalEquals("Instr('AAA','AA',-1,2)", 1L);
    evalEquals("Instr('ABABA','ABA',-1,2)", 1L);
    // Position at the bounds of the string, -1 is the last character
    evalEquals("Instr('ABC','C',-1)", 3L);
    evalEquals("Instr('ABC','A',-3)", 1L);
    evalEquals("Instr('ABC','A',-4)", 0L);
    evalEquals("Instr('ABC','C',3)", 3L);
    evalEquals("Instr('ABC','A',4)", 0L);
    // A position or an occurrence out of the int range must not wrap
    evalEquals("Instr('ABC','A',4294967297)", 0L);
    evalEquals("Instr('ABC','C',-4294967295)", 0L);
    evalEquals("Instr('ABAB','AB',1,4294967297)", 0L);
    evalFails("Instr('ABAB','AB',1,-4294967295)", ErrorCode.ARGUMENT_OUT_OF_RANGE);

    // Null handling
    evalNull("Instr(NULL_STRING,'test')");
    evalNull("Instr('test',NULL_STRING)");
    evalNull("Instr(NULL_STRING,NULL_STRING)");
    evalNull("Instr('CORPORATE FLOOR','OR',NULL_INTEGER)");
    evalNull("Instr('CORPORATE FLOOR','OR',3,NULL_INTEGER)");

    // Check operands
    evalFails("Instr()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Instr('CORPORATE FLOOR','OR',-3, 0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Instr('CORPORATE FLOOR','OR',0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
  }

  @Test
  void RPad() throws Exception {
    // Documentation example
    evalEquals("RPAD('123',5,'*')", "123**");
    // A NULL pad is the default pad
    evalEquals("RPAD('test',6,NULL_STRING)", "test  ");
    // The maximum length is 8192
    evalEquals("LENGTH(RPAD('a',8192))", 8192L);
    evalFails("RPAD('a',8193)", ErrorCode.PADDING_LENGTH_EXCEEDS_MAXIMUM_LIMIT);
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
    // Truncated even with an empty pad
    evalEquals("RPad('test',3,'')", "tes");
    evalEquals("RPad(BINARY '2A3B4C',2,BINARY '')", new byte[] {0x2A, 0x3B});

    // If length is a negative number, the result of the function is an empty string or binary.
    evalEquals("RPad('test',-8)", "");
    evalEquals("RPad(FIELD_BINARY,-8)", new byte[0]);

    // Null handling
    evalNull("RPad(NULL_STRING,2)").returnType(StringType.STRING);
    evalNull("RPad(NULL_BINARY,2)").returnType(BinaryType.BINARY);
    evalNull("RPad(NULL_STRING,-8)");
    evalNull("RPad(NULL_BINARY,-8)");
    evalNull("RPad('test',NULL_INTEGER)");
    evalNull("RPad('test',NULL_INTEGER,'*')");
    evalNull("RPad(BINARY '1A2B3C',NULL_INTEGER,BINARY '4D5E6F')");

    // Check operands
    evalFails("RPad('test')", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Check padding length exceeds the maximum limit
    evalFails("RPad('test',10000,'t')", ErrorCode.PADDING_LENGTH_EXCEEDS_MAXIMUM_LIMIT);
    evalFails(
        "RPad(FIELD_BINARY,10000, FIELD_BINARY)", ErrorCode.PADDING_LENGTH_EXCEEDS_MAXIMUM_LIMIT);

    // A length out of the int range must not wrap
    evalFails("RPad('test',4294967298)", ErrorCode.PADDING_LENGTH_EXCEEDS_MAXIMUM_LIMIT);
    evalFails("RPad(FIELD_BINARY,4294967298)", ErrorCode.PADDING_LENGTH_EXCEEDS_MAXIMUM_LIMIT);
    evalEquals("RPad('test',-4294967295)", "");
    evalEquals("RPad(FIELD_BINARY,-4294967295)", new byte[0]);
  }

  @Test
  void LPad() throws Exception {
    // Documentation example
    evalEquals("LPAD('123',5,'*')", "**123");
    // A NULL pad is the default pad
    evalEquals("LPAD('test',6,NULL_STRING)", "  test");
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
    // Truncated even with an empty pad
    evalEquals("LPad('test',3,'')", "tes");
    evalEquals("LPad(BINARY '2A3B4C',2,BINARY '')", new byte[] {0x2A, 0x3B});

    // If length is a negative number, the result of the function is an empty string or binary.
    evalEquals("LPad('test',-8)", "");

    // Null handling
    evalNull("LPad(NULL_STRING,2)").returnType(StringType.STRING);
    evalNull("LPad(NULL_STRING,-8)");
    evalNull("LPad(NULL_BINARY,2)").returnType(BinaryType.BINARY);
    evalNull("LPad(NULL_BINARY,-8)");
    evalNull("LPad('test',NULL_INTEGER)");
    evalNull("LPad('test',NULL_INTEGER,'*')");
    evalNull("LPad(BINARY '1A2B3C',NULL_INTEGER,BINARY '4D5E6F')");

    // Missing arguments
    evalFails("LPad('test')", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Check padding length exceeds the maximum limit
    evalFails("LPad('test',10000,'t')", ErrorCode.PADDING_LENGTH_EXCEEDS_MAXIMUM_LIMIT);
    evalFails(
        "LPad(FIELD_BINARY,10000, FIELD_BINARY)", ErrorCode.PADDING_LENGTH_EXCEEDS_MAXIMUM_LIMIT);

    // A length out of the int range must not wrap
    evalFails("LPad('test',4294967298)", ErrorCode.PADDING_LENGTH_EXCEEDS_MAXIMUM_LIMIT);
    evalFails("LPad(FIELD_BINARY,4294967298)", ErrorCode.PADDING_LENGTH_EXCEEDS_MAXIMUM_LIMIT);
    evalEquals("LPad('test',-4294967295)", "");
    evalEquals("LPad(FIELD_BINARY,-4294967295)", new byte[0]);
  }

  @Test
  void Lower() throws Exception {
    evalEquals("Lower('TesT')", "test").returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("Lower(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("Lower()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Lower('Test','Test')", ErrorCode.TOO_MANY_ARGUMENTS);

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
    evalFails("SQUEEZE()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("SQUEEZE('Test','Test')", ErrorCode.TOO_MANY_ARGUMENTS);

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

    // A zero or negative length returns an empty string
    evalEquals("Substring('TEST',1,0)", "").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Substring('TEST',2,-1)", "");

    // Null handling
    evalNull("Substring(NULL_STRING,1,1)").returnType(StringType.STRING);
    evalNull("Substring('TEST',NULL_INTEGER)");
    evalNull("Substring('TEST',NULL_INTEGER,1)");
    evalNull("Substring('TEST',1,NULL_INTEGER)");

    // Compatibility mode
    evalEquals("Substring('ABCDEFG',0,1)", "A");

    // Alias
    evalEquals("Substr('TEST',5)", "");

    // Start out of the string
    evalEquals("Substring('TEST',6)", "");
    evalEquals("Substring('TEST',-4)", "TEST");
    evalEquals("Substring('TEST',-10)", "TEST");
    // Out of the string with a length, an empty string like without length
    evalEquals("Substring('TEST',6,2)", "");
    evalEquals("Substring('TEST',5,1)", "");
    evalEquals("Substring('TEST',-10,2)", "");
    evalEquals("Substring('TEST',-10,8)", "TE");

    // A position or a length out of the int range must not wrap
    evalEquals("Substring('TEST',4294967297)", "");
    evalEquals("Substring('TEST',4294967297,2)", "");
    evalEquals("Substring('TEST',2,4294967296)", "EST");
    evalEquals("Substring('TEST',2,2147483647)", "EST");
  }

  @Test
  void Split_part() throws Exception {
    // Documentation example
    evalEquals("SPLIT_PART('127.0.0.1','.',1)", "127");
    // Part 0 returns an empty string
    evalEquals("SPLIT_PART('a.b','.',0)", "");
    evalEquals("Split_Part('127.1.2.3','.',1)", "127").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Split_Part('127.1.2.3','.',2)", "1");
    evalEquals("Split_Part('127.1.2.3','.',4)", "3");
    evalEquals("Split_Part('127.1.2.3','.',-1)", "3");
    evalEquals("Split_Part('127.1.2.3','.',-2)", "2");
    evalEquals("SPLIT_PART('user@hop.apache.org', '.', -2)", "apache");
    evalEquals("Split_Part('AAA-@-BBB-BBB-@-CCC','-@-',2)", "BBB-BBB");

    // No split if delimiter is empty
    evalEquals("Split_Part('127.1.2.3','',1)", "127.1.2.3");
    evalEquals("Split_Part('hello world','',1)", "hello world");

    // Empty parts between delimiters are kept
    evalEquals("Split_Part('a..b','.',2)", "");
    evalEquals("Split_Part('a..b','.',3)", "b");
    evalEquals("Split_Part('.a.b','.',1)", "");
    evalEquals("Split_Part('.a.b','.',2)", "a");
    evalEquals("Split_Part('a.b.','.',-1)", "");

    // If the part index is out of range, the returned value is an empty string.
    evalEquals("Split_Part('127.1.2.3','.',5)", "");

    // Null handling
    evalNull("Split_Part(NULL_STRING,'.',5)");
    evalNull("Split_Part('127.1.2.3',NULL_STRING,5)");
    evalNull("Split_Part('127.1.2.3','.',NULL_INTEGER)");

    // Check operands
    evalFails("Split_Part('127.1.2.3','.')", ErrorCode.NOT_ENOUGH_ARGUMENTS);
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
    evalFails("Strtok()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Strtok('127.1.2.3','.',5,5)", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Space() throws Exception {
    evalEquals("Space(4)", "    ").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Space(0)", "");

    // Null handling
    evalNull("Space(-3)");
    evalNull("Space(NULL_INTEGER)");

    // Check operands
    evalFails("Space()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Space(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("Space(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Space('str')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);

    // Simplify
    optimize("SPACE(4)", "'    '");
  }

  @Test
  void Trim() throws Exception {
    // All the whitespace characters are removed by default
    evalEquals("TRIM(CHR(9)||' a '||CHR(10))", "a");
    evalEquals("Trim('a')", "a").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Trim(' a ')", "a");
    evalEquals("Trim('  a b  ')", "a b");
    evalEquals("Trim('01ABC10 ', '012')", "ABC10 ");
    evalEquals("Trim(' 01ABC10 ', ' 012')", "ABC");

    // Null handling
    evalNull("Trim(NULL_STRING)").returnType(StringType.STRING);
    evalNull("Trim(' 01ABC012 ',NULL_STRING)");

    // Check operands
    evalFails("Trim()", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    optimize("Trim(' test ')", "'test'");
    optimizeTrue("Trim(' test ')='test'");

    // Function repetition
    optimize("TRIM(TRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
    optimize("TRIM(LTRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
    optimize("TRIM(RTRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
    // Not simplified when the inner function trims specified characters
    optimize("TRIM(TRIM(FIELD_STRING,'T'))", "TRIM(TRIM(FIELD_STRING,'T'))");
    optimize("TRIM(LTRIM(FIELD_STRING,'T'))", "TRIM(LTRIM(FIELD_STRING,'T'))");
    optimize("TRIM(RTRIM(FIELD_STRING,'T'))", "TRIM(RTRIM(FIELD_STRING,'T'))");
    evalEquals("TRIM(TRIM(FIELD_STRING,'T'))", "ES");
  }

  @Test
  void LTrim() throws Exception {
    // All the whitespace characters are removed by default
    evalEquals("LTRIM(CHR(9)||'a')", "a");
    evalEquals("LTrim('a')", "a").returnType(StringType.STRING_NOT_NULL);
    evalEquals("LTrim(' a ')", "a ");
    evalEquals("LTrim('01ABC012', '012')", "ABC012");
    evalNull("LTrim(NULL_STRING)").returnType(StringType.STRING);
    evalNull("LTrim('01ABC012',NULL_STRING)");

    // Check operands
    evalFails("LTrim()", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Function repetition
    optimize("LTRIM(LTRIM(FIELD_STRING))", "LTRIM(FIELD_STRING)");
    optimize("LTRIM(TRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
    optimize("LTRIM(RTRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
    // Not simplified when the inner function trims specified characters
    optimize("LTRIM(LTRIM(FIELD_STRING,'T'))", "LTRIM(LTRIM(FIELD_STRING,'T'))");
    optimize("LTRIM(TRIM(FIELD_STRING,'T'))", "LTRIM(TRIM(FIELD_STRING,'T'))");
    optimize("LTRIM(RTRIM(FIELD_STRING,'T'))", "LTRIM(RTRIM(FIELD_STRING,'T'))");
    evalEquals("LTRIM(RTRIM(FIELD_STRING,'T'))", "TES");
  }

  @Test
  void RTrim() throws Exception {
    // All the whitespace characters are removed by default
    evalEquals("RTRIM('a'||CHR(13)||CHR(10))", "a");
    evalEquals("RTrim('a')", "a").returnType(StringType.STRING_NOT_NULL);
    evalEquals("RTrim(' a ')", " a");
    evalEquals("RTrim('012ABC10', '012')", "012ABC");

    // Null handling
    evalNull("RTrim(NULL_STRING)").returnType(StringType.STRING);
    evalNull("RTrim('01ABC012',NULL_STRING)");

    // Check operands
    evalFails("RTrim()", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Function repetition
    optimize("RTRIM(RTRIM(FIELD_STRING))", "RTRIM(FIELD_STRING)");
    optimize("RTRIM(TRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
    optimize("RTRIM(LTRIM(FIELD_STRING))", "TRIM(FIELD_STRING)");
    // Not simplified when the inner function trims specified characters
    optimize("RTRIM(RTRIM(FIELD_STRING,'T'))", "RTRIM(RTRIM(FIELD_STRING,'T'))");
    optimize("RTRIM(TRIM(FIELD_STRING,'T'))", "RTRIM(TRIM(FIELD_STRING,'T'))");
    optimize("RTRIM(LTRIM(FIELD_STRING,'T'))", "RTRIM(LTRIM(FIELD_STRING,'T'))");
    evalEquals("RTRIM(LTRIM(FIELD_STRING,'T'))", "EST");
  }

  @Test
  void Length() throws Exception {
    // String
    evalEquals("Length('TEST')", 4L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Len('TEST')", 4L).returnType(IntegerType.INTEGER_NOT_NULL);
    // A character outside the Basic Multilingual Plane counts as one character
    evalEquals("Length(CHR(1114111))", 1L);
    evalEquals("Length('a'||CHR(128512)||'b')", 3L);

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
    evalFails("Length()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Length('test','z')", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Left() throws Exception {
    // String
    evalEquals("Left('TEST FROM',4)", "TEST").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Left('',1)", "");
    evalEquals("Left('TEST',10)", "TEST");
    evalEquals("Left('TEST',-1)", "");
    // A length out of the int range must not wrap
    evalEquals("Left('ABC',4294967296)", "ABC");
    evalEquals("Left('ABC',-4294967295)", "");

    // Binary
    evalEquals("Left(BINARY '12345678', 4)", new byte[] {0x12, 0x34, 0x56, 0x78})
        .returnType(BinaryType.BINARY_NOT_NULL);
    evalEquals("Left(BINARY '12345678', 2)", new byte[] {0x12, 0x34});
    evalEquals("Left(BINARY '12345678', -2)", new byte[] {});
    evalEquals("Left(BINARY '12345678', 4294967296)", new byte[] {0x12, 0x34, 0x56, 0x78});

    // Null handling
    evalNull("Left(NULL_STRING,4)");
    evalNull("Left(FIELD_STRING,NULL_INTEGER)");
    evalNull("Left(NULL_BINARY,4)");
    evalNull("Left(FIELD_BINARY,NULL_INTEGER)");

    // Coercion to String
    evalEquals("Left(FIELD_BOOLEAN_TRUE,1)", "T");
    evalEquals("Left(FIELD_DATE,4)", "1981");

    // Check operands
    evalFails("Left('Test')", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Left('TEST',10,0)", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Insert() throws Exception {
    // A negative length or a position out of the string raises an error
    evalFails("INSERT('abcd',2,-1,'QW')", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("INSERT('abcd',0,0,'QW')", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("INSERT('abcd',6,0,'QW')", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    // String
    evalEquals("Insert('abcd', 1, 0, 'QW')", "QWabcd").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Insert('abcd', 2, 1, 'QW')", "aQWcd");
    evalEquals("Insert('abcd', 2, 2, 'QW')", "aQWd");
    evalEquals("Insert('abcd', 5, 0, 'QW')", "abcdQW");

    // Replace the whole string
    evalEquals("Insert('abcdefg', 1, 7, 'zy')", "zy");
    // The length must not exceed the end of the string
    evalFails("Insert('abcdefg', 1, 9, 'zy')", ErrorCode.ARGUMENT_OUT_OF_RANGE);

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
    // The length of bytes replaced does not depend on the length of the inserted value
    evalEquals("Insert(BINARY '123456', 1, 2, BINARY '78')", new byte[] {0x78, 0x56});
    evalEquals("Insert(BINARY '123456', 2, 2, BINARY '78')", new byte[] {0x12, 0x78});
    evalEquals("Insert(BINARY '123456', 1, 3, BINARY '78')", new byte[] {0x78});
    evalEquals(
        "Insert(BINARY '123456', 1, 1, BINARY '789A')", new byte[] {0x78, (byte) 0x9A, 0x34, 0x56});
    evalNull("Insert(NULL_BINARY, 1, 0, BINARY '56')");

    // Check operands
    evalFails("Insert()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Insert(BINARY '1234', 0, 0, BINARY '56')", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Insert(BINARY '1234', 4, 0, BINARY '56')", ErrorCode.ARGUMENT_OUT_OF_RANGE);

    // A position or a length out of the int range must not wrap
    evalFails("Insert('abcd', 4294967297, 0, 'x')", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Insert('abcd', 1, 4294967297, 'x')", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Insert(BINARY '1234', 4294967297, 0, BINARY '56')", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Insert(BINARY '1234', 1, 4294967297, BINARY '56')", ErrorCode.ARGUMENT_OUT_OF_RANGE);
  }

  @Test
  void Right() throws Exception {
    // Documentation example
    evalEquals("RIGHT('ABCDEF',3)", "DEF");
    evalEquals("Right('TEST',0)", "");
    // String
    evalEquals("Right('TEST FROM',4)", "FROM").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Right('',1)", "");
    evalEquals("Right('TEST',10)", "TEST");
    evalEquals("Right('TEST',-1)", "");
    // A length out of the int range must not wrap
    evalEquals("Right('ABC',4294967296)", "ABC");
    evalEquals("Right('ABC',-4294967295)", "");

    // Null handling
    evalNull("Right(NULL_STRING,4)");
    evalNull("Right('TEST',NULL_INTEGER)");

    // Binary
    evalEquals("Right(BINARY '12345678', 2)", new byte[] {0x56, 0x78})
        .returnType(BinaryType.BINARY_NOT_NULL);
    evalEquals("Right(BINARY '12345678', 4)", new byte[] {0x12, 0x34, 0x56, 0x78});
    evalEquals("Right(BINARY '12345678', -2)", new byte[] {});
    evalEquals("Right(BINARY '12345678', 4294967296)", new byte[] {0x12, 0x34, 0x56, 0x78});

    evalNull("Right(NULL_BINARY,4)");
    evalNull("Right(FIELD_BINARY,NULL_INTEGER)");

    // Coercion to String
    evalEquals("Right(FIELD_BOOLEAN_TRUE,1)", "E");
    evalEquals("Right(FIELD_TIMESTAMP,8)", "22:11:01");

    // Check operands
    evalFails("Right()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
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
    evalFails("Repeat()", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Result size to large
    evalFails("Repeat('ABC',9999999999999)", ErrorCode.RESULT_SIZE_TOO_LARGE);
    evalFails("Repeat(BINARY '1234',9999999999999)", ErrorCode.RESULT_SIZE_TOO_LARGE);

    // Negative number of repetitions, same as 0
    evalEquals("Repeat('ABC',-1)", "");
    evalEquals("Repeat(BINARY '1234',-1)", new byte[] {});
  }

  @Test
  void Replace() throws Exception {
    evalEquals("Replace('ABCD','CD')", "AB").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Replace('ABCDEFCD','CD','EF')", "ABEFEFEF");

    // An empty search string matches nothing
    evalEquals("Replace('AB','','X')", "AB");
    evalEquals("Replace('AB','')", "AB");
    evalEquals("Replace('','','X')", "");

    // Null handling
    evalNull("Replace(NULL_STRING,'CD','EF')");
    evalNull("Replace('ABCD',NULL_STRING,'EF')");
    evalNull("Replace('ABCD','CD',NULL_STRING)");

    // Check operands
    evalFails("Replace()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
  }

  @Test
  void HexEncode() throws Exception {
    evalEquals("HEX_ENCODE('Apache Hop')", "41706163686520486f70");
    evalEquals("HEX_ENCODE('')", "");
    // String encoded in UTF-8
    evalEquals("HEX_ENCODE('€')", "e282ac");
    // Binary encoded as is
    evalEquals("HEX_ENCODE(BINARY 'FF')", "ff");
    evalEquals("HEX_ENCODE(BINARY '00FF7A')", "00ff7a");
    evalEquals("HEX_ENCODE(FIELD_BINARY)", "54455354");
    // Other values are encoded as string
    evalEquals("HEX_ENCODE(255)", "323535");
    // Round trip
    evalEquals("HEX_DECODE(HEX_ENCODE('Apache Hop €'))", "Apache Hop €");

    // Null handling
    evalNull("HEX_ENCODE(NULL_STRING)");

    // Check operands
    evalFails("HEX_ENCODE()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("HEX_ENCODE(0x01,0x02)", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void HexDecode() throws Exception {
    // An odd number of characters or a non hexadecimal character raises an error
    evalFails("HEX_DECODE('ABC')", ErrorCode.CALL_FUNCTION_ERROR);
    evalFails("HEX_DECODE('zz')", ErrorCode.CALL_FUNCTION_ERROR);
    evalEquals("HEX_DECODE('')", "");
    evalEquals("HEX_DECODE('41706163686520486f70')", "Apache Hop");

    evalNull("HEX_DECODE(NULL_STRING)");

    // Check operands
    evalFails("HEX_DECODE()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("HEX_DECODE('p1','p2')", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Overlay() throws Exception {
    // Documentation example, only the first length characters of replacement are used
    evalEquals("OVERLAY('Apxxxe','ache',3,2)", "Apacxe");
    // A length of 0 defaults to the length of replacement
    evalEquals("OVERLAY('Apxxxe','ach',3,0)", "Apache");
    // A position beyond the end places replacement after string
    evalEquals("OVERLAY('Apxxxe','ach',7)", "Apxxxeach");
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
    evalFails("Overlay()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Overlay(FIELD_DATE)", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Position and length out of range
    evalEquals("Overlay('Apxxxe','ach',1)", "achxxe");
    evalFails("Overlay('Apxxxe','ach',0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Overlay('Apxxxe','ach',-1)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Overlay('Apxxxe','ach',3,-1)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    // A position or a length out of the int range must not wrap
    evalEquals("Overlay('Apxxxe','ach',4294967297)", "Apxxxeach");
    evalEquals("Overlay('Apxxxe','ach',3,4294967297)", "Apach");
    evalEquals("Overlay('Apxxxe','ach',3,2147483647)", "Apach");
    evalFails("Overlay('Apxxxe','ach',3,-4294967295)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
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
    evalFails("Reverse()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Reverse('str','bad')", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Soundex() throws Exception {
    // Documentation example
    evalEquals("SOUNDEX('  ')", "");
    evalEquals("Soundex('robert')", "R163");
    evalEquals("Soundex('1abc')", "A120");
    evalEquals("Soundex('Wikipedia')", "W213").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Soundex('I LOVE ROCKS.')", "I416");
    evalEquals("Soundex('I LOVE ROCK AND ROLL MUSIC.')", "I416");
    // Empty string or without any letter
    evalEquals("Soundex('123456')", "");
    evalEquals("Soundex('')", "");
    evalEquals("Soundex(' ')", "");

    // Null handling
    evalNull("Soundex(NULL_STRING)").returnType(StringType.STRING);

    // Coercion to string
    evalEquals("Soundex(FIELD_DATE)", "");

    // Check operands
    evalFails("Soundex()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Soundex('str','bad')", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Difference() throws Exception {
    evalEquals("Difference('Perfect', 'Perfect')", 4L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Difference('Juice', 'Jucy')", 4L);
    evalEquals("Difference('Apple', 'Orange')", 0L);

    // No similarity when a string has no letter, its SOUNDEX is empty
    evalEquals("Difference('123', 'abc')", 0L);
    evalEquals("Difference('abc', '123')", 0L);
    evalEquals("Difference('', '')", 0L);

    // Null handling
    evalNull("Difference(NULL_STRING,NULL_STRING)");
    evalNull("Difference(NULL_STRING,'Jucy')");
    evalNull("Difference('Juice',NULL_STRING)");

    evalFails("Difference()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Difference('str')", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Difference('str','bad','xx')", ErrorCode.TOO_MANY_ARGUMENTS);
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
    // A NULL parameters argument is treated as its default value
    evalFalse("Regexp_Like('A','a',NULL_STRING)");
    evalTrue("Regexp_Like('aaa','a{2,4}')").returnType(BooleanType.BOOLEAN_NOT_NULL);
    // Character classes use the Java syntax, the POSIX syntax like [:alnum:] is not supported
    evalTrue("Regexp_Like('Erd beere','Erd[a\\p{Space}b]beere')");
    evalFalse("Regexp_Like('Erdcbeere','Erd[a\\p{Space}b]beere')");
    evalTrue("Regexp_Like('12345TEST','^123\\p{Alnum}*$')");
    evalFalse("Regexp_Like('12345 TEST','^123\\p{Alnum}*$')");
    evalTrue("Regexp_Like('ABcdf987','^\\p{XDigit}*$')");
    evalFalse("Regexp_Like('ABcdfg987','^\\p{XDigit}*$')");
    evalTrue("Regexp_Like('A','[a-z]','i')");
    evalFalse("Regexp_Like('A','[a-z]','c')");

    // Null handling
    evalNull("Regexp_Like(NULL_STRING,'A')").returnType(BooleanType.BOOLEAN);
    evalNull("Regexp_Like('A', NULL_STRING)").returnType(BooleanType.BOOLEAN);

    // An empty pattern '' matches nothing
    evalFalse("Regexp_Like('','')");
    evalFalse("Regexp_Like('ABC','')");

    // Check operands
    evalFails("Regexp_Like()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Regexp_Like('A')", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Regexp_Like('A','[a-z]','z')", ErrorCode.INVALID_ARGUMENT);

    // Parameters, the last of the 'c' and 'i' letters wins
    evalTrue("Regexp_Like('A','a','ci')");
    evalFalse("Regexp_Like('A','a','ic')");
    evalFalse("Regexp_Like('A','a','c')");
    evalFalse("Regexp_Like('A','a')");
    // Multi-line mode
    evalTrue("Regexp_Like('a'||Chr(10)||'b','^b$','m')");
    evalFalse("Regexp_Like('a'||Chr(10)||'b','^b$')");
    // The period matches the newline
    evalTrue("Regexp_Like('a'||Chr(10)||'b','a.b','n')");
    evalFalse("Regexp_Like('a'||Chr(10)||'b','a.b')");

    // Invalid pattern, constant or not
    evalFails("Regexp_Like('abc','(')", ErrorCode.INVALID_REGEXP_PATTERN);
    evalFails("Regexp_Like('abc','('||FIELD_STRING)", ErrorCode.INVALID_REGEXP_PATTERN);
  }

  @Test
  void Regexp_Replace() throws Exception {
    // The position must not wrap
    evalEquals("Regexp_Replace('aaa','a','b',4294967297)", "aaa");
    // A NULL position or parameters argument is treated as its default value
    evalEquals("Regexp_Replace('aaa','a','b',NULL_INTEGER)", "bbb");
    evalEquals("Regexp_Replace('Aa','a','x',1,0,NULL_STRING)", "Ax");
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

    // The search starts at the position, the characters before are kept
    evalEquals("Regexp_Replace('aaa','a','b',1)", "bbb");
    evalEquals("Regexp_Replace('aaa','a','b',2)", "abb");
    evalEquals("Regexp_Replace('aaa','a','b',3)", "aab");
    evalEquals("Regexp_Replace('aaa','a','b',2,1)", "aba");
    evalEquals("Regexp_Replace('aaa','a','b',2,2)", "aab");
    evalEquals("Regexp_Replace('aaa','a','b',4)", "aaa");
    evalEquals("Regexp_Replace('aaa','a','b',5)", "aaa");
    evalFails("Regexp_Replace('aaa','a','b',0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Regexp_Replace('aaa','a','b',-1)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    // An occurrence greater than the number of matches replaces nothing
    evalEquals("Regexp_Replace('aaa','a','b',1,4)", "aaa");

    // The occurrence 0 replaces all, a negative occurrence is rejected
    evalEquals("Regexp_Replace('aaa','a','b',1,0)", "bbb");
    evalEquals("Regexp_Replace('aaa','a','b',1,4294967298)", "aaa");
    evalFails("Regexp_Replace('aaa','a','b',1,-1)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Regexp_Replace('aaa','a','b',1,-4294967295)", ErrorCode.ARGUMENT_OUT_OF_RANGE);

    // An empty pattern matches nothing
    evalEquals("Regexp_Replace('ABCDEEEEEEFG', '','E')", "ABCDEEEEEEFG");

    // Back reference
    evalEquals(
        "Regexp_Replace('FIRSTNAME MIDDLENAME LASTNAME','(.*) (.*) (.*)','\\3, \\1 \\2')",
        "LASTNAME, FIRSTNAME MIDDLENAME");

    // Null handling
    evalNull("Regexp_Replace(NULL_STRING,'A')").returnType(StringType.STRING);
    evalNull("Regexp_Replace('A', NULL_STRING)");
    evalNull("Regexp_Replace('A', 'A', NULL_STRING)");

    // Check operands
    evalFails("Regexp_Replace()", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Parameters
    evalEquals("Regexp_Replace('Aa','a','x',1,0,'i')", "xx");
    evalEquals("Regexp_Replace('Aa','a','x',1,0,'c')", "Ax");
    evalEquals("Regexp_Replace('a'||Chr(10)||'a','^a','x',1,0,'m')", "x\nx");
    evalEquals("Regexp_Replace('a'||Chr(10)||'a','^a','x',1,0)", "x\na");
    evalEquals("Regexp_Replace('a'||Chr(10)||'b','a.b','x',1,0,'n')", "x");
    evalFails("Regexp_Replace('Aa','a','x',1,0,'z')", ErrorCode.INVALID_ARGUMENT);

    // Invalid pattern, constant or not
    evalFails("Regexp_Replace('abc','(')", ErrorCode.INVALID_REGEXP_PATTERN);
    evalFails("Regexp_Replace('abc','('||FIELD_STRING)", ErrorCode.INVALID_REGEXP_PATTERN);
  }

  @Test
  void Regexp_Count() throws Exception {
    // The position must not wrap
    evalEquals("Regexp_Count('aaa','a',4294967296)", 0L);
    // A NULL parameters argument is treated as its default value
    evalEquals("Regexp_Count('Aa','a',1,NULL_STRING)", 1L);
    // An empty pattern matches nothing
    evalEquals("Regexp_Count('abc','')", 0L);
    evalEquals("Regexp_Count('An apple costs 50 cents, a banana costs 10 cents.', '\\d+')", 2L)
        .returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Regexp_Count('An apple costs 50 cents, a banana costs 10 cents.', '\\d+', 20)", 1L);
    evalEquals(
        "Regexp_Count('An apple costs 50 cents, a banana costs 10 cents.', 'CENTS', 1, 'i')", 2L);

    // Search from the first character if the position is null
    evalEquals(
        "Regexp_Count('An apple costs 50 cents, a banana costs 10 cents.', '\\d+', NULL_INTEGER)",
        2L);

    // Position at the bounds of the string
    evalEquals("Regexp_Count('aaa', 'a', 1)", 3L);
    evalEquals("Regexp_Count('aaa', 'a', 3)", 1L);
    evalEquals("Regexp_Count('aaa', 'a', 4)", 0L);
    evalEquals("Regexp_Count('aaa', 'a', 5)", 0L);
    evalFails("Regexp_Count('aaa', 'a', 0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Regexp_Count('aaa', 'a', -1)", ErrorCode.ARGUMENT_OUT_OF_RANGE);

    // Null handling
    evalNull("Regexp_Count(NULL_STRING, 'a')").returnType(IntegerType.INTEGER);
    evalNull("Regexp_Count('abc', NULL_STRING)").returnType(IntegerType.INTEGER);

    // Parameters
    evalEquals("Regexp_Count('Aa','a',1,'i')", 2L);
    evalEquals("Regexp_Count('Aa','a',1,'c')", 1L);
    evalEquals("Regexp_Count('a'||Chr(10)||'a','^a$',1,'m')", 2L);
    evalEquals("Regexp_Count('a'||Chr(10)||'a','^a$',1)", 0L);
    evalEquals("Regexp_Count('a'||Chr(10)||'b','a.b',1,'n')", 1L);
    evalFails("Regexp_Count('Aa','a',1,'z')", ErrorCode.INVALID_ARGUMENT);

    // Invalid pattern, constant or not
    evalFails("Regexp_Count('abc','(')", ErrorCode.INVALID_REGEXP_PATTERN);
    evalFails("Regexp_Count('abc','('||FIELD_STRING)", ErrorCode.INVALID_REGEXP_PATTERN);
  }

  @Test
  void Regexp_Instr() throws Exception {
    // The position must not wrap
    evalEquals("Regexp_Instr('abc','a',4294967297)", 0L);
    // A nonzero return option returns the position following the match
    evalEquals("Regexp_Instr('abc','b',1,1,2)", 3L);
    // A NULL position argument is treated as its default value
    evalEquals("Regexp_Instr('abab','b',NULL_INTEGER)", 2L);
    evalEquals("Regexp_Instr('email@apache.org', '@[^.]*')", 6L)
        .returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Regexp_Instr('hello to YOU', '(.o).', 1, 3, 1,'i')", 13L);
    evalEquals(
        "Regexp_Instr('REGEXP_INSTR is an advanced extension of the INSTR function','[:a-z]{3,8}', 3, 2, 1)",
        37L);

    // The position is 1-based
    evalEquals("Regexp_Instr('abc', 'a')", 1L);
    evalEquals("Regexp_Instr('abc', 'a', 1)", 1L);
    evalEquals("Regexp_Instr('abc', 'a', 2)", 0L);
    evalEquals("Regexp_Instr('abc', 'b', 2)", 2L);
    evalEquals("Regexp_Instr('abc', 'c', 3, 1, 1)", 4L);
    evalEquals("Regexp_Instr('abc', '.', 4)", 0L);
    evalEquals("Regexp_Instr('abc', '.', 5)", 0L);
    evalFails("Regexp_Instr('abc', '.', 0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Regexp_Instr('abc', '.', -1)", ErrorCode.ARGUMENT_OUT_OF_RANGE);

    // The occurrence must be positive, NULL is the first occurrence
    evalEquals("Regexp_Instr('abab', 'b', 1, NULL_INTEGER)", 2L);
    evalEquals("Regexp_Instr('abab', 'b', 1, 2)", 4L);
    evalEquals("Regexp_Instr('abab', 'b', 1, 4294967298)", 0L);
    evalFails("Regexp_Instr('abab', 'b', 1, 0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Regexp_Instr('abab', 'b', 1, -1)", ErrorCode.ARGUMENT_OUT_OF_RANGE);

    // An empty pattern matches nothing
    evalEquals("Regexp_Instr('email@apache.org', '')", 0L);

    // Parameters
    evalEquals("Regexp_Instr('Aa','a',1,1,0,'i')", 1L);
    evalEquals("Regexp_Instr('Aa','a',1,1,0,'c')", 2L);
    evalEquals("Regexp_Instr('a'||Chr(10)||'b','^b',1,1,0,'m')", 3L);
    evalEquals("Regexp_Instr('a'||Chr(10)||'b','^b',1,1,0)", 0L);
    evalEquals("Regexp_Instr('a'||Chr(10)||'b','a.b',1,1,0,'n')", 1L);
    evalFails("Regexp_Instr('Aa','a',1,1,0,'z')", ErrorCode.INVALID_ARGUMENT);

    // Invalid pattern, constant or not
    evalFails("Regexp_Instr('abc','(')", ErrorCode.INVALID_REGEXP_PATTERN);
    evalFails("Regexp_Instr('abc','('||FIELD_STRING)", ErrorCode.INVALID_REGEXP_PATTERN);
  }

  @Test
  void Regexp_Substr() throws Exception {
    // The position must not wrap
    evalNull("Regexp_Substr('abc','a',4294967297)");
    // A NULL position argument is treated as its default value
    evalEquals("Regexp_Substr('abc','b',NULL_INTEGER)", "b");
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
    // The position is 1-based
    evalEquals("regexp_substr('abc', 'a')", "a");
    evalEquals("regexp_substr('abc', '.b.', 1)", "abc");
    evalNull("regexp_substr('abc', '.b.', 2)");
    evalEquals("regexp_substr('abc', 'b', 2)", "b");
    evalEquals("regexp_substr('abc', '.', 3)", "c");
    evalNull("regexp_substr('abc', '.', 4)");
    evalNull("regexp_substr('abc', '.', 5)");
    evalFails("regexp_substr('abc', '.', 0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("regexp_substr('abc', '.', -1)", ErrorCode.ARGUMENT_OUT_OF_RANGE);

    // The occurrence must be positive, NULL is the first occurrence
    evalEquals("regexp_substr('a1b2', '[a-z]', 1, NULL_INTEGER)", "a");
    evalEquals("regexp_substr('a1b2', '[a-z]', 1, 2)", "b");
    evalNull("regexp_substr('a1b2', '[a-z]', 1, 4294967298)");
    evalFails("regexp_substr('a1b2', '[a-z]', 1, 0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("regexp_substr('a1b2', '[a-z]', 1, -1)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    // The groups do not change the result, the third argument is the position not a group
    evalEquals("regexp_substr('abc', '([a-z])(b)')", "ab");
    evalEquals("regexp_substr('abc', '([a-z])(c)', 2)", "bc");

    // An empty pattern matches nothing
    evalNull("regexp_substr('email@apache.org', '')");

    evalNull("regexp_substr(NULL_STRING,  '@[^.]*')");
    evalNull("regexp_substr('email@apache.org', NULL_STRING)");

    // Parameters
    evalEquals("Regexp_Substr('ABC','b',1,1,'i')", "B");
    evalNull("Regexp_Substr('ABC','b',1,1,'c')");
    evalEquals("Regexp_Substr('a'||Chr(10)||'b','^b',1,1,'m')", "b");
    evalNull("Regexp_Substr('a'||Chr(10)||'b','^b',1,1)");
    evalEquals("Regexp_Substr('a'||Chr(10)||'b','a.b',1,1,'n')", "a\nb");
    evalFails("Regexp_Substr('ABC','b',1,1,'z')", ErrorCode.INVALID_ARGUMENT);

    // Invalid pattern, constant or not
    evalFails("Regexp_Substr('abc','(')", ErrorCode.INVALID_REGEXP_PATTERN);
    evalFails("Regexp_Substr('abc','('||FIELD_STRING)", ErrorCode.INVALID_REGEXP_PATTERN);
  }

  @Test
  void EqualNull() throws Exception {
    evalFalse("Equal_Null(1,NULL_INTEGER)").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalTrue("Equal_Null(NULL_STRING,NULL_STRING)").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalTrue("Equal_Null(NULL_INTEGER,NULL_NUMBER)");
    evalTrue("Equal_Null(DATE '2019-01-01',DATE '2019-01-01')");
    evalFalse("Equal_Null(DATE '2019-01-01',DATE '2018-01-01')")
        .returnType(BooleanType.BOOLEAN_NOT_NULL);

    evalFails("Equal_Null()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Equal_Null(NULL_INTEGER)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Equal_Null(NULL_INTEGER, NULL_NUMBER, NULL_INTEGER)", ErrorCode.TOO_MANY_ARGUMENTS);

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
    // Documentation example
    evalEquals("CONCAT('AB','CD','12')", "ABCD12");
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

    evalFails("Concat()", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Coercion to string
    evalEquals("4 || 2", "42").returnType(StringType.of(2, false));
    evalEquals("4 || '2'", "42").returnType(StringType.of(2, false));
    evalEquals("Concat(FIELD_STRING, FIELD_INTEGER)", "TEST40");
    evalEquals("Concat(FIELD_INET, FIELD_STRING)", "10.10.10.1TEST");
    // Date, a type without precision
    evalEquals("Concat('A', DATE '2024-01-02')", "A2024-01-02")
        .returnType(StringType.STRING_NOT_NULL);
    evalEquals("DATE '2024-01-02' || 'A'", "2024-01-02A").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Concat('A', TIMESTAMP '2024-01-02 10:11:12')", "A2024-01-02 10:11:12");

    // TODO:Mix String and Binary

    // Array to array concatenation
    evalEquals("CARDINALITY([1,2,3] || [4,5])", 5L);
    optimize("[1,2,3] || [4,5] || [6,7]", "[1,2,3,4,5,6,7]");

    // optimize("[1,2,3] || [[4,5,6],[7,8,9]]", "[[1,2,3],[4,5,6],[7,8,9]]");

    // Element to array concatenation
    optimize("1 || [2,3,4]", "[1,2,3,4]");
    optimize("FIELD_STRING || ['A','B','C']", "[FIELD_STRING,'A','B','C']");

    // Array to element concatenation
    optimize("[1,2,3] || 4", "[1,2,3,4]");
    optimize("['A','B','C'] || FIELD_STRING", "['A','B','C',FIELD_STRING]");

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

    // Empty strings are values, separated like the others
    evalEquals("CONCAT_WS(',','','a')", ",a");
    evalEquals("CONCAT_WS(',','a','','b')", "a,,b");
    evalEquals("CONCAT_WS(',','','')", ",");
    evalEquals("CONCAT_WS(',','')", "");
    evalEquals("CONCAT_WS(',',NULL_STRING,'')", "");

    // Binary
    evalEquals(
            "CONCAT_WS(BINARY '1F',BINARY '2A3B',BINARY '4D',BINARY '5E')",
            new byte[] {0x2A, 0x3B, 0x1F, 0x4D, 0x1F, 0x5E})
        .returnType(BinaryType.of(6, false));
    evalEquals("CONCAT_WS(BINARY '1F',BINARY '',BINARY '2A')", new byte[] {0x1F, 0x2A});
    evalEquals("CONCAT_WS(BINARY '1F',BINARY '')", new byte[] {});

    // Number
    evalEquals("CONCAT_WS(':',4,2)", "4:2").returnType(StringType.of(3, false));

    // Date, a type without precision
    evalEquals("CONCAT_WS(',',DATE '2024-01-02')", "2024-01-02")
        .returnType(StringType.STRING_NOT_NULL);
    evalEquals("CONCAT_WS(',','A',DATE '2024-01-02')", "A,2024-01-02")
        .returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("CONCAT_WS(NULL_STRING,'FIRST')").returnType(StringType.of(5));
    evalNull("CONCAT_WS('a',NULL_STRING)");
    evalNull("CONCAT_WS(BINARY '1F',NULL_STRING,NULL_STRING)").returnType(BinaryType.BINARY);

    evalFails("CONCAT_WS()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("CONCAT_WS(',')", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Mix String and Binary
    // TODO: evalFails("CONCAT_WS(FIELD_STRING,0x2A3B)");
  }

  @Test
  void Chr() throws Exception {
    // The last valid code point
    evalEquals("UNICODE(CHR(1114111))", 1114111L);
    evalFails("CHR(1114112)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalEquals("Chr(83)", "S").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Chr(115)", "s");
    evalEquals("Chr(233)", "é");
    evalEquals("Chr(945)", "α");
    evalEquals("Chr(8364)", "€");
    evalEquals("Chr(33288)", "興");

    // Null handling
    evalNull("Chr(NULL_INTEGER)");

    // Check operands
    evalFails("Chr()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Chr('bad')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Chr(-1)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Chr(999999999999)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    // A code point out of the int range must not wrap to a valid one
    evalFails("Chr(4294967361)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
  }

  @Test
  void Ascii() throws Exception {
    // Documentation example
    evalEquals("ASCII('X')", 88L);
    evalEquals("Ascii('ABC')", 65L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Ascii('é')", 233L);
    evalEquals("Ascii('€')", 8364L);
    evalEquals("Ascii('興')", 33288L);
    evalEquals("Ascii('')", 0L);

    // Null handling
    evalNull("Ascii(NULL_STRING)");

    // Check operands
    evalFails("Ascii()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Ascii('a','b')", ErrorCode.TOO_MANY_ARGUMENTS);
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
    evalFails("Unicode()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Unicode('a','b')", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void String_To_Array() throws Exception {
    evalEquals(
        "String_To_Array('Green|Yellow|Blue','|')",
        new Array(Literal.of("Green"), Literal.of("Yellow"), Literal.of("Blue")));
    evalEquals("String_To_Array('AB|CD','')", new Array(Literal.of("AB|CD")));

    // No split on white spaces if delimiter is empty
    evalEquals("String_To_Array('AB CD','')", new Array(Literal.of("AB CD")));

    // Empty elements between delimiters are kept
    evalEquals(
        "String_To_Array('a||b','|')", new Array(Literal.of("a"), Literal.of(""), Literal.of("b")));
    evalEquals(
        "String_To_Array('|a|','|')", new Array(Literal.of(""), Literal.of("a"), Literal.of("")));

    // Multiple characters delimiter
    evalEquals("String_To_Array('a--b-c','--')", new Array(Literal.of("a"), Literal.of("b-c")));

    // Null handling
    evalNull("String_To_Array('Green|Yellow|Blue',NULL_STRING)");
    evalNull("String_To_Array(NULL_STRING,',')");

    // Check operands
    evalFails("String_To_Array()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
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
    // Octal escapes, escaped in the Java source to reach the function
    evalEquals("String_Decode('\\366\\344\\374')", "öäü");

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
    evalFails("Html_Encode()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Html_Encode('x','y')", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Html_Decode() throws Exception {
    evalEquals("Html_Decode('18&euro; &amp; &lt;test&gt; &#8482;')", "18€ & <test> ™");

    // Null handling
    evalNull("Html_Decode(NULL_STRING)");

    // Check operands
    evalFails("Html_Decode()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Html_Decode('x','y')", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Url_Encode() throws Exception {
    evalEquals("Url_Encode('a b')", "a+b").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Url_Encode('a+b')", "a%2Bb");
    evalEquals("Url_Encode('âéè')", "%C3%A2%C3%A9%C3%A8");

    // Null handling
    evalNull("Url_Encode(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("Url_Encode()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Url_Encode('x','y')", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Url_Decode() throws Exception {
    // A malformed escape sequence raises an error
    evalFails("Url_Decode('a%%2Bb')", ErrorCode.CALL_FUNCTION_ERROR);
    evalEquals("Url_Decode('a+b')", "a b").returnType(StringType.STRING_NOT_NULL);
    evalEquals("Url_Decode('a%2Bb')", "a+b");
    evalEquals("Url_Decode('%C3%A2%C3%A9%C3%A8')", "âéè");

    // Null handling
    evalNull("Url_Decode(NULL_STRING)");

    // Check operands
    evalFails("Url_Decode()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Url_Decode('x','y')", ErrorCode.TOO_MANY_ARGUMENTS);
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
    evalFails("Base64_Encode()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
  }

  @Test
  void Base64_Decode() throws Exception {
    // The decoded bytes are interpreted as UTF-8, '4oKs' is the euro sign
    evalEquals("BASE64_DECODE('4oKs')", "€");
    evalEquals("Base64_Decode('QXBhY2hlIEhvcA==')", "Apache Hop")
        .returnType(StringType.STRING_NOT_NULL);
    evalEquals("Base64_Decode('QXBhY2hlIEhvcA=='::Binary)", "Apache Hop")
        .returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("Base64_Decode(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("Base64_Decode()", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Invalid symbol
    evalFails("Base64_Decode('QXBh Y2hlIEhvcA==')", ErrorCode.INVALID_DECODING_SYMBOL);
    assertDecodingSymbolMessage("Base64_Decode('QXBh&Y2hlIEhvcA==')", "&", "base64");
    assertDecodingSymbolMessage("Base64_Decode('QXBh€Y2hlIEhvcA==')", "€", "base64");

    // The padding is only allowed at the end
    assertDecodingSymbolMessage("Base64_Decode('QQ=A')", "=", "base64");
    assertDecodingSymbolMessage("Base64_Decode('QQ==QQ==')", "=", "base64");

    // Truncated value
    evalFails("Base64_Decode('QXBhY2hlIEhvcA=')", ErrorCode.TRUNCATED_DECODING_SEQUENCE);
    assertDecodingMessage(
        "Base64_Decode('QXBhY')", ErrorCode.TRUNCATED_DECODING_SEQUENCE.message("base64"));
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
    evalFails("Base32_Encode()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
  }

  @Test
  void Base32_Decode() throws Exception {
    evalEquals("Base32_Decode('IFYGCY3IMUQEQ33Q')", "Apache Hop")
        .returnType(StringType.STRING_NOT_NULL);
    evalEquals("Base32_Decode('IFYGCY3IMUQEQ33Q'::Binary)", "Apache Hop")
        .returnType(StringType.STRING_NOT_NULL);
    evalEquals("Base32_Decode('ifygcy3imuqeq33q')", "Apache Hop");
    evalEquals("Base32_Decode('MY======')", "f");
    evalEquals("Base32_Decode('')", "");
    evalEquals("Base32_Decode(Base32_Encode('Apache Hop €'))", "Apache Hop €");

    // Null handling
    evalNull("Base32_Decode(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("Base32_Decode()", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Invalid symbol
    evalFails("Base32_Decode('IFYG CY3IMUQEQ33Q')", ErrorCode.INVALID_DECODING_SYMBOL);
    evalFails("Base32_Decode('IFYGCY3I!MUQEQ33Q')", ErrorCode.INVALID_DECODING_SYMBOL);
    evalFails("Base32_Decode('IFYGCY3I1MUQEQ33Q')", ErrorCode.INVALID_DECODING_SYMBOL);
    assertDecodingSymbolMessage("Base32_Decode('IFYGCY3IéMUQEQ33Q')", "é", "base32");
    // The padding is only allowed at the end
    assertDecodingSymbolMessage("Base32_Decode('=MY')", "=", "base32");
    assertDecodingSymbolMessage("Base32_Decode('MY======MY======')", "=", "base32");

    // Truncated value
    evalFails("Base32_Decode('IFYGCY3IMUQEQ33')", ErrorCode.TRUNCATED_DECODING_SEQUENCE);
    evalFails("Base32_Decode('IFY')", ErrorCode.TRUNCATED_DECODING_SEQUENCE);
    assertDecodingMessage(
        "Base32_Decode('I')", ErrorCode.TRUNCATED_DECODING_SEQUENCE.message("base32"));
  }

  /** Check that the expression fails with the invalid symbol and the encoding in the message. */
  private void assertDecodingSymbolMessage(String source, String symbol, String encoding)
      throws Exception {
    assertDecodingMessage(source, ErrorCode.INVALID_DECODING_SYMBOL.message(symbol, encoding));
  }

  /** Check that the expression fails with the expected decoding error message. */
  private void assertDecodingMessage(String source, String message) throws Exception {
    IExpression expression = IExpression.of(createExpressionContext(), source);
    ExpressionException exception = assertThrows(ExpressionException.class, expression::getValue);
    assertTrue(exception.getMessage().contains(message), exception.getMessage());
  }

  @Test
  void Uuid() throws Exception {
    assertFalse(FunctionRegistry.getFunction("UUID").isDeterministic());

    evalEquals("Length(Uuid())", 36L);
    evalEquals("Substr(Uuid(),15,1)", "7");

    optimize("UUID()").returnType(StringType.STRING_NOT_NULL);

    // Check operands
    evalFails("UUID(1)", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Compress() throws Exception {
    // The result is in the GZIP format
    evalEquals("Left(Compress('Test'::BINARY),2)", new byte[] {0x1F, (byte) 0x8B});
    evalEquals("Decompress(Compress('Test'::BINARY))::STRING", "Test");

    // Null handling
    evalNull("Compress(NULL_BINARY)").returnType(BinaryType.BINARY);
  }

  @Test
  void Decompress() throws Exception {
    evalEquals("Decompress(Compress('Test'::BINARY))::STRING", "Test");
    evalEquals("Decompress(Compress(BINARY ''))", new byte[0]);
    // A value larger than the decompression buffer
    evalEquals("Length(Decompress(Compress(Repeat('Test',5000)::BINARY)))", 20000L);

    // Null handling
    evalNull("Decompress(NULL_BINARY)").returnType(BinaryType.BINARY);

    // Not a GZIP value or a truncated one
    evalFails("Decompress(BINARY '2A3B4C')", ErrorCode.DECOMPRESSION_ERROR);
    evalFails("Decompress(Left(Compress('Test'::BINARY),12))", ErrorCode.DECOMPRESSION_ERROR);
  }

  @Test
  void Position() throws Exception {
    // Equivalent to INSTR, an empty substring is found at position 1
    evalEquals("POSITION('def' IN 'abcdefgh')", 4L);
    evalTrue("POSITION('def' IN 'abcdefgh') = INSTR('abcdefgh','def')");
    evalEquals("POSITION('' IN 'abc')", 1L);
    evalEquals("Position('abc' IN 'abcdefgh')", 1L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Position('XYZ' IN 'abcdefgh')", 0L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Position('def' IN 'abcdefgh')", 4L).returnType(IntegerType.INTEGER_NOT_NULL);

    // Null handling
    evalNull("Position(NULL_STRING IN 'abcdefgh')").returnType(IntegerType.INTEGER);
    evalNull("Position('abc' IN NULL_STRING)").returnType(IntegerType.INTEGER);

    // Check operands
    evalFails("Position()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Position('test')", ErrorCode.NOT_ENOUGH_ARGUMENTS);

    // Check syntax
    evalFails("Position( ", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("Position('abc' ", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("Position('abc' IN ", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("Position('abc' IN 'abcd'", ErrorCode.MISSING_RIGHT_PARENTHESIS);
    evalFails("Position( IN 'fsd'", ErrorCode.SYNTAX_ERROR);

    optimize("POSITION('abc' IN FIELD_STRING)");
  }

  /**
   * A character outside the Basic Multilingual Plane, like the emoji 😀, is stored as a surrogate
   * pair of two chars, but the positions and lengths of the string functions count it as one
   * character.
   */
  @Test
  void SurrogatePair() throws Exception {
    evalEquals("Length('a😀b')", 3L);
    evalEquals("Left('😀ab',1)", "😀");
    evalEquals("Left('a😀b',2)", "a😀");
    evalEquals("Right('ab😀',1)", "😀");
    evalEquals("Right('😀ab',3)", "😀ab");
    evalEquals("Substring('a😀b',2,1)", "😀");
    evalEquals("Substring('a😀b',3)", "b");
    evalEquals("Substring('a😀b',-2)", "😀b");
    evalEquals("Position('b' IN 'a😀b')", 3L);
    evalEquals("Instr('a😀b','b')", 3L);
    evalEquals("Instr('a😀b😀','😀',1,2)", 4L);
    evalEquals("Instr('😀a😀a','a',-1)", 4L);
    evalEquals("Instr('😀a😀a','a',-2)", 2L);
    evalEquals("Insert('a😀b',3,1,'X')", "a😀X");
    evalEquals("Insert('a😀b',2,1,'X')", "aXb");
    evalEquals("Overlay('a😀b','X',2)", "aXb");
    evalEquals("Overlay('abc','😀😀',2,1)", "a😀c");
    evalEquals("LPad('a',3,'😀')", "😀😀a");
    evalEquals("RPad('a',3,'😀')", "a😀😀");
    evalEquals("LPad('😀😀😀',2)", "😀😀");
    evalEquals("RPad('😀😀😀',2)", "😀😀");
    evalEquals("Translate('a😀b','😀','X')", "aXb");
    evalEquals("Translate('abc','b','😀')", "a😀c");
    evalEquals("Reverse('a😀b')", "b😀a");
    evalEquals("Regexp_Instr('a😀b','b')", 3L);
    evalEquals("Regexp_Instr('a😀b','😀',1,1,1)", 3L);
    evalEquals("Regexp_Substr('😀ab','.',2)", "a");
    evalEquals("Regexp_Count('😀a😀a','a',3)", 1L);
    evalEquals("Regexp_Replace('😀aa','a','X',3)", "😀aX");
  }
}
