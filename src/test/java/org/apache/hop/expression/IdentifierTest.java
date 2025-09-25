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
package org.apache.hop.expression;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.Month;
import org.apache.hop.expression.type.DateType;
import org.junit.jupiter.api.Test;

public class IdentifierTest extends ExpressionTest {

  @Test
  void identifier() throws Exception {
    Identifier identifier1 = new Identifier(0, "FIELD_STRING");
    Identifier identifier2 = new Identifier(10, "FIELD_STRING");
    Identifier identifier3 = new Identifier("FIELD_STRING");
    identifier3.validate(createExpressionContext());
    assertEquals("FIELD_STRING", identifier1.getName());
    assertEquals(identifier1, identifier2);
    assertEquals(identifier1.hashCode(), identifier2.hashCode());
    assertNotEquals(null, identifier1);
    assertNotEquals(identifier1, identifier3);
  }

  @Test
  void quoteIfNeeded() {
    assertEquals("NORMAL", Identifier.quoteIfNeeded("NORMAL"));
    assertEquals("\"TRIM\"", Identifier.quoteIfNeeded("TRIM"));
    assertEquals("\"OR\"", Identifier.quoteIfNeeded("OR"));
    assertEquals("\"FIELD WITH SPACE\"", Identifier.quoteIfNeeded("FIELD WITH SPACE"));
  }

  @Test
  void eval() throws Exception {
    evalEquals("FIELD_INTEGER%2", 0D);
    evalEquals(" \t\n\"FIELD_INTEGER\"%2", 0D);
    evalEquals("\"IDENTIFIER SPACE\"", "SPACE");
    evalEquals("\"IDENTIFIER_UNDERSCORE\"", "UNDERSCORE");
    evalEquals("\"IDENTIFIER lower\"", "lower");

    evalFails("BIDON||'X'", ErrorCode.UNRESOLVED_IDENTIFIER);
    evalFails("ABS(FIELD_STRING)", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("SIN(FIELD_STRING)", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("CAST(FIELD_STRING as INTEGER)", ErrorCode.CONVERSION_ERROR);
  }

  @Test
  void coercionValueMetaBoolean() throws Exception {
    // From boolean value meta
    evalEquals("Upper(FIELD_BOOLEAN_TRUE)", "TRUE");
    evalEquals("Upper(FIELD_BOOLEAN_FALSE)", "FALSE");
    evalEquals("ABS(FIELD_BOOLEAN_TRUE)", 1L);
    evalEquals("ABS(FIELD_BOOLEAN_FALSE)", 0L);
    evalEquals("LEFT('ABC',FIELD_BOOLEAN_TRUE)", "A");
    evalEquals("LEFT('ABC',FIELD_BOOLEAN_FALSE)", "");
  }

  @Test
  void coercionValueMetaString() throws Exception {
    // From string value meta
    evalEquals("Upper(FIELD_STRING_BOOLEAN_TRUE)", "TRUE");
    evalEquals("Upper(FIELD_STRING_BOOLEAN_FALSE)", "FALSE");
    evalEquals(
        "ADD_YEARS(Date '2000-01-01',FIELD_STRING_INTEGER::INTEGER)",
        LocalDate.of(2025, Month.JANUARY, 1));
    evalEquals("Abs(FIELD_STRING_NUMBER::NUMBER)", new BigDecimal("12.56"));
    evalEquals("LOWER(FIELD_STRING_JSON)", "{id:\"01\",name:\"john\",age:29}");
    evalEquals("Json_Value(FIELD_STRING_JSON, '$.age')", new BigDecimal("29"));
    // evalEquals("DECOMPRESS(FIELD_STRING)","");
  }

  @Test
  void coercionValueMetaInteger() throws Exception {
    // From integer value meta
    evalTrue("FIELD_INTEGER IS TRUE");
    evalTrue("FIELD_INTEGER_ZERO IS FALSE");
    evalEquals("Upper(FIELD_INTEGER)", "40");
    evalEquals("ADD_YEARS(Date '2000-01-01',FIELD_INTEGER)", LocalDate.of(2040, Month.JANUARY, 1));
    evalFails("DECOMPRESS(FIELD_INTEGER)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void coercionValueMetaNumber() throws Exception {
    // From number value meta
    evalEquals("Upper(FIELD_NUMBER)", "-5.12");
    evalTrue("FIELD_NUMBER IS TRUE");
    evalTrue("FIELD_NUMBER_ZERO IS FALSE");
    evalEquals("CONCAT('ABC-',ABS(FIELD_NUMBER))", "ABC-5.12");
    evalEquals(
        "ADD_YEARS(Date '2020-01-01',ROUND(FIELD_NUMBER))", LocalDate.of(2015, Month.JANUARY, 1));
    evalFails("DECOMPRESS(FIELD_NUMBER)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void coercionValueMetaBigNumber() throws Exception {
    // From bignumber value meta
    evalEquals("Upper(FIELD_BIGNUMBER)", "123456.789");
    evalTrue("FIELD_BIGNUMBER IS TRUE");
    evalTrue("FIELD_BIGNUMBER_ZERO IS FALSE");
    evalEquals("CONCAT('ABC-',FIELD_BIGNUMBER)", "ABC-123456.789");
    evalFails("DECOMPRESS(FIELD_BIGNUMBER)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void coercionValueMetaBinary() throws Exception {
    // From binary value meta
    evalEquals("FIELD_BINARY::STRING", "TEST");
  }

  @Test
  void coercionValueMetaDate() throws Exception {}

  @Test
  void coercionValueMetaTimestamp() throws Exception {
    evalEquals("FIRST_DAY(FIELD_TIMESTAMP)", LocalDate.of(2023, 2, 1)).returnType(DateType.DATE);
    evalEquals("HOUR(FIELD_TIMESTAMP)", 22L);
  }

  @Test
  void coercionValueMetaJson() throws Exception {
    evalEquals("Json_Value(FIELD_JSON, '$.store.book[0].title')", "Sayings of the Century");
    evalEquals("LENGTH(FIELD_JSON::STRING)", 466L);
    evalFails("FIELD_JSON IS TRUE", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void coercionValueMetaInet() throws Exception {

    evalEquals("LENGTH(FIELD_INET)", 10L);

    // Unsupported value meta
    evalFails("FIELD_INET IS TRUE", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void escape() throws Exception {
    // Reserved word
    evalEquals("Upper(\"STRING\")", "PARIS");
    // Field name like a function name
    evalEquals("\"YEAR\"", 2020L);
  }

  @Test
  void write() throws Exception {
    optimize("FIELD_STRING");
    // Reserved word
    optimize("\"CASE\"", "\"CASE\"");
    // Data type name
    optimize("\"STRING\"");
    // Time unit name
    optimize("\"CENTURY\"");
    optimize("\"YEAR\"");
    // Function name
    optimize("\"ASCII\"");
    // Contains space
    optimize("Trim(\"IDENTIFIER SPACE\")", "TRIM(\"IDENTIFIER SPACE\")");
  }
}
