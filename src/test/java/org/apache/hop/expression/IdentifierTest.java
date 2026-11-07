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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaBigNumber;
import org.apache.hop.core.row.value.ValueMetaBinary;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaNumber;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.row.value.ValueMetaTimestamp;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.InetType;
import org.apache.hop.expression.type.TimestampType;
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
    assertEquals("NAME_WITH_UNDERSCORE", Identifier.quoteIfNeeded("NAME_WITH_UNDERSCORE"));
    assertEquals("NAME2", Identifier.quoteIfNeeded("NAME2"));

    // Other white spaces
    assertEquals("\"FIELD\tTAB\"", Identifier.quoteIfNeeded("FIELD\tTAB"));
    assertEquals("\"FIELD\nLINE\"", Identifier.quoteIfNeeded("FIELD\nLINE"));
    assertEquals("\"FIELD NBSP\"", Identifier.quoteIfNeeded("FIELD NBSP"));

    // A character that ends an identifier, A-B would be read as a subtraction
    for (char c : "()/*%,^&><=~+-.!|$:[]".toCharArray()) {
      String name = "A" + c + "B";
      assertEquals('"' + name + '"', Identifier.quoteIfNeeded(name));
    }

    // Starts like a number or a string
    assertEquals("\"2020_SALES\"", Identifier.quoteIfNeeded("2020_SALES"));
    assertEquals("\".5\"", Identifier.quoteIfNeeded(".5"));
    assertEquals("\"'NAME\"", Identifier.quoteIfNeeded("'NAME"));

    // Empty name
    assertEquals("\"\"", Identifier.quoteIfNeeded(""));
  }

  @Test
  void unparse() {
    assertEquals("NORMAL", new Identifier("NORMAL").toString());
    assertEquals("\"FIELD WITH SPACE\"", new Identifier("FIELD WITH SPACE").toString());
    assertEquals("\"PRICE.TTC\"", new Identifier("PRICE.TTC").toString());
    assertEquals("\"A-B\"", new Identifier("A-B").toString());
    assertEquals("\"X(1)\"", new Identifier("X(1)").toString());
    assertEquals("\"2020_SALES\"", new Identifier("2020_SALES").toString());
  }

  @Test
  void eval() throws Exception {
    evalEquals("FIELD_INTEGER%2", 0D);
    evalEquals(" \t\n\"FIELD_INTEGER\"%2", 0D);
    evalEquals("\"IDENTIFIER SPACE\"", "SPACE");
    evalEquals("\"IDENTIFIER_UNDERSCORE\"", "UNDERSCORE");
    evalEquals("\"IDENTIFIER lower\"", "lower");

    evalFails("BIDON||'X'", ErrorCode.UNRESOLVED_IDENTIFIER);
    evalFails("ABS(FIELD_STRING)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("SIN(FIELD_STRING)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
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
    evalEquals("Json_Value(FIELD_STRING_JSON, '$.age')", "29");
    evalFails("DECOMPRESS(FIELD_STRING)", ErrorCode.DECOMPRESSION_ERROR);
  }

  @Test
  void coercionValueMetaInteger() throws Exception {
    // From integer value meta
    evalTrue("FIELD_INTEGER IS TRUE");
    evalTrue("FIELD_INTEGER_ZERO IS FALSE");
    evalEquals("Upper(FIELD_INTEGER)", "40");
    evalEquals("ADD_YEARS(Date '2000-01-01',FIELD_INTEGER)", LocalDate.of(2040, Month.JANUARY, 1));
    evalFails("DECOMPRESS(FIELD_INTEGER)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
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
    evalFails("DECOMPRESS(FIELD_NUMBER)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void coercionValueMetaBigNumber() throws Exception {
    // From bignumber value meta
    evalEquals("Upper(FIELD_BIGNUMBER)", "123456.789");
    evalTrue("FIELD_BIGNUMBER IS TRUE");
    evalTrue("FIELD_BIGNUMBER_ZERO IS FALSE");
    evalEquals("CONCAT('ABC-',FIELD_BIGNUMBER)", "ABC-123456.789");
    evalFails("DECOMPRESS(FIELD_BIGNUMBER)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void coercionValueMetaBinary() throws Exception {
    // From binary value meta
    evalEquals("FIELD_BINARY::STRING", "TEST");
  }

  @Test
  void coercionValueMetaDate() throws Exception {
    // A date field is a date without time zone, with the local date and time it shows
    evalEquals("FIELD_DATE", LocalDateTime.of(1981, 6, 23, 21, 44, 58, 123000000))
        .returnType(DateType.DATE);
    evalEquals("FIRST_DAY(FIELD_DATE)", LocalDateTime.of(1981, 6, 1, 21, 44, 58, 123000000))
        .returnType(DateType.DATE);
    evalEquals("HOUR(FIELD_DATE)", 21L);
    evalNull("NULL_DATE").returnType(DateType.DATE);

    // Converted to a timestamp in the system time zone
    evalEquals(
            "CAST(FIELD_DATE AS TIMESTAMP)",
            ZonedDateTime.of(1981, 6, 23, 21, 44, 58, 123000000, ZoneId.systemDefault()))
        .returnType(TimestampType.TIMESTAMP);

    // The epoch of a date is nominal, 1981-06-23 21:44:58 UTC
    evalEquals("EPOCH(FIELD_DATE)", 362180698L);

    // A date has no time zone
    evalFails("EXTRACT(TIMEZONE_HOUR FROM FIELD_DATE)", ErrorCode.INVALID_ARGUMENT);
  }

  @Test
  void coercionValueMetaTimestamp() throws Exception {
    // A timestamp field is a timestamp at the same instant, in the system time zone
    ZonedDateTime timestamp = ZonedDateTime.of(2023, 2, 28, 22, 11, 1, 0, ZoneId.systemDefault());
    evalEquals("FIELD_TIMESTAMP", timestamp).returnType(TimestampType.TIMESTAMP);
    evalEquals(
            "FIRST_DAY(FIELD_TIMESTAMP)",
            ZonedDateTime.of(2023, 2, 1, 22, 11, 1, 0, ZoneId.systemDefault()))
        .returnType(TimestampType.TIMESTAMP);
    evalEquals("HOUR(FIELD_TIMESTAMP)", 22L);
    evalNull("NULL_TIMESTAMP").returnType(TimestampType.TIMESTAMP);

    // Converted to a date, the local date and time is kept
    evalEquals("CAST(FIELD_TIMESTAMP AS DATE)", LocalDateTime.of(2023, 2, 28, 22, 11, 1))
        .returnType(DateType.DATE);

    // The epoch of a timestamp is its instant
    evalEquals("EPOCH(FIELD_TIMESTAMP)", timestamp.toEpochSecond());

    // A timestamp has the time zone of the system
    evalEquals(
        "EXTRACT(TIMEZONE_HOUR FROM FIELD_TIMESTAMP)",
        (long) timestamp.getOffset().getTotalSeconds() / 3600);
  }

  @Test
  void coercionValueMetaJson() throws Exception {
    evalEquals("Json_Value(FIELD_JSON, '$.store.book[0].title')", "Sayings of the Century");
    evalEquals("LENGTH(FIELD_JSON::STRING)", 466L);
    evalFails("FIELD_JSON IS TRUE", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void coercionValueMetaInet() throws Exception {

    evalEquals("LENGTH(FIELD_INET)", 10L);
    evalNull("NULL_INET").returnType(InetType.INET);

    // Unsupported value meta
    evalFails("FIELD_INET IS TRUE", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
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

  /**
   * Creates a context with a single field named FIELD.
   *
   * @param valueMeta the value meta of the field, its name is replaced
   * @param value the value of the field in the row
   */
  private static RowExpressionContext createContext(IValueMeta valueMeta, Object value) {
    valueMeta.setName("FIELD");
    RowMeta rowMeta = new RowMeta();
    rowMeta.addValueMeta(valueMeta);
    RowExpressionContext context = new RowExpressionContext(new Variables(), rowMeta);
    context.setRow(new Object[] {value});
    return context;
  }

  /**
   * Creates a context with a field stored as a binary string, like with the lazy conversion of a
   * text file input.
   *
   * @param valueMeta the value meta of the field
   * @param mask the conversion mask of the string, or null
   * @param text the text of the value
   */
  private static RowExpressionContext createBinaryStringContext(
      IValueMeta valueMeta, String mask, String text) {
    IValueMeta storage = new ValueMetaString("FIELD");
    storage.setConversionMask(mask);
    valueMeta.setConversionMask(mask);
    valueMeta.setStorageType(IValueMeta.STORAGE_TYPE_BINARY_STRING);
    valueMeta.setStorageMetadata(storage);
    return createContext(valueMeta, text.getBytes(StandardCharsets.UTF_8));
  }

  @Test
  void binaryStringStorage() throws Exception {
    // The value is converted from the binary string, it is not read as is
    IExpressionContext context = createBinaryStringContext(new ValueMetaString(), null, "Paris");
    evalEquals(context, "FIELD", "Paris");
    evalEquals(context, "UPPER(FIELD)", "PARIS");

    context = createBinaryStringContext(new ValueMetaInteger(), null, "40");
    evalEquals(context, "FIELD", 40L);
    evalEquals(context, "FIELD::STRING", "40");
    evalEquals(context, "FIELD + 1", 41L);

    context = createBinaryStringContext(new ValueMetaBinary(), null, "TEST");
    evalEquals(context, "FIELD::STRING", "TEST");
    assertArrayEquals(
        "TEST".getBytes(StandardCharsets.UTF_8), (byte[]) new Evaluator(context, "FIELD").eval());

    context = createBinaryStringContext(new ValueMetaBigNumber(), null, "123.456");
    evalEquals(context, "FIELD", 123.456D);
    evalEquals(context, "FIELD::STRING", "123.456");

    context = createBinaryStringContext(new ValueMetaNumber(), null, "-5.12");
    evalEquals(context, "FIELD", -5.12D);

    context = createBinaryStringContext(new ValueMetaBoolean(), null, "Y");
    evalEquals(context, "UPPER(FIELD)", "TRUE");
    assertEquals(Boolean.TRUE, new Evaluator(context, "FIELD").eval());

    context =
        createBinaryStringContext(
            new ValueMetaDate(), "yyyy-MM-dd HH:mm:ss", "2023-02-28 22:11:01");
    evalEquals(context, "FIELD", LocalDateTime.of(2023, 2, 28, 22, 11, 1))
        .returnType(DateType.DATE);
    evalEquals(context, "HOUR(FIELD)", 22L);

    context =
        createBinaryStringContext(
            new ValueMetaTimestamp(), "yyyy-MM-dd HH:mm:ss", "2023-02-28 22:11:01");
    evalEquals(
            context, "FIELD", ZonedDateTime.of(2023, 2, 28, 22, 11, 1, 0, ZoneId.systemDefault()))
        .returnType(TimestampType.TIMESTAMP);
    evalEquals(context, "HOUR(FIELD)", 22L);
  }

  @Test
  void stringWithTrimType() throws Exception {
    // A string with a trim type is trimmed when read, it is not read as is
    ValueMetaString valueMeta = new ValueMetaString();
    valueMeta.setTrimType(IValueMeta.TRIM_TYPE_BOTH);
    IExpressionContext context = createContext(valueMeta, "  Paris  ");
    evalEquals(context, "FIELD", "Paris");
    evalEquals(context, "LENGTH(FIELD)", 5L);
  }

  @Test
  void numberField() throws Exception {
    // A number field is read as a double, converted to the expected class
    IExpressionContext context = createContext(new ValueMetaNumber(), 2.7D);
    evalEquals(context, "LEFT('ABCDEF', FIELD)", "AB");
    evalEquals(context, "CONCAT('x', FIELD)", "x2.7");
    evalEquals(context, "UPPER(FIELD)", "2.7");
    assertEquals(Boolean.TRUE, new Evaluator(context, "FIELD IS TRUE").eval());
  }

  @Test
  void timestampFieldWithNanoseconds() throws Exception {
    IExpressionContext context =
        createContext(new ValueMetaTimestamp(), Timestamp.valueOf("2023-02-28 22:11:01.123456789"));
    evalEquals(context, "EXTRACT(NANOSECOND FROM FIELD)", 123456789L);
    evalEquals(
        context,
        "FIELD",
        ZonedDateTime.of(2023, 2, 28, 22, 11, 1, 123456789, ZoneId.systemDefault()));
  }

  @Test
  void errors() throws Exception {
    // Without row, the value of a field can't be read
    RowExpressionContext context = createContext(new ValueMetaInteger(), 1L);
    context.setRow(null);
    Evaluator evaluator = new Evaluator(context, "FIELD + 1");
    ExpressionException exception = assertThrows(ExpressionException.class, evaluator::eval);
    assertEquals(ErrorCode.CONTEXT_ERROR, exception.getErrorCode());
    evaluator = new Evaluator(context, "FIELD");
    exception = assertThrows(ExpressionException.class, evaluator::eval);
    assertEquals(ErrorCode.CONTEXT_ERROR, exception.getErrorCode());

    // The error of the conversion of a value read from the value meta is kept
    context = createBinaryStringContext(new ValueMetaBigNumber(), null, "1E30");
    evaluator = new Evaluator(context, "LEFT('ABCDEF', FIELD)");
    exception = assertThrows(ExpressionException.class, evaluator::eval);
    assertEquals(ErrorCode.INTEGER_CONVERSION_OVERFLOW, exception.getErrorCode());

    // A value that can't be converted to the type of the field, the reason of the value meta is
    // kept
    context = createContext(new ValueMetaInteger(), "abc");
    evaluator = new Evaluator(context, "FIELD");
    exception = assertThrows(ExpressionException.class, evaluator::eval);
    assertEquals(ErrorCode.CONVERSION_ERROR, exception.getErrorCode());
    assertNotNull(exception.getCause());
    evaluator = new Evaluator(context, "FIELD + 1");
    exception = assertThrows(ExpressionException.class, evaluator::eval);
    assertEquals(ErrorCode.CONVERSION_ERROR, exception.getErrorCode());
  }
}
