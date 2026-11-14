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
import org.apache.hop.expression.Array;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionTest;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.JsonType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.util.JsonConversion;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.parallel.Execution;

@TestInstance(PER_CLASS)
@Execution(CONCURRENT)
class JsonFunctionTest extends ExpressionTest {

  @Test
  void Json_Value() throws Exception {
    // dot–notation
    evalEquals("Json_Value(FIELD_JSON, '$.store.book[0].title')", "Sayings of the Century");

    // evalEquals("Json_Value('[{a:100}, {b:200}, {c:300}]', '$[*].c')", "300");

    // embedded in array (won't work in transformation after variables resolution because $[0] is
    // translated)
    // https://hop.apache.org/manual/latest/variables.html#_hexadecimal_values
    evalEquals(
        "JSON_VALUE('[ { \"id\":3902113,\"similarity\":0.95 } ]','$[0].similarity')", "0.95");

    // bracket–notation
    evalEquals(
        "Json_Value(FIELD_JSON, '$[''store''][''book''][0][''title'']')", "Sayings of the Century");

    // boolean type
    evalFalse("Json_Value('{\"a\":[true, false, true, false]}', '$.a[1]' RETURNING BOOLEAN)");
    evalTrue("Json_Value('{\"a\":[true, false, true, false]}', '$.a[2]' RETURNING BOOLEAN)");

    // numeric type
    evalEquals("Json_Value('{\"name\":\"Smith\", \"age\":29}','$.age' RETURNING INTEGER)", 29L);
    evalEquals("Json_Value('{\"a\":[5, 10, 15, 20]}', '$.a[2]' RETURNING INTEGER)", 15L);
    evalEquals(
        "Json_Value(FIELD_JSON, '$.store.book[0].price' RETURNING NUMBER)", new BigDecimal("8.95"));

    // date type
    // evalEquals("Json_Value('{\"name\":\"Smith\", \"createTime\":\"2024-06-14
    // 13:07:21\"}','$.createTime' RETURNING DATE)", LocalDateTime.of(2024,6,14,13,7,21));

    // TODO: Syntax of the special characters $[01] or $[6F,FF,00,1F] in variable resolution not
    // compatible with JsonPath array
    // evalEquals("Json_Value('{\"a\":[5, 10, 15, 20]}', '$[''a''][2]')", 15L);
    // evalEquals("Json_Value('{\"name\":\"Smith\", \"age\":29}','$[''name'']')", "Smith");
    // evalEquals("Json_Value('[0, 1, 2, 3]', '$[1]')", 1L);
    // evalEquals("Json_Value('[{\"a\":100}, {\"a\":200}, {\"a\":300}]', '$[1].a')", 200L);

    // JSON 'null' should return a NULL value
    evalNull("Json_Value('{\"name\":\"Smith\", \"age\":29, \"department\":null}','$.department')");

    // Support JSON without field name quotes
    evalEquals("Json_Value('{name:\"Smith\", age:29}','$.name')", "Smith");

    // Support JSON function, the returning type also applies to its result
    evalEquals("Json_Value(FIELD_JSON, '$..book.length()' RETURNING INTEGER)", 4L);
    evalEquals("Json_Value(FIELD_JSON, '$..book.length()')", "4");

    // Return NULL from NULL value
    evalNull("Json_Value(NULL_STRING,'$.name')");
    evalNull("Json_Value(NULL_JSON,'$.name')");

    // Return NULL if the JSON path does not match a value
    evalNull("Json_Value(FIELD_JSON,'$.notexist')");

    // Return NULL if the JSON path is null
    evalNull("Json_Value(FIELD_JSON,NULL_STRING)");

    evalFails("Json_Value(FIELD_JSON)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Json_Value(FIELD_DATE,'$')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Json_Value(FIELD_INTEGER,'$')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Json_Value(FIELD_JSON,'$$.invalid')", ErrorCode.INVALID_JSON_PATH);
    evalFails("Json_Value(FIELD_JSON,'$.notexist' RETURNING INET)", ErrorCode.UNEXPECTED_DATA_TYPE);

    // A path which is not constant is compiled at evaluation
    evalEquals(
        "Json_Value(FIELD_JSON, NVL(NULL_STRING,'$.store.book[0].title'))",
        "Sayings of the Century");
    evalFails("Json_Value(FIELD_JSON,NVL(NULL_STRING,'$$.invalid'))", ErrorCode.INVALID_JSON_PATH);
  }

  @Test
  void Json_Path_Validation() throws Exception {
    // A literal path that is not valid is rejected when validated, not when a row is evaluated
    validateFails("JSON_VALUE(FIELD_JSON,'$$.invalid')", ErrorCode.INVALID_JSON_PATH);
    validateFails(
        "JSON_VALUE(FIELD_JSON,'$$.invalid' RETURNING INTEGER)", ErrorCode.INVALID_JSON_PATH);
    validateFails("JSON_VALUE(FIELD_JSON,'')", ErrorCode.INVALID_JSON_PATH);
    validateFails("JSON_QUERY(FIELD_JSON,'$$.invalid')", ErrorCode.INVALID_JSON_PATH);
    validateFails("JSON_KEYS(FIELD_JSON,'$$.invalid')", ErrorCode.INVALID_JSON_PATH);

    // The position of the call
    validateFails("1 = JSON_VALUE(FIELD_JSON,'$$.invalid')", ErrorCode.INVALID_JSON_PATH, 4);
  }

  @Test
  void Json_Value_Unparse() throws Exception {
    // The returning type is written with its clause, so the text can be parsed again
    optimize("JSON_VALUE(FIELD_JSON,'$.a')", "JSON_VALUE(FIELD_JSON,'$.a' RETURNING STRING)");
    optimize("JSON_VALUE(FIELD_JSON,'$.a' RETURNING STRING)");
    optimize("JSON_VALUE(FIELD_JSON,'$.a' RETURNING INTEGER)");
    optimize("JSON_VALUE(FIELD_JSON,'$.a' RETURNING NUMBER(10,2))");
    optimize("JSON_VALUE(FIELD_JSON,FIELD_STRING RETURNING BOOLEAN)");
  }

  @Test
  void Json_Value_NotScalar() throws Exception {
    // An object or an array is not a value, it is returned by JSON_QUERY
    evalNull("JSON_VALUE('{\"a\":{\"b\":1}}','$.a')");
    evalNull("JSON_VALUE('{\"a\":[1,2]}','$.a')");
    evalNull("JSON_VALUE('{\"a\":{\"b\":1}}','$.a' RETURNING INTEGER)");
    evalNull("JSON_VALUE('{\"a\":[1]}','$.a' RETURNING BOOLEAN)");
    evalNull("JSON_VALUE('{\"a\":1}','$')");

    // A path with several results gives an array, even with a single result
    evalNull("JSON_VALUE('[{\"a\":1},{\"a\":2}]','$..a')");
    evalNull("JSON_VALUE('[{\"a\":1},{\"a\":2}]','$[*].a')");
    evalNull("JSON_VALUE('[{\"a\":1},{\"a\":2}]','$[?(@.a>1)].a')");
    evalNull("JSON_VALUE('{\"a\":1,\"b\":2}','$.keys()')");

    // An element of an array or a root scalar is a value
    evalEquals("JSON_VALUE('{\"a\":[1,2]}','$.a[1]')", "2");
    evalEquals("JSON_VALUE('\"x\"','$')", "x");
  }

  @Test
  void Json_Value_Returning() throws Exception {
    // A string is the text of the scalar
    evalEquals("JSON_VALUE('{\"a\":true}','$.a')", "true");
    evalEquals("JSON_VALUE('{\"a\":1.50}','$.a')", "1.50");
    evalEquals("JSON_VALUE('[1,2,3]','$.sum()')", "6.0");

    // The scalar is converted to the returning type like a cast
    evalEquals("JSON_VALUE('{\"a\":\"12.5\"}','$.a' RETURNING NUMBER)", new BigDecimal("12.5"));
    evalEquals("JSON_VALUE('{\"a\":true}','$.a' RETURNING NUMBER)", BigDecimal.ONE);
    evalEquals("JSON_VALUE('{\"a\":\"12\"}','$.a' RETURNING INTEGER)", 12L);
    evalEquals("JSON_VALUE('{\"a\":true}','$.a' RETURNING INTEGER)", 1L);
    evalEquals("JSON_VALUE('{\"a\":1.9}','$.a' RETURNING INTEGER)", 2L);
    evalEquals("JSON_VALUE('{\"a\":-1.9}','$.a' RETURNING INTEGER)", -2L);
    evalEquals(
        "JSON_VALUE('{\"a\":12345678901234567890}','$.a' RETURNING NUMBER)",
        new BigDecimal("12345678901234567890"));
    evalTrue("JSON_VALUE('{\"a\":\"true\"}','$.a' RETURNING BOOLEAN)");
    evalTrue("JSON_VALUE('{\"a\":\"yes\"}','$.a' RETURNING BOOLEAN)");
    evalTrue("JSON_VALUE('{\"a\":1}','$.a' RETURNING BOOLEAN)");
    evalFalse("JSON_VALUE('{\"a\":0}','$.a' RETURNING BOOLEAN)");
    evalNull("JSON_VALUE('{\"a\":null}','$.a' RETURNING INTEGER)");

    // An impossible conversion raises an error
    evalFails("JSON_VALUE('{\"a\":\"abc\"}','$.a' RETURNING NUMBER)", ErrorCode.CONVERSION_ERROR);
    evalFails("JSON_VALUE('{\"a\":\"abc\"}','$.a' RETURNING INTEGER)", ErrorCode.CONVERSION_ERROR);
    evalFails("JSON_VALUE('{\"a\":\"abc\"}','$.a' RETURNING BOOLEAN)", ErrorCode.CONVERSION_ERROR);
    evalFails(
        "JSON_VALUE('{\"a\":12345678901234567890}','$.a' RETURNING INTEGER)",
        ErrorCode.INTEGER_CONVERSION_OVERFLOW);

    // The precision and the scale of the returning type are applied
    evalEquals("JSON_VALUE('{\"a\":\"xyz\"}','$.a' RETURNING STRING(1))", "x");
    evalEquals(
        "JSON_VALUE('{\"a\":123.456}','$.a' RETURNING NUMBER(5,1))", new BigDecimal("123.5"));
    evalFails(
        "JSON_VALUE('{\"a\":123456.7}','$.a' RETURNING NUMBER(5,1))",
        ErrorCode.NUMBER_CONVERSION_OVERFLOW);
  }

  @Test
  void Json_Query_PathFunction() throws Exception {
    // A JsonPath function returns a value which is not a JSON node
    evalEquals("JSON_QUERY('[1,2,3]','$.length()')", JsonConversion.convert("3"));
    evalEquals("JSON_QUERY('[1,2,3]','$.sum()')", JsonConversion.convert("6.0"));
    evalEquals(
        "JSON_QUERY('{\"a\":1,\"b\":2}','$.keys()')", JsonConversion.convert("[\"a\",\"b\"]"));
    evalEquals("CAST(JSON_QUERY('[1,2,3]','$.length()') AS STRING)", "3");
  }

  @Test
  void Json_DecimalNumber() throws Exception {
    // A decimal number is read as a decimal, not as a double which would lose digits
    evalEquals(
        "CAST(TO_JSON('{\"a\":12345678901234567890.123456789}') AS STRING)",
        "{\"a\":12345678901234567890.123456789}");
    evalEquals(
        "JSON_VALUE('{\"a\":12345678901234567890.123456789}','$.a' RETURNING NUMBER)",
        new BigDecimal("12345678901234567890.123456789"));
    evalEquals("CAST(JSON '{\"a\":0.1}' AS STRING)", "{\"a\":0.1}");

    // The trailing zeros are kept
    evalEquals("CAST(JSON '{\"a\":1.10}' AS STRING)", "{\"a\":1.10}");
    evalEquals("CAST(JSON '{\"a\":5.0}' AS STRING)", "{\"a\":5.0}");

    // A number with an exponent, also beyond the range of a double
    evalEquals("JSON_VALUE('{\"a\":1E2}','$.a' RETURNING NUMBER)", new BigDecimal("100"));
    evalEquals("CAST(TO_JSON('1e400') AS STRING)", "1E+400");
    evalEquals("JSON_VALUE('{\"a\":1e400}','$.a')", "1E+400");
    // Beyond the precision of a NUMBER
    evalFails(
        "JSON_VALUE('{\"a\":1e400}','$.a' RETURNING NUMBER)", ErrorCode.NUMBER_CONVERSION_OVERFLOW);
  }

  @Test
  void Json_NumberEquality() throws Exception {
    // The numbers are compared as decimals, a double cannot hold exactly a large integer
    evalFalse("JSON '9007199254740993' = JSON '9007199254740992'");
    evalFalse("JSON '12345678901234567890' = JSON '12345678901234567891'");
    evalTrue("JSON '12345678901234567890' = JSON '12345678901234567890.0'");
    evalFalse("JSON '0.1' = JSON '0.10000000000000000001'");

    // An integer and a decimal with the same value are equal
    evalTrue("JSON '{\"a\":1}' = JSON '{\"a\":1.0}'");
    evalTrue("JSON '0.1' = JSON '0.10'");
    evalTrue("JSON '1e2' = JSON '100'");
    evalTrue("JSON '[1,2.50]' = JSON '[1.0,2.5]'");
  }

  @Test
  void Json_Query() throws Exception {
    // No JSON path
    evalEquals(
            "Json_Query('{name:\"Smith\",age:29}'::JSON)",
            JsonConversion.convert("{name:\"Smith\",age:29}"))
        .returnType(JsonType.JSON_NOT_NULL);

    // Root JSON path
    evalEquals(
        "Json_Query('{Suspect:{Name:\"Smith\",Hobbies:[\"Eating\",\"Sleeping\",\"Base Jumping\"]}}'::JSON,'$.Suspect.Hobbies')",
        JsonConversion.convert("[\"Eating\", \"Sleeping\", \"Base Jumping\"]"));
    evalEquals("Json_Query('null'::JSON,'$')", JsonConversion.convert("null"));

    // Coerce a JSON-formatted string to JSON
    evalEquals(
        "Json_Query('{\"language\":[\"English\", \"French\"]}','$.language')",
        JsonConversion.convert("[\"English\",\"French\"]"));
    evalEquals("Json_Query('{name:\"Smith\"}')", JsonConversion.convert("{name:\"Smith\"}"));
    evalNull("Json_Query(NULL_STRING,'$')");

    // Wildcard all elements
    evalEquals(
        "Json_Query(FIELD_JSON, '$.store.book[*].author')",
        JsonConversion.convert(
            "[\"Nigel Rees\",\"Evelyn Waugh\",\"Herman Melville\",\"J. R. R. Tolkien\"]"));

    // Child property at any level deeper
    evalEquals(
        "Json_Query(FIELD_JSON, '$..author')",
        JsonConversion.convert(
            "[\"Nigel Rees\",\"Evelyn Waugh\",\"Herman Melville\",\"J. R. R. Tolkien\"]"));

    // Array indexes
    evalEquals(
        "Json_Query(FIELD_JSON, '$.store.book[2].title')", JsonConversion.convert("\"Moby Dick\""));
    evalEquals(
        "Json_Query(FIELD_JSON, '$.store.book[0,1,2].title')",
        JsonConversion.convert("[\"Sayings of the Century\",\"Sword of Honour\",\"Moby Dick\"]"));

    // Array slice
    evalEquals(
        "Json_Query(FIELD_JSON, '$.store.book[0:2].title')",
        JsonConversion.convert("[\"Sayings of the Century\",\"Sword of Honour\"]"));

    // Filter predicate
    evalEquals(
        "Json_Query(FIELD_JSON, '$..book[?(@.isbn)].title')",
        JsonConversion.convert("[\"Moby Dick\",\"The Lord of the Rings\"]"));
    evalEquals(
        "Json_Query(FIELD_JSON, '$..book[?(@.price<10)].price')",
        JsonConversion.convert("[8.95,8.99]"));

    // Null handling
    evalNull("Json_Query(NULL_JSON,'$')").returnType(JsonType.JSON);
    evalNull("Json_Query(FIELD_JSON,NULL_STRING)").returnType(JsonType.JSON);
    evalNull("Json_Query(FIELD_JSON,'$.notexist')").returnType(JsonType.JSON);

    // Check operands
    evalFails("Json_Query()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Json_Query(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails(
        "Json_Query(JSON '{\"name\":\"Smith\", \"age\":29}','$$.invalid')",
        ErrorCode.INVALID_JSON_PATH);

    // A path which is not constant is compiled at evaluation
    evalEquals(
        "Json_Query(FIELD_JSON, NVL(NULL_STRING,'$.store.book[2].title'))",
        JsonConversion.convert("\"Moby Dick\""));
    evalFails("Json_Query(FIELD_JSON,NVL(NULL_STRING,'$$.invalid'))", ErrorCode.INVALID_JSON_PATH);
  }

  @Test
  void Json_Object() throws Exception {
    evalEquals(
            "Json_Object(KEY 'name' VALUE 'Smith')", JsonConversion.convert("{\"name\":\"Smith\"}"))
        .returnType(JsonType.JSON_NOT_NULL);
    evalEquals(
        "Json_Object(KEY 'name' VALUE 'Smith', KEY 'langue' VALUE 'english')",
        JsonConversion.convert("{\"name\":\"Smith\",\"langue\":\"english\"}"));

    // Support null
    evalEquals(
        "Json_Object(KEY 'name' VALUE 'Smith', KEY 'empty' VALUE null)",
        JsonConversion.convert("{\"name\":\"Smith\",\"empty\":null}"));

    // Support duplicate key
    evalEquals(
        "Json_Object(KEY 'name' VALUE 'Smith', KEY 'name' VALUE 'John')",
        JsonConversion.convert("{\"name\":\"Smith\", \"name\":\"John\"}"));

    // Accept optional syntax with missing KEY
    evalEquals("Json_Object('name' VALUE 'Smith')", JsonConversion.convert("{\"name\":\"Smith\"}"));

    evalFails("Json_Object()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Json_Object(", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("Json_Object(KEY)", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("Json_Object(KEY 'name' VALUE )", ErrorCode.SYNTAX_ERROR);
    evalFails("Json_Object(KEY 'name' VALUE 3,)", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("Json_Object(KEY VALUE 'Smith')", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("Json_Object(KEY 'name' VALUE 'Smith'", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    optimize(
        "JSON_OBJECT(KEY 'name' VALUE 'Smith',KEY 'langue' VALUE 'english')",
        "JSON '{\"name\":\"Smith\",\"langue\":\"english\"}'");
    optimize("JSON_OBJECT(KEY 'name' VALUE FIELD_STRING,KEY 'langue' VALUE 'english')");
  }

  @Test
  void Json_Object_ValueExpression() throws Exception {
    // The value is an expression, not only a term
    evalTrue("Json_Object(KEY 'a' VALUE 1+1) = JSON '{\"a\":2}'");
    evalEquals(
        "Json_Object(KEY 'a' VALUE 'x'||'y', KEY 'b' VALUE 2*3 > 5)",
        JsonConversion.convert("{\"a\":\"xy\",\"b\":true}"));
    evalEquals(
        "Json_Object(KEY 'a' VALUE Upper(FIELD_STRING)||'!')",
        JsonConversion.convert("{\"a\":\"TEST!\"}"));

    optimize("JSON_OBJECT(KEY 'a' VALUE 1+FIELD_INTEGER)");
  }

  @Test
  void Json_Object_KeyExpression() throws Exception {
    // The key is a string expression
    evalTrue("Json_Object(KEY 'a'||'b' VALUE 1) = JSON '{\"ab\":1}'");
    evalTrue("Json_Object(KEY FIELD_STRING VALUE 1) = JSON '{\"TEST\":1}'");
    evalTrue("Json_Object(Lower(FIELD_STRING) VALUE 1) = JSON '{\"test\":1}'");

    // A key that is not a string is rejected, it was taken as the text of the key
    evalFails("Json_Object(KEY 1 VALUE 1)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Json_Object(KEY FIELD_INTEGER VALUE 1)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Json_Object(KEY NULL VALUE 1)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);

    // A key cannot be NULL
    evalFails("Json_Object(KEY NULL_STRING VALUE 1)", ErrorCode.NULL_JSON_KEY);

    optimize("JSON_OBJECT(KEY FIELD_STRING VALUE 1)");
  }

  @Test
  void Json_Keys() throws Exception {
    // No JSON path
    evalEquals(
        "Json_Keys(JSON '{\"a\": 1, \"b\": {\"c\": 30}}')",
        new Array(ArrayType.of(StringType.STRING), Literal.of("a"), Literal.of("b")));

    // Root JSON path
    evalEquals(
        "Json_Keys(JSON '{\"a\": 1, \"b\": {\"c\": 30}}','$')",
        new Array(ArrayType.of(StringType.STRING), Literal.of("a"), Literal.of("b")));

    evalEquals(
        "Json_Keys(JSON '{\"a\": 1, \"b\": {\"c\": 30}}','$.b')",
        new Array(ArrayType.of(StringType.STRING), Literal.of("c")));

    // Coerce a JSON-formatted string to JSON
    evalEquals(
        "Json_Keys('{\"a\": 1, \"b\": {\"c\": 30}}')",
        new Array(ArrayType.of(StringType.STRING), Literal.of("a"), Literal.of("b")));
    evalEquals(
        "Json_Keys('{\"a\": 1, \"b\": {\"c\": 30}}','$.b')",
        new Array(ArrayType.of(StringType.STRING), Literal.of("c")));

    // If the JSON object has no keys, an empty array is returned.
    evalEquals("Json_Keys(JSON '{}')", new Array(ArrayType.of(StringType.STRING)));

    // Keys are de-duplicated and returned in alphabetical order.
    evalEquals(
        "Json_Keys(JSON '{\"b\": 1, \"a\": 1, \"c\": 30, \"a\": 1}')",
        new Array(
            ArrayType.of(StringType.STRING), Literal.of("a"), Literal.of("b"), Literal.of("c")));

    // Null handling
    evalNull("Json_Keys(NULL_JSON)").returnType(ArrayType.of(StringType.STRING));
    evalNull("Json_Keys(NULL_STRING)").returnType(ArrayType.of(StringType.STRING));
    evalNull("Json_Keys(JSON '{\"a\": 1, \"b\": {\"c\": 30}}',NULL_STRING)");

    // Returns `NULL` if the given path does not locate an object
    evalNull("Json_Keys(JSON '{\"a\": 1, \"b\": {\"c\": 30}}','$.c')");

    // Returns `NULL` if the path or the JSON value does not locate an object
    evalNull("Json_Keys(JSON '{\"a\": 1, \"b\": {\"c\": 30}}','$.a')");
    evalNull("Json_Keys(JSON '[1,2]')");

    // Check operands
    evalFails("Json_Keys()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Json_Keys(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails(
        "Json_Keys(JSON '{\"name\":\"Smith\", \"age\":29}','$$.invalid')",
        ErrorCode.INVALID_JSON_PATH);

    // A path which is not constant is compiled at evaluation
    evalEquals(
        "Json_Keys(JSON '{\"a\": 1, \"b\": {\"c\": 30}}',NVL(NULL_STRING,'$.b'))",
        new Array(ArrayType.of(StringType.STRING), Literal.of("c")));
    evalFails(
        "Json_Keys(JSON '{\"a\": 1}',NVL(NULL_STRING,'$$.invalid'))", ErrorCode.INVALID_JSON_PATH);
  }

  @Test
  void Json_Keys_AlphabeticalOrder() throws Exception {
    // The order does not depend on the order of the keys in the object, an upper case letter
    // comes before a lower case letter
    evalEquals(
        "Json_Keys(JSON '{\"zebra\":1,\"apple\":2,\"Mango\":3,\"key10\":4,\"key2\":5,\"banana\":6}')",
        new Array(
            ArrayType.of(StringType.STRING),
            Literal.of("Mango"),
            Literal.of("apple"),
            Literal.of("banana"),
            Literal.of("key10"),
            Literal.of("key2"),
            Literal.of("zebra")));
  }

  @Test
  void debug() throws Exception {
    // evalEquals("Json_Object(KEY 'updated' VALUE 362180698)",
    // JsonConversion.convert("{\"updated\":362180698}"));
  }
}
