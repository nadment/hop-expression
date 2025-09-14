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
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionTest;
import org.apache.hop.expression.type.JsonType;
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

    // TODO: bracket–notation
    // evalEquals("Json_Value(FIELD_JSON, '$['store']['book'][0]['title']')", "Sayings of the
    // Century");

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

    // Json 'null' should return a NULL value
    evalNull("Json_Value('{\"name\":\"Smith\", \"age\":29, \"department\":null}','$.department')");

    // Support Json without field name quotes
    evalEquals("Json_Value('{name:\"Smith\", age:29}','$.name')", "Smith");

    // Support Json function
    evalEquals("Json_Value(FIELD_JSON, '$..book.length()' RETURNING INTEGER)", 4L);

    // Return NULL from NULL value
    evalNull("Json_Value(NULL_STRING,'$.name')");
    evalNull("Json_Value(NULL_JSON,'$.name')");

    // Return NULL if JsonPath does not match a value
    evalNull("Json_Value(FIELD_JSON,'$.notexist')");

    // Return NULL if json path is null
    evalNull("Json_Value(FIELD_JSON,NULL_STRING)");

    evalFails("Json_Value(FIELD_JSON)", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Json_Value(FIELD_JSON,'$.notexist' RETURNING INET)", ErrorCode.UNEXPECTED_DATA_TYPE);
  }

  @Test
  void Json_Query() throws Exception {
    // No Json path
    evalEquals(
            "Json_Query('{name:\"Smith\",age:29}'::JSON)",
            JsonConversion.convert("{name:\"Smith\",age:29}"))
        .returnType(JsonType.JSON_NOT_NULL);

    // Root JSON path
    evalEquals(
        "Json_Query('{Suspect:{Name:\"Smith\",Hobbies:[\"Eating\",\"Sleeping\",\"Base Jumping\"]}}'::JSON,'$.Suspect.Hobbies')",
        JsonConversion.convert("[\"Eating\", \"Sleeping\", \"Base Jumping\"]"));
    evalEquals("Json_Query('null'::JSON,'$')", JsonConversion.convert("null"));

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

    evalNull("Json_Query(NULL_JSON,'$')").returnType(JsonType.JSON);

    // Check operands
    evalFails(
        "Json_Query('{\"name\":\"Smith\", \"age\":29}',NULL_STRING)", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails(
        "Json_Query('{\"name\":\"Smith\", \"age\":29}','$.notexist')", ErrorCode.ILLEGAL_ARGUMENT);
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

    // Accept optional missing KEY
    evalEquals("Json_Object('name' VALUE 'Smith')", JsonConversion.convert("{\"name\":\"Smith\"}"));

    evalFails("Json_Object()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Json_Object(KEY)", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("Json_Object(KEY 'name' VALUE )", ErrorCode.SYNTAX_ERROR);
    evalFails("Json_Object(KEY 'name' VALUE 3,)", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("Json_Object(KEY VALUE 'Smith')", ErrorCode.SYNTAX_ERROR_FUNCTION);
    evalFails("Json_Object(", ErrorCode.MISSING_RIGHT_PARENTHESIS);
    evalFails("Json_Object(KEY 'name' VALUE 'Smith'", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    optimize(
        "JSON_OBJECT(KEY 'name' VALUE 'Smith',KEY 'langue' VALUE 'english')",
        "JSON '{\"name\":\"Smith\",\"langue\":\"english\"}'");
    optimize("JSON_OBJECT(KEY 'name' VALUE FIELD_STRING,KEY 'langue' VALUE 'english')");
  }
}
