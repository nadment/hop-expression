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
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

import java.time.LocalDate;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.TypeName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.parallel.Execution;

@TestInstance(PER_CLASS)
@Execution(CONCURRENT)
public class UserDefinedFunctionTest extends ExpressionTest {

  @Test
  void testString() throws Exception {
    UserDefinedFunctionMeta meta = new UserDefinedFunctionMeta();
    meta.setName("UCASE");
    meta.setDescription("UDF test");
    meta.setSource("Case when v0 is null then '*' else Left(Upper(v0),v1) end");
    meta.getArguments().add(new FunctionArgument("v0", TypeName.STRING));
    meta.getArguments().add(new FunctionArgument("v1", TypeName.INTEGER));

    assertEquals("UCASE", meta.getName());
    assertEquals("UDF test", meta.getDescription());
    assertEquals("v0", meta.getArguments().getFirst().getName());

    UserDefinedFunction udf = new UserDefinedFunction(meta);
    FunctionRegistry.register(udf.getName(), udf);

    evalEquals("UCASE('abcd',3)", "ABC").returnType(StringType.STRING_NOT_NULL);
    evalEquals("UCASE(NULL_STRING,2)", "*").returnType(StringType.STRING);
    evalFails("UCASE()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("UCASE(1,2,3)", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void testDate() throws Exception {
    UserDefinedFunctionMeta meta = new UserDefinedFunctionMeta();
    meta.setName("DATE_FROM_ID");
    meta.setSource("case when v0 is NULL then null else TO_DATE(TO_CHAR(v0),'YYYYMMDD') end");
    meta.getArguments().add(new FunctionArgument("v0", TypeName.INTEGER));

    UserDefinedFunction udf = new UserDefinedFunction(meta);
    FunctionRegistry.register(udf.getName(), udf);

    // The NULL result is removed, the argument being not NULL
    evalEquals("DATE_FROM_ID(20230105)", LocalDate.of(2023, 1, 5))
        .returnType(DateType.DATE_NOT_NULL);
    evalNull("DATE_FROM_ID(NULLIF(1,1))").returnType(DateType.DATE);
    evalFails("DATE_FROM_ID()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("DATE_FROM_ID(1,2,3)", ErrorCode.TOO_MANY_ARGUMENTS);

    assertThrows(ExpressionException.class, () -> optimize("DATE_FROM_ID()"));
  }

  @Test
  void testCompilationError() {
    UserDefinedFunctionMeta meta = new UserDefinedFunctionMeta();
    meta.setName("ERROR_UDF");
    meta.setSource("case when v1 is NULL then null else TO_DATE(TO_CHAR(v0),'YYYYMMDD') end");
    meta.getArguments().add(new FunctionArgument("v0", TypeName.INTEGER));
    UserDefinedFunction udf = new UserDefinedFunction(meta);
    FunctionRegistry.register(udf.getName(), udf);

    ExpressionException exception =
        assertThrows(ExpressionException.class, () -> optimize("ERROR_UDF(20230105)"));
    assertEquals(ErrorCode.UDF_COMPILATION_ERROR, exception.getErrorCode());
    // The message gives the cause of the error
    assertTrue(exception.getMessage().contains(ErrorCode.UNRESOLVED_IDENTIFIER.message("v1")));
  }

  @Test
  void testNameIgnoreCase() throws Exception {
    UserDefinedFunctionMeta meta = new UserDefinedFunctionMeta();
    meta.setName("add_one");
    meta.setSource("v0+1");
    meta.getArguments().add(new FunctionArgument("v0", TypeName.INTEGER));
    FunctionRegistry.register(meta.getName(), new UserDefinedFunction(meta));

    evalEquals("ADD_ONE(1)", 2L);
    evalEquals("add_one(1)", 2L);

    assertEquals("add_one", FunctionRegistry.unregister("Add_One").getName());
    evalFails("ADD_ONE(1)", ErrorCode.FUNCTION_DOES_NOT_EXIST);
  }

  @Test
  void testLambda() throws Exception {
    UserDefinedFunctionMeta meta = new UserDefinedFunctionMeta();
    meta.setName("SUFFIX_ALL");
    // The lambda parameter is not an argument, the argument v1 is used in the body
    meta.setSource("ARRAY_TO_STRING(TRANSFORM(STRING_TO_ARRAY(v0,','),x->UPPER(x)||v1),',')");
    meta.getArguments().add(new FunctionArgument("v0", TypeName.STRING));
    meta.getArguments().add(new FunctionArgument("v1", TypeName.STRING));
    FunctionRegistry.register(meta.getName(), new UserDefinedFunction(meta));

    evalEquals("SUFFIX_ALL('a,b','!')", "A!,B!");
    evalEquals("SUFFIX_ALL(FIELD_STRING,'!')", "TEST!");

    FunctionRegistry.unregister(meta.getName());
  }

  @Test
  void testArray() throws Exception {
    UserDefinedFunctionMeta meta = new UserDefinedFunctionMeta();
    meta.setName("ADD_ALL");
    meta.setSource("TRANSFORM([1,2],x->x+v0)");
    meta.getArguments().add(new FunctionArgument("v0", TypeName.INTEGER));
    FunctionRegistry.register(meta.getName(), new UserDefinedFunction(meta));

    // A function returning an array is the operand of an array function or operator
    evalEquals("ADD_ALL(1)[1]", 2L);
    evalEquals("ADD_ALL(FIELD_INTEGER)[2]", 42L);
    evalEquals("CARDINALITY(ADD_ALL(FIELD_INTEGER))", 2L);
    evalEquals("ARRAY_TO_STRING(TRANSFORM(ADD_ALL(FIELD_INTEGER),x->x*2),',')", "82,84");
    evalEquals("ARRAY_TO_STRING(ARRAY_APPEND(ADD_ALL(FIELD_INTEGER),5),',')", "41,42,5");
    evalEquals("ARRAY_TO_STRING(ARRAY_SORT(ADD_ALL(FIELD_INTEGER),FALSE),',')", "42,41");
    evalEquals("ARRAY_TO_STRING(ADD_ALL(ADD_ALL(FIELD_INTEGER)[2]),',')", "43,44");
    evalEquals("ARRAY_TO_STRING(TRANSFORM([10,FIELD_INTEGER],y->ADD_ALL(y)[1]),',')", "11,41");

    FunctionRegistry.unregister(meta.getName());
  }
}
