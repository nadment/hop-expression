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

import java.time.LocalDate;
import org.apache.hop.expression.type.TypeId;
import org.apache.hop.expression.type.Types;
import org.junit.jupiter.api.Test;

public class UserDefinedFunctionTest extends ExpressionTest {

  // @Test
  public void testString() throws Exception {
    UserDefinedFunctionMeta meta = new UserDefinedFunctionMeta();
    meta.setName("UCASE");
    meta.setDescription("UDF test");
    meta.setSource("Case when v0 is null then '*' else Left(Upper(v0),v1) end");
    meta.getArguments().add(new FunctionArgument("v0", TypeId.STRING));
    meta.getArguments().add(new FunctionArgument("v1", TypeId.INTEGER));

    assertEquals("UCASE", meta.getName());
    assertEquals("UDF test", meta.getDescription());
    assertEquals("v0", meta.getArguments().get(0).getName());

    UserDefinedFunction udf = new UserDefinedFunction(meta);
    FunctionRegistry.register(udf.getName(), udf);

    evalEquals("UCASE('abcd',3)", "ABC").returnType(Types.STRING);
    evalEquals("UCASE(NULL_STRING,2)", "*").returnType(Types.STRING);
    evalFails("UCASE()");
    evalFails("UCASE(1,2,3)");
  }

  @Test
  public void testDate() throws Exception {
    UserDefinedFunctionMeta meta = new UserDefinedFunctionMeta();
    meta.setName("DATE_FROM_ID");
    meta.setSource("case when v0 is NULL then null else TO_DATE(TO_CHAR(v0),'YYYYMMDD') end");
    meta.getArguments().add(new FunctionArgument("v0", TypeId.INTEGER));

    UserDefinedFunction udf = new UserDefinedFunction(meta);
    FunctionRegistry.register(udf.getName(), udf);

    evalEquals("DATE_FROM_ID(20230105)", LocalDate.of(2023, 1, 5)).returnType(Types.DATE);
    evalNull("DATE_FROM_ID(null)").returnType(Types.DATE);
    evalFails("DATE_FROM_ID()");
    evalFails("DATE_FROM_ID(1,2,3)");

    assertThrows(ExpressionException.class, () -> optimize("DATE_FROM_ID()"));
  }

  @Test
  public void testCompilationError() throws Exception {
    UserDefinedFunctionMeta meta = new UserDefinedFunctionMeta();
    meta.setName("ERROR_UDF");
    meta.setSource("case when v1 is NULL then null else TO_DATE(TO_CHAR(v0),'YYYYMMDD') end");
    meta.getArguments().add(new FunctionArgument("v0", TypeId.INTEGER));
    UserDefinedFunction udf = new UserDefinedFunction(meta);
    FunctionRegistry.register(udf.getName(), udf);

    assertThrows(ExpressionException.class, () -> optimize("ERROR_UDF(20230105)"));
  }
}
