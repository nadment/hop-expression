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
package org.apache.hop.core.expression;

import static org.junit.Assert.assertEquals;
import org.apache.hop.expression.Argument;
import org.apache.hop.expression.DataTypeName;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.UserDefinedFunction;
import org.apache.hop.expression.UserDefinedFunctionMeta;
import org.junit.Test;

public class UdfTest extends BaseExpressionTest {
 
  @Test
  public void test() throws Exception {
      UserDefinedFunctionMeta meta = new UserDefinedFunctionMeta();
      meta.setName("UCASE");
      meta.setDescription("UDF test");
      meta.setSource("Case when v0 is null then '*' else Left(Upper(v0),v1) end");
      meta.getArguments().add(new Argument("v0", DataTypeName.STRING));
      meta.getArguments().add(new Argument("v1", DataTypeName.INTEGER));
      
      //meta.getArguments().add(new Argument("v2", DataTypeName.DATE));
      //meta.getArguments().add(new Argument("v3", DataTypeName.BOOLEAN));
      //meta.getArguments().add(new Argument("v4", DataTypeName.NUMBER));
      //meta.getArguments().add(new Argument("v5", DataTypeName.BIGNUMBER));
    
      assertEquals("UCASE", meta.getName());
      assertEquals("UDF test", meta.getDescription());
      assertEquals("v0", meta.getArguments().get(0).getName());
      
      UserDefinedFunction udf = new UserDefinedFunction(meta);      
      FunctionRegistry.register(udf.getName(), udf);
      
      evalEquals("UCASE('abcd',3)", "ABC");
      evalEquals("UCASE(null,2)", "*");
  }
}