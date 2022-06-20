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
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.expression.Argument;
import org.apache.hop.expression.DataTypeName;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.Udf;
import org.apache.hop.expression.UdfMeta;
import org.junit.Test;

public class UdfTest extends BaseExpressionTest {
 
  @Test
  public void test() throws Exception {
      UdfMeta meta = new UdfMeta();
      meta.setName("UCASE");
      meta.setDescription("UDF test");
      meta.setSource("If(v0=null,'<null>',Upper(v0))");
      meta.getArguments().add(new Argument("v0", DataTypeName.STRING));
    
      assertEquals("UCASE", meta.getName());
      assertEquals("UDF test", meta.getDescription());
      
      Udf udf = new Udf(meta.getName(), 1);
      
      FunctionRegistry.register(meta.getName(), udf);
      
      
      
      // Convert arguments to row meta
//      IRowMeta rowMeta = new RowMeta();
//      for (Argument argument:udfMeta.getArguments() ) {
//        IValueMeta vm = createValueMeta(argument.getType(), argument.getName());
//        rowMeta.addValueMeta(vm);
//      }
//      
//      ExpressionContext context = new ExpressionContext(variables, rowMeta);          
//      IExpression expression = ExpressionBuilder.compile(context, udfMeta.getSource());
      
      
     // evalEquals("UCASE('True')", "TRUE");
      //evalEquals("UCASE(null)", "<null>");
  }
}