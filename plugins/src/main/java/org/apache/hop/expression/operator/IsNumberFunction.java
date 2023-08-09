/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression.operator;

import org.apache.hop.expression.Call;
import org.apache.hop.expression.Category;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.TypeFamily;
import org.apache.hop.expression.util.NumberFormat;


/**
 * Check if a string is a valid number.
 */
@FunctionPlugin
public class IsNumberFunction extends Function {

  private static final NumberFormat FORMAT = NumberFormat.of("TM");

  public IsNumberFunction() {
    super("IS_NUMBER", ReturnTypes.BOOLEAN, OperandTypes.ANY,
        Category.COMPARISON, "/docs/is_number.html");
  }

  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {

    if ( call.getOperand(0).getType().isSameFamily(TypeFamily.STRING) ) {
      return call;
    }
    
    // Optimize "IS_NUMBER(n)" to "n IS NOT NULL"
    if ( call.getOperand(0).getType().isSameFamily(TypeFamily.NUMERIC) ) {
      return new Call(Operators.IS_NOT_NULL, call.getOperand(0));
    }
    
    // Other data type are always false
    return Literal.FALSE;
  }
  
  @Override
  public Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null)
      return Boolean.FALSE;
    
    try {
      FORMAT.parse(value);
      return Boolean.TRUE;
    } catch (Exception e) {
      return Boolean.FALSE;
    }
  }
}
