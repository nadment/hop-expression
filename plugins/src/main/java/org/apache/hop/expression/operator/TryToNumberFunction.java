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
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.exception.ParseNumberException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.NumberFormat;

/**
 * Converts a string expression to a number value with optional format.
 */
@FunctionPlugin
public class TryToNumberFunction extends Function {
  
  private final NumberFormat format;
  
  public TryToNumberFunction() {
    this(null);
  }

  protected TryToNumberFunction(NumberFormat format) {
    super("TRY_TO_NUMBER", ReturnTypes.NUMBER_NULLABLE, OperandTypes.STRING.or(OperandTypes.STRING_TEXT),
        OperatorCategory.CONVERSION, "/docs/to_number.html");
    
    this.format = format;
  }
  
  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {

    // Already compiled
    if (format != null) {
      return call;
    }
    
    String pattern = "TM";
    // With specified format
    if (call.getOperandCount() == 2) {
      pattern = call.getOperand(1).getValue(String.class);
    }

    // Compile format to check it
    NumberFormat fmt = NumberFormat.of(pattern);

    return new Call(new TryToNumberFunction(fmt), call.getOperands());
  }
  
  @Override
  public Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null)
      return null;
  
    try {
      return format.parse(value);
    } catch (ParseNumberException e) {
      return null;
    }
  }
}
