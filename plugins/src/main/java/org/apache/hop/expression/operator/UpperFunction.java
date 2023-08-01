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
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.util.Locale;

/**
 * The function convert a string value to upper case.
 * 
 * @See {@link LowerFunction}, {@link InitCapFunction}
 */
@FunctionPlugin
public class UpperFunction extends Function {

  public static final UpperFunction INSTANCE = new UpperFunction();
  
  public UpperFunction() {
    super("UPPER", ReturnTypes.STRING, OperandTypes.STRING, Category.STRING, "/docs/upper.html");
  }

  @Override
    public Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null)
      return null;
    return value.toUpperCase(Locale.getDefault());
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression operand = call.getOperand(0);
    
    // Repetitions of functions that do not have any effects on the result
    if (operand.is(call.getOperator()) || operand.is(LowerFunction.INSTANCE)
        || operand.is(InitCapFunction.INSTANCE)) {
      return new Call(call.getOperator(), operand.asCall().getOperand(0));
    }

    return call;
  }
}
