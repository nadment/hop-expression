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

import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;

/** Returns an array concatenating an element at the end of an array. */
@FunctionPlugin
public class ArrayAppendFunction extends Function {

  public static final Function INSTANCE = new ArrayAppendFunction();

  public ArrayAppendFunction() {
    super(
        "ARRAY_APPEND",
        ReturnTypes.ARRAY,
        OperandTypes.ARRAY_ANY,
        OperatorCategory.ARRAY,
        "/docs/array_append.html");
  }

  @Override
  public boolean checkOperandTypes(Call call) {
    boolean success = super.checkOperandTypes(call);
    if (success) {
      Type arrayType = call.getOperand(0).getType().getElementType();
      Type elementType = call.getOperand(1).getType();

      // Check if appended element is coercible to array element type
      success = elementType.isCoercible(arrayType);
    }

    return success;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    // If the operand is an array (not an operator)
    if (call.getOperand(0).is(Kind.ARRAY)) {
      return (Array) eval(call.getOperands());
    }
    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Array array = (Array) operands[0];
    IExpression element = operands[1];
    return array.append(element);
  }
}
