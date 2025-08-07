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

import java.util.ArrayList;
import java.util.List;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.IOperandCountRange;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.OperandCountRange;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;

/**
 * Compares the select expression to each search expression in order. As soon as a search expression
 * matches the selection expression, the corresponding result expression is returned.
 */
@FunctionPlugin
public class DecodeFunction extends Function {

  public static final IOperandTypeChecker OTC = new DecodeFunctionOperandTypeChecker();

  public DecodeFunction() {
    super("DECODE", ReturnTypes.ARG2, OTC, OperatorCategory.CONDITIONAL, "/docs/decode.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Simplify unreachable DECODE clauses
    // DECODE(a, b, 1, c, 2, b, 3) → DECODE(a, b, 1, c, 2)
    // DECODE(a, b, 1, c, 2, b, 3, 4) → DECODE(a, b, 1, c, 2, 4)

    List<IExpression> operands = new ArrayList<>();
    List<IExpression> searchs = new ArrayList<>();

    int count = ((call.getOperandCount() - 1) / 2) * 2;

    operands.add(call.getOperand(0));

    for (int i = 1; i < count; i += 2) {
      IExpression search = call.getOperand(i);
      if (!searchs.contains(search)) {
        searchs.add(search);
        operands.add(search);
        operands.add(call.getOperand(i + 1));
      }
    }

    // Check type if function has a default value
    if ((call.getOperandCount() - 1) > count) {
      operands.add(call.getOperand(call.getOperandCount() - 1));
    }

    return new Call(call.getOperator(), operands);
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Type type = operands[0].getType();
    Object search = operands[0].getValue();

    int index = -1;
    for (int i = 1, len = operands.length - 1; i < len; i += 2) {
      Object value = operands[i].getValue();
      // DECODE use compareEqualNull to handle NULL value
      if (type.compareEqualNull(search, value)) {
        index = i + 1;
        break;
      }
    }
    if (index < 0 && operands.length % 2 == 0) {
      index = operands.length - 1;
    }
    if (index < 0) return null;

    return operands[index].getValue();
  }

  public static class DecodeFunctionOperandTypeChecker implements IOperandTypeChecker {

    public DecodeFunctionOperandTypeChecker() {
      super();
    }

    @Override
    public boolean checkOperandTypes(Call call) {
      Type search = call.getOperand(0).getType();
      Type result = firstNonNull(call.getOperands()).getType();

      int count = ((call.getOperandCount() - 1) / 2) * 2;
      for (int i = 1; i < count; i += 2) {
        if (!search.isFamily(call.getOperand(i).getType().getFamily())) {
          return false;
        }

        IExpression operandResult = call.getOperand(i + 1);
        if (!(result.isFamily(operandResult.getType().getFamily()) || operandResult.isNull())) {
          return false;
        }
      }

      // Check type if function has a default value
      return (call.getOperandCount() - 1) <= count
          || call.getOperand(count + 1).getType().isCoercible(result);
    }

    private IExpression firstNonNull(IExpression[] operands) {
      for (int i = 2; i < operands.length; i += 2) {
        if (!operands[i].isNull()) return operands[i];
      }
      return Literal.NULL;
    }

    @Override
    public IOperandCountRange getOperandCountRange() {
      return OperandCountRange.between(3, Integer.MAX_VALUE);
    }
  }
}
