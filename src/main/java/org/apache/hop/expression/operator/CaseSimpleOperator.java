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

import static org.apache.hop.expression.type.Types.coerceOperandType;
import static org.apache.hop.expression.type.Types.getLeastRestrictive;

import java.io.StringWriter;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** An operator describing the <code>CASE</code> operator. */
@NullMarked
public class CaseSimpleOperator extends Operator {
  public static final CaseSimpleOperator INSTANCE = new CaseSimpleOperator();

  public CaseSimpleOperator() {
    super(
        "CASE",
        120,
        Associativity.LEFT,
        ReturnTypes.CASE_OPERATOR,
        null,
        OperatorCategory.CONDITIONAL,
        "/docs/case.html");
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    int index = 0;
    IExpression valueExpression = operands[0];
    Array whenTerm = (Array) operands[1];
    Array thenTerm = (Array) operands[2];
    IExpression elseTerm = operands[3];

    Object condition = valueExpression.getValue();
    if (condition != null) {
      Type type = operands[0].getType();

      for (IExpression whenOperand : whenTerm) {

        // Multi-values
        if (whenOperand.is(Kind.ARRAY)) {
          for (IExpression expression : (Array) whenOperand) {
            Object value = expression.getValue();
            // A null value never matches
            if (value != null && type.compareEqual(condition, value)) {
              return thenTerm.get(index).getValue();
            }
          }
        } else {
          Object value = whenOperand.getValue();
          if (value != null && type.compareEqual(condition, value)) {
            return thenTerm.get(index).getValue();
          }
        }
        index++;
      }
    }

    return elseTerm.getValue();
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Array whenTerm = (Array) call.getOperand(1);
    Array thenTerm = (Array) call.getOperand(2);
    IExpression elseTerm = call.getOperand(3);

    if (whenTerm.size() == 1) {
      IExpression whenOperand = whenTerm.get(0);

      // Multi-values: CASE value WHEN x1,x2 THEN y ELSE z END → IF(value IN (x1,x2),y,z)
      if (whenOperand.is(Kind.ARRAY)) {
        return new Call(
            IfFunction.INSTANCE,
            new Call(InListOperator.INSTANCE, call.getOperand(0), whenOperand),
            thenTerm.get(0),
            elseTerm);
      }

      // CASE value WHEN x THEN y ELSE z END → IF(value=x,y,z)
      return new Call(
          IfFunction.INSTANCE,
          new Call(EqualOperator.INSTANCE, call.getOperand(0), whenOperand),
          thenTerm.get(0),
          elseTerm);
    }
    return call;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append("CASE ");

    operands[0].unparse(writer, 0, 0);

    int index = 0;
    Array whenTerms = (Array) operands[1];
    Array thenTerms = (Array) operands[2];
    for (IExpression whenOperand : whenTerms) {
      writer.append(" WHEN ");
      if (whenOperand instanceof Array array) {
        array.unparseValues(writer);
      } else whenOperand.unparse(writer, 0, 0);
      writer.append(" THEN ");
      IExpression thenOperand = thenTerms.get(index++);
      thenOperand.unparse(writer, 0, 0);
    }

    IExpression elseOperand = operands[3];
    if (!elseOperand.isNull()) {
      writer.append(" ELSE ");
      elseOperand.unparse(writer, 0, 0);
    }
    writer.append(" END");
  }

  @Override
  public boolean checkOperandTypes(Call call) {

    IExpression valueOperand = call.getOperand(0);

    // Check WHEN operands are comparable with the value, like the equal operator
    if (!valueOperand.isNull()) {
      for (IExpression operand : (Array) call.getOperand(1)) {
        if (operand.is(Kind.ARRAY)) {
          // Mutli-values simple form
          for (IExpression value : (Array) operand) {
            if (!(value.isNull() || Types.isComparable(valueOperand, value))) {
              return false;
            }
          }
        } else if (!(operand.isNull() || Types.isComparable(valueOperand, operand))) {
          return false;
        }
      }
    }

    return CaseSearchOperator.checkResultTypes(call);
  }

  /**
   * Find common type for all the then operands and else operands, then try to coerce the then/else
   * operands to the type if needed.
   */
  @Override
  public boolean coerceOperandsType(Call call) {
    boolean coerced = false;

    Type type = getLeastRestrictive(call.getOperand(1));
    type = getLeastRestrictive(type, call.getOperand(0).getType());

    // Coerce value operand
    coerced |= coerceOperandType(call, type, 0);

    // Coerce WHEN operands
    coerced |= coerceOperandType(call, type, 1);

    // Coerce THEN and ELSE operands
    coerced |= coerceOperandType(call, call.getType(), 2);
    coerced |= coerceOperandType(call, call.getType(), 3);

    return coerced;
  }
}
