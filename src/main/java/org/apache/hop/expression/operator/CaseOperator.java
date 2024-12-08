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
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.Comparison;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeId;
import org.apache.hop.expression.type.Types;

/** An operator describing the <code>CASE</code> operator. */
public class CaseOperator extends Operator {
  public enum When {
    /** Simple switch case */
    SIMPLE,
    /** Search case */
    SEARCH
  }

  private final When when;

  public CaseOperator(When when) {
    super(
        "CASE",
        120,
        true,
        ReturnTypes.CASE_OPERATOR,
        null,
        OperatorCategory.CONDITIONAL,
        "/docs/case.html");
    this.when = when;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    int index = 0;
    Array whenTerm = (Array) operands[1];
    Array thenTerm = (Array) operands[2];
    IExpression elseTerm = operands[3];

    if (when == When.SIMPLE) {
      IExpression valueExpression = operands[0];
      Object condition = valueExpression.getValue();
      if (condition != null) {
        for (IExpression whenOperand : whenTerm) {

          // Multi-values
          if (whenOperand.is(Kind.ARRAY)) {
            for (IExpression expression : (Array) whenOperand) {
              Object value = expression.getValue();
              if (Comparison.equals(condition, value)) {
                return thenTerm.get(index).getValue();
              }
            }
          } else {
            Object value = whenOperand.getValue();
            if (Comparison.equals(condition, value)) {
              return thenTerm.get(index).getValue();
            }
          }
          index++;
        }
      }
    } else {
      for (IExpression whenOperand : whenTerm) {
        Boolean predicat = whenOperand.getValue(Boolean.class);
        if (predicat != null && predicat) {
          return thenTerm.get(index).getValue();
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
    if (when == When.SIMPLE) {
      if (whenTerm.size() == 1) {

        // CASE value WHEN x THEN y ELSE z END → IF(value=x,y,z)
        return new Call(
            IfFunction.INSTANCE,
            new Call(Operators.EQUAL, call.getOperand(0), whenTerm.get(0)),
            thenTerm.get(0),
            elseTerm);
      }
    } else {
      // Flatten search case
      //
      // When a searched CASE expression nests another CASE expression in its ELSE clause, that can
      // be flattened into the top level CASE expression.
      if (elseTerm.isOperator(Operators.CASE_SEARCH)) {
        List<IExpression> whenOperands = new ArrayList<>();
        whenTerm.forEach(whenOperands::add);
        array(call(elseTerm).getOperand(1)).forEach(whenOperands::add);

        List<IExpression> thenOperands = new ArrayList<>();
        thenTerm.forEach(thenOperands::add);
        array(call(elseTerm).getOperand(2)).forEach(thenOperands::add);

        return new Call(
            Operators.CASE_SEARCH,
            Literal.NULL,
            new Array(whenOperands),
            new Array(thenOperands),
            call(elseTerm).getOperand(3));
      }

      // Search CASE expressions with one condition can be turned into COALESCE, NULLIF, NVL2 or
      // simple case
      //
      if (whenTerm.size() == 1) {
        IExpression whenTerm0 = whenTerm.get(0);
        IExpression thenTerm0 = thenTerm.get(0);

        // CASE WHEN x IS NULL THEN y ELSE x END → IFNULL(x,y)
        if (whenTerm0.isOperator(Operators.IS_NULL)
            && call(whenTerm0).getOperand(0).equals(elseTerm)) {
          return new Call(IfNullFunction.INSTANCE, call(whenTerm0).getOperand(0), thenTerm0);
        }

        // CASE WHEN x=y THEN NULL ELSE x END → NULLIF(x,y)
        if (whenTerm0.isOperator(Operators.EQUAL) && thenTerm0.isNull()) {

          if (call(whenTerm0).getOperand(0).equals(elseTerm)) {
            return new Call(
                NullIfFunction.INSTANCE,
                call(whenTerm0).getOperand(0),
                call(whenTerm0).getOperand(1));
          }

          if (call(whenTerm0).getOperand(1).equals(elseTerm)) {
            return new Call(
                NullIfFunction.INSTANCE,
                call(whenTerm0).getOperand(1),
                call(whenTerm0).getOperand(0));
          }
        }

        // CASE WHEN x IS NOT NULL THEN y ELSE z END → NVL2(x,y,z)
        if (whenTerm0.isOperator(Operators.IS_NOT_NULL)) {
          return new Call(
              Nvl2Function.INSTANCE, call(whenTerm0).getOperand(0), thenTerm0, elseTerm);
        }

        // CASE WHEN x IS NULL THEN y ELSE z END → NVL2(x,z,y)
        if (whenTerm0.isOperator(Operators.IS_NULL)) {
          return new Call(
              Nvl2Function.INSTANCE, call(whenTerm0).getOperand(0), elseTerm, thenTerm0);
        }

        // CASE WHEN a=b THEN 1 END to CASE a WHEN b THEN 1 END
        // Not always compatible with flatten search case
        // if (whenTerm0.is(Operators.EQUAL)) {
        // return new Call(Operators.CASE, whenTerm0.asCall().getOperand(0), new
        // Array(whenTerm0.asCall().getOperand(1)), thenTerm, elseTerm);
        // }
      }
    }
    return call;
  }

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof CaseOperator other) {
      return when.equals(other.when);
    }
    return false;
  }

  @Override
  public int hashCode() {
    return Objects.hash(super.hashCode(), this.when);
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append("CASE");

    // Simple case
    if (when == When.SIMPLE) {
      writer.append(' ');
      operands[0].unparse(writer, 0, 0);
    }

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

    Array whenTerm = (Array) call.getOperand(1);
    Array thenTerm = (Array) call.getOperand(2);
    IExpression elseTerm = call.getOperand(3);

    // Search case should be a predicate
    Type valueType = Types.BOOLEAN;

    // Simple case
    if (when == When.SIMPLE) {
      valueType = call.getOperand(0).getType();
    }

    // Check WHEN operands
    for (IExpression operand : whenTerm) {
      if (operand.is(Kind.ARRAY)) {
        // Mutli-values simple form
        for (IExpression value : (Array) operand) {
          if (!valueType.isCoercible(value.getType())) {
            return false;
          }
        }
      } else if (!valueType.isCoercible(operand.getType())) {
        return false;
      }
    }

    // Determine common return type
    Type returnType = getLeastRestrictive(getLeastRestrictive(thenTerm), elseTerm.getType());
    if (returnType.is(TypeId.UNKNOWN)) return false;

    // Check then operands
    for (IExpression thenOperand : thenTerm) {
      if (!(returnType.isCoercible(thenOperand.getType()) || thenOperand.isNull())) {
        return false;
      }
    }

    // Check else operand
    return elseTerm.isNull() || returnType.isCoercible(elseTerm.getType());
  }

  /**
   * Find common type for all the then operands and else operands, then try to coerce the then/else
   * operands to the type if needed.
   */
  @Override
  public boolean coerceOperandsType(Call call) {
    boolean coerced = false;

    // Simple case operator
    if (when == When.SIMPLE) {

      Type type = getLeastRestrictive((Array) call.getOperand(1));
      type = getLeastRestrictive(type, call.getOperand(0).getType());

      // Coerce value operand
      coerced |= coerceOperandType(call, type, 0);
      // Coerce WHEN operands
      coerced |= coerceOperandType(call, type, 1);
    }

    // Coerce THEN and ELSE operands
    coerced |= coerceOperandType(call, call.getType(), 2);
    coerced |= coerceOperandType(call, call.getType(), 3);

    return coerced;
  }
}
