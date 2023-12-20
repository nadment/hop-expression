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
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.Tuple;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.Comparison;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

/**
 * An operator describing the <code>CASE</code> operator.
 */
public class CaseOperator extends Operator {

  public CaseOperator() {
    super("CASE", 120, true, ReturnTypes.CASE_OPERATOR, OperandTypes.CASE_OPERATOR,
        OperatorCategory.CONDITIONAL, "/docs/case.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    int index = 0;
    IExpression valueExpression = operands[0];
    Tuple whenTuple = (Tuple) operands[1];
    Tuple thenTuple = (Tuple) operands[2];
    IExpression elseExpression = operands[3];

    // Search case
    if (valueExpression == Literal.UNKNOWN) {
      for (IExpression whenOperand : whenTuple) {
        Boolean predicat = whenOperand.getValue(Boolean.class);
        if (predicat != null && predicat) {
          return thenTuple.get(index).getValue();
        }
        index++;
      }
    }
    // Simple case
    else {
      Object condition = valueExpression.getValue();
      if (condition != null) {
        for (IExpression whenOperand : whenTuple) {

          // Multi values
          if (whenOperand.is(Kind.TUPLE)) {
            for (IExpression expression : whenOperand.asTuple()) {
              Object value = expression.getValue();
              if (Comparison.compare(condition, value) == 0) {
                return thenTuple.get(index).getValue();
              }
            }
          } else {
            Object value = whenOperand.getValue();
            if (Comparison.compare(condition, value) == 0) {
              return thenTuple.get(index).getValue();
            }
          }
          index++;
        }
      }
    }

    return elseExpression.getValue();
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Tuple whenTerm = call.getOperand(1).asTuple();
    Tuple thenTerm = call.getOperand(2).asTuple();
    IExpression elseTerm = call.getOperand(3);

    // Search CASE
    if (call.getOperand(0) == Literal.UNKNOWN) {

      // Flatten search case
      //
      // When a searched CASE expression nests another CASE expression in its ELSE clause, that can
      // be flattened into the top level CASE expression.
      if (elseTerm.is(Operators.CASE) && elseTerm.asCall().getOperand(0) == Literal.UNKNOWN) {
        List<IExpression> whenOperands = new ArrayList<>();
        whenTerm.forEach(whenOperands::add);
        elseTerm.asCall().getOperand(1).asTuple().forEach(whenOperands::add);

        List<IExpression> thenOperands = new ArrayList<>();
        thenTerm.forEach(thenOperands::add);
        elseTerm.asCall().getOperand(2).asTuple().forEach(thenOperands::add);

        return new Call(Operators.CASE, Literal.UNKNOWN, new Tuple(whenOperands),
            new Tuple(thenOperands), elseTerm.asCall().getOperand(3));
      }

      // Search CASE expressions with one condition can be turned into COALESCE, NULLIF, NVL2 or
      // simple case
      //
      if (whenTerm.size() == 1) {
        IExpression whenTerm0 = whenTerm.get(0);
        IExpression thenTerm0 = thenTerm.get(0);

        // CASE WHEN x IS NULL THEN y ELSE x END → IFNULL(x,y)
        if (whenTerm0.is(Operators.IS_NULL) && whenTerm0.asCall().getOperand(0).equals(elseTerm)) {
          return new Call(Operators.IFNULL, whenTerm0.asCall().getOperand(0), thenTerm0);
        }

        // CASE WHEN x=y THEN NULL ELSE x END → NULLIF(x,y)
        if (whenTerm0.is(Operators.EQUAL) && thenTerm0.isNull()) {

          if (whenTerm0.asCall().getOperand(0).equals(elseTerm)) {
            return new Call(Operators.NULLIF, whenTerm0.asCall().getOperand(0),
                whenTerm0.asCall().getOperand(1));
          }

          if (whenTerm0.asCall().getOperand(1).equals(elseTerm)) {
            return new Call(Operators.NULLIF, whenTerm0.asCall().getOperand(1),
                whenTerm0.asCall().getOperand(0));
          }
        }

        // CASE WHEN x IS NOT NULL THEN y ELSE z END → NVL2(x,y,z)
        if (whenTerm0.is(Operators.IS_NOT_NULL)) {
          return new Call(Operators.NVL2, whenTerm0.asCall().getOperand(0), thenTerm0, elseTerm);
        }

        // CASE WHEN x IS NULL THEN y ELSE z END → NVL2(x,z,y)
        if (whenTerm0.is(Operators.IS_NULL)) {
          return new Call(Operators.NVL2, whenTerm0.asCall().getOperand(0), elseTerm, thenTerm0);
        }

        // CASE WHEN a=b THEN 1 END to CASE a WHEN b THEN 1 END
        // Not always compatible with flatten search case
        // if (whenTerm0.is(Operators.EQUAL)) {
        // return new Call(Operators.CASE, whenTerm0.asCall().getOperand(0), new
        // Tuple(whenTerm0.asCall().getOperand(1)), thenTerm, elseTerm);
        // }
      }
    }
    // Simple case
    else {
      if (whenTerm.size() == 1) {
        IExpression searchTerm = call.getOperand(0);

        // CASE value WHEN x THEN y ELSE z END → IF(value=x,y,z)
        return new Call(Operators.IF, new Call(Operators.EQUAL, searchTerm, whenTerm.get(0)),
            thenTerm.get(0), elseTerm);
      }
    }

    return call;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append("CASE");

    // Simple case
    IExpression valueExpression = operands[0];
    if (valueExpression != Literal.UNKNOWN) {
      writer.append(' ');
      valueExpression.unparse(writer);
    }

    int index = 0;
    Tuple whenTuple = operands[1].asTuple();
    Tuple thenTuple = operands[2].asTuple();
    for (IExpression whenOperand : whenTuple) {
      writer.append(" WHEN ");
      whenOperand.unparse(writer);
      IExpression thenOperand = thenTuple.get(index++);
      writer.append(" THEN ");
      thenOperand.unparse(writer);
    }

    IExpression elseExpression = operands[3];
    if (elseExpression != Literal.NULL) {
      writer.append(" ELSE ");
      elseExpression.unparse(writer);
    }
    writer.append(" END");
  }
}
