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
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.Tuple;
import org.apache.hop.expression.type.Converter;
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
  public Object eval(final IExpressionContext context, IExpression[] operands) throws Exception {
    int index = 0;
    IExpression switchExpression = operands[0];
    Tuple whenTuple = (Tuple) operands[1];
    Tuple thenTuple = (Tuple) operands[2];
    IExpression elseExpression = operands[3];

    // Search case
    if (switchExpression == Literal.NULL) {
      for (IExpression whenOperand : whenTuple) {
        Boolean predicat = whenOperand.getValue(context, Boolean.class);
        if (predicat != null && predicat) {
          return thenTuple.get(index).getValue(context);
        }
        index++;
      }
    }
    // Simple case
    else {
      Object condition = switchExpression.getValue(context);
      for (IExpression whenOperand : whenTuple) {
        Object value = whenOperand.getValue(context);
        if (Converter.compare(condition, value) == 0) {
          return thenTuple.get(index).getValue(context);
        }
        index++;
      }
    }

    return elseExpression.getValue(context);
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Tuple whenTerm = (Tuple) call.getOperand(1);
    Tuple thenTerm = (Tuple) call.getOperand(2);
    IExpression elseTerm = call.getOperand(3);

    // Search CASE
    if (call.getOperand(0) == Literal.NULL) {

      // Flatten search case
      //
      // When a searched CASE expression nests another CASE expression in its ELSE clause, that can
      // be flattened into the top level CASE expression.
      if (elseTerm.is(Operators.CASE)) {
        if (elseTerm.asCall().getOperand(0) == Literal.NULL) {
          List<IExpression> whenOperands = new ArrayList<>();
          whenTerm.forEach(whenOperands::add);
          ((Tuple) elseTerm.asCall().getOperand(1)).forEach(whenOperands::add);

          List<IExpression> thenOperands = new ArrayList<>();
          thenTerm.forEach(thenOperands::add);
          ((Tuple) elseTerm.asCall().getOperand(2)).forEach(thenOperands::add);

          return new Call(Operators.CASE, Literal.NULL, new Tuple(whenOperands),
              new Tuple(thenOperands), elseTerm.asCall().getOperand(3));
        }
      }

      // Search CASE expressions with one condition can be turned into COALESCE, NULLIF, NVL2
      //
      if (whenTerm.size() == 1) {
        IExpression whenTerm0 = whenTerm.get(0);
        IExpression thenTerm0 = thenTerm.get(0);

        // "CASE WHEN x IS NULL THEN y ELSE x END" to "IFNULL(x, y)"
        if (whenTerm0.is(Operators.IS_NULL) && whenTerm0.asCall().getOperand(0).equals(elseTerm) ) {
          return new Call(FunctionRegistry.getFunction("IFNULL"),
              whenTerm0.asCall().getOperand(0), thenTerm0);
        }

        // "CASE WHEN x = y THEN NULL ELSE x END" to "NULLIF(x, y)"
        if (whenTerm0.is(Operators.EQUAL) && thenTerm0 == Literal.NULL) {
          
          if ( whenTerm0.asCall().getOperand(0).equals(elseTerm)) {          
            return new Call(FunctionRegistry.getFunction("NULLIF"), whenTerm0.asCall().getOperand(0), whenTerm0.asCall().getOperand(1));
          }

          if ( whenTerm0.asCall().getOperand(1).equals(elseTerm)) {          
            return new Call(FunctionRegistry.getFunction("NULLIF"), whenTerm0.asCall().getOperand(1), whenTerm0.asCall().getOperand(0));
          }         
        }

        // "CASE WHEN x IS NOT NULL THEN y ELSE z END" to "NVL2(x, y, z)"
        if (whenTerm0.is(Operators.IS_NOT_NULL)) {
          return new Call(FunctionRegistry.getFunction("NVL2"), whenTerm0.asCall().getOperand(0),
              thenTerm0, elseTerm);
        }

        // "CASE WHEN x IS NULL THEN y ELSE z END" to "NVL2(x, z, y)"
        if (whenTerm0.is(Operators.IS_NULL)) {
          return new Call(FunctionRegistry.getFunction("NVL2"), whenTerm0.asCall().getOperand(0),
              elseTerm, thenTerm0);
        }
      }
    }

    return call;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append("CASE");

    // Form switch expression
    IExpression switchExpression = operands[0];
    if (switchExpression != Literal.NULL) {
      writer.append(' ');
      switchExpression.unparse(writer);
    }

    int index = 0;
    Tuple whenTuple = (Tuple) operands[1];
    Tuple thenTuple = (Tuple) operands[2];
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
