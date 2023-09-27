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
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.TypeFamily;
import java.io.StringWriter;

/**
 * Generic subtraction operator.
 * <br>
 * <strong>Syntax:</strong> <code>x - y</code>
 */
public class SubtractOperator extends Operator {

  public SubtractOperator() {
    this("SUBTRACT");
  }
  
  protected SubtractOperator(String id) {
    super(id, "-", 100, true, ReturnTypes.ADDITIVE_OPERATOR, OperandTypes.NUMERIC_NUMERIC.or(OperandTypes.DATE_INTERVAL).or(OperandTypes.DATE_NUMERIC).or(OperandTypes.INTERVAL_INTERVAL),
        Category.MATHEMATICAL, "/docs/subtract.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    if (left.getType().isSameFamily(TypeFamily.TEMPORAL)) {
      // Supports the basic subtraction of days to DATE values
      if ( right.getType().isSameFamily(TypeFamily.NUMERIC)) {
        return new Call(AddDaysFunction.INSTANCE, left, new Call(call.getPosition(), Operators.NEGATIVE, right));
      }      
      return new Call(Operators.SUBTRACT_DATE_INTERVAL, call.getOperands());
    }
    else if (left.getType().isSameFamily(TypeFamily.INTERVAL)) {
      return new Call(call.getPosition(), Operators.SUBTRACT_INTERVAL, call.getOperands());
    }

    return new Call(Operators.SUBTRACT_NUMERIC, call.getOperands()); 
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append('-');
    operands[1].unparse(writer);
  }
}
