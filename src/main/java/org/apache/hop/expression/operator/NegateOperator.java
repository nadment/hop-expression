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

import java.io.StringWriter;
import java.math.BigDecimal;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.Interval;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeId;

/** Prefix arithmetic minus (negative) operator '<code>-</code>' for numeric or interval. */
public class NegateOperator extends PrefixUnaryOperator {

  public NegateOperator() {
    super(
        "NEGATE",
        "-",
        30,
        true,
        ReturnTypes.ARG0,
        OperandTypes.NUMERIC.or(OperandTypes.INTERVAL),
        OperatorCategory.MATHEMATICAL,
        "/docs/negate.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression operand = call.getOperand(0);

    // Simplify -(-(A)) → A
    if (operand.isOperator(Operators.NEGATE)) {
      return operand.asCall().getOperand(0);
    }

    Type type = call.getOperand(0).getType();
    if (type.is(TypeId.INTERVAL)) {
      return new Call(call.getPosition(), NegateInterval.INSTANCE, call.getOperands());
    }

    // Simplify arithmetic -(A-B) → B-A
    if (operand.isOperator(Operators.SUBTRACT)) {
      Call subtract = operand.asCall();
      return new Call(Operators.SUBTRACT, subtract.getOperand(1), subtract.getOperand(0));
    }

    NegateOperator operator = NegateNumber.INSTANCE;
    if (type.is(TypeId.INTEGER)) {
      operator = NegateInteger.INSTANCE;
    }

    return new Call(call.getPosition(), operator, call.getOperands());
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append('-');
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
  }

  private static final class NegateInteger extends NegateOperator {
    private static final NegateOperator INSTANCE = new NegateInteger();

    private NegateInteger() {
      super();
    }

    @Override
    public Object eval(final IExpression[] operands) {
      Long value = operands[0].getValue(Long.class);
      if (value == null) return null;

      if (value == Long.MIN_VALUE) {
        throw new ArithmeticException(ErrorCode.ARITHMETIC_OVERFLOW.message(value));
      }
      return Long.valueOf(-value);
    }
  }

  private static final class NegateNumber extends NegateOperator {
    private static final NegateOperator INSTANCE = new NegateNumber();

    private NegateNumber() {
      super();
    }

    @Override
    public Object eval(final IExpression[] operands) {
      BigDecimal value = operands[0].getValue(BigDecimal.class);
      if (value == null) return null;
      return value.negate();
    }
  }

  private static final class NegateInterval extends NegateOperator {
    private static final NegateOperator INSTANCE = new NegateInterval();

    private NegateInterval() {
      super();
    }

    @Override
    public Object eval(final IExpression[] operands) {
      Interval interval = operands[0].getValue(Interval.class);
      if (interval == null) return null;

      return interval.negate();
    }
  }
}
