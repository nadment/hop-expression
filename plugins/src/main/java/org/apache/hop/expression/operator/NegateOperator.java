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
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeFamily;
import java.io.StringWriter;

/**
 * Prefix arithmetic minus (negative) operator '<code>-</code>'.
 */
public class NegateOperator extends Operator {
  public NegateOperator() {
    super("NEGATE", "-", 30, true, ReturnTypes.ARG0, OperandTypes.NUMERIC.or(OperandTypes.INTERVAL),
        OperatorCategory.MATHEMATICAL, "/docs/negate.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    Type type = call.getOperand(0).getType();

    if (type.isFamily(TypeFamily.INTERVAL)) {
      return new Call(call.getPosition(), NegateIntervalOperator.INSTANCE, call.getOperands());
    }

    return new Call(call.getPosition(), NegateNumericOperator.INSTANCE, call.getOperands());
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append('-');
    operands[0].unparse(writer);
  }
}
