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
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.util.Random;


/**
 * Return a random number between 0 (inclusive) and 1 (exclusive).
 */
@FunctionPlugin(names = "RAND")
public class RandomFunction extends Function {

  public RandomFunction() {
    super("RANDOM", ReturnTypes.NUMBER_NOT_NULL, OperandTypes.OPTIONAL_NUMERIC, OperatorCategory.MATHEMATICAL,
        "/docs/random.html");
  }

  @Override
  public boolean isDeterministic() {
    return false;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Random random = operands[1].getValue(Random.class);
    return BigDecimal.valueOf(random.nextDouble());
  }

  @Override
  public IExpression compile(final IExpressionContext context, final Call call) {
    if (call.getOperandCount() == 0) {
      return new Call(call.getOperator(), Literal.UNKNOWN, Literal.of(new Random()));
    }

    if (call.getOperandCount() == 1) {
      try {
        Long seed = call.getOperand(0).getValue(Long.class);
        Random random = new Random();
        random.setSeed(seed);
        return new Call(call.getOperator(), call.getOperand(0), Literal.of(random));
      } catch (Exception e) {
        throw new ExpressionException(ErrorCode.INVALID_NUMBER, call.getOperand(0));
      }
    }

    return call;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append(this.getName());
    writer.append('(');
    if (operands.length > 0 && !operands[0].isNull()) {
      operands[0].unparse(writer);
    }
    writer.append(')');
  }
}
