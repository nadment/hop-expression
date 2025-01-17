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

import java.math.BigDecimal;
import java.util.Objects;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/** Return a random number between 0 (inclusive) and 1 (exclusive). */
@FunctionPlugin(names = "RAND")
public class RandomFunction extends Function {

  private final Random random;

  public RandomFunction() {
    this(null);
  }

  public RandomFunction(Random random) {
    super(
        "RANDOM",
        ReturnTypes.NUMBER_NOT_NULL,
        OperandTypes.NILADIC.or(OperandTypes.INTEGER),
        OperatorCategory.MATHEMATICAL,
        "/docs/random.html");
    this.random = random;
  }

  @Override
  public boolean isDeterministic() {
    return false;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    return BigDecimal.valueOf(random.nextDouble());
  }

  @Override
  public IExpression compile(final IExpressionContext context, final Call call) {

    // Already compiled
    if (random != null) {
      return call;
    }

    if (call.getOperandCount() == 1) {
      try {
        Long seed = call.getOperand(0).getValue(Long.class);
        RandomFunction function = new RandomFunction(new Random(seed));
        return new Call(function, call.getOperands());
      } catch (Exception e) {
        throw new ExpressionException(ErrorCode.INVALID_NUMBER, call.getOperand(0));
      }
    }

    RandomFunction function = new RandomFunction(ThreadLocalRandom.current());
    return new Call(function, call.getOperands());
  }

  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof RandomFunction other)) {
      return false;
    }

    if (random == null) return super.equals(obj);

    return super.equals(other) && this.random.equals(other.random);
  }

  @Override
  public int hashCode() {
    return Objects.hash(getId(), getName(), random);
  }
}
