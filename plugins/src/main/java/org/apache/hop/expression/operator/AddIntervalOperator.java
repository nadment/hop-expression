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
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.exception.ExpressionException;
import java.time.ZonedDateTime;

/**
 * Adds a specified interval to a date or timestamp
 */
public class AddIntervalOperator extends AddOperator {
  public static final AddIntervalOperator INSTANCE = new AddIntervalOperator();

  public AddIntervalOperator() {
    super();
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Simplify arithmetic A+INTERVAL 0 → A
    IExpression operand = call.getOperand(1);
    if (operand.isConstant() && !operand.isNull()) {
      Interval interval = operand.getValue(Interval.class);
      if (interval.isZero()) {
        return call.getOperand(0);
      }
    }

    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    ZonedDateTime datetime = operands[0].getValue(ZonedDateTime.class);
    if (datetime == null)
      return null;

    Interval interval = operands[1].getValue(Interval.class);
    if (interval == null)
      return null;

    return interval.addTo(datetime);
  }
}
