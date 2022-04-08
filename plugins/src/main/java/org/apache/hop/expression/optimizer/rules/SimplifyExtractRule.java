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
package org.apache.hop.expression.optimizer.rules;

import org.apache.hop.expression.Call;
import org.apache.hop.expression.DatePart;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.optimizer.OptimizerRule;

/**
 * Replace EXTRACT with the corresponding function only if without time zone
 */
public class SimplifyExtractRule implements OptimizerRule {
  @Override
  public IExpression apply(IExpressionContext context, Call call) {
    try {
      if (call.is(Operators.EXTRACT) && call.getOperandCount() == 2) {
        DatePart part = DatePart.get(call.getOperand(0).eval(context));

        switch (part) {
          case YEAR:
          case ISOYEAR:
          case QUARTER:
          case MONTH:
          case WEEK:
          case ISOWEEK:
          case DAY:
          case DAYOFYEAR:
          case DAYOFWEEK:
          case ISODAYOFWEEK:
          case HOUR:
          case MINUTE:
          case SECOND:
            Function function = FunctionRegistry.getFunction(part.name());
            return new Call(function, call.getOperand(1));
          default:
        }
      }
      return call;
    } catch (ExpressionException e) {
      return call;
    }
  }
}
