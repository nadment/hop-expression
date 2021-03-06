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

import org.apache.hop.expression.DatePart;
import org.apache.hop.expression.ExpressionCall;
import org.apache.hop.expression.ExpressionRegistry;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.optimizer.Optimizer.Rule;

/**
 * Replace EXTRACT with the corresponding function only if without time zone
 */
public class SimplifyExtractRule implements Rule {
  @Override
  public IExpression apply(IExpressionContext context, ExpressionCall call) {

    if (call.getKind() == Kind.EXTRACT && call.getOperandCount() == 2) {

      DatePart part = (DatePart) call.getOperand(0).eval(context);

      switch (part) {
        case YEAR:
        case MONTH:
        case QUARTER:
        case DAY:
        case HOUR:
        case MINUTE:
        case SECOND:
        case WEEK:
        case DAYOFYEAR:
        case DAYOFWEEK:
          Function function = ExpressionRegistry.getInstance().getFunction(part.name());
          return new ExpressionCall(function, call.getOperand(1));
        default:
      }
    }

    return call;
  }
}
