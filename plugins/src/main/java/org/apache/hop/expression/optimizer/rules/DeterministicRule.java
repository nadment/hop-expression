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

import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCall;
import org.apache.hop.expression.Tuple;
import org.apache.hop.expression.optimizer.OptimizerRule;

public class DeterministicRule implements OptimizerRule {
  @Override
  public IExpression apply(IExpressionContext context, OperatorCall call) {
    try {
      Operator operator = call.getOperator();

      if (!operator.isDeterministic())
        return call;

      for (IExpression operand : call.getOperands()) {
        if (operand == null)
          continue;

        if (operand instanceof Tuple) {
          for (IExpression expression : (Tuple) operand) {
            if (!expression.isKind(Kind.LITERAL)) {
              return call;
            }
          }
        } else if (!operand.isKind(Kind.LITERAL)) {
          return call;
        }
      }

      return Literal.of(call.eval(context));
    } catch (Exception e) {
      return call;
    }
  }
}
