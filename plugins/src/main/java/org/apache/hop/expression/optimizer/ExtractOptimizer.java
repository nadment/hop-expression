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
package org.apache.hop.expression.optimizer;

import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.UserDefinedFunction;
import org.apache.hop.expression.util.Coerse;
import org.apache.hop.expression.util.TimeUnit;

/**
 * Replace EXTRACT with the corresponding function only if without time zone
 */
public class ExtractOptimizer extends Optimizer {

  @Override
  public IExpression apply(IExpressionContext context, Call call) {
    try {
      if (call.is(Operators.EXTRACT) && call.getOperandCount() == 2) {
        TimeUnit unit = Coerse.toTimeUnit(call.getOperand(0).getValue(context));
        Function function = FunctionRegistry.getFunction(unit.name());
        if (function != null && ! (function instanceof UserDefinedFunction) ) {
          return new Call(function, call.getOperand(1));
        }
      }
      return call;
    } catch (ExpressionException e) {
      return call;
    }
  }
}
