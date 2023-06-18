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
package org.apache.hop.expression;

import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.IReturnTypeInference;

public abstract class AggregateFunction extends Function {

  protected AggregateFunction(String id, IReturnTypeInference returnTypeInference,
      IOperandTypeChecker operandTypeChecker, String documentationUrl) {
    super(id, returnTypeInference, operandTypeChecker, OperatorCategory.AGGREGATION,
        documentationUrl);
  }

  @Override
  public boolean isAggregate() {
    return true;
  }

  public abstract IExpressionProcessor createProcessor(IExpressionContext context,
      IExpression[] operands) throws ExpressionException ;
}
