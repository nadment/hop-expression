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
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Converts a value of one data type into another data type <code>TRY_CAST(value AS type [FORMAT format])</code>.
 * 
 * @see CastFunction
 * @see CastOperator
 */
@FunctionPlugin
public class TryCastFunction extends Function {
  
  public TryCastFunction() {
    super("TRY_CAST", ReturnTypes.CAST_OPERATOR, OperandTypes.CAST_OPERATOR,
        OperatorCategory.CONVERSION, "/docs/cast.html");
  }
  
  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {
    return new Call(Operators.TRY, new Call(Operators.CAST_FUNCTION, call.getOperands()));
  }
}
