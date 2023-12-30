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
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.Comparison;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;

/**
 * The function returns the largest value that is not NULL, or NULL if all values are NULL.
 *
 * @see {@link LeastFunction}
 */
@FunctionPlugin
public class GreatestFunction extends Function {

  public GreatestFunction() {
    super("GREATEST", ReturnTypes.LEAST_RESTRICTIVE, OperandTypes.COMPARABLE_ORDERED_VARIADIC,
        OperatorCategory.CONDITIONAL, "/docs/greatest.html");
  }
  
  @Override
  public Call castType(Call call) {
    Types.comparisonCoercion(call);    
    return super.castType(call);
  }
  
  @Override
  public Object eval(final IExpression[] operands) {
    Object result = null;
    for (IExpression operand : operands) {
      Object value = operand.getValue();
      if (Comparison.compare(result, value) < 0)
        result = value;
    }

    return result;
  }
}
