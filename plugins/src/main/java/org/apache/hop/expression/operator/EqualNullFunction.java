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

import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.Converter;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Compares whether two expressions are equal.
 *
 * <p>
 * The function is NULL-safe, meaning it treats NULLs as known values for comparing equality. Note
 * that this is different from the EQUAL comparison operator (=), which treats NULLs as unknown
 * values.
 */
@FunctionPlugin
public class EqualNullFunction extends Function {

  public EqualNullFunction() {
    super("EQUAL_NULL", true, ReturnTypes.BOOLEAN, OperandTypes.ANY_ANY,
        OperatorCategory.COMPARISON, "/docs/equal_null.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object v0 = operands[0].getValue(context);
    Object v1 = operands[1].getValue(context);

    return Converter.compare(v0, v1) == 0;
  }
}
