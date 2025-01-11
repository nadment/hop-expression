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
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;

/**
 * An operator describing the <code>IS NOT TRUE</code> operator.
 *
 * @see {@link IsTrueOperator}
 */
public class IsNotTrueOperator extends PostfixUnaryOperator {

  public IsNotTrueOperator() {
    super(
        "IS NOT TRUE",
        140,
        true,
        ReturnTypes.BOOLEAN_NOT_NULL,
        OperandTypes.BOOLEAN,
        OperatorCategory.COMPARISON,
        "/docs/is-true.html");
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coerceOperandType(call, Types.BOOLEAN, 0);
  }

  @Override
  public Operator not() {
    return Operators.IS_TRUE;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Boolean value = operands[0].getValue(Boolean.class);

    // NULL is always NOT TRUE
    return value != Boolean.TRUE;
  }
}
