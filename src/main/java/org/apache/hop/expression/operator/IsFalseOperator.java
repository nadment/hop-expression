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

import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * An operator describing the <code>IS FALSE</code> operator.
 *
 * @see {@link IsNotFalseOperator}
 */
public class IsFalseOperator extends PostfixUnaryOperator {

  public IsFalseOperator() {
    super(
        "IS FALSE",
        140,
        true,
        ReturnTypes.BOOLEAN_NOT_NULL,
        OperandTypes.BOOLEAN,
        OperatorCategory.COMPARISON,
        "/docs/is-false.html");
  }

  @Override
  public Operator not() {
    return Operators.IS_NOT_FALSE;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Boolean value = operands[0].getValue(Boolean.class);

    // NULL is never FALSE
    return value == Boolean.FALSE;
  }
}
