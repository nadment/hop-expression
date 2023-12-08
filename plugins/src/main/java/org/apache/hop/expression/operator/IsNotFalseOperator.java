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

import org.apache.hop.expression.Category;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;

/**
 * An operator describing the <code>IS NOT FALSE</code> operator.
 */
public class IsNotFalseOperator extends Operator {

  public IsNotFalseOperator() {
    super("IS NOT FALSE", 140, true, ReturnTypes.BOOLEAN_NOT_NULL, OperandTypes.BOOLEAN, Category.COMPARISON,
        "/docs/is-false.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Boolean value = operands[0].getValue(Boolean.class);

    // NULL is always NOT FALSE
    return value != Boolean.FALSE;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" IS NOT FALSE");
  }
}
