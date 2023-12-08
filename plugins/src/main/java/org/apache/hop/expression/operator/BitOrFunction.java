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
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;

/**
 * Bitwise OR operator.
 * <br>
 * <strong>Syntax:</strong> <code>x | y</code>
 */
@FunctionPlugin
public class BitOrFunction extends Function {

  public BitOrFunction() {
    super("BIT_OR", ReturnTypes.INTEGER_NULLABLE, OperandTypes.NUMERIC_NUMERIC, Category.BITWISE,
        "/docs/bit_or.html");
  }

  public BitOrFunction(String name) {
    super("BIT_OR", name, 90, true, ReturnTypes.INTEGER_NULLABLE, OperandTypes.NUMERIC_NUMERIC,
        Category.BITWISE, "/docs/bit_or.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Long left = operands[0].getValue(Long.class);
    if (left == null)
      return null;
    Long right = operands[1].getValue(Long.class);
    if (right == null)
      return null;
    return left | right;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append('|');
    operands[1].unparse(writer);
  }
}
