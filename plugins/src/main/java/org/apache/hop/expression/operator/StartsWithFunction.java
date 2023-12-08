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
import org.apache.hop.expression.Category;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeFamily;

/**
 * The function returns TRUE if the first value starts with second value. Both values must be the
 * same data
 * type string or binary.
 *
 * @see {@link EndsWithFunction}
 */
@FunctionPlugin
public class StartsWithFunction extends Function {

  public static final StartsWithFunction StartsWithString = new StartsWithString();
  public static final StartsWithFunction StartsWithBinary = new StartsWithBinary();

  public StartsWithFunction() {
    super("STARTSWITH", ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.STRING_STRING.or(OperandTypes.BINARY_BINARY), Category.COMPARISON,
        "/docs/startswith.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getOperand(0).getType();
    if (type.isFamily(TypeFamily.BINARY)) {
      return new Call(StartsWithBinary, call.getOperands());
    }

    return new Call(StartsWithString, call.getOperands());
  }

  /**
   * The function returns TRUE if the first value starts with second value. Both values must be data
   * type string or binary.
   */
  private static final class StartsWithString extends StartsWithFunction {
    @Override
    public Object eval(final IExpression[] operands) {

      String value = operands[0].getValue(String.class);
      if (value == null)
        return null;
      String prefix = operands[1].getValue(String.class);
      if (prefix == null)
        return null;

      return value.startsWith(prefix);
    }
  }

  /**
   * The function returns TRUE if the first value starts with second value. Both values must be data
   * type binary.
   */
  private static final class StartsWithBinary extends StartsWithFunction {
    @Override
    public Object eval(final IExpression[] operands) {

      byte[] value = operands[0].getValue(byte[].class);
      if (value == null)
        return null;
      byte[] prefix = operands[1].getValue(byte[].class);
      if (prefix == null)
        return null;

      if (prefix.length > value.length) {
        return Boolean.TRUE;
      } else {
        int end = prefix.length;
        for (int i = 0; i < end; i++) {
          if (value[i] != prefix[i]) {
            return Boolean.FALSE;
          }
        }
      }
      return Boolean.TRUE;
    }
  }
}
