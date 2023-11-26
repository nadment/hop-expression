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
 * The function returns TRUE if the first value ends with second value. Both values must be data
 * type of string or binary.
 *
 * @see {@link StartWithFunction}
 */
@FunctionPlugin
public class EndsWithFunction extends Function {
  public static final EndsWithFunction EndsWithString = new EndsWithString();
  public static final EndsWithFunction EndsWithBinary = new EndsWithBinary();

  public EndsWithFunction() {
    super("ENDSWITH", ReturnTypes.BOOLEAN,
        OperandTypes.STRING_STRING.or(OperandTypes.BINARY_BINARY), Category.COMPARISON,
        "/docs/endswith.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getOperand(0).getType();
    if (type.isFamily(TypeFamily.BINARY)) {
      return new Call(EndsWithBinary, call.getOperands());
    }

    return new Call(EndsWithString, call.getOperands());
  }


  /**
   * The function returns TRUE if the first value ends with second value. Both values must be data
   * type of string.
   */
  private static final class EndsWithString extends EndsWithFunction {
    @Override
    public Object eval(final IExpression[] operands) {
      String value = operands[0].getValue(String.class);
      if (value == null)
        return null;
      String suffix = operands[1].getValue(String.class);
      if (suffix == null)
        return null;

      return value.endsWith(suffix);
    }
  }

  /**
   * The function returns TRUE if the first value ends with second value. Both values must be data
   * type of binary.
   */
  private static final class EndsWithBinary extends EndsWithFunction {

    @Override
    public Object eval(final IExpression[] operands) {
      byte[] value = operands[0].getValue(byte[].class);
      if (value == null)
        return null;
      byte[] suffix = operands[1].getValue(byte[].class);
      if (suffix == null)
        return null;

      int offset = value.length - suffix.length;

      if (offset < 0) {
        return Boolean.FALSE;
      } else {
        for (int i = 0; i < suffix.length; i++) {
          if (value[offset + i] != suffix[i]) {
            return Boolean.FALSE;
          }
        }
      }
      return Boolean.TRUE;
    }
  }
}
