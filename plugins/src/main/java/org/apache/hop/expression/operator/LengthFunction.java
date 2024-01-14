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
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeFamily;

/**
 * The function returns the number of characters of the specified string or binary.
 */
@FunctionPlugin
public class LengthFunction extends Function {
  public static final LengthFunction LengthString = new LengthStringFunction();
  public static final LengthFunction LengthBinary = new LengthBinaryFunction();

  public LengthFunction() {
    super("LENGTH", ReturnTypes.INTEGER_NULLABLE, OperandTypes.STRING.or(OperandTypes.BINARY),
        OperatorCategory.STRING, "/docs/length.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getOperand(0).getType();

    // Binary first
    if (type.isFamily(TypeFamily.BINARY)) {
      return new Call(LengthBinary, call.getOperands());
    }
    return new Call(LengthString, call.getOperands());
  }

  /**
   * The function returns the number of characters of the specified string.
   */
  private static final class LengthStringFunction extends LengthFunction {
    @Override
    public Object eval(final IExpression[] operands) {
      String value = operands[0].getValue(String.class);
      if (value == null)
        return value;
      return Long.valueOf(value.length());
    }
  }

  /**
   * The function returns the number of characters of the specified binary.
   */
  private static final class LengthBinaryFunction extends LengthFunction {
    @Override
    public Object eval(final IExpression[] operands) {
      byte[] value = operands[0].getValue(byte[].class);
      if (value == null)
        return value;
      return Long.valueOf(value.length);
    }
  }
}
