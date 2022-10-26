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
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;
import java.nio.charset.StandardCharsets;

/**
 * Converts a string, number or binary value to a hexadecimal string.
 */
@FunctionPlugin
public class ToHexFunction extends Function {

  private static final byte[] HEX = "0123456789abcdef".getBytes(StandardCharsets.US_ASCII);

  public ToHexFunction() {
    super("TO_HEX", true, ReturnTypes.STRING, OperandTypes.STRING_OR_BINARY_OR_NUMERIC,
        "i18n::Operator.Category.Conversion", "/docs/to_hex.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object v0 = operands[0].getValue(context);
    if (v0 == null) {
      return null;
    }

    if ( v0 instanceof Number ) {
      return Long.toHexString(Coerse.toInteger(v0));
    }
    
    byte[] bytes = Coerse.toBinary(v0);
    byte[] hexChars = new byte[bytes.length * 2];
    for (int j = 0; j < bytes.length; j++) {
      int v = bytes[j] & 0xFF;
      hexChars[j * 2] = HEX[v >>> 4];
      hexChars[j * 2 + 1] = HEX[v & 0x0F];
    }
    return new String(hexChars, StandardCharsets.UTF_8);
  }
}
