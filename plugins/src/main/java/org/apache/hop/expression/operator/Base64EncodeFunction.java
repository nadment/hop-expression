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
import java.util.Base64;

/**
 * The function encode the input (string or binary) using Base64 encoding.
 *
 * @see {@link Base64DecodeFunction}
 */
@FunctionPlugin
public class Base64EncodeFunction extends Function {

  public Base64EncodeFunction() {
    super("BASE64_ENCODE", true, ReturnTypes.STRING, OperandTypes.STRING_OR_BINARY,
        "i18n::Operator.Category.String", "/docs/base64_encode.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object value = operands[0].getValue(context);
    if (value == null)
      return null;

    if (value instanceof String) {
      String str = (String) value;
      return Base64.getEncoder().encodeToString(str.getBytes(StandardCharsets.UTF_8));
    }

    return Base64.getEncoder().encodeToString(Coerse.toBinary(value));
  }
}
