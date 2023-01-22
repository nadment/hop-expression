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
import org.apache.hop.expression.type.Coerce;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Base64.Decoder;

/**
 * The function encode the input (string or binary) using Base64 encoding.
 *
 * @see {@link Base64DecodeFunction}
 */
@FunctionPlugin
public class Base64DecodeFunction extends Function {

  public Base64DecodeFunction() {
    super("BASE64_DECODE", true, ReturnTypes.STRING, OperandTypes.STRING.or(OperandTypes.BINARY),
        "i18n::Operator.Category.String", "/docs/base64_decode.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object value = operands[0].getValue(context);
    if (value == null)
      return null;

    Decoder decoder = Base64.getDecoder();
    if (value instanceof String) {
      String str = (String) value;
      return new String(decoder.decode(str), StandardCharsets.UTF_8);
    }

    return new String(decoder.decode(Coerce.toBinary(value)), StandardCharsets.UTF_8);
  }
}
