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
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Base64.Decoder;

/**
 * The function encode the input (string or binary) using Base64 encoding.
 *
 * @see {@link Base64EncodeFunction}
 */
@FunctionPlugin
public class Base64DecodeFunction extends Function {
  private static final Decoder DECODER = Base64.getDecoder();

  public Base64DecodeFunction() {
    super("BASE64_DECODE", ReturnTypes.STRING_NULLABLE, OperandTypes.STRING.or(OperandTypes.BINARY),
        Category.STRING, "/docs/base64_decode.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    byte[] value = operands[0].getValue(byte[].class);
    if (value == null)
      return null;

    return new String(DECODER.decode(value), StandardCharsets.UTF_8);
  }
}
