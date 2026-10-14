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

import java.nio.charset.StandardCharsets;
import org.apache.commons.codec.CodecPolicy;
import org.apache.commons.codec.binary.Base32;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * The function decodes the input (string or binary) using Base32 encoding.
 *
 * @see Base32EncodeFunction
 */
@FunctionPlugin
@NullMarked
public class Base32DecodeFunction extends Function {
  // Strict decoding, rejects an encoded value whose last character is not possible
  private static final Base32 BASE32 = new Base32(0, null, false, (byte) '=', CodecPolicy.STRICT);

  public Base32DecodeFunction() {
    super(
        "BASE32_DECODE",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING.or(OperandTypes.BINARY),
        OperatorCategory.STRING,
        "/docs/base32_decode.html");
  }

  @Override
  public @Nullable Object eval(IExpression[] operands) {
    byte[] value = operands[0].getValue(byte[].class);
    if (value == null) return null;

    // The decoder silently ignores the characters outside the alphabet and the characters after
    // the padding, reject them
    int padding = -1;
    for (int i = 0; i < value.length; i++) {
      if (value[i] == '=') {
        if (padding < 0) padding = i;
      } else if (padding >= 0 || !BASE32.isInAlphabet(value[i])) {
        // The padding is only allowed at the end, so report it rather than the next character
        int index = (padding >= 0) ? padding : i;
        // The previous bytes are ASCII, so the byte index is also the index of the character
        String symbol =
            Character.toString(new String(value, StandardCharsets.UTF_8).codePointAt(index));
        throw new ExpressionException(ErrorCode.INVALID_DECODING_SYMBOL, symbol, "base32");
      }
    }

    try {
      return new String(BASE32.decode(value), StandardCharsets.UTF_8);
    } catch (IllegalArgumentException e) {
      // The symbols are valid, so the last ones do not form whole bytes
      throw new ExpressionException(ErrorCode.TRUNCATED_DECODING_SEQUENCE, "base32");
    }
  }
}
