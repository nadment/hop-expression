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
import java.util.Arrays;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Characters;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * The function decodes the input (string or binary) using Base64 encoding.
 *
 * <p>The whitespace characters are ignored and the padding is required. The symbols after the
 * padding and the unused bits of the last symbol are not checked.
 *
 * @see Base64EncodeFunction
 */
@FunctionPlugin
@NullMarked
public class Base64DecodeFunction extends Function {

  public Base64DecodeFunction() {
    super(
        "BASE64_DECODE",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING.or(OperandTypes.BINARY),
        OperatorCategory.STRING,
        "/docs/hop-user-manual/expression/base64_decode.html");
  }

  @Override
  public @Nullable Object eval(IExpression[] operands) {
    byte[] value = operands[0].getValue(byte[].class);
    if (value == null) return null;

    return Characters.decodeUtf8(decode(value), "base64");
  }

  /**
   * Decodes a base64 value, by blocks of 4 symbols that give 3 bytes, or less with the padding.
   * Also used by {@code TO_BINARY} with the format BASE64.
   *
   * @param value the symbols to decode
   * @return the decoded bytes
   * @throws ExpressionException if a symbol is invalid or the last block is not complete
   */
  static byte[] decode(byte[] value) {
    byte[] result = new byte[value.length / 4 * 3];
    int length = 0;
    int buffer = 0;
    // The position of the symbol in its block
    int position = 0;
    // The number of decoded bytes of the last block, set by the first padding symbol
    int end = 0;

    for (int i = 0; i < value.length; i++) {
      byte symbol = value[i];
      if (symbol == ' ' || symbol == '\t' || symbol == '\n' || symbol == '\r') continue;

      int bits;
      if (symbol == '=') {
        // The padding is only allowed at the 3rd or 4th position of a block
        if (end == 0) {
          if (position == 2) end = 1;
          else if (position == 3) end = 2;
          else throw new ExpressionException(ErrorCode.INVALID_DECODING_SYMBOL, "=", "base64");
        }
        bits = 0;
      } else {
        bits = valueOf(symbol);
        if (bits < 0) {
          // The previous bytes are ASCII, so the byte index is also the index of the character
          String character =
              Character.toString(new String(value, StandardCharsets.UTF_8).codePointAt(i));
          throw new ExpressionException(ErrorCode.INVALID_DECODING_SYMBOL, character, "base64");
        }
      }

      buffer = (buffer << 6) + bits;
      if (++position == 4) {
        result[length++] = (byte) (buffer >> 16);
        if (end == 0 || end > 1) result[length++] = (byte) (buffer >> 8);
        if (end == 0 || end > 2) result[length++] = (byte) buffer;
        buffer = 0;
        position = 0;
      }
    }

    // The value is missing padding or is truncated
    if (position != 0) {
      throw new ExpressionException(ErrorCode.TRUNCATED_DECODING_SEQUENCE, "base64");
    }

    return Arrays.copyOf(result, length);
  }

  /** Returns the 6 bits of a symbol of the standard base64 alphabet, or -1 if it is invalid. */
  private static int valueOf(byte symbol) {
    if (symbol >= 'A' && symbol <= 'Z') return symbol - 'A';
    if (symbol >= 'a' && symbol <= 'z') return symbol - 'a' + 26;
    if (symbol >= '0' && symbol <= '9') return symbol - '0' + 52;
    if (symbol == '+') return 62;
    if (symbol == '/') return 63;
    return -1;
  }
}
