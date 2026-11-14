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

import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Characters;
import org.apache.hop.expression.util.Hex;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Converts an encoded hexadecimal string to a string. */
@FunctionPlugin
@NullMarked
public class HexDecodeFunction extends Function {

  /** The name of the encoding in the error messages. */
  private static final String ENCODING = "hexadecimal";

  public HexDecodeFunction() {
    super(
        "HEX_DECODE",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING,
        OperatorCategory.STRING,
        "/docs/hop-user-manual/expression/hex_decode.html");
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null) {
      return null;
    }

    // Checked here to report the invalid symbol or the truncated value like the other decoding
    // functions
    for (int i = 0; i < value.length(); i++) {
      if (!Characters.isHexDigit(value.charAt(i))) {
        throw new ExpressionException(
            ErrorCode.INVALID_DECODING_SYMBOL, Character.toString(value.codePointAt(i)), ENCODING);
      }
    }
    if (value.length() % 2 != 0) {
      throw new ExpressionException(ErrorCode.TRUNCATED_DECODING_SEQUENCE, ENCODING);
    }

    return Characters.decodeUtf8(Hex.decode(value), ENCODING);
  }
}
