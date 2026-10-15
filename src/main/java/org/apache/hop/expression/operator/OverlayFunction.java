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
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** The function replaces input with replace that starts at pos and is of length len. */
@FunctionPlugin
@NullMarked
public class OverlayFunction extends Function {

  public OverlayFunction() {
    super(
        "OVERLAY",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING_STRING_INTEGER.or(OperandTypes.STRING_STRING_INTEGER_INTEGER),
        OperatorCategory.STRING,
        "/docs/overlay.html");
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    String str = operands[0].getValue(String.class);
    if (str == null) return null;

    String replace = operands[1].getValue(String.class);
    if (replace == null) return null;

    Long position = operands[2].getValue(Long.class);
    if (position == null) return null;

    if (position < 1) {
      throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 3, position);
    }
    // The positions are in characters, a surrogate pair is a single character
    int len = Characters.length(str);
    // If position is beyond the end of string, replacement is placed directly after string.
    // Compare the long value, the conversion to int would wrap a value out of the int range
    int start = (int) Math.min(position - 1, len);

    // If count is omitted, it defaults to the length of replace.
    int count = Characters.length(replace);
    if (operands.length == 4) {
      Long arg3 = operands[3].getValue(Long.class);
      if (arg3 == null) return null;
      if (arg3 < 0) {
        throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 4, arg3);
      }
      // Clamp to the int range, a count beyond the end of string replaces up to the end
      count = Math.clamp(arg3, 0, Integer.MAX_VALUE);
      if (count == 0) {
        count = Characters.length(replace);
      } else if (count < Characters.length(replace)) {
        replace = replace.substring(0, Characters.offset(replace, count));
      }
    }

    // A long sum, a huge count would overflow the int range
    long end = (long) start + count;

    StringBuilder builder = new StringBuilder();
    builder.append(str, 0, Characters.offset(str, start));
    builder.append(replace);
    if (end < len) {
      builder.append(str.substring(Characters.offset(str, end)));
    }

    return builder.toString();
  }
}
