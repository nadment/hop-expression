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
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/** The function replaces input with replace that starts at pos and is of length len. */
@FunctionPlugin
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
  public Object eval(final IExpression[] operands) {
    String str = operands[0].getValue(String.class);
    if (str == null) return null;

    String replace = operands[1].getValue(String.class);
    if (replace == null) return null;

    Long position = operands[2].getValue(Long.class);
    if (position == null) return null;

    int start = position.intValue() - 1;
    int len = str.length();
    // If position is beyond the end of string, replacement is placed directly after string.
    if (start > len) {
      start = len;
    }

    // If count is omitted, it defaults to the length of replace.
    int count = replace.length();
    if (operands.length == 4) {
      Long arg3 = operands[3].getValue(Long.class);
      if (arg3 == null) return null;
      count = arg3.intValue();
      if (count == 0) {
        count = replace.length();
      } else if (count < replace.length()) {
        replace = replace.substring(0, count);
      }
    }

    int end = start + count;

    StringBuilder builder = new StringBuilder();
    builder.append(str, 0, start);
    builder.append(replace);
    if (end < len) {
      builder.append(str.substring(end));
    }

    return builder.toString();
  }
}
