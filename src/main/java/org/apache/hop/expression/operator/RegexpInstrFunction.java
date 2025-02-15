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

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Regexp;

/** */
@FunctionPlugin
public class RegexpInstrFunction extends Function {

  public RegexpInstrFunction() {
    super(
        "REGEXP_INSTR",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.STRING_STRING
            .or(OperandTypes.STRING_STRING_INTEGER)
            .or(OperandTypes.STRING_STRING_INTEGER_INTEGER)
            .or(OperandTypes.STRING_STRING_INTEGER_INTEGER_INTEGER)
            .or(OperandTypes.STRING_STRING_INTEGER_INTEGER_INTEGER_STRING),
        OperatorCategory.STRING,
        "/docs/regexp_instr.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String input = operands[0].getValue(String.class);
    if (input == null) {
      return null;
    }

    String regexp = operands[1].getValue(String.class);
    if (regexp == null) {
      return null;
    }

    // An empty pattern matches nothing
    if (regexp.isEmpty()) {
      return 0L;
    }

    // Default position 1
    int position = 1;
    if (operands.length >= 3) {
      Long v2 = operands[2].getValue(Long.class);
      if (v2 != null) {
        position = v2.intValue();
      }
    }

    // Default occurrence
    int occurrence = 0;
    if (operands.length >= 4) {
      Long v3 = operands[3].getValue(Long.class);
      if (v3 != null) {
        occurrence = v3.intValue();
      }
    }

    // Return-option
    int returnOption = 0;
    if (operands.length >= 5) {
      Long v4 = operands[4].getValue(Long.class);
      if (v4 != null) {
        returnOption = v4.intValue();
      }
    }

    // Flags
    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 6) {
      String v5 = operands[5].getValue(String.class);
      flags = Regexp.parseFlags(v5);
    }

    try {
      Matcher matcher =
          Pattern.compile(regexp, flags).matcher(input).region(position - 1, input.length());

      boolean found = matcher.find(position);
      for (int index = 1; index < occurrence && found; index++) {
        found = matcher.find();
      }

      if (found) {
        return Long.valueOf(1L + ((returnOption == 0) ? matcher.start() : matcher.end()));
      }

      return 0L;
    } catch (PatternSyntaxException e) {
      throw new ExpressionException(ErrorCode.INVALID_REGEXP_PATTERN, regexp);
    }
  }
}
