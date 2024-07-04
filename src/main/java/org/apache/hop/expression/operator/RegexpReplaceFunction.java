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
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.TypeFamily;
import org.apache.hop.expression.util.Regexp;

/** */
@FunctionPlugin
public class RegexpReplaceFunction extends Function {

  private static final IOperandTypeChecker OTC =
      OperandTypes.family(
              TypeFamily.STRING,
              TypeFamily.STRING,
              TypeFamily.STRING,
              TypeFamily.NUMERIC,
              TypeFamily.NUMERIC,
              TypeFamily.STRING)
          .optional(i -> i >= 2);

  public RegexpReplaceFunction() {
    super(
        "REGEXP_REPLACE",
        ReturnTypes.STRING_NULLABLE,
        OTC,
        OperatorCategory.STRING,
        "/docs/regexp_replace.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String input = operands[0].getValue(String.class);
    if (input == null) {
      return null;
    }

    String pattern = operands[1].getValue(String.class);
    if (pattern == null) {
      return null;
    }

    // An empty pattern matches nothing
    if (pattern.length() == 0) return input;

    // Default empty string
    String replacement = "";
    if (operands.length >= 3) {
      replacement = operands[2].getValue(String.class);
    }

    // Default position 1
    int position = 1;
    if (operands.length >= 4) {
      Long v3 = operands[3].getValue(Long.class);
      if (v3 != null) {
        position = v3.intValue();
      }
    }

    // Default occurrence 0
    int occurrence = 0;
    if (operands.length >= 5) {
      Long v4 = operands[4].getValue(Long.class);
      if (v4 != null) {
        occurrence = v4.intValue();
      }
    }

    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 6) {
      String v5 = operands[5].getValue(String.class);
      flags = Regexp.parseFlags(v5);
    }

    try {

      // Back reference
      if ((replacement.indexOf('\\') >= 0) || (replacement.indexOf('$') >= 0)) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < replacement.length(); i++) {
          char c = replacement.charAt(i);
          if (c == '$') {
            builder.append('\\');
          } else if (c == '\\' && ++i < replacement.length()) {
            c = replacement.charAt(i);
            builder.append(c >= '0' && c <= '9' ? '$' : '\\');
          }
          builder.append(c);
        }
        replacement = builder.toString();
      }

      Matcher matcher =
          Pattern.compile(pattern, flags).matcher(input).region(position - 1, input.length());
      if (occurrence == 0) {
        return matcher.replaceAll(replacement);
      } else {
        StringBuffer buffer = new StringBuffer();
        int index = 1;
        while (matcher.find()) {
          if (index == occurrence) {
            matcher.appendReplacement(buffer, replacement);
            break;
          }
          index++;
        }
        matcher.appendTail(buffer);
        return buffer.toString();
      }
    } catch (PatternSyntaxException e) {
      throw new ExpressionException(ErrorCode.INVALID_REGEXP_PATTERN, pattern);
      // } catch (Exception e) {
      /// throw new ExpressionException(ExpressionError.REGEXP_REPLACE_ERROR, replacement);
    }
  }
}
