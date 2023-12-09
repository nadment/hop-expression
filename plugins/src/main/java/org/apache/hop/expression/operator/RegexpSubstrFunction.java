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

import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Regexp;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * Returns characters from a string by searching it for a regular expression pattern.
 */
@FunctionPlugin
public class RegexpSubstrFunction extends Function {

  public RegexpSubstrFunction() {
    super("REGEXP_SUBSTR", ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING_STRING.or(OperandTypes.STRING_STRING_NUMERIC)
            .or(OperandTypes.STRING_STRING_NUMERIC_NUMERIC)
            .or(OperandTypes.STRING_STRING_NUMERIC_NUMERIC_STRING),
        OperatorCategory.STRING, "/docs/regexp_substr.html");
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
    if (regexp.length() == 0)
      return null;

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

    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 5) {
      String v4 = operands[5].getValue(String.class);
      flags = Regexp.parseFlags(v4);
    }

    try {
      Matcher matcher = Pattern.compile(regexp, flags).matcher(input);

      boolean found = matcher.find(position);
      for (int index = 1; index < occurrence && found; index++) {
        found = matcher.find();
      }

      if (found) {
        return matcher.group(0);
      }

      return null;
    } catch (PatternSyntaxException e) {
      throw new ExpressionException(ErrorCode.INVALID_REGEXP_PATTERN, regexp);
    }
  }
}
