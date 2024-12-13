/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
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

/**
 * Searches a string for a regular expression pattern and returns an integer that indicates the
 * number of times the pattern occurs in the string. If no match is found, then the function returns
 * 0.
 *
 * <p>REGEXP_COUNT ( source, pattern [, start [, parameters ] ] )
 */
@FunctionPlugin
public class RegexpCountFunction extends Function {

  public RegexpCountFunction() {
    super(
        "REGEXP_COUNT",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.STRING_STRING
            .or(OperandTypes.STRING_STRING_INTEGER)
            .or(OperandTypes.STRING_STRING_INTEGER_STRING),
        OperatorCategory.STRING,
        "/docs/regexp_count.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {

    long count = 0L;

    String source = operands[0].getValue(String.class);
    if (source == null) {
      return null;
    }

    String pattern = operands[1].getValue(String.class);
    if (pattern == null) {
      return count;
    }

    // An empty pattern matches nothing
    if (pattern.length() == 0) return count;

    int start = 1;
    int parameters = Pattern.UNICODE_CASE;
    if (operands.length >= 3) {
      Long v2 = operands[2].getValue(Long.class);
      start = v2.intValue();

      if (operands.length == 4) {
        String v3 = operands[3].getValue(String.class);
        parameters = Regexp.parseFlags(v3);
      }
    }

    try {
      Matcher matcher =
          Pattern.compile(pattern, parameters).matcher(source).region(start - 1, source.length());
      while (matcher.find()) {
        count++;
      }
    } catch (PatternSyntaxException e) {
      throw new ExpressionException(ErrorCode.INVALID_REGEXP_PATTERN, pattern);
    }

    return count;
  }
}
