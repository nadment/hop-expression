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

import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;
import org.apache.hop.expression.util.Regexp;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * 
 */
@FunctionPlugin
public class RegexpReplaceFunction extends Function {

  public RegexpReplaceFunction() {
    super("REGEXP_REPLACE", true, ReturnTypes.STRING, OperandTypes.CUSTOM_REGEXP_REPLACE, "i18n::Operator.Category.String", "/docs/regexp_replace.html");
  }
  
  @Override  
  public Object eval(final IExpressionContext context,
      final IExpression[] operands) throws ExpressionException {
    Object v0 = operands[0].getValue(context);
    if (v0 == null) {
      return null;
    }
    String input = Coerse.toString(v0);

    Object v1 = operands[1].getValue(context);
    if (v1 == null) {
      return null;
    }
    String regexp = Coerse.toString(v1);

    // An empty pattern matches nothing
    if (regexp.length() == 0)
      return input;

    // Default empty string
    String replacement = "";
    if (operands.length >= 3) {
      Object v2 = operands[2].getValue(context);
      if (v2 != null) {
        replacement = Coerse.toString(v2);
      }
    }

    // Default position 1
    int position = 1;
    if (operands.length >= 4) {
      Object v3 = operands[3].getValue(context);
      if (v3 != null) {
        position = Coerse.toInteger(v3).intValue();
      }
    }

    // Default occurrence 0
    int occurrence = 0;
    if (operands.length >= 5) {
      Object v4 = operands[4].getValue(context);
      if (v4 != null) {
        occurrence = Coerse.toInteger(v4).intValue();
      }
    }

    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 6) {
      Object v5 = operands[5].getValue(context);
      flags = Regexp.parseFlags(Coerse.toString(v5));
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
          Pattern.compile(regexp, flags).matcher(input).region(position - 1, input.length());
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
      throw new ExpressionException(ExpressionError.INVALID_REGEXP_PATTERN, regexp);
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.REGEXP_REPLACE_ERROR, replacement);
    }
  }
}
