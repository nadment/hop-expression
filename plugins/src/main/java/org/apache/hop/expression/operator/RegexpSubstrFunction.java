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
import org.apache.hop.expression.type.Coerce;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Regexp;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * 
 */
@FunctionPlugin
public class RegexpSubstrFunction extends Function {

  // TODO: Check operands type
  public RegexpSubstrFunction() {
    super("REGEXP_SUBSTR", true, ReturnTypes.STRING, OperandTypes.STRING_STRING_OPTIONAL_NUMERIC_NUMERIC_STRING,
        "i18n::Operator.Category.String", "/docs/regexp_substr.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object v0 = operands[0].getValue(context);
    if (v0 == null) {
      return null;
    }
    String input = Coerce.toString(v0);

    Object v1 = operands[1].getValue(context);
    if (v1 == null) {
      return null;
    }
    String regexp = Coerce.toString(v1);
    // An empty pattern matches nothing
    if (regexp.length() == 0)
      return null;

    // Default position 1
    int position = 1;
    if (operands.length >= 3) {
      Object v2 = operands[2].getValue(context);
      if (v2 != null) {
        position = Coerce.toInteger(v2).intValue();
      }
    }

    // Default occurrence
    int occurrence = 0;
    if (operands.length >= 4) {
      Object v3 = operands[3].getValue(context);
      if (v3 != null) {
        occurrence = Coerce.toInteger(v3).intValue();
      }
    }

    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 5) {
      Object v4 = operands[5].getValue(context);
      flags = Regexp.parseFlags(Coerce.toString(v4));
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
      throw new ExpressionException(ExpressionError.INVALID_REGEXP_PATTERN, regexp);
    }
  }



}
