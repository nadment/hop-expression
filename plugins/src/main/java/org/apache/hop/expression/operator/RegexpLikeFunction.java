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
 * The REGEXP_LIKE function and RLIKE operator.
 */
@FunctionPlugin
public class RegexpLikeFunction extends Function {

  public RegexpLikeFunction() {
    super("REGEXP_LIKE", true, ReturnTypes.BOOLEAN, OperandTypes.STRING_STRING_OPTIONAL_STRING,
        "i18n::Operator.Category.Comparison", "/docs/regexp_like.html");
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
    String pattern = Coerce.toString(v1);
    // An empty pattern matches nothing
    if (pattern.length() == 0)
      return Boolean.FALSE;

    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 3) {
      Object v2 = operands[2].getValue(context);
      flags = Regexp.parseFlags(Coerce.toString(v2));
    }

    try {
      Matcher matcher = Pattern.compile(pattern, flags).matcher(input);
      return matcher.find();
    } catch (PatternSyntaxException e) {
      throw new ExpressionException(ExpressionError.INVALID_REGEXP_PATTERN, pattern);
    }
  }
}
