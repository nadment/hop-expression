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

import org.apache.hop.core.util.Utils;
import org.apache.hop.expression.Category;
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Regexp;
import java.io.StringWriter;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * Comparison SIMILAR TO operator.
 * <br>
 * <strong>Syntax:</strong> <code>'abc' SIMILAR TO 'regexp'</code>
 */
public class SimilarToOperator extends Operator {

  public SimilarToOperator() {
    super("SIMILAR TO", 10, true, ReturnTypes.BOOLEAN, OperandTypes.STRING_STRING,
        Category.COMPARISON, "/docs/similar-to.html");
  }

  @Override
  public Object eval(IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null) {
      return null;
    }

    String pattern = operands[1].getValue(String.class);
    if (pattern == null) {
      return null;
    }

    if (Utils.isEmpty(pattern)) {
      return false;
    }

    try {
      pattern = Regexp.toSimilarTo(pattern, '\\');

      Matcher matcher = Pattern.compile(pattern, Pattern.UNICODE_CASE).matcher(value);
      return matcher.matches();
    } catch (PatternSyntaxException e) {
      throw new IllegalArgumentException(ExpressionError.INVALID_REGEXP_PATTERN.message(pattern));
    }
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" SIMILAR TO ");
    operands[1].unparse(writer);
  }
}
