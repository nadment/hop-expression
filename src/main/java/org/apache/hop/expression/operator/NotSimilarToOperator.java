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

import java.io.StringWriter;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.hop.core.util.Utils;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Regexp;

/**
 * Comparison NOT SIMILAR TO operator. <br>
 * <strong>Syntax:</strong> <code>'abc' NOT SIMILAR TO 'regexp'</code>
 */
public class NotSimilarToOperator extends BinaryOperator {
  public static final NotSimilarToOperator INSTANCE = new NotSimilarToOperator();

  public NotSimilarToOperator() {
    super(
        "NOT SIMILAR TO",
        "NOT SIMILAR TO",
        10,
        Associativity.LEFT,
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.STRING_STRING,
        OperatorCategory.COMPARISON,
        "/docs/similar-to.html");
  }

  @Override
  public Operator not() {
    return SimilarToOperator.INSTANCE;
  }

  @Override
  public Object eval(final IExpression[] operands) {
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

    pattern = Regexp.toSimilarTo(pattern, '\\');
    Matcher matcher = Pattern.compile(pattern, Pattern.UNICODE_CASE).matcher(value);
    return !matcher.matches();
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append(" NOT SIMILAR TO ");
    operands[1].unparse(writer, getLeftPrec(), getRightPrec());
  }
}
