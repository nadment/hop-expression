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

import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;

/**
 * The function return the Unicode code point for the first Unicode character in a string. If the
 * string is empty, a value of 0 is returned.
 *
 * @see {@link ChrFunction}, {@link AsciiFunction},
 */
@FunctionPlugin(id = "UNICODE", category = "i18n::Operator.Category.String", documentationUrl = "/docs/unicode.html")
public class UnicodeFunction extends Function {

  public UnicodeFunction() {
    super("UNICODE", true, ReturnTypes.INTEGER, OperandTypes.STRING, "i18n::Operator.Category.String", "/docs/unicode.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].getValue(context);
    if (value == null)
      return null;
    String string = Coerse.toString(value);
    int codePoint = 0;
    if (string.length() > 0) {
      codePoint = string.codePointAt(0);
    }
    return Long.valueOf(codePoint);
  }
}