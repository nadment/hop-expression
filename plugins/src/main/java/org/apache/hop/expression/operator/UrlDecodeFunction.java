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
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;

/**
 * The function decode the URL to a string.
 *
 * @see {@link UrlEncodeFunction}
 */
@FunctionPlugin(id = "URLDECODE", category = "i18n::Operator.Category.String", documentationUrl = "/docs/urldecode.html")
public class UrlDecodeFunction extends Function {

  public UrlDecodeFunction() {
    super("URLDECODE", true, ReturnTypes.STRING, OperandTypes.STRING, "i18n::Operator.Category.String", "/docs/urldecode.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].getValue(context);
    if (value == null)
      return null;
    try {
      return URLDecoder.decode(Coerse.toString(value), StandardCharsets.UTF_8.name());
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.URLDECODE_ERROR, value, e.getMessage());
    }
  }
}
