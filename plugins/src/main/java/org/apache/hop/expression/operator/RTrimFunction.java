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

import org.apache.commons.lang.StringUtils;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.Coerce;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * The function removes leading characters from a string.
 *
 * @see {@link TrimFunction}, {@link LTrimFunction}
 */
@FunctionPlugin
public class RTrimFunction extends Function {

  public RTrimFunction() {
    super("RTRIM", true, ReturnTypes.STRING, OperandTypes.STRING_OPTIONAL_STRING,
        "i18n::Operator.Category.String", "/docs/rtrim.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object value = operands[0].getValue(context);
    if (value == null)
      return null;

    String string = Coerce.toString(value);
    String characters = null;

    if (operands.length == 2) {
      Object stripChars = operands[1].getValue(context);
      if (stripChars == null)
        return null;
      characters = Coerce.toString(stripChars);
    }

    return StringUtils.stripEnd(string, characters);
  }
}
