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
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;


/**
 * Splits a string on the specified string delimiter and returns the part at the specified
 * position.
 * 
 * @param context
 * @param operands
 * @return
 * @throws ExpressionException
 * @See {@link StrtokFunction}
 */
@FunctionPlugin
public class SplitPartFunction extends Function {

  public SplitPartFunction() {
    super("SPLIT_PART", true, ReturnTypes.STRING, OperandTypes.STRING_STRING_NUMERIC, "i18n::Operator.Category.String", "/docs/split_part.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].getValue(context);
    if (v0 == null)
      return null;

    Object v1 = operands[1].getValue(context);
    if (v1 == null)
      return null;

    Object v2 = operands[2].getValue(context);
    if (v2 == null)
      return null;

    String str = Coerse.toString(v0);
    String delimiter = Coerse.toString(v1);
    int index = Coerse.toInteger(v2).intValue();

    String[] parts = StringUtils.splitByWholeSeparator(str, delimiter, -1);

    // If the part number is negative, the parts are counted backward from the end of the string.
    if (index < 0)
      index += parts.length + 1;

    // If the part index is out of range, the returned value is an empty string.
    if (index < 1 || index > parts.length) {
      return "";
    }

    return parts[index - 1];
  }

}
