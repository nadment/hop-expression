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
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.Coerce;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Splits a string on the specified list of delimiter characters and returns the part at the
 * specified position.
 * 
 * @param context The expression context
 * @param operands The operands
 * @return value
 * @throws Exception
 * @See {@link SplitPartFunction}
 */
@FunctionPlugin
public class StrtokFunction extends Function {

  private static final IOperandTypeChecker OTC = OperandTypes.STRING.or(OperandTypes.STRING_STRING).or(OperandTypes.STRING_NUMERIC).or(OperandTypes.STRING_STRING_NUMERIC);
  
  public StrtokFunction() {
    super("STRTOK", true, ReturnTypes.STRING, OTC, OperatorCategory.STRING, "/docs/strtok.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object v0 = operands[0].getValue(context);
    if (v0 == null)
      return null;
    String str = Coerce.toString(v0);

    // Default value
    String delimiter = " ";
    int index = 1;

    if (operands.length == 2) {
      Object v1 = operands[1].getValue(context);
      if (v1 == null)
        return null;

      if (v1 instanceof Number) {
        index = Coerce.toInteger(v1).intValue();
      } else {
        delimiter = Coerce.toString(v1);
      }
    } else if (operands.length == 3) {
      Object v1 = operands[1].getValue(context);
      if (v1 == null)
        return null;
      delimiter = Coerce.toString(v1);
      Object v2 = operands[2].getValue(context);
      if (v2 == null)
        return null;
      index = Coerce.toInteger(v2).intValue();
    }

    String[] parts = StringUtils.splitPreserveAllTokens(str, delimiter, 256);

    if (index < 0)
      index += parts.length + 1;

    // If the part index is out of range, the returned value is an empty string.
    if (index < 1 || index > parts.length) {
      return null;
    }

    return parts[index - 1];
  }



}
