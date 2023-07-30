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
import org.apache.hop.expression.Category;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.StringType;

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

  private static final IOperandTypeChecker OTC = OperandTypes.STRING.or(OperandTypes.STRING_STRING)
      .or(OperandTypes.STRING_NUMERIC).or(OperandTypes.STRING_STRING_NUMERIC);

  public StrtokFunction() {
    super("STRTOK", ReturnTypes.STRING, OTC, Category.STRING, "/docs/strtok.html");
  }

  @Override
  public Object eval(IExpression[] operands) {
    String str = operands[0].getValue(String.class);
    if (str == null)
      return null;


    // Default value
    String delimiter = " ";
    int index = 1;

    if (operands.length == 2) {
      Object v1 = operands[1].getValue();
      if (v1 == null)
        return null;

      if (v1 instanceof Number) {
        index = IntegerType.coerce(v1).intValue();
      } else {
        delimiter = StringType.coerce(v1);
      }
    } else if (operands.length == 3) {
      delimiter = operands[1].getValue(String.class);
      if (delimiter == null)
        return null;

      Long v2 = operands[2].getValue(Long.class);
      if (v2 == null)
        return null;
      index = v2.intValue();
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
