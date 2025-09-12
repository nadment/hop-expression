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

import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.StringUtils;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.StringType;

/** Splits a string into an array of strings based on a specified delimiter. */
@FunctionPlugin
public class StringToArrayFunction extends Function {

  public static final Function INSTANCE = new StringToArrayFunction();

  private static final ArrayType DEFAULT_TYPE = ArrayType.of(StringType.STRING);

  public StringToArrayFunction() {
    super(
        "STRING_TO_ARRAY",
        ReturnTypes.ARRAY,
        OperandTypes.STRING_STRING,
        OperatorCategory.ARRAY,
        "/docs/string_to_array.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String str = operands[0].getValue(String.class);
    if (str == null) return null;

    String delimiter = operands[1].getValue(String.class);
    if (delimiter == null) return null;

    String[] parts = StringUtils.splitByWholeSeparator(str, delimiter, -1);

    List<IExpression> list = new ArrayList<>();
    for (String s : parts) {
      list.add(Literal.of(s));
    }

    return new Array(DEFAULT_TYPE, list);
  }
}
