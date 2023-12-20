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
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;


/**
 * The function compute Levenshtein distance.
 */
@FunctionPlugin
public class Levenshtein extends Function {

  public Levenshtein() {
    super("LEVENSHTEIN", ReturnTypes.INTEGER_NULLABLE, OperandTypes.STRING_STRING, OperatorCategory.STRING,
        "/docs/levenshtein.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String str1 = operands[0].getValue(String.class);
    if (str1 == null)
      return null;
    String str2 = operands[1].getValue(String.class);
    if (str2 == null)
      return null;

    return Long.valueOf(StringUtils.getLevenshteinDistance(str1, str2));
  }
}
