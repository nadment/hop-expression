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

import org.apache.commons.lang3.Strings;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Removes all occurrences of a specified substring and optionally replaces them with another
 * string.
 */
@FunctionPlugin
public class ReplaceFunction extends Function {

  public ReplaceFunction() {
    super(
        "REPLACE",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING_STRING.or(OperandTypes.STRING_STRING_STRING),
        OperatorCategory.STRING,
        "/docs/replace.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String string = operands[0].getValue(String.class);
    if (string == null) return null;
    String search = operands[1].getValue(String.class);
    if (search == null) return null;

    if (operands.length == 3) {
      String replacement = operands[2].getValue(String.class);
      return string.replace(search, replacement);
    }

    return Strings.CS.remove(string, search);
  }
}
