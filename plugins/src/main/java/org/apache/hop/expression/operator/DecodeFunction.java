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

import org.apache.hop.expression.Category;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.type.Comparison;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Compares the select expression to each search expression in order. As soon as a search
 * expression matches the selection expression, the corresponding result expression is returned.
 */
@FunctionPlugin
public class DecodeFunction extends Function {

  public DecodeFunction() {
    super("DECODE", ReturnTypes.ARG2, OperandTypes.DECODE_FUNCTION, Category.CONDITIONAL,
        "/docs/decode.html");
  }

  @Override
  public Object eval(IExpression[] operands)
      throws Exception {
    Object value = operands[0].getValue();

    int index = -1;
    for (int i = 1, len = operands.length - 1; i < len; i += 2) {
      Object search = operands[i].getValue();
      if (Comparison.compare(value, search) == 0) {
        index = i + 1;
        break;
      }
    }
    if (index < 0 && operands.length % 2 == 0) {
      index = operands.length - 1;
    }
    if (index < 0)
      return null;

    return operands[index].getValue();
  }
}
