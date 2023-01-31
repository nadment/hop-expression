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
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.NumberFormat;

/**
 * Converts a string expression to a number value.
 */
@FunctionPlugin
public class ToNumberFunction extends Function {

  public ToNumberFunction() {
    super("TO_NUMBER", true, ReturnTypes.BIGNUMBER, OperandTypes.STRING_OPTIONAL_STRING,
        OperatorCategory.CONVERSION, "/docs/to_number.html");
  }

  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    String value = operands[0].getValue(context, String.class);
    if (value == null)
      return null;

    try {     
      String format = "TM";

      // With format
      if (operands.length == 2) {
        format = operands[1].getValue(context, String.class);
      }
      return NumberFormat.of(format).parse(value);
    } catch (Exception e) {
      throw new ExpressionException(e.getMessage());
    }
  }
}
