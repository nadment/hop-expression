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

/**
 * The function left-pads a string with another string, to a certain length.
 *
 * @see {@link RPadFunction}
 */
@FunctionPlugin
public class LPadFunction extends Function {

  /** The maximum size to which the padding can expand. */
  private static final int PAD_LIMIT = 8192;

  public LPadFunction() {
    super("LPAD", ReturnTypes.STRING, OperandTypes.STRING_NUMERIC_OPTIONAL_STRING,
        OperatorCategory.STRING, "/docs/lpad.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    String value = operands[0].getValue(context, String.class);
    if (value == null)
      return null;

    Long v1 = operands[1].getValue(context, Long.class);
    int length = v1.intValue();

    // If this parameter is omitted, the function will pad spaces
    String pad = null;
    if (operands.length == 3) {
      pad = operands[2].getValue(context, String.class);      
    }

    if (length < 0) {
      length = 0;
    } else if (length > PAD_LIMIT) {
      throw new ExpressionException("Paddind length exceeds maximum limit: " + PAD_LIMIT);
    }

    // If this parameter is omitted, the function will pad spaces
    if (pad == null) {
      pad = " ";
    }

    final int size = pad.length();
    final int index = length - value.length();

    if (index <= 0) {
      value = value.substring(0, length);
    } else if (size == 0) {
      // nothing to do
    } else if (index == size) {
      value = pad.concat(value);
    } else if (index < size) {
      value = pad.substring(0, index).concat(value);
    } else {
      final char[] padding = new char[index];
      final char[] padChars = pad.toCharArray();
      for (int i = 0; i < index; i++) {
        padding[i] = padChars[i % size];
      }
      value = new String(padding).concat(value);
    }

    return value;
  }

}
