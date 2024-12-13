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

import org.apache.hop.expression.Array;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * The ARRAY_TO_STRING function convert an input array to a string by casting all values to strings
 * and concatenating them, using the string from the second argument to separate the elements.
 */
@FunctionPlugin
public class ArrayToStringFunction extends Function {

  public static final Function INSTANCE = new ArrayToStringFunction();

  public ArrayToStringFunction() {
    super(
        "ARRAY_TO_STRING",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.ARRAY_STRING,
        OperatorCategory.ARRAY,
        "/docs/array_to_string.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Array array = (Array) operands[0];
    if (array == null) return null;
    String separator = operands[1].getValue(String.class);
    if (separator == null) return null;

    StringBuilder builder = new StringBuilder();
    join(builder, array, separator);
    return builder.toString();
  }

  public void join(final StringBuilder builder, final Array array, final String separator) {
    boolean first = true;
    for (IExpression expression : array) {
      if (first) first = false;
      else {
        builder.append(separator);
      }
      Object value = expression.getValue();
      if (value instanceof Array children) {
        join(builder, children, separator);
      } else {
        builder.append(value);
      }
    }
  }
}
