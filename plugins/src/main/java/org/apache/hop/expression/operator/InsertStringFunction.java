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

import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.IExpression;

/**
 * Replaces a substring of the specified length, starting at the specified position, with a new
 * string value.
 */
public class InsertStringFunction extends InsertFunction {

  public static final InsertStringFunction INSTANCE = new InsertStringFunction();

  public InsertStringFunction() {
    super();
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String original = operands[0].getValue(String.class);
    if (original == null)
      return null;
    Long pos = operands[1].getValue(Long.class);
    if (pos == null)
      return null;
    Long len = operands[2].getValue(Long.class);
    if (len == null)
      return null;
    String insert = operands[3].getValue(String.class);
    if (insert == null)
      return null;

    // Valid values are between 1 and one more than the length of the string (inclusive).
    if (pos <= 0 || pos > original.length() + 1)
      throw new IllegalArgumentException(ExpressionError.ARGUMENT_OUT_OF_RANGE.message(pos));

    // Valid values range from 0 to the number of characters between pos and the end of the string.
    if (len < 0 || len > original.length() - pos + 1)
      throw new IllegalArgumentException(ExpressionError.ARGUMENT_OUT_OF_RANGE.message(len));

    int start = Math.min(Math.max(0, pos.intValue() - 1), original.length());
    int length = Math.min(len.intValue(), original.length());

    StringBuilder builder = new StringBuilder();
    builder.append(original.substring(0, start));
    builder.append(insert);
    builder.append(original.substring(start + length));
    return builder.toString();
  }
}
