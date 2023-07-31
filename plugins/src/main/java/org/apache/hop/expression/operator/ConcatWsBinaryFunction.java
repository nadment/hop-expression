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

import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.exception.ExpressionException;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

/**
 * Binary concatenation function with separator
 */
public class ConcatWsBinaryFunction extends ConcatWsFunction {
  static final ConcatWsBinaryFunction INSTANCE = new ConcatWsBinaryFunction();

  public ConcatWsBinaryFunction() {
    super();
  }

  @Override
  public Object eval(final IExpression[] operands) {

    byte[] separator = operands[0].getValue(byte[].class);
    if (separator == null)
      return null;

    boolean notFirstValue = false;

    try {
      ByteArrayOutputStream output = new ByteArrayOutputStream();
      for (int i = 1; i < operands.length; i++) {
        byte[] value = operands[i].getValue(byte[].class);

        if (value != null) {
          if (notFirstValue) {
            output.write(separator);
          }
          notFirstValue = true;
          output.write(value);
        }
      }

      if (output.size() == 0)
        return null;

      return output.toByteArray();
    } catch (IOException e) {
      throw new ExpressionException(e.getMessage());
    }
  }
}
