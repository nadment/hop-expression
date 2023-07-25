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

import org.apache.hop.expression.Function;
import org.apache.hop.expression.IExpression;
import java.io.ByteArrayOutputStream;


/**
 * Binary concatenation operator '<code>||</code>'
 */
public class ConcatBinaryFunction extends ConcatFunction {
  static final Function INSTANCE = new ConcatBinaryFunction();
  
  // Function
  public ConcatBinaryFunction() {
    super();
  }

  @Override
  public Object eval(final IExpression[] operands) throws Exception {

    Object firstNotNull = null;
    Object[] values = new Object[operands.length];
    int i = 0;
    for (IExpression operand : operands) {
      Object value = operand.getValue();
      if (firstNotNull == null && value != null)
        firstNotNull = value;
      values[i++] = value;
    }

    if (firstNotNull == null)
      return null;

      ByteArrayOutputStream output = new ByteArrayOutputStream();
      for (Object value : values) {
        if (value != null) {
          output.write((byte[]) value);
        }
      }
      return output.toByteArray();
  }
}
