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
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.util.Hex;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

/**
 * Converts the string expression to a binary value.
 */
@FunctionPlugin
public class TryToBinaryFunction extends ToBinaryFunction {

  public TryToBinaryFunction() {
    super("TRY_TO_BINARY");
  }

  @Override
    public Object eval(final IExpression[] operands) {
    final String value = operands[0].getValue(String.class);
    if (value == null)
      return null;

    final String format = operands[1].getValue(String.class);

    try {
      if (format.equals("HEX")) {
        return Hex.decode(value);
      }
      if (format.equals("UTF8")) {
        return value.getBytes(StandardCharsets.UTF_8);
      }
      if (format.equals("BASE64")) {
        return Base64.getDecoder().decode(value);
      }
    } catch (RuntimeException e) {
      return null;
    }    
    
    throw new ExpressionException(ExpressionError.INVALID_BINARY_FORMAT, format);
  }
}
