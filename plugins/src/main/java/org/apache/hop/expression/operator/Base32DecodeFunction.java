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

import org.apache.commons.codec.binary.Base32;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.nio.charset.StandardCharsets;

/**
 * The function encode the input (string or binary) using Base32 encoding.
 *
 * @see {@link Base32EncodeFunction}
 */
@FunctionPlugin
public class Base32DecodeFunction extends Function {
  private static final Base32 BASE32 = new Base32();
  
  public Base32DecodeFunction() {
    super("BASE32_DECODE", ReturnTypes.STRING, OperandTypes.STRING.or(OperandTypes.BINARY),
        OperatorCategory.STRING, "/docs/base32_decode.html");
  }

  @Override
  public Object eval(IExpression[] operands)
      throws Exception {
    Object value = operands[0].getValue();
    if (value == null)
      return null;
   
    if (value instanceof String) {
      String str = (String) value;
      return new String(BASE32.decode(str), StandardCharsets.UTF_8);
    }

    return new String(BASE32.decode(BinaryType.coerce(value)), StandardCharsets.UTF_8);
  }
}