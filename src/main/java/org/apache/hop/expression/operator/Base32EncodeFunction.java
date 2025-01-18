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
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * The function encode the input (string or binary) using Base32 encoding.
 *
 * @see Base32DecodeFunction
 */
@FunctionPlugin
public class Base32EncodeFunction extends Function {
  private static final Base32 BASE32 = new Base32();

  public Base32EncodeFunction() {
    super(
        "BASE32_ENCODE",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING.or(OperandTypes.BINARY),
        OperatorCategory.STRING,
        "/docs/base32_encode.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    byte[] value = operands[0].getValue(byte[].class);
    if (value == null) return null;

    return BASE32.encodeToString(value);
  }
}
