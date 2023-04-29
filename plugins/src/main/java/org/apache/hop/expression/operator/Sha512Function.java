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

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * The function calculate the SHA-512 hash of a data value. The hash will be returned as a 128
 * characters hex-encoded string.
 *
 * @see {@link Md5Function}, {@link Sha1Function}, {@link Sha256Function}, {@link Sha384Function}
 */
@FunctionPlugin
public class Sha512Function extends Function {

  public Sha512Function() {
    super("SHA512", ReturnTypes.STRING, OperandTypes.STRING.or(OperandTypes.BINARY),
        OperatorCategory.CRYPTOGRAPHIC, "/docs/sha512.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    byte[] value = operands[0].getValue(context, byte[].class);
    if (value == null) {
      return null;
    }
    return DigestUtils.sha512Hex(value);
  }
}
