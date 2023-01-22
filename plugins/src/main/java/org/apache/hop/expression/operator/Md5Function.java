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
import org.apache.hop.expression.type.Coerce;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * The function calculate the MD5 hash of a data value.
 * The hash will be returned as a 32 characters hex-encoded string.
 *
 * @see {@link Sha1Function}, {@link Sha224Function}, {@link Sha256Function},
 *      {@link Sha384Function}, {@link Sha512Function}
 */
@FunctionPlugin
public class Md5Function extends Function {

  public Md5Function() {
    super("MD5", true, ReturnTypes.STRING, OperandTypes.STRING.or(OperandTypes.BINARY),
        "i18n::Operator.Category.Cryptographic", "/docs/md5.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {

    Object v0 = operands[0].getValue(context);
    if (v0 == null) {
      return null;
    }

    return DigestUtils.md5Hex(Coerce.toBinary(v0));
  }
}
