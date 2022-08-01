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
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;
import java.util.zip.CRC32C;

/**
 * 
 */
@FunctionPlugin(id = "CRC32", category = "i18n::Operator.Category.Cryptographic", documentationUrl = "/docs/crc32.html")
public class Crc32Function extends Function {

  public Crc32Function() {
    super("CRC32", true, ReturnTypes.INTEGER, OperandTypes.BINARY, "i18n::Operator.Category.Cryptographic", "/docs/crc32.html");
  }
  
  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].getValue(context);
    if (v0 == null)
      return null;

    byte[] bytes = Coerse.toBinary(v0);
    CRC32C crc = new CRC32C();
    crc.update(bytes, 0, bytes.length);

    return Long.toHexString(crc.getValue());
  }
}
