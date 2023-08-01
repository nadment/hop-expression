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

import org.apache.hop.expression.Category;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.util.zip.CRC32C;

/**
 * 
 */
@FunctionPlugin
public class Crc32Function extends Function {

  public Crc32Function() {
    super("CRC32", ReturnTypes.STRING, OperandTypes.STRING.or(OperandTypes.BINARY),
        Category.CRYPTOGRAPHIC, "/docs/crc32.html");
  }

  @Override
    public Object eval(final IExpression[] operands) {
    byte[] bytes = operands[0].getValue(byte[].class);
    if (bytes == null)
      return null;
    CRC32C crc = new CRC32C();
    crc.update(bytes, 0, bytes.length);

    return Long.toHexString(crc.getValue());
  }
}
