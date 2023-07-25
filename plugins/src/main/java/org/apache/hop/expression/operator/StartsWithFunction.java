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
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.StringType;

/**
 * The function returns TRUE if the first value starts with second value. Both values must be data
 * type string or binary.
 *
 * @see {@link EndsWithFunction}
 */
@FunctionPlugin
public class StartsWithFunction extends Function {

  public StartsWithFunction() {
    super("STARTSWITH", ReturnTypes.BOOLEAN,
        OperandTypes.STRING_STRING.or(OperandTypes.BINARY_BINARY), Category.COMPARISON,
        "/docs/startswith.html");
  }

  @Override
  public Object eval(IExpression[] operands)
      throws Exception {

    Object v0 = operands[0].getValue();
    if (v0 == null)
      return null;
    Object v1 = operands[1].getValue();
    if (v1 == null)
      return null;

    if (v0 instanceof byte[]) {
      byte[] data = BinaryType.coerce(v0);
      byte[] prefix = BinaryType.coerce(v1);
      if (prefix.length > data.length) {
        return Boolean.TRUE;
      } else {
        int end = prefix.length;
        for (int i = 0; i < end; i++) {
          if (data[i] != prefix[i]) {
            return Boolean.FALSE;
          }
        }
      }
      return Boolean.TRUE;
    }

    return StringType.coerce(v0).startsWith(StringType.coerce(v1));
  }
}
