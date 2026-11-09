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

import java.nio.ByteBuffer;
import org.apache.commons.codec.digest.MurmurHash3;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;

/**
 * Returns a signed 64-bit hash of one or more values, computed with the non-cryptographic
 * MurmurHash3 algorithm.
 *
 * <p>Each value is encoded with its length, so that a NULL differs from an empty string and the
 * values {@code ('ab','c')} differ from {@code ('a','bc')}.
 */
@FunctionPlugin
@NullMarked
public class HashFunction extends Function {

  /** The length written for a NULL value, an empty value has a length of 0. */
  private static final int NULL_LENGTH = -1;

  public HashFunction() {
    super(
        "HASH",
        ReturnTypes.INTEGER_NOT_NULL,
        OperandTypes.ANY_VARIADIC,
        OperatorCategory.CRYPTOGRAPHIC,
        "/docs/hop-user-manual/expression/hash.html");
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    // A value which is not a string or a binary, like a number or a date, is hashed as its text
    boolean coerced = false;
    for (int i = 0; i < call.getOperandCount(); i++) {
      Type type = call.getOperand(i).getType();
      if (!Types.isBinary(type) && !Types.isString(type)) {
        call.setOperand(i, Types.cast(call.getOperand(i), StringType.STRING));
        coerced = true;
      }
    }
    return coerced;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    byte[][] values = new byte[operands.length][];
    int size = 0;
    for (int i = 0; i < operands.length; i++) {
      // A string is hashed in UTF-8
      values[i] = operands[i].getValue(byte[].class);
      size += Integer.BYTES + (values[i] == null ? 0 : values[i].length);
    }

    ByteBuffer buffer = ByteBuffer.allocate(size);
    for (byte[] value : values) {
      if (value == null) {
        buffer.putInt(NULL_LENGTH);
      } else {
        buffer.putInt(value.length);
        buffer.put(value);
      }
    }

    // The first 64 bits of the 128-bit MurmurHash3 (x64 variant), the 64-bit variant of
    // commons-codec is deprecated because it is not the reference algorithm
    return MurmurHash3.hash128x64(buffer.array())[0];
  }
}
