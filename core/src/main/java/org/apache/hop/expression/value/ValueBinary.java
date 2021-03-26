/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression.value;

import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Objects;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Value;
import org.apache.hop.expression.DataType;
import org.apache.hop.expression.ExpressionException;

public class ValueBinary extends Value {
  /** The binary data. */
  private final byte[] value;

  public ValueBinary(byte[] value) {
    this.value = Objects.requireNonNull(value);
  }

  @Override
  public DataType getType() {
    return DataType.BINARY;
  }

  @Override
  public Object getObject() {
    return value;
  }

  @Override
  public int hashCode() {    
    return Arrays.hashCode(value);
  }

  @Override
  public boolean equals(Object other) {
    return other instanceof ValueBinary && Arrays.equals(value, ((ValueBinary) other).value);
  }

  @Override
  public int compare(Value v) {
    byte[] o = v.toBinary();

    int length = value.length < o.length ? value.length : o.length;

    int compare = value.length - o.length;
    if (compare == 0) {
      for (int i = 0; i < length; i++) {
        compare = value[i] - o[i];
        if (compare != 0) {
          compare = compare < 0 ? -1 : 1;
          break;
        }
      }
    }

    return compare;
  }

  @Override
  public byte[] toBinary() throws ExpressionException {
    return value;
  }

  @Override
  public long toInteger() {

    if (value.length > 8) throw new ExpressionException("Binary too big to fit in integer");

    long result = 0;

    for (int i = 0; i < value.length; i++) {
      result = result << 8;
      result = result | (value[i] & 0xFF);
    }

    return result;
  }

  @Override
  public double toNumber() {
    if (value.length > 8) throw new ExpressionException("Binary too big to fit in double");

    long result = 0;

    for (int i = 0; i < value.length; i++) {
      result = result << 8;
      result = result | (value[i] & 0xFF);
    }

    return result;
  }

  @Override
  public Value eval(IExpressionContext context) throws ExpressionException {
    return this;
  }

  public void write(StringWriter writer, int leftPrec, int rightPrec) {
    writer.append('\'');
    writer.append(toString());
    writer.append('\'');
  }

  @Override
  public String toString() {
    return new String(value, StandardCharsets.UTF_8);
  }
}
