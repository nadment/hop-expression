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

package org.apache.hop.expression.util;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import org.apache.hop.expression.ConversionException;
import org.apache.hop.expression.type.TypeName;

public final class BinaryConversion extends Conversion<byte[]> {

  private BinaryConversion() {
    // Utility class
  }

  @Override
  public Class<byte[]> getConvertedType() {
    return byte[].class;
  }

  @Override
  public TypeName getTypeName() {
    return TypeName.BINARY;
  }

  public static byte[] convert(Long number) throws ConversionException {
    ByteBuffer bytes = ByteBuffer.allocate(Long.BYTES);
    bytes.putLong(number);
    return bytes.array();
  }

  public static byte[] convert(final String str) throws ConversionException {
    return str.getBytes(StandardCharsets.UTF_8);
  }
}
