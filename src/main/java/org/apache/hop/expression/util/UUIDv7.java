/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package org.apache.hop.expression.util;

import java.nio.ByteBuffer;
import java.security.SecureRandom;
import java.util.UUID;

/** Universal Unique Identifier version 7 defined by RFC 9562 */
public class UUIDv7 {
  private static final SecureRandom random = new SecureRandom();

  public static UUID randomUUID() {
    byte[] value = randomBytes();
    ByteBuffer buf = ByteBuffer.wrap(value);
    long high = buf.getLong();
    long low = buf.getLong();
    return new UUID(high, low);
  }

  private static byte[] randomBytes() {
    // Random bytes
    byte[] value = new byte[16];
    random.nextBytes(value);

    // Current timestamp in ms
    ByteBuffer timestamp = ByteBuffer.allocate(Long.BYTES);
    timestamp.putLong(System.currentTimeMillis());

    // Timestamp
    System.arraycopy(timestamp.array(), 2, value, 0, 6);

    // Version and variant
    value[6] = (byte) ((value[6] & 0x0F) | 0x70);
    value[8] = (byte) ((value[8] & 0x3F) | 0x80);

    return value;
  }
}
