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
package org.apache.hop.expression.util;

public class Bytes {

  /**
   * Checks if this has the passed prefix
   *
   * @param prefix is a Bytes object to compare to this
   * @return true or false
   */
  public static boolean startsWith(byte[] data, byte[] prefix) {

    if (prefix.length > data.length) {
      return false;
    } else {
      int end = prefix.length;
      for (int i = 0; i < end; i++) {
        if (data[i] != prefix[i]) {
          return false;
        }
      }
    }
    return true;
  }

  /**
   * Checks if this has the passed suffix
   *
   * @param suffix is a Bytes object to compare to this
   * @return true or false
   * @since 1.1.0
   */
  public static boolean endsWith(byte[] data, byte[] suffix) {
    int startOffset = data.length - suffix.length;

    if (startOffset < 0) {
      return false;
    } else {
      int end = startOffset + suffix.length;
      for (int i = startOffset; i < end; i++) {
        if (data[i] != suffix[i]) {
          return false;
        }
      }
    }
    return true;
  }

  public static byte[] reverse(byte[] data) {
    byte[] value = new byte[data.length];
    for (int i = data.length - 1, j = 0; i >= 0; i--, j++) {
      value[j] = data[i];
    }
    return value;
  }
}
