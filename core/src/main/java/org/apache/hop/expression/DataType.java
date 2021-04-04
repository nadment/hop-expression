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
package org.apache.hop.expression;

/**
 * Enumeration of the data type which can be used to construct a expression.
 *
 * <p>The order of enum declaration is important to be usable with <code>compareTo</code> method.
 *
 * <p>If values need to be converted to match the other operands data type, the value with the lower
 * order is converted to the value with the higher order.
 */
public enum DataType {
  /** A unknown data type */
  NONE,

  /** Unlimited length text */
  STRING,

  /** Boolean value (true or false) */
  BOOLEAN,

  /** Signed long (64-bit) integer */
  INTEGER,

  /** Double precision floating point number */
  NUMBER,

  /** Unlimited precision number */
  BIGNUMBER,

  /** Date-time value with nanosecond precision */
  DATE,

  /** A binary type can be images, sounds, videos, and other types of binary data */
  BINARY;

  public static DataType of(final String s) {
    for (DataType type : DataType.values()) {
      if (type.name().equalsIgnoreCase(s)) {
        return type;
      }
    }
    throw new IllegalArgumentException("Invalid data type: "+s);
  }
  

  /**
   * Check if data type exist.
   * 
   * @param str the name to check
   * @return 
   */  
  public static boolean exist(final String s) {
    for (DataType type : DataType.values()) {
      if (type.name().equalsIgnoreCase(s)) {
        return true;
      }
    }
    return false;
  }
}
