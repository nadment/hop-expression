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

import org.apache.hop.expression.type.TypeName;

public abstract class Conversion<T> {

  /**
   * Return the Java class representing the logical type.
   *
   * @return a Java class returned by from methods and accepted by to methods
   */
  public abstract Class<T> getConvertedType();

  /**
   * Return the type this class converts.
   *
   * @return a TypeName type name
   */
  public abstract TypeName getTypeName();
  //
  //  public T convert(Boolean value) {
  //    throw new UnsupportedOperationException("fromBoolean is not supported for " +
  // getTypeName());
  //  }
  //
  //  public T convert(Long value) {
  //    throw new UnsupportedOperationException("fromInteger is not supported for " +
  // getTypeName());
  //  }
  //
  //  public T convert(String value) {
  //    throw new UnsupportedOperationException("fromString is not supported for " + getTypeName());
  //  }

  //  public Boolean toBoolean(T value) {
  //    throw new UnsupportedOperationException("toBoolean is not supported for " + getTypeName());
  //  }
  //
  //  public Long toInteger(T value) {
  //    throw new UnsupportedOperationException("toInteger is not supported for " + getTypeName());
  //  }
  //
  //  public BigDecimal toNumber(T value) {
  //    throw new UnsupportedOperationException("toNumber is not supported for " + getTypeName());
  //  }
  //
  //  public String toString(T value) {
  //    throw new UnsupportedOperationException("toString is not supported for " + getTypeName());
  //  }
  //
  //  public InetAddress toInet(T value) {
  //    throw new UnsupportedOperationException("toInet is not supported for " + getTypeName());
  //  }
  //
  //  public byte[] toBinary(T value) {
  //    throw new UnsupportedOperationException("toBinary is not supported for " + getTypeName());
  //  }
}
