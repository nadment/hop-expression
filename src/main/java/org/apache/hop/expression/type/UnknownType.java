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

package org.apache.hop.expression.type;

import org.apache.hop.expression.ConversionException;

public final class UnknownType extends Type {

  UnknownType(boolean nullable) {
    super(PRECISION_NOT_SPECIFIED, PRECISION_NOT_SPECIFIED, nullable);
    this.signature = generateSignature();
  }

  @Override
  public UnknownType withNullability(boolean nullable) {
    if (nullable == this.isNullable()) {
      return this;
    }
    return new UnknownType(nullable);
  }

  @Override
  public TypeName getName() {
    return TypeName.UNKNOWN;
  }

  @Override
  public TypeComparability getComparability() {
    return TypeComparability.NONE;
  }

  @Override
  public <T> T convert(final Object value, final Class<T> clazz) throws ConversionException {
    if (value == null) return null;
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    return super.convert(value, clazz);
  }
}
