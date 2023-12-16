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

import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.exception.ConversionException;

public final class UnknownType extends Type {

  public static final UnknownType UNKNOWN = new UnknownType(true);

  private UnknownType(boolean nullable) {
    super(PRECISION_NOT_SPECIFIED, PRECISION_NOT_SPECIFIED, nullable);
  }

  @Override
  public UnknownType withNullability(boolean nullable) {
    return new UnknownType(nullable);
  }

  @Override
  public TypeId getId() {
    return TypeId.UNKNOWN;
  }

  @Override
  public TypeComparability getComparability() {
    return TypeComparability.NONE;
  } 
  
  @Override
  public <T> T convert(final Object value, final Class<T> clazz) throws ConversionException {
    if (value == null)
      return null;
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    return super.convert(value, clazz);
  }

  @Override
  public Object cast(final Object value) throws ConversionException {
    throw new ConversionException(ErrorCode.INTERNAL_ERROR);
  }

  @Override
  public Object cast(final Object value, final String pattern) throws ConversionException {
    throw new ConversionException(ErrorCode.INTERNAL_ERROR);
  }
}
