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

import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.exception.ConversionException;
import java.math.BigDecimal;
import java.math.BigInteger;
import com.fasterxml.jackson.databind.JsonNode;

public final class UnknownType extends Type {

  public static final UnknownType UNKNOWN = new UnknownType(TypeName.UNKNOWN);
  public static final UnknownType ANY = new UnknownType(TypeName.ANY);
  public static final UnknownType SYMBOL = new UnknownType(TypeName.SYMBOL);
  
  private UnknownType(TypeName name) {
    super(name);
  }

  @Override
  public <T> T convert(Object value, Class<T> clazz) throws ConversionException {

    if (value == null) {
      return null;
    }
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    
    // JSon function return type ANY
    if (value instanceof String) {
      if (clazz == Boolean.class) {
        return clazz.cast(BooleanType.convertStringToBoolean((String) value));
      }
      if (clazz == Long.class) {
        return clazz.cast(IntegerType.convertStringToInteger((String) value));
      }
      if (clazz == BigDecimal.class) {
        return clazz.cast(NumberType.convertStringToNumber((String) value));
      }
      if (clazz == byte[].class) {
        return clazz.cast(BinaryType.convertStringToBinary((String) value));
      }
      if (clazz == JsonNode.class) {
        return clazz.cast(JsonType.convertStringToJson((String) value));
      }
    }
    if (value instanceof BigDecimal) {
      if (clazz == Boolean.class) {
        return clazz.cast(((BigDecimal) value).unscaledValue() != BigInteger.ZERO);
      }
      if (clazz == Long.class) {
        return clazz.cast(((BigDecimal) value).longValue());
      }
      if (clazz == String.class) {
        return clazz.cast(StringType.convertNumberToString((BigDecimal) value));
      }
    }
    if (value instanceof Boolean) {
      if (clazz == String.class) {
        return clazz.cast(String.valueOf(value));
      }
      if (clazz == Long.class) {
        return clazz.cast(((boolean) value) ? 1L : 0L);
      }
      if (clazz == BigDecimal.class) {
        return clazz.cast(((boolean) value) ? BigDecimal.ONE : BigDecimal.ZERO);
      }
    }
    return super.convert(value, clazz);
  }
  
  @Override
  public Object cast(final Object value) throws ConversionException {
    throw new ConversionException(ExpressionError.INTERNAL_ERROR);
  }

  @Override
  public Object cast(final Object value, final String pattern) throws ConversionException {
    throw new ConversionException(ExpressionError.INTERNAL_ERROR);
  }
}
