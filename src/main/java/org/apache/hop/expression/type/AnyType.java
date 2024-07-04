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

import com.fasterxml.jackson.databind.JsonNode;
import java.math.BigDecimal;
import java.math.BigInteger;
import org.apache.hop.expression.ConversionException;

public final class AnyType extends Type {

  AnyType(boolean nullable) {
    super(PRECISION_NOT_SPECIFIED, PRECISION_NOT_SPECIFIED, nullable);
    this.signature = generateSignature();
  }

  @Override
  public AnyType withNullability(boolean nullable) {
    return new AnyType(nullable);
  }

  @Override
  public TypeId getId() {
    return TypeId.ANY;
  }

  @Override
  public TypeComparability getComparability() {
    return TypeComparability.NONE;
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
    if (value instanceof String str) {
      if (clazz == Boolean.class) {
        return clazz.cast(BooleanType.convertToBoolean(str));
      }
      if (clazz == Long.class) {
        return clazz.cast(IntegerType.convertToInteger(str));
      }
      if (clazz == BigDecimal.class) {
        return clazz.cast(NumberType.convertToNumber(str));
      }
      if (clazz == byte[].class) {
        return clazz.cast(BinaryType.convertToBinary(str));
      }
      if (clazz == JsonNode.class) {
        return clazz.cast(JsonType.convertToJson(str));
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
        return clazz.cast(StringType.convertToString((BigDecimal) value));
      }
    }
    if (value instanceof Boolean bool) {
      if (clazz == String.class) {
        return clazz.cast(String.valueOf(bool));
      }
      if (clazz == Long.class) {
        return clazz.cast(bool ? 1L : 0L);
      }
      if (clazz == BigDecimal.class) {
        return clazz.cast(bool ? BigDecimal.ONE : BigDecimal.ZERO);
      }
    }
    return super.convert(value, clazz);
  }
}
