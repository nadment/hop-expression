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
import org.apache.hop.expression.util.BinaryConversion;
import org.apache.hop.expression.util.BooleanConversion;
import org.apache.hop.expression.util.IntegerConversion;
import org.apache.hop.expression.util.JsonConversion;
import org.apache.hop.expression.util.NumberConversion;
import org.apache.hop.expression.util.StringConversion;

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
  public TypeName getName() {
    return TypeName.ANY;
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
        return clazz.cast(BooleanConversion.convert(str));
      }
      if (clazz == Long.class) {
        return clazz.cast(IntegerConversion.convert(str));
      }
      if (clazz == BigDecimal.class) {
        return clazz.cast(NumberConversion.convert(str));
      }
      if (clazz == byte[].class) {
        return clazz.cast(BinaryConversion.convert(str));
      }
      if (clazz == JsonNode.class) {
        return clazz.cast(JsonConversion.convert(str));
      }
    }
    if (value instanceof BigDecimal number) {
      if (clazz == Boolean.class) {
        return clazz.cast(number.unscaledValue() != BigInteger.ZERO);
      }
      if (clazz == Long.class) {
        return clazz.cast(number.longValue());
      }
      if (clazz == String.class) {
        return clazz.cast(StringConversion.convert(number));
      }
    }
    if (value instanceof Boolean bool) {
      if (clazz == String.class) {
        return clazz.cast(StringConversion.convert(bool));
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
