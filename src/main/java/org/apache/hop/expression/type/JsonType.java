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
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.util.JsonComparator;
import org.apache.hop.expression.util.JsonConversion;
import org.apache.hop.expression.util.StringConversion;

public final class JsonType extends Type {
  private static final JsonComparator JSON_COMPARATOR = new JsonComparator();

  JsonType(boolean nullable) {
    super(PRECISION_NOT_SPECIFIED, SCALE_NOT_SPECIFIED, nullable);
    this.signature = generateSignature();
  }

  @Override
  public JsonType withNullability(boolean nullable) {
    if (nullable == this.isNullable()) {
      return this;
    }
    return new JsonType(nullable);
  }

  @Override
  public TypeName getName() {
    return TypeName.JSON;
  }

  @Override
  public TypeComparability getComparability() {
    return TypeComparability.UNORDERED;
  }

  @Override
  public <T> T convert(final Object value, final Class<T> clazz) throws ExpressionException {

    if (value == null) {
      return null;
    }
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    if (clazz == String.class) {
      return clazz.cast(StringConversion.convert((JsonNode) value));
    }

    return super.convert(value, clazz);
  }

  @Override
  public JsonNode cast(final Object value) throws ExpressionException {
    return cast(value, null);
  }

  /**
   * Convert a value to the specified type {@link JsonType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *     numeric, or null if none
   * @return the converted value
   */
  @Override
  public JsonNode cast(final Object value, String pattern) throws ExpressionException {

    if (value == null) {
      return null;
    }

    if (value instanceof JsonNode json) {
      return json;
    }

    if (value instanceof String str) {
      return JsonConversion.convert(str);
    }

    throw new ExpressionException(
        ErrorCode.UNSUPPORTED_CONVERSION, value, TypeName.fromValue(value), this);
  }

  @Override
  public boolean compareEqual(Object left, Object right) {
    if (left instanceof JsonNode l && right instanceof JsonNode r) {
      // Ignores the order of attributes
      return l.equals(JSON_COMPARATOR, r);
    }
    return super.compareEqual(left, right);
  }
}
