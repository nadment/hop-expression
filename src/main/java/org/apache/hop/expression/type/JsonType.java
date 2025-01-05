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
import org.apache.hop.expression.ConversionException;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.util.JsonConverter;
import org.apache.hop.expression.util.StringConverter;

public final class JsonType extends Type {

  JsonType(boolean nullable) {
    super(PRECISION_NOT_SPECIFIED, SCALE_NOT_SPECIFIED, nullable);
    this.signature = generateSignature();
  }

  @Override
  public JsonType withNullability(boolean nullable) {
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

  /**
   * Coerce value to data type JSON
   *
   * @param value the value to coerce
   * @return String
   */
  public static final JsonNode coerce(final Object value) throws ConversionException {
    if (value == null) {
      return null;
    }
    if (value instanceof JsonNode json) {
      return json;
    }
    //    if (value instanceof String) {
    //      return JsonType.convertStringToJson((String) value);
    //    }

    throw new ConversionException(
        ErrorCode.UNSUPPORTED_COERCION, value, TypeName.fromValue(value), TypeName.JSON);
  }

  @Override
  public <T> T convert(final Object value, final Class<T> clazz) throws ConversionException {

    if (value == null) {
      return null;
    }
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    if (clazz == String.class) {
      return clazz.cast(StringConverter.convert((JsonNode) value));
    }

    return super.convert(value, clazz);
  }

  @Override
  public JsonNode cast(final Object value) throws ConversionException {
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
  public JsonNode cast(final Object value, String pattern) throws ConversionException {

    if (value == null) {
      return null;
    }

    if (value instanceof JsonNode json) {
      return json;
    }

    if (value instanceof String str) {
      return JsonConverter.convert(str);
    }

    throw new ConversionException(
        ErrorCode.UNSUPPORTED_CONVERSION, value, TypeName.fromValue(value), this);
  }
}
