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
import org.apache.hop.expression.util.DateTimeFormat;
import java.time.ZonedDateTime;

public final class DateDataType extends DataType {

  /**
   * Default DATE type with default parameters.
   */
  public static final DateDataType DATE = new DateDataType();
  
  public DateDataType() {
    super(DataName.DATE, PRECISION_NOT_SPECIFIED, 9);
  } 
  
  @Override
  public ZonedDateTime cast(final Object value) {
    return cast(value, null);
  }
  
  /**
   * Convert a value to the specified type {@link DateDataType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *        numeric, or null if none
   * @return the converted value
   */
  @Override
  public ZonedDateTime cast(final Object value, String pattern) {

    if (value == null) {
      return null;
    }
    
    if (value instanceof ZonedDateTime) {
      return (ZonedDateTime) value;
    }
    if (value instanceof String) {
      return DateTimeFormat.of(pattern).parse((String) value);
    }

    throw new IllegalArgumentException(
        ExpressionError.UNSUPPORTED_CONVERSION.message(value, DataName.from(value), this));
  }
  
  /**
   * Coerce value to data type TIMESTAMP.
   * 
   * @param value the value to coerce
   * @return ZonedDateTime
   */
  public static final ZonedDateTime coerce(final Object value) {
    if (value == null) {
      return null;
    }
    
    if (value instanceof ZonedDateTime) {
      return (ZonedDateTime) value;
    }

    throw new IllegalArgumentException(ExpressionError.UNSUPPORTED_COERCION.message(value,
        DataName.from(value), DataName.DATE));
  }
}
