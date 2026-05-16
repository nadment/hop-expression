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

import java.math.BigDecimal;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.util.BooleanConversion;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@NullMarked
public final class BooleanType extends Type {

  /** Default BOOLEAN type. */
  public static final BooleanType BOOLEAN = new BooleanType(true);

  /** Default BOOLEAN NOT NULL type. */
  public static final BooleanType BOOLEAN_NOT_NULL = new BooleanType(false);

  BooleanType(boolean nullable) {
    super(1, 0, nullable);
    this.signature = generateSignature();
  }

  @Override
  public BooleanType withNullability(boolean nullable) {
    return (nullable) ? BOOLEAN : BOOLEAN_NOT_NULL;
  }

  @Override
  public TypeName getName() {
    return TypeName.BOOLEAN;
  }

  @Override
  public TypeComparability getComparability() {
    return TypeComparability.ALL;
  }

  @Override
  public @Nullable <T> T convert(final @Nullable Object value, final Class<T> clazz)
      throws ExpressionException {

    if (value == null) {
      return null;
    }
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    if (clazz == String.class) {
      return clazz.cast((boolean) value ? "TRUE" : "FALSE");
    }
    if (clazz == Long.class) {
      return clazz.cast(((boolean) value) ? 1L : 0L);
    }
    if (clazz == BigDecimal.class) {
      return clazz.cast(((boolean) value) ? BigDecimal.ONE : BigDecimal.ZERO);
    }

    return super.convert(value, clazz);
  }

  @Override
  public @Nullable Boolean cast(final @Nullable Object value) throws ExpressionException {
    return cast(value, null);
  }

  /**
   * Convert a value to the specified type {@link BooleanType} with a pattern.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when the value is date or
   *     numeric, or null if none
   * @return the converted value
   */
  @Override
  public @Nullable Boolean cast(final @Nullable Object value, final @Nullable String pattern)
      throws ExpressionException {
    return switch (value) {
      case null -> null;
      case Boolean bool -> bool;
      case Long number -> number != 0;
      case BigDecimal number -> number.signum() != 0;
      case String str -> BooleanConversion.convert(str);
      default ->
          throw new ExpressionException(
              ErrorCode.UNSUPPORTED_CONVERSION, value, TypeName.fromValue(value), this);
    };
  }

  @Override
  public boolean compareEqual(@Nullable Object left, @Nullable Object right) {
    if (left instanceof Boolean l && right instanceof Boolean r) {
      return l.equals(r);
    }
    return super.compareEqual(left, right);
  }

  @Override
  public int compare(@Nullable Object left, @Nullable Object right) {
    if (left instanceof Boolean l && right instanceof Boolean r) {
      return l.compareTo(r);
    }
    return super.compare(left, right);
  }
}
