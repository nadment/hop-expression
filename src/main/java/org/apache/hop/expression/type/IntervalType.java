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
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Interval;

public final class IntervalType extends Type {
  /** Default INTERVAL type with default parameters. */
  public static final IntervalType INTERVAL = new IntervalType(true);

  /** Default INTERVAL NOT NULL type with default parameters. */
  public static final IntervalType INTERVAL_NOT_NULL = new IntervalType(false);

  IntervalType(boolean nullable) {
    super(PRECISION_NOT_SPECIFIED, SCALE_NOT_SPECIFIED, nullable);
    this.signature = generateSignature();
    this.checkPrecisionAndScale();
  }

  /**
   * Convert String value to Interval.
   *
   * @param str the string to convert
   * @return Interval
   */
  public static Interval convert(final String str) throws ExpressionException {
    if (str == null) return null;
    return Interval.of(str);
  }

  @Override
  public IntervalType withNullability(boolean nullable) {
    if (nullable == this.isNullable()) {
      return this;
    }
    return new IntervalType(nullable);
  }

  @Override
  public TypeName getName() {
    return TypeName.INTERVAL;
  }

  @Override
  public TypeComparability getComparability() {
    return TypeComparability.ALL;
  }

  @Override
  public <T> T convert(Object value, Class<T> clazz) throws ExpressionException {
    if (value == null) {
      return null;
    }
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }

    return super.convert(value, clazz);
  }

  @Override
  public Object cast(final Object value) throws ExpressionException {
    return cast(value, null);
  }

  @Override
  public Object cast(final Object value, final String pattern) throws ExpressionException {

    if (value == null) {
      return null;
    }

    if (value instanceof String str) {
      return Interval.of(str);
    }

    throw new ExpressionException(
        ErrorCode.UNSUPPORTED_CONVERSION, value, TypeName.fromValue(value), this);
  }

  @Override
  public boolean compareEqual(Object left, Object right) {
    if (left instanceof Interval l && right instanceof Interval r) {
      return l.compareTo(r) == 0;
    }
    return super.compareEqual(left, right);
  }

  @Override
  public int compare(Object left, Object right) {
    if (left instanceof Interval l && right instanceof Interval r) {
      return l.compareTo(r);
    }
    return super.compare(left, right);
  }
}
