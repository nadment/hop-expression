/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.expression.util;

import java.util.Objects;

public final class Range<T extends Comparable<? super T>> {
  private final T lowerBound;
  private final T upperBound;

  public static <T extends Comparable<? super T>> Range<T> singleton(T value) {
    return new Range<>(value, value);
  }

  public static <T extends Comparable<? super T>> Range<T> lessThan(T value) {
    return new Range<>(null, value);
  }

  public static <T extends Comparable<? super T>> Range<T> greaterThan(T value) {
    return new Range<>(value, null);
  }

  public Range(final T lower, final T upper) {
    lowerBound = Objects.requireNonNull(lower, "lower must not be null");
    upperBound = Objects.requireNonNull(upper, "upper must not be null");
    if (lower.compareTo(upper) > 0) {
      throw new IllegalArgumentException("lower must be less than or equal to upper");
    }
  }

  /**
   * Get the lower value.
   */
  public T getLower() {
    return lowerBound;
  }

  /**
   * Get the upper value.
   *
   */
  public T getUpper() {
    return upperBound;
  }

  /**
   * Checks if the {@code value} is within the bounds of this range.
   */
  public boolean contains(T value) {
    Objects.requireNonNull(value, "value must not be null");
    boolean gteLower = value.compareTo(lowerBound) >= 0;
    boolean lteUpper = value.compareTo(upperBound) <= 0;
    return gteLower && lteUpper;
  }

  public Range<T> intersect(T lower, T upper) {
    Objects.requireNonNull(lower, "lower must not be null");
    Objects.requireNonNull(upper, "upper must not be null");
    int cmpLower = lower.compareTo(lowerBound);
    int cmpUpper = upper.compareTo(upperBound);
    if (cmpLower <= 0 && cmpUpper >= 0) {
      // [lower, upper] includes this
      return this;
    } else {
      return new Range<T>(cmpLower <= 0 ? lowerBound : lower, cmpUpper >= 0 ? upperBound : upper);
    }
  }


  /**
   * Return the range as a string representation {@code "[lower, upper]"}.
   *
   * @return string representation of the range
   */
  @Override
  public String toString() {
    return String.format("[%s, %s]", lowerBound, upperBound);
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    } else if (this == obj) {
      return true;
    } else if (obj instanceof Range) {
      Range<?> other = (Range<?>) obj;
      return lowerBound.equals(other.lowerBound) && upperBound.equals(other.upperBound);
    }
    return false;
  }

  @Override
  public int hashCode() {
    return Objects.hash(lowerBound, upperBound);
  }

}
