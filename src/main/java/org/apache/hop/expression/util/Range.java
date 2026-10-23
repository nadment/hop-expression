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
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * An immutable range of objects from a lower to upper point inclusive or exclusive.
 *
 * <p>The objects need to either be implementations of {@link Comparable}.
 */
@NullMarked
public final class Range<T extends Comparable<? super T>> {

  private static final String VALUE_IS_NULL = "value must not be null";

  private final Bound<T> lowerBound;
  private final Bound<T> upperBound;

  private Range(final Bound<T> lower, final Bound<T> upper) {
    lowerBound = Objects.requireNonNull(lower, "lower must not be null");
    upperBound = Objects.requireNonNull(upper, "upper must not be null");
    if (lower.compareTo(upper) > 0 && lower.endpoint.compareTo(upper.endpoint) != 0) {
      throw new IllegalArgumentException(
          String.format("lower must be less than or equal to upper %s", this));
    }
  }

  /**
   * Returns a range {@code [a..a]} that contains a single value.
   *
   * @param value the value
   * @return the range
   */
  public static <T extends Comparable<? super T>> Range<T> singleton(T value) {
    Objects.requireNonNull(value, VALUE_IS_NULL);
    return new Range<>(
        new Above<>(value, BoundType.INCLUSIVE), new Below<>(value, BoundType.INCLUSIVE));
  }

  public static <T extends Comparable<? super T>> Range<T> all() {
    return new Range<>(new BelowAll<T>(), new AboveAll<T>());
  }

  /**
   * Returns a range {@code (-∞..a)} that contains all values less than (but not equal to) a value.
   *
   * @param value the maximum value (exclusive)
   * @return the range
   */
  public static <T extends Comparable<? super T>> Range<T> lessThan(T value) {
    Objects.requireNonNull(value, VALUE_IS_NULL);
    return new Range<>(new BelowAll<>(), new Below<>(value, BoundType.EXCLUSIVE));
  }

  /**
   * Returns a range {@code (-∞..a]} that contains all values less than or equal to a value.
   *
   * @param value the maximum value (inclusive)
   * @return the range
   */
  public static <T extends Comparable<? super T>> Range<T> lessThanOrEqual(T value) {
    Objects.requireNonNull(value, VALUE_IS_NULL);
    return new Range<>(new BelowAll<>(), new Below<>(value, BoundType.INCLUSIVE));
  }

  /**
   * Returns a range {@code (a..+∞)} that contains all values greater than (but not equal to) a
   * value.
   *
   * @param value the minimum value (exclusive)
   * @return the range
   */
  public static <T extends Comparable<? super T>> Range<T> greaterThan(T value) {
    Objects.requireNonNull(value, VALUE_IS_NULL);
    return new Range<>(new Above<>(value, BoundType.EXCLUSIVE), new AboveAll<>());
  }

  /**
   * Returns a range {@code [a..+∞)} that contains all values greater than or equal to a value.
   *
   * @param value the minimum value (inclusive)
   * @return the range
   */
  public static <T extends Comparable<? super T>> Range<T> greaterThanOrEqual(T value) {
    Objects.requireNonNull(value, VALUE_IS_NULL);
    return new Range<>(new Above<>(value, BoundType.INCLUSIVE), new AboveAll<>());
  }

  public static <T extends Comparable<? super T>> Range<T> of(
      T lower, BoundType lowerType, T upper, BoundType upperType) {
    return new Range<>(new Above<>(lower, lowerType), new Below<>(upper, upperType));
  }

  public static <T extends Comparable<? super T>> Range<T> between(T lower, T upper) {
    return new Range<>(
        new Above<>(lower, BoundType.INCLUSIVE), new Below<>(upper, BoundType.INCLUSIVE));
  }

  /** Returns the lower value of this range. */
  Bound<T> getLowerBound() {
    return lowerBound;
  }

  /** Get the upper value of this range. */
  Bound<T> getUpperBound() {
    return upperBound;
  }

  /**
   * Returns {@code true} if this range is of the form {@code (a..a)}, {@code [a..a)} or {@code
   * (a..a]}.
   */
  public boolean isEmpty() {
    // An unbounded range is never empty
    return lowerBound.endpoint != null
        && upperBound.endpoint != null
        && lowerBound.endpoint.compareTo(upperBound.endpoint) == 0
        && (lowerBound.type == BoundType.EXCLUSIVE || upperBound.type == BoundType.EXCLUSIVE);
  }

  /** Returns {@code true} if this range is of the form {@code (-∞..+∞)}. */
  public boolean isAll() {
    return lowerBound instanceof BelowAll && upperBound instanceof AboveAll;
  }

  /** Checks if the {@code value} is within the bounds of this range. */
  public boolean contains(T value) {
    Objects.requireNonNull(value, VALUE_IS_NULL);
    return lowerBound.isGreater(value) && upperBound.isLess(value);
  }

  /**
   * Returns the maximal range by both this range and {@code connectedRange}, if such a range
   * exists.
   *
   * <p>For example, the intersection of {@code [1..5]} and {@code (3..7)} is {@code (3..5]}. The
   * resulting range may be empty; for example, {@code [1..5)} intersected with {@code [5..7)}
   * yields the empty range {@code [5..5)}.
   */
  public Range<T> intersect(Range<T> other) {
    Objects.requireNonNull(other, "range must not be null");
    int lowerCmp = lowerBound.compareTo(other.lowerBound);
    int upperCmp = upperBound.compareTo(other.upperBound);

    // The other range is fully contained in this range
    if (lowerCmp <= 0 && upperCmp >= 0) {
      return other;
    }
    // This range is fully contained in the other range
    if (lowerCmp > 0 && upperCmp < 0) {
      return this;
    }

    Bound<T> newLower = (lowerCmp >= 0) ? lowerBound : other.lowerBound;
    Bound<T> newUpper = (upperCmp <= 0) ? upperBound : other.upperBound;

    // if (newLower.endpoint.compareTo(newUpper.endpoint) >= 0 && newLower.type != newUpper.type) {
    int cmp = newLower.endpoint.compareTo(newUpper.endpoint);
    if (cmp == 0) {
      BoundType type =
          (newLower.type == BoundType.EXCLUSIVE || newUpper.type == BoundType.EXCLUSIVE)
              ? BoundType.EXCLUSIVE
              : BoundType.INCLUSIVE;
      return of(newUpper.endpoint, type, newUpper.endpoint, type);
    }
    // Return empty range for disconnected ranges
    else if (cmp > 0) {
      return of(newUpper.endpoint, BoundType.EXCLUSIVE, newUpper.endpoint, BoundType.EXCLUSIVE);
    }

    return new Range<>(newLower, newUpper);
  }

  /**
   * Return the range as a string representation.
   *
   * <p>Notation: a square bracket is inclusive and parenthesis is exclusive.
   *
   * <ul>
   *   <li>{@code (A..B)} greater than A and less than B
   *   <li>{@code [A..B]} greater than or equal A and less than or equal B
   *   <li>{@code [A..B)} greater than or equal A and less than B
   *   <li>{@code (-∞..B]} less than B
   *   <li>{@code (-∞..B)} less then or equal B
   *   <li>{@code [A..+∞)} greater than or equal A
   *   <li>{@code (A..+∞)} greater than A
   *   <li>{@code (-∞..+∞)} all values
   * </ul>
   *
   * @return string representation of the range
   */
  @Override
  public String toString() {
    return String.format("%s..%s", lowerBound, upperBound);
  }

  @Override
  public boolean equals(Object object) {
    if (object instanceof Range<?> other) {
      return this.lowerBound.equals(other.lowerBound) && this.upperBound.equals(other.upperBound);
    }
    return false;
  }

  @Override
  public int hashCode() {
    return lowerBound.hashCode() * 31 + upperBound.hashCode();
  }

  public enum BoundType {
    /** The value is considered part of the range. */
    INCLUSIVE,
    /** The value is not considered part of the range. */
    EXCLUSIVE;
  }

  abstract static class Bound<T extends Comparable<? super T>> implements Comparable<Bound<T>> {
    final T endpoint;
    final BoundType type;

    Bound(T endpoint, BoundType type) {
      this.endpoint = endpoint;
      this.type = type;
    }

    @Override
    public int compareTo(Bound<T> other) {
      if (other instanceof BelowAll) {
        return 1;
      }
      if (other instanceof AboveAll) {
        return -1;
      }
      int result = this.endpoint.compareTo(other.endpoint);
      if (result != 0) {
        return result;
      }
      if (this.type == other.type) {
        return 0;
      }
      // same value. An exclusive lower bound comes after an inclusive one, an exclusive upper bound
      // comes before an inclusive one
      int order = (this.type == BoundType.EXCLUSIVE) ? 1 : -1;
      return (this instanceof Below) ? -order : order;
    }

    boolean isLess(T value) {
      int compare = endpoint.compareTo(value);
      return (type == BoundType.INCLUSIVE) ? compare >= 0 : compare > 0;
    }

    boolean isGreater(T value) {
      int compare = endpoint.compareTo(value);
      return (type == BoundType.INCLUSIVE) ? compare <= 0 : compare < 0;
    }

    @Override
    public boolean equals(@Nullable Object obj) {
      // A lower bound is never equal to an upper bound
      if (obj == null || getClass() != obj.getClass()) {
        return false;
      }
      // The endpoints can be of another type, they are compared with equals like in hashCode
      Bound<?> that = (Bound<?>) obj;
      return type == that.type && Objects.equals(endpoint, that.endpoint);
    }

    @Override
    public int hashCode() {
      // A lower bound and an upper bound on the same endpoint have different hash codes
      int hash = Objects.hash(endpoint, type);
      return (this instanceof Above) ? ~hash : hash;
    }
  }

  private static final class BelowAll<T extends Comparable<? super T>> extends Bound<T> {

    BelowAll() {
      super(null, BoundType.EXCLUSIVE);
    }

    @Override
    boolean isLess(T v) {
      return false;
    }

    @Override
    boolean isGreater(T value) {
      return true;
    }

    @Override
    public int compareTo(Bound<T> other) {
      return (other instanceof BelowAll) ? 0 : -1;
    }

    @Override
    public boolean equals(@Nullable Object obj) {
      return obj instanceof BelowAll;
    }

    @Override
    public int hashCode() {
      return toString().hashCode();
    }

    @Override
    public String toString() {
      return "(-∞";
    }
  }

  private static final class Above<T extends Comparable<? super T>> extends Bound<T> {
    Above(T endpoint, BoundType type) {
      super(endpoint, type);
    }

    @Override
    public String toString() {
      return (type == BoundType.INCLUSIVE ? "[" : "(") + String.valueOf(endpoint);
    }
  }

  private static final class AboveAll<T extends Comparable<? super T>> extends Bound<T> {

    AboveAll() {
      super(null, BoundType.EXCLUSIVE);
    }

    @Override
    boolean isLess(T value) {
      return true;
    }

    @Override
    boolean isGreater(T value) {
      return false;
    }

    @Override
    public int compareTo(Bound<T> other) {
      return (other instanceof AboveAll) ? 0 : 1;
    }

    @Override
    public boolean equals(@Nullable Object obj) {
      return obj instanceof AboveAll;
    }

    @Override
    public int hashCode() {
      return toString().hashCode();
    }

    @Override
    public String toString() {
      return "+∞)";
    }
  }

  private static final class Below<T extends Comparable<? super T>> extends Bound<T> {
    Below(T endpoint, BoundType type) {
      super(endpoint, type);
    }

    @Override
    public String toString() {
      return String.valueOf(endpoint) + (type == BoundType.INCLUSIVE ? "]" : ")");
    }
  }
}
