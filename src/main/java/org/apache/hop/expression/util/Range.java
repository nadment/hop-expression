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

/**
 * An immutable range of objects from a lower to upper point inclusive or exclusive.
 *
 * <p>The objects need to either be implementations of {@link Comparable}.
 */
public final class Range<T extends Comparable<? super T>> {

  public enum BoundType {
    /** The value is considered part of the range. */
    INCLUSIVE,
    /** The value is not considered part of the range. */
    EXCLUSIVE;
  }

  abstract static class Bound<T extends Comparable<?>> implements Comparable<Bound<T>> {
    final T endpoint;
    final BoundType type;

    Bound(T endpoint, BoundType type) {
      this.endpoint = endpoint;
      this.type = type;
    }

    @Override
    public int compareTo(Bound<T> other) {
      if (other == BelowAll.INSTANCE) {
        return 1;
      }
      if (other == AboveAll.INSTANCE) {
        return -1;
      }
      int result = Range.compareOrThrow(endpoint, other.endpoint);
      if (result != 0) {
        return result;
      }
      // same value. below comes before above
      if (this.type == BoundType.EXCLUSIVE && other.type == BoundType.INCLUSIVE) return 1;
      if (this.type == BoundType.INCLUSIVE && other.type == BoundType.EXCLUSIVE) return -1;
      return 0;
    }

    @SuppressWarnings("unchecked") // catching CCE
    @Override
    public boolean equals(Object obj) {
      if (obj instanceof Bound) {
        // It might not really be a Bound<T>, but we'll catch a CCE if it's not
        Bound<T> that = (Bound<T>) obj;
        try {
          int compareResult = compareTo(that);
          return compareResult == 0;
        } catch (ClassCastException wastNotComparableToOurType) {
          return false;
        }
      }
      return false;
    }

    abstract boolean isLess(T v);

    abstract boolean isGreater(T v);
  }

  private static final class BelowAll<T extends Comparable<?>> extends Bound<T> {
    static final Bound<?> INSTANCE = new BelowAll<>();

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
      return (other == this) ? 0 : -1;
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

  private static final class Above<T extends Comparable<?>> extends Bound<T> {
    Above(T endpoint, BoundType type) {
      super(endpoint, type);
    }

    @Override
    boolean isLess(T value) {
      int compare = Range.compareOrThrow(endpoint, value);
      return (type == BoundType.INCLUSIVE) ? compare >= 0 : compare > 0;
    }

    @Override
    boolean isGreater(T value) {
      int compare = Range.compareOrThrow(endpoint, value);
      return (type == BoundType.INCLUSIVE) ? compare <= 0 : compare < 0;
    }

    @Override
    public int hashCode() {
      return ~Objects.hash(endpoint, type);
    }

    @Override
    public String toString() {
      return (type == BoundType.INCLUSIVE ? "[" : "(") + String.valueOf(endpoint);
    }
  }

  private static final class AboveAll<T extends Comparable<?>> extends Bound<T> {
    static final Bound<?> INSTANCE = new AboveAll<>();

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
      return (other == this) ? 0 : 1;
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

  private static final class Below<T extends Comparable<?>> extends Bound<T> {
    Below(T endpoint, BoundType type) {
      super(endpoint, type);
    }

    @Override
    boolean isLess(T value) {
      int compare = Range.compareOrThrow(endpoint, value);
      return (type == BoundType.INCLUSIVE) ? compare >= 0 : compare > 0;
    }

    @Override
    boolean isGreater(T value) {
      int compare = Range.compareOrThrow(endpoint, value);
      return (type == BoundType.INCLUSIVE) ? compare <= 0 : compare < 0;
    }

    @Override
    public int hashCode() {
      return Objects.hash(endpoint, type);
    }

    @Override
    public String toString() {
      return String.valueOf(endpoint) + (type == BoundType.INCLUSIVE ? "]" : ")");
    }
  }

  private final Bound<T> lowerBound;
  private final Bound<T> upperBound;

  private Range(final Bound<T> lower, final Bound<T> upper) {
    lowerBound = Objects.requireNonNull(lower, "lower must not be null");
    upperBound = Objects.requireNonNull(upper, "upper must not be null");
    if (lower.compareTo(upper) > 0 && lower.endpoint != upper.endpoint) {
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
    Objects.requireNonNull(value, "value must not be null");
    return new Range<>(
        new Above<>(value, BoundType.INCLUSIVE), new Below<>(value, BoundType.INCLUSIVE));
  }

  public static <T extends Comparable<? super T>> Range<T> all() {
    return new Range<>((Bound<T>) BelowAll.INSTANCE, (Bound<T>) AboveAll.INSTANCE);
  }

  /**
   * Returns a range {@code (-∞..a)} that contains all values less than (but not equal to) a value.
   *
   * @param value the maximum value (exclusive)
   * @return the range
   */
  @SuppressWarnings("unchecked")
  public static <T extends Comparable<? super T>> Range<T> lessThan(T value) {
    Objects.requireNonNull(value, "value must not be null");
    return new Range<>((Bound<T>) BelowAll.INSTANCE, new Below<>(value, BoundType.EXCLUSIVE));
  }

  /**
   * Returns a range {@code (-∞..a]} that contains all values less than or equal to a value.
   *
   * @param value the maximum value (inclusive)
   * @return the range
   */
  @SuppressWarnings("unchecked")
  public static <T extends Comparable<? super T>> Range<T> lessThanOrEqual(T value) {
    Objects.requireNonNull(value, "value must not be null");
    return new Range<>((Bound<T>) BelowAll.INSTANCE, new Below<>(value, BoundType.INCLUSIVE));
  }

  /**
   * Returns a range {@code (a..+∞)} that contains all values greater than (but not equal to) a
   * value.
   *
   * @param value the minimum value (exclusive)
   * @return the range
   */
  @SuppressWarnings("unchecked")
  public static <T extends Comparable<? super T>> Range<T> greaterThan(T value) {
    Objects.requireNonNull(value, "value must not be null");
    return new Range<>(new Above<>(value, BoundType.EXCLUSIVE), (Bound<T>) AboveAll.INSTANCE);
  }

  /**
   * Returns a range {@code [a..+∞)} that contains all values greater than or equal to a value.
   *
   * @param value the minimum value (inclusive)
   * @return the range
   */
  @SuppressWarnings("unchecked")
  public static <T extends Comparable<? super T>> Range<T> greaterThanOrEqual(T value) {
    Objects.requireNonNull(value, "value must not be null");
    return new Range<>(new Above<>(value, BoundType.INCLUSIVE), (Bound<T>) AboveAll.INSTANCE);
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

  @SuppressWarnings("unchecked") // this method may throw CCE
  static int compareOrThrow(Comparable left, Comparable right) {
    return left.compareTo(right);
  }

  /**
   * Returns {@code true} if this range is of the form {@code (a..a)}, {@code [a..a)} or {@code
   * (a..a]}.
   */
  public boolean isEmpty() {
    return lowerBound.endpoint.equals(upperBound.endpoint)
        && (lowerBound.type == BoundType.EXCLUSIVE || upperBound.type == BoundType.EXCLUSIVE);
  }

  /** Returns {@code true} if this range is of the form {@code (-∞..+∞)}. */
  public boolean isAll() {
    return lowerBound == AboveAll.INSTANCE && upperBound == BelowAll.INSTANCE;
  }

  /** Checks if the {@code value} is within the bounds of this range. */
  public boolean contains(T value) {
    Objects.requireNonNull(value, "value must not be null");
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
}
