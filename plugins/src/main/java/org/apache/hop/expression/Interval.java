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
package org.apache.hop.expression;

import java.time.Duration;
import java.time.YearMonth;
import java.time.ZonedDateTime;

public abstract class Interval {

  /**
   * Negate the interval (change its sign)
   */
  public abstract Interval negated();

  /**
   * Get the absolute value of the interval (set its sign to positive)
   */
  public abstract Interval abs();

  /**
   * The sign of the interval
   *
   * @return <code>1</code> for positive or zero, <code>-1</code> for negative
   */
  public abstract int getSign();


  /**
   * Get a duration representation of this interval.
   * <p>
   * There is an obvious {@link Duration} representation for
   * {@link DayToSecond} intervals. If the interval contains {@link YearMonth}
   * information, then the corresponding duration will use:
   * <p>
   * <ul>
   * <li>1 year = 365.25 days</li>
   * <li>1 month = 30 days</li>
   * </ul>
   * <p>
   * This corresponds to PostgreSQL's
   * <code>EXTRACT(EPOCH FROM my_interval)</code> behaviour.
   */
  public abstract Duration toDuration();

  /**
   * Adds this interval to the specified temporal object.
   * @param temporal the temporal object to adjust, not null
   */
  public abstract ZonedDateTime addTo(ZonedDateTime temporal);

  /**
   * Subtracts this interval to the specified temporal object.
   * @param temporal the temporal object to adjust, not null
   */  
  public abstract ZonedDateTime subtractTo(ZonedDateTime temporal);
  
  protected static <T extends Comparable<T>> void validate(T value, T minValue, T maxValue,
      String fieldName) {
    if (value.compareTo(minValue) < 0 || value.compareTo(maxValue) > 0) {
      throw new IllegalArgumentException(
          String.format("Interval %s field value overflow, %s is out of range %s to %s.", fieldName,
              value, minValue, maxValue));
    }
  }
  

  protected static int parseIntZero(String str) {
    if (str == null || str.length() == 0)
      return 0;
    return Integer.parseInt(str);
  }
}
