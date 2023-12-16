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

package org.apache.hop.expression.type;

import org.apache.hop.expression.util.JsonComparator;
import java.math.BigDecimal;
import java.time.ZonedDateTime;
import com.fasterxml.jackson.databind.JsonNode;

public class Comparison {

  private static final JsonComparator JSON_COMPARATOR = new JsonComparator();

  /**
   * Private constructor since this is a utility class.
   */
  private Comparison() {}


  public static final boolean equals(final Object left, final Object right) {

    if (left == null || right == null)
      return false;

    // The lower order data type is converted
    if (left instanceof byte[] || right instanceof byte[]) {
      return equalsTo(BinaryType.coerce(left), BinaryType.coerce(right));
    }
    if (left instanceof JsonNode || right instanceof JsonNode) {
      return equalsTo(JsonType.coerce(left), JsonType.coerce(right));
    }
    if (left instanceof ZonedDateTime || right instanceof ZonedDateTime) {
      return compareTo(DateType.coerce(left), DateType.coerce(right))==0;
    }
    if (left instanceof BigDecimal || right instanceof BigDecimal) {
      return NumberType.coerce(left).compareTo(NumberType.coerce(right))==0;
    }
    if (left instanceof Long || right instanceof Long) {
      return IntegerType.coerce(left).compareTo(IntegerType.coerce(right))==0;
    }
    if (left instanceof Boolean || right instanceof Boolean) {
      return BooleanType.coerce(left).equals(BooleanType.coerce(right));
    }

    return StringType.coerce(left).compareTo(StringType.coerce(right))==0;
  }

  /**
   * Compare this value against another value. If values need to be converted to match the other
   * operands data type, the value with the lower order is converted to the value with the higher
   * order.
   *
   * @param value the other value
   * @return 0 if both values are equal, -1 if this value is smaller, and 1 otherwise
   */
  public static final int compare(final Object left, final Object right) {

    if (left == null && right == null)
      return 0;
    if (left == null)
      return -1;
    if (right == null)
      return 1;

    // The lower order data type is converted
    if (left instanceof byte[] || right instanceof byte[]) {
      return compareTo(BinaryType.coerce(left), BinaryType.coerce(right));
    }
    if (left instanceof ZonedDateTime || right instanceof ZonedDateTime) {
      return compareTo(DateType.coerce(left), DateType.coerce(right));
    }
    if (left instanceof BigDecimal || right instanceof BigDecimal) {
      return NumberType.coerce(left).compareTo(NumberType.coerce(right));
    }
    if (left instanceof Long || right instanceof Long) {
      return IntegerType.coerce(left).compareTo(IntegerType.coerce(right));
    }
    if (left instanceof Boolean || right instanceof Boolean) {
      return BooleanType.coerce(left).compareTo(BooleanType.coerce(right));
    }

    return StringType.coerce(left).compareTo(StringType.coerce(right));
  }

  protected static boolean equalsTo(final JsonNode left, final JsonNode right) {
    // Ignores the order of attributes
    return left.equals(JSON_COMPARATOR, right);
  }

  protected static int compareTo(final ZonedDateTime left, final ZonedDateTime right) {
    // Two timestamp are equal if they represent the same moment in time:
    // Timestamp '2019-01-01 8:00:00 -8:00' = Timestamp '2019-01-01 11:00:00 -5:00'
    if (left.isEqual(right)) {
      return 0;
    }
    return left.compareTo(right);
  }

  protected static int compareTo(final String left, final String right) {
    return left.compareTo(right);
  }

  protected static boolean equalsTo(final byte[] left, final byte[] right) {

    if (left.length != right.length)
      return false;

    for (int i = 0; i < left.length; i++) {
      int compare = left[i] - right[i];
      if (compare != 0) {
        return false;
      }
    }
    return true;
  }

  protected static int compareTo(final byte[] left, final byte[] right) {
    int length = left.length < right.length ? left.length : right.length;

    int compare = left.length - right.length;
    if (compare == 0) {
      for (int i = 0; i < length; i++) {
        compare = left[i] - right[i];
        if (compare != 0) {
          compare = compare < 0 ? -1 : 1;
          break;
        }
      }
    }

    return compare;
  }
}
