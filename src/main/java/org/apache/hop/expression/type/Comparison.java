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

import com.fasterxml.jackson.databind.JsonNode;
import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.time.ZonedDateTime;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.util.JsonComparator;

public class Comparison {

  private static final JsonComparator JSON_COMPARATOR = new JsonComparator();

  /** Private constructor since this is a utility class. */
  private Comparison() {}

  public static final boolean equals(final Object left, final Object right) {

    if (left == null || right == null) return false;

    if (left instanceof Long l && right instanceof Long r) {
      return l.equals(r);
    }
    if (left instanceof BigDecimal l && right instanceof BigDecimal r) {
      return l.compareTo(r) == 0;
    }
    if (left instanceof Boolean l && right instanceof Boolean r) {
      return l.equals(r);
    }
    if (left instanceof String l && right instanceof String r) {
      return l.compareTo(r) == 0;
    }
    if (left instanceof ZonedDateTime l && right instanceof ZonedDateTime r) {
      return l.isEqual(r);
    }
    if (left instanceof Interval l && right instanceof Interval r) {
      return l.compareTo(r) == 0;
    }
    if (left instanceof byte[] l && right instanceof byte[] r) {
      return equalsTo(l, r);
    }
    if (left instanceof JsonNode l && right instanceof JsonNode r) {
      // Ignores the order of attributes
      return l.equals(JSON_COMPARATOR, r);
    }

    throw new ExpressionException(ErrorCode.INTERNAL_ERROR, "Equals error");
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

    if (left == null && right == null) return 0;
    if (left == null) return -1;
    if (right == null) return 1;

    if (left instanceof Long l && right instanceof Long r) {
      return l.compareTo(r);
    }
    if (left instanceof BigDecimal l && right instanceof BigDecimal r) {
      return l.compareTo(r);
    }
    if (left instanceof Boolean l && right instanceof Boolean r) {
      return l.compareTo(r);
    }
    if (left instanceof String l && right instanceof String r) {
      return l.compareTo(r);
    }
    if (left instanceof ZonedDateTime l && right instanceof ZonedDateTime r) {
      // Two timestamp are equal if they represent the same moment in time:
      // Timestamp '2019-01-01 8:00:00 -8:00' = Timestamp '2019-01-01 11:00:00 -5:00'
      return l.compareTo(r);
    }
    if (left instanceof byte[] l && right instanceof byte[] r) {
      return ByteBuffer.wrap(l).compareTo(ByteBuffer.wrap(r));
    }
    if (left instanceof Interval l && right instanceof Interval r) {
      return l.compareTo(r);
    }

    throw new ExpressionException(ErrorCode.INTERNAL_ERROR, "Compare error");
  }

  protected static boolean equalsTo(final byte[] left, final byte[] right) {

    if (left.length != right.length) return false;

    for (int i = 0; i < left.length; i++) {
      int compare = left[i] - right[i];
      if (compare != 0) {
        return false;
      }
    }
    return true;
  }
}
