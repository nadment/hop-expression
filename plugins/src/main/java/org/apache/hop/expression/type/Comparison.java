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

import org.apache.hop.expression.ExpressionException;
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


  /**
   * Compare this value against another value. If values need to be converted to match the other
   * operands data type, the value with the lower order is converted to the value with the higher
   * order.
   *
   * @param value the other value
   * @return 0 if both values are equal, -1 if this value is smaller, and 1 otherwise
   */
  public static final int compare(final Object left, final Object right)
      throws ExpressionException {

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

    if (left instanceof JsonNode || right instanceof JsonNode) {

      JsonNode l = JsonType.coerce(left);
      JsonNode r = JsonType.coerce(right);

      // Ignores the order of attributes
      return l.equals(JSON_COMPARATOR, r) ? 0 : 1;
    }

    if (left instanceof ZonedDateTime || right instanceof ZonedDateTime) {
      ZonedDateTime dt1 = DateType.coerce(left);
      ZonedDateTime dt2 = DateType.coerce(right);
      // Two timestamp are equal if they represent the same moment in time:
      // Timestamp '2019-01-01 8:00:00 -8:00' = Timestamp '2019-01-01 11:00:00 -5:00'
      if (dt1.isEqual(dt2)) {
        return 0;
      }
      return dt1.compareTo(dt2);
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
