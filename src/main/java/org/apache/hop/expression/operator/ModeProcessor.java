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
package org.apache.hop.expression.operator;

import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.util.LinkedHashMap;
import java.util.Map;
import org.apache.hop.expression.IAggregateProcessor;
import org.apache.hop.expression.IExpression;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Returns the most frequent value of an expression over a group of rows, the NULL values are
 * ignored. If several values have the same highest frequency, the first one met is returned.
 */
@NullMarked
public class ModeProcessor implements IAggregateProcessor {

  /** The occurrences of a value, with the first value met to return it unchanged. */
  private static final class Occurrence {
    private final Object value;
    private long count;

    private Occurrence(Object value) {
      this.value = value;
    }
  }

  // Keep the order in which the values are met to return the first one in case of a tie
  private final Map<Object, Occurrence> occurrences = new LinkedHashMap<>();

  @Override
  public void process(IExpression[] operands) throws Exception {
    Object value = operands[0].getValue();
    if (value != null) {
      occurrences.computeIfAbsent(key(value), k -> new Occurrence(value)).count++;
    }
  }

  @Override
  public @Nullable Object getValue() throws Exception {
    // The result of an empty group is NULL
    Occurrence mode = null;
    for (Occurrence occurrence : occurrences.values()) {
      if (mode == null || occurrence.count > mode.count) {
        mode = occurrence;
      }
    }
    return mode == null ? null : mode.value;
  }

  /**
   * Returns the key to count the occurrences of a value, equal for the values which are equal.
   *
   * @param value the value
   * @return the key
   */
  private static Object key(Object value) {
    return switch (value) {
        // The same number whatever its scale, 1.0 as 1.00
      case BigDecimal number -> number.stripTrailingZeros();
        // An array is equal to another array only if it is the same instance
      case byte[] bytes -> ByteBuffer.wrap(bytes);
      default -> value;
    };
  }
}
