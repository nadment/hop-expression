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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.NumericNode;
import java.io.Serial;
import java.io.Serializable;
import java.util.Comparator;
import org.jspecify.annotations.NullMarked;

/**
 * Compare Json Node
 *
 * <ul>
 *   <li>Ignores the order of attributes
 *   <li>Considers numeric values 5.0 and 5 as equals
 * </ul>
 */
@NullMarked
public class JsonComparator implements Comparator<JsonNode>, Serializable {
  @Serial private static final long serialVersionUID = 1L;

  @Override
  public int compare(final JsonNode o1, final JsonNode o2) {
    if (o1.equals(o2)) {
      return 0;
    }
    if ((o1 instanceof NumericNode n1) && (o2 instanceof NumericNode n2)) {
      // Compared as decimals, a double cannot hold exactly an integer beyond 2^53
      try {
        return n1.decimalValue().compareTo(n2.decimalValue());
      } catch (NumberFormatException e) {
        // A double that is not a number or is infinite has no decimal value
        return Double.compare(n1.asDouble(), n2.asDouble());
      }
    }
    return 1;
  }
}
