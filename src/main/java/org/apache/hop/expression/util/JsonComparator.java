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
import java.util.Comparator;

/**
 * Compare Json Node
 *
 * <ul>
 *   <li>Ignores the order of attributes
 *   <li>Considers numeric values 5.0 and 5 as equals
 * </ul>
 */
public class JsonComparator implements Comparator<JsonNode> {
  @Override
  public int compare(final JsonNode o1, final JsonNode o2) {
    if (o1.equals(o2)) {
      return 0;
    }
    if ((o1 instanceof NumericNode n1) && (o2 instanceof NumericNode n2)) {
      return Double.compare(n1.asDouble(), n2.asDouble());
    }
    return 1;
  }
}
