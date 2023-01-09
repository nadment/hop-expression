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

import java.util.Comparator;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.NumericNode;
/**
 * Compare Json Node
 * <ul>
 * <li>Ignores the order of attributes</li>
 * <li>Considers numeric values 5.0 and 5 as equals</li>
 * </ul>
 */
public class JsonComparator implements Comparator<JsonNode>  {
 @Override
    public int compare(final JsonNode o1, final JsonNode o2)
    {
        if (o1.equals(o2)){
           return 0;
        }
        if ((o1 instanceof NumericNode) && (o2 instanceof NumericNode)){
            Double d1 = ((NumericNode) o1).asDouble();
            Double d2 = ((NumericNode) o2).asDouble(); 
            if (d1.compareTo(d2) == 0) {
               return 0;
            }
        }
        return 1;
    }
}