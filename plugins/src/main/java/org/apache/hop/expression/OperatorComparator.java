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
package org.apache.hop.expression;

import java.util.Comparator;

public class OperatorComparator implements Comparator<Operator> {

  public OperatorComparator() {
    // Constructor
  }

  @Override
  public int compare(Operator o1, Operator o2) {

    // Compare with id
    int compare = o1.getId().compareTo(o2.getId());
    if (compare != 0) return compare;

    // Primary operator first and alias last
    if (o1.getId().equals(o1.getName())) return 99;

    return o1.getName().compareTo(o2.getName());
  }
}
