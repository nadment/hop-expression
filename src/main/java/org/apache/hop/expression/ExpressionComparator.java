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

public class ExpressionComparator implements Comparator<IExpression> {

  public ExpressionComparator() {
    // Constructor
  }

  @Override
  public int compare(final IExpression e1, final IExpression e2) {

    // First order by cost
    if (e1.getCost() < e2.getCost()) return -1;
    if (e1.getCost() > e2.getCost()) return 1;

    // If same cost

    // Order identifier by name (useful for test)
    if (e1 instanceof Identifier i1 && e2 instanceof Identifier i2) {
      return i1.getName().compareTo(i2.getName());
    }

    // Order constant by family
    if (e1.isConstant() && e2.isConstant()) {
      int familyCompare = e1.getType().getFamily().compareTo(e2.getType().getFamily());

      if (familyCompare != 0) return familyCompare;

      return e1.hashCode() - e2.hashCode();
    }

    // Order with textual representation
    return e1.toString().compareTo(e2.toString());
  }
}
