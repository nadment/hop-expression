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
package org.apache.hop.expression.type;

import org.apache.hop.expression.Call;

/**
 * Calculate return type precision and scale for x + y and x - y
 * 
 * <ul>
 * <li>Let p1, s1 be the precision and scale of the first operand</li>
 * <li>Let p2, s2 be the precision and scale of the second operand</li>
 * <li>Let p, s be the precision and scale of the result</li>
 * <li>Let d be the number of whole digits in the result</li>
 * <li>Then the result type is a decimal with:
 * <ul>
 * <li>s = max(s1, s2)</li>
 * <li>p = max(p1 - s1, p2 - s2) + s + 1</li>
 * </ul>
 * </li>
 * <li>p and s are capped at their maximum values</li>
 * </ul>
 * 
 */
public class AdditiveOperatorReturnTypeInference implements IReturnTypeInference {

  public AdditiveOperatorReturnTypeInference() {
    super();
  }

  @Override
  public Type inferReturnType(final Call call) {
    Type type1 = call.getOperand(0).getType();
    Type type2 = call.getOperand(1).getType();

    int p1 = type1.getPrecision();
    int p2 = type2.getPrecision();
    int s1 = type1.getScale();
    int s2 = type2.getScale();

    // Return type scale
    int s = Math.max(s1, s2);

    // Return type precision
    int p = Math.min(TypeName.NUMBER.getMaxPrecision(), Math.max(p1 - s1, p2 - s2) + s + 1);

    // Optimize to INTEGER type
    if (!(type1.is(TypeName.NUMBER) || type2.is(TypeName.NUMBER))
        && p <= TypeName.INTEGER.getMaxPrecision() && s == 0) {
      return new IntegerType(p);
    }

    return new NumberType(p, s);
  }
}
