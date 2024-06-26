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

/** A class that describes how many operands an operator can take. */
public class OperandCountRange implements IOperandCountRange {

  public static IOperandCountRange of(int length) {
    return new OperandCountRange(length, length);
  }

  public static IOperandCountRange between(int min, int max) {
    return new OperandCountRange(min, max);
  }

  public static IOperandCountRange from(int min) {
    return new OperandCountRange(min, -1);
  }

  public static IOperandCountRange any() {
    return new OperandCountRange(0, -1);
  }

  private final int min;
  private final int max;

  OperandCountRange(int min, int max) {
    this.min = min;
    this.max = max;
  }

  @Override
  public boolean isValid(int count) {
    return count >= min && (max == -1 || count <= max);
  }

  @Override
  public int getMin() {
    return min;
  }

  @Override
  public int getMax() {
    return max;
  }
}
