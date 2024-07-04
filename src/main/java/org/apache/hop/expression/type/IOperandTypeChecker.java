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

/** Strategy interface to check for allowed operand types of an operator call. */
public interface IOperandTypeChecker {

  /**
   * Checks the types of all operands to an operator call.
   *
   * @param call the call to be checked
   * @return whether check succeeded
   */
  boolean checkOperandTypes(Call call);

  /** Returns the range of operand counts allowed in a call. */
  IOperandCountRange getOperandCountRange();

  /** Returns whether the {@code i}th operand is optional. */
  default boolean isOptional(int i) {
    return false;
  }

  /** Composes this with another checker using AND. */
  default IOperandTypeChecker and(IOperandTypeChecker checker) {
    return OperandTypes.and(this, checker);
  }

  /** Composes this with another checker using OR. */
  default IOperandTypeChecker or(IOperandTypeChecker checker) {
    return OperandTypes.or(this, checker);
  }
}
