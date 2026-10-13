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
import org.apache.hop.expression.UserDefinedFunctionMeta;
import org.jspecify.annotations.NullMarked;

/**
 * Operand type-checking strategy for a user-defined function.
 *
 * <p>Only the number of operands is checked here. Their types are checked when the call is
 * compiled, by validating the source of the function with the operands in place of the arguments.
 */
@NullMarked
public class UserDefinedFunctionOperandTypeChecker implements IOperandTypeChecker {
  private final UserDefinedFunctionMeta meta;

  /**
   * Creates a checker for a user-defined function.
   *
   * @param meta the metadata of the function
   */
  public UserDefinedFunctionOperandTypeChecker(UserDefinedFunctionMeta meta) {
    this.meta = meta;
  }

  /**
   * {@inheritDoc}
   *
   * <p>Always succeeds, the types are checked when the call is compiled.
   */
  @Override
  public boolean checkOperandTypes(Call call) {
    return true;
  }

  /**
   * {@inheritDoc}
   *
   * <p>A user-defined function expects exactly one operand per argument.
   */
  @Override
  public IOperandCountRange getOperandCountRange() {
    return OperandCountRange.of(meta.getArguments().size());
  }
}
