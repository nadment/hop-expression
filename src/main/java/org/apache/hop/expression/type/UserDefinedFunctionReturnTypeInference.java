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
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.UserDefinedFunction;
import org.apache.hop.expression.UserDefinedFunctionMeta;
import org.jspecify.annotations.NullMarked;

/**
 * Type-inference strategy for a user-defined function.
 *
 * <p>The result type of the call is the type of the source of the function for arguments of the
 * declared types, so a function returning an array can be the operand of an array function or of
 * the operator {@code []}. The exact type is inferred when the call is compiled, as the source of
 * the function replaces the call with the operands in place of the arguments.
 */
@NullMarked
public class UserDefinedFunctionReturnTypeInference implements IReturnTypeInference {
  private final UserDefinedFunctionMeta meta;

  /**
   * Creates a type-inference strategy for a user-defined function.
   *
   * @param meta the metadata of the function
   */
  public UserDefinedFunctionReturnTypeInference(UserDefinedFunctionMeta meta) {
    this.meta = meta;
  }

  /**
   * {@inheritDoc}
   *
   * <p>The type is ANY if the source of the function is invalid or returns an untyped NULL, the
   * error is reported when the call is compiled.
   */
  @Override
  public Type inferReturnType(Call call) {
    try {
      Type type = UserDefinedFunction.parseSource(meta).getType();
      return type.isName(TypeName.UNKNOWN) ? AnyType.ANY : type;
    } catch (ExpressionException e) {
      return AnyType.ANY;
    }
  }
}
