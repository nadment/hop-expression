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

import static java.util.Objects.requireNonNull;

import org.apache.hop.expression.IExpression;

/** A collection of reusable instances of {@link ITypeTransform}. */
public final class TypeTransforms {

  /**
   * Type transform strategy where a derived type is transformed into the same type but nullable if
   * any of the call operands is nullable.
   */
  public static final ITypeTransform TO_NULLABLE =
      (call, typeToTransform) -> {
        boolean nullable = false;
        for (IExpression operand : call.getOperands()) {
          if (operand.getType().isNullable()) {
            nullable = true;
          }
        }

        return typeToTransform.withNullability(nullable);
      };

  /**
   * Type transform strategy where a derived type is transformed into the same type, but nullable if
   * and only if all of a call's operands are nullable.
   */
  public static final ITypeTransform TO_NULLABLE_ALL =
      (call, typeToTransform) -> {
        boolean nullable = true;
        for (IExpression operand : call.getOperands()) {
          if (!operand.getType().isNullable()) {
            nullable = false;
          }
        }

        return typeToTransform.withNullability(nullable);
      };

  /**
   * Type transform strategy where a derived type is transformed into the same type but not
   * nullable.
   */
  public static final ITypeTransform TO_NOT_NULLABLE =
      (call, typeToTransform) -> typeToTransform.withNullability(false);

  /**
   * Type transform strategy where a derived type is transformed into the same type with nulls
   * allowed.
   */
  public static final ITypeTransform FORCE_NULLABLE =
      (call, typeToTransform) -> typeToTransform.withNullability(true);

  /**
   * Type transform strategy where the result is NOT NULL if any of the arguments is NOT NULL,
   * otherwise the type is unchanged.
   */
  public static final ITypeTransform LEAST_NULLABLE =
      (call, typeToTransform) -> {
        boolean nullable = typeToTransform.isNullable();
        for (IExpression operand : call.getOperands()) {
          if (!operand.getType().isNullable()) {
            nullable = false;
          }
        }

        return typeToTransform.withNullability(nullable);
      };

  public static final ITypeTransform TO_MAX_PRECISION =
      (call, typeToTransform) ->
          switch (requireNonNull(typeToTransform).getName()) {
            case STRING -> StringType.STRING;
            case BINARY -> BinaryType.BINARY;
            case INTEGER -> IntegerType.INTEGER;
            case NUMBER -> NumberType.NUMBER;
            default -> typeToTransform;
          };

  private TypeTransforms() {
    // Utility class
  }
}
