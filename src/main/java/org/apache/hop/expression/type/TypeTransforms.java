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

/** A collection of strategies for type transform. */
public final class TypeTransforms {

  private TypeTransforms() {
    // Utility class
  }

  public static final ITypeTransform TO_NULLABLE =
      typeToTransform -> typeToTransform.withNullability(true);

  public static final ITypeTransform TO_NOT_NULLABLE =
      typeToTransform -> typeToTransform.withNullability(false);

  public static final ITypeTransform TO_MAX_PRECISION =
      typeToTransform ->
          switch (requireNonNull(typeToTransform).getName()) {
            case STRING -> Types.STRING;
            case BINARY -> Types.BINARY;
            case INTEGER -> Types.INTEGER;
            case NUMBER -> Types.NUMBER;
            default -> typeToTransform;
          };
}
