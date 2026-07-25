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
package org.apache.hop.expression.jit;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.type.Type;
import org.jspecify.annotations.NullMarked;

/**
 * Mutable state accumulated while {@link JitCodeGenerator} walks an expression tree.
 *
 * <p>Two kinds of runtime constants are captured by reference so the generated Java source never
 * needs to embed arbitrary object literals:
 *
 * <ul>
 *   <li>{@code leaves}: expression nodes that are evaluated through the normal interpreted path
 *       (fields, literals, or sub-calls with no code generation support) and referenced from
 *       generated code as {@code leaves[i]}.
 *   <li>{@code types}: {@link Type} instances needed by comparison operators, referenced from
 *       generated code as {@code types[i]}.
 * </ul>
 */
@NullMarked
public final class JitContext {

  private final List<IExpression> leaves = new ArrayList<>();
  private final Map<IExpression, Integer> leafIndex = new IdentityHashMap<>();
  private final List<Type> types = new ArrayList<>();

  /**
   * Registers an expression to be evaluated through the interpreted path at runtime.
   *
   * @return the index into the generated class's {@code leaves} array
   */
  public int addLeaf(IExpression expression) {
    Integer existing = leafIndex.get(expression);
    if (existing != null) {
      return existing;
    }
    int index = leaves.size();
    leaves.add(expression);
    leafIndex.put(expression, index);
    return index;
  }

  /**
   * Registers a {@link Type} instance needed by generated code.
   *
   * @return the index into the generated class's {@code types} array
   */
  public int addType(Type type) {
    int index = types.size();
    types.add(type);
    return index;
  }

  public List<IExpression> getLeaves() {
    return leaves;
  }

  public List<Type> getTypes() {
    return types;
  }
}
