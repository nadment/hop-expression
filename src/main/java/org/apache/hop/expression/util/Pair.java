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
package org.apache.hop.expression.util;

import java.util.Objects;

/** Pair of objects. */
public record Pair<T1, T2>(T1 left, T2 right) {
  public static <T1, T2> Pair<T1, T2> of(T1 left, T2 right) {
    return new Pair<>(left, right);
  }

  /**
   * Creates a Pair.
   *
   * @param left left value
   * @param right right value
   */
  public Pair(final T1 left, final T2 right) {
    this.left = Objects.requireNonNull(left, "left must not be null");
    this.right = Objects.requireNonNull(right, "right must not be null");
  }

  /** Get the left object. */
  @Override
  public T1 left() {
    return left;
  }

  /** Get the right object. */
  @Override
  public T2 right() {
    return right;
  }

  @Override
  public String toString() {
    return "<" + left + ", " + right + ">";
  }

  @Override
  public boolean equals(Object obj) {
    return this == obj
        || (obj instanceof Pair)
            && Objects.equals(this.left, ((Pair<?, ?>) obj).left)
            && Objects.equals(this.right, ((Pair<?, ?>) obj).right);
  }
}
