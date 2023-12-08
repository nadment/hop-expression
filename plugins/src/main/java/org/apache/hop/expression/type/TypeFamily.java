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

import java.util.Set;

/**
 * Represents a family of related types.
 */
public enum TypeFamily {
  // -------------------------------------------
  // Primary
  // -------------------------------------------
  //
  BINARY,
  //
  NUMERIC,
  //
  TEMPORAL,
  //
  BOOLEAN,
  //
  STRING,
  //
  JSON,
  //
  INTERVAL,
  // -------------------------------------------
  // Secondary
  // -------------------------------------------
  //
  NONE,
  //
  ANY,
  //
  SYMBOL;

  /** Returns the collection of {@link TypeId}s included in this family. */
  public Set<TypeId> getTypeIds() {
    switch (this) {
      case BOOLEAN:
        return TypeId.BOOLEAN_TYPES;
      case BINARY:
        return TypeId.BINARY_TYPES;
      case NUMERIC:
        return TypeId.NUMERIC_TYPES;
      case STRING:
        return TypeId.STRING_TYPES;
      case TEMPORAL:
        return TypeId.TEMPORAL_TYPES;
      case JSON:
        return TypeId.JSON_TYPES;
      case ANY:
        return TypeId.ALL_TYPES;
      default:
        return Set.of();
    }
  }

  /**
   * Returns whether type are in same type family.
   */
  public boolean isFamily(final TypeFamily... families) {
    if (families == null)
      return false;
    for (TypeFamily family : families) {
      if (ANY == this || family == ANY || this == family)
        return true;
    }
    return false;
  }


  /**
   * Returns whether type are in same type family with implicit coercion.
   */
  public boolean isCompatibleWithCoercion(final TypeFamily family) {
    if (family == null)
      return false;

    switch (this) {
      case BOOLEAN:
        return family.isFamily(BOOLEAN, NUMERIC, BINARY, STRING);
      case STRING:
        return family.isFamily(STRING, BOOLEAN, NUMERIC, TEMPORAL, BINARY, JSON);
      case TEMPORAL:
        return family.isFamily(TEMPORAL);
      case NUMERIC:
        return family.isFamily(NUMERIC, BOOLEAN, BINARY, STRING);
      case BINARY:
        return family.isFamily(BINARY, STRING);
      case JSON:
        return family.isFamily(JSON, STRING);
      case INTERVAL:
        return family.isFamily(INTERVAL);
      case ANY:
        return true;
      case NONE:
      case SYMBOL:
      default:
        return false;
    }
  }
}
