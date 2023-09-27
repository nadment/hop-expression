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

  /** Return the default {@link Type} that belongs to this family. */
  public Type getDefaultDataType() {
    switch (this) {
      case BOOLEAN:
        return BooleanType.BOOLEAN;
      case BINARY:
        return BinaryType.BINARY;
      case STRING:
        return StringType.STRING;
      case TEMPORAL:
        return DateType.DATE;
      case NUMERIC:
        return NumberType.NUMBER;
      case JSON:
        return JsonType.JSON;
      default:
        return null;
    }
  }

  /** Returns the collection of {@link TypeName}s included in this family. */
  public Set<TypeName> getDataTypeNames() {
    switch (this) {
      case BOOLEAN:
        return TypeName.BOOLEAN_TYPES;
      case BINARY:
        return TypeName.BINARY_TYPES;
      case NUMERIC:
        return TypeName.NUMERIC_TYPES;
      case STRING:
        return TypeName.STRING_TYPES;
      case TEMPORAL:
        return TypeName.TEMPORAL_TYPES;
      case JSON:
        return TypeName.JSON_TYPES;
      case ANY:
        return TypeName.ALL_TYPES;
      default:
        return Set.of();
    }
  }

  /**
   * Returns whether type are in same type family.
   */
  public boolean isSameFamily(final TypeFamily... families) {
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
        return family.isSameFamily(BOOLEAN, NUMERIC, BINARY, STRING);
      case STRING:
        return family.isSameFamily(STRING, BOOLEAN, NUMERIC, TEMPORAL, BINARY, JSON);
      case TEMPORAL:
        return family.isSameFamily(TEMPORAL);
      case NUMERIC:
        return family.isSameFamily(NUMERIC, BOOLEAN, BINARY, STRING);
      case BINARY:
        return family.isSameFamily(BINARY, STRING);
      case JSON:
        return family.isSameFamily(JSON, STRING);
      case INTERVAL:
        return family.isSameFamily(INTERVAL);
      case ANY:
      case NONE:
        return true;
      case SYMBOL:
      default:
        return false;
    }
  }
}
