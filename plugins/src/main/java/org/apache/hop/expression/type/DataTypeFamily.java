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

import java.util.List;

public enum DataTypeFamily {
  // Primary
  BINARY, NUMERIC, DATE, BOOLEAN, STRING, JSON,
  // Secondary
  ANY;

  /** Return the default {@link DataType} that belongs to this family. */
  public DataType getDefaultDataType() {
    switch (this) {
      case BOOLEAN:
        return DataType.BOOLEAN;
      case BINARY:
        return DataType.BINARY;
      case STRING:
        return DataType.STRING;
      case DATE:
        return DataType.DATE;
      case NUMERIC:
        return DataType.BIGNUMBER;
      case JSON:
        return DataType.JSON;
      default:
        return null;
    }
  }

  /** Returns the collection of {@link DataTypeName}s included in this family. */
  public List<DataTypeName> getDataTypeNames() {
    switch (this) {
      case BOOLEAN:
        return DataTypeName.BOOLEAN_TYPES;
      case BINARY:
        return DataTypeName.BINARY_TYPES;
      case NUMERIC:
        return DataTypeName.NUMERIC_TYPES;
      case STRING:
        return DataTypeName.STRING_TYPES;
      case DATE:
        return DataTypeName.DATE_TYPES;
      case JSON:
        return DataTypeName.JSON_TYPES;
      case ANY:
        return DataTypeName.ALL_TYPES;
    }

    return List.of();
  }

  /**
   * Returns whether type are in same type family.
   */
  public boolean isSameFamily(final DataTypeFamily... families) {
    if (families == null)
      return false;
    for (DataTypeFamily family : families) {
      if (ANY == this || family == ANY || this == family)
        return true;
    }
    return false;
  }

  /**
   * Returns whether type are in same type family with implicit coercion.
   */
  public boolean isCompatibleWithCoercion(final DataTypeFamily family) {
    if (family == null)
      return false;

    switch (this) {
      case BOOLEAN:
        return family.isSameFamily(BOOLEAN, NUMERIC, BINARY, STRING);
      case STRING:
        return family.isSameFamily(STRING, BOOLEAN, NUMERIC, DATE, BINARY, JSON);
      case DATE:
        return family.isSameFamily(DATE);
      case NUMERIC:
        return family.isSameFamily(NUMERIC, BOOLEAN, BINARY, STRING);
      case BINARY:
        return family.isSameFamily(BINARY, STRING);
      case JSON:
        return family.isSameFamily(JSON, STRING);
      case ANY:
        return true;
      default:
        return false;
    }
  }
}
