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

import java.util.Objects;

public class DataType {
  
  public static final int SCALE_NOT_SPECIFIED = -1;
  public static final int PRECISION_NOT_SPECIFIED = -1;
  
  /**
   * NULL type with parameters.
   */
  //public static final DataType NULL;

  /**
   * Default BINARY type with default parameters.
   */
  public static final DataType BINARY;

  /**
   * BOOLEAN type with parameters.
   */
  public static final DataType BOOLEAN;
  /**
   * Default DATE type with parameters.
   */
  public static final DataType DATE;
  /**
   * Default STRING type with parameters.
   */
  public static final DataType STRING;
  /**
   * JSON type with parameters.
   */
  public static final DataType JSON;
  
  /**
   * Default INTEGER type with parameters.
   */
  public static final DataType INTEGER;
  public static final DataType NUMBER;
  public static final DataType BIGNUMBER;
  
  static {
    //NULL = new DataType(DataTypeName.UNKNOWN);
    BOOLEAN = new DataType(DataTypeName.BOOLEAN, 1 ,0);
    BINARY = new DataType(DataTypeName.BINARY);
    DATE = new DataType(DataTypeName.DATE);
    JSON = new DataType(DataTypeName.JSON);    
    STRING = new DataType(DataTypeName.STRING);    
    INTEGER = new DataType(DataTypeName.INTEGER, 10, 0);
    NUMBER = new DataType(DataTypeName.NUMBER, 38, 0);
    BIGNUMBER = new DataType(DataTypeName.BIGNUMBER, 38, 0);
  }

  private DataTypeName name;
  private int precision;
  private int scale;

  private DataType(final DataTypeName name) {
    this.name = name;
    this.precision = PRECISION_NOT_SPECIFIED;
    this.scale = SCALE_NOT_SPECIFIED;
  }
  
  private DataType(final DataTypeName name, final int precision, final int scale) {
    this.name = Objects.requireNonNull(name);
    this.precision = precision;
    this.scale = scale;
  }

  /**
   * Gets the {@link DataTypeName} of this type.
   *
   * @return DataTypeName, never null
   */
  public DataTypeName getName() {
    return name;
  }
  
  /**
   * Gets the family of this type.
   *
   * @return type family, never null
   */
  public DataTypeFamily getFamily() {
    return name.getFamily();
  }
  
  /**
   * Gets the precision of this type.
   *
   * <p>Returns {@link #PRECISION_NOT_SPECIFIED} (-1) if precision is not
   * applicable for this type.</p>
   *
   * @return number of 
   * decimal digits for exact numeric types; 
   * number of decimal digits in mantissa for approximate numeric types;
   * number of decimal digits for fractional seconds of datetime types; 
   * length in characters for String types; 
   * length in bytes for Binary types;
   * 1 for BOOLEAN;
   * -1 if precision is not valid for this type
   */
  public int getPrecision() {
    return precision;
  }

  /**
   * Gets the scale of this type. Returns {@link #SCALE_NOT_SPECIFIED} (-1) if
   * scale is not valid for this type.
   *
   * @return number of digits of scale
   */
  public int getScale() {
    return scale;
  }
}

