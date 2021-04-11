/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression.value;

import org.apache.hop.expression.DataType;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpressionContext;
import java.io.StringWriter;
import java.math.BigDecimal;

public class ValueBoolean extends Value {


  /** Boolean value of TRUE. */
  public static final Value TRUE = new ValueBoolean(true);
  
  /** Boolean value of FALSE. */
  public static final Value FALSE = new ValueBoolean(false);
  
  /**
   * Get the boolean value for the given boolean.
   *
   * @param b the boolean
   * @return the value
   */
  public static Value of(boolean value) {
    return value ? TRUE : FALSE;
  }
  
  private final boolean value;

  private ValueBoolean(boolean value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return value ? "TRUE" : "FALSE";
  }

  @Override
  public boolean toBoolean() {
    return value;
  }

  @Override
  public int hashCode() {
    return value ? 1 : 0;
  }

  @Override
  public boolean equals(Object other) {
    // there are only ever two instances, so the instance must match
    return this == other;
  }

  @Override
  public int compare(Value v) {
    if (value && v.toBoolean() || !value && !v.toBoolean()) {
      return 0; // true == true, false == false
    }
    if (value && !v.toBoolean()) {
      return 1; // true > false
    }
    return -1; // false < true
  }

  @Override
  public Value eval(IExpressionContext context) throws ExpressionException {
    return this;
  }

  @Override
  public DataType getType() {
    return DataType.BOOLEAN;
  }

  @Override
  public Object getObject() {
    return value;
  }

  public void write(StringWriter writer, int leftPrec, int rightPrec) {
    writer.append(this.toString());
  }

  @Override
  public Value negate() {
    return value ? ValueBoolean.FALSE : ValueBoolean.TRUE;
  }

  @Override
  public BigDecimal toBigNumber() throws ExpressionException {
    return value ? BigDecimal.ONE : BigDecimal.ZERO;
  }

  @Override
  public double toNumber() throws ExpressionException {
    return value ? 1.0 : 0.0;
  }

  @Override
  public long toInteger() throws ExpressionException {
    // avoids creating new Long objects all the time.
    return value ? 1L : 0L;
  }
}
