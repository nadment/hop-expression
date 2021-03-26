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

import java.io.StringWriter;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Value;
import org.apache.hop.expression.DataType;
import org.apache.hop.expression.ExpressionException;

/** Implementation of NULL. NULL is not a regular data type. */
public class ValueNull extends Value {

  public ValueNull() {
    // No value
  }

  @Override
  public DataType getType() {
    return DataType.NONE;
  }

  @Override
  public Object getObject() {
    return null;
  }

  @Override
  public boolean toBoolean() {
    return false;
  }

  @Override
  public String toString() {
    return null;
  }

  @Override
  public int hashCode() {
    return 0;
  }

  @Override
  public boolean equals(Object other) {
    return other == this;
  }
  
  @Override
  public boolean isNull() {
    return true;
  }

  @Override
  public Value negate() {
    return this;
  }

  @Override
  public Value eval(IExpressionContext context) throws ExpressionException {
    return this;
  }

  public void write(StringWriter writer, int leftPrec, int rightPrec) {
    writer.append("NULL");
  }

  @Override
  public int compare(Value o) {
    if (o.isNull()) return 0;

    // null is always smaller
    return -1;
  }
}
