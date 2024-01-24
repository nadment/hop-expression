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
package org.apache.hop.expression.experimental;

import static java.util.Objects.requireNonNull;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.type.Type;
import java.lang.invoke.MethodHandle;
import java.util.Arrays;
import java.util.Objects;

/**
 * An expression formed by a call to an {@link Operator} with zero or more expressions as operands.
 */
public class MethodCall extends Call {

  protected final MethodHandle method;

  public MethodCall(Operator operator, MethodHandle method, Type type, IExpression... operands) {
    super(operator, operands);
    this.method = requireNonNull(method, "method");
    this.type = requireNonNull(type, "type");
  }

  /**
   * Get the operator
   *
   * @return the operator
   */
  public Operator getOperator() {
    return operator;
  }

  public Object getValue() {
    try {
      return method.invokeExact(operator, operands);
    } catch (Throwable e) {
      throw new ExpressionException(ErrorCode.OPERATOR_ERROR, operator, e.getMessage());
    }
  }

  public <T> T getValue(final Class<T> clazz) {
    try {
      Object value = method.invokeExact(operator, operands);
      return type.convert(value, clazz);
    } catch (Throwable e) {
      throw new ExpressionException(ErrorCode.OPERATOR_ERROR, operator, e.getMessage());
    }
  }

  @Override
  public boolean equals(Object o) {
    if (this == o)
      return true;

    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    MethodCall call = (MethodCall) o;
    return this.type.equals(call.type) && this.operator.equals(call.operator)
        && Arrays.equals(this.operands, call.operands);
  }

  @Override
  public int hashCode() {
    return Objects.hash(type, operator, operands);
  }
}

