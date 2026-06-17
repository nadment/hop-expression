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
package org.apache.hop.expression.operator;

import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Array to array concatenation */
@NullMarked
public class ArrayConcatFunction extends ConcatFunction {

  public static final Function INSTANCE = new ArrayConcatFunction();

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    if (call.isConstant()) {
      return (IExpression) eval(call.getOperands());
    }
    return call;
  }

  @Override
  public @Nullable Object eval(IExpression[] operands) {
    Array result = null;

    for (IExpression operand : operands) {
      if (operand.isNull()) continue;

      Array array = operand.getValue(Array.class);
      if (array != null) {
        if (result == null) {
          result = array;
        } else {
          int size0 = result.size();
          int size1 = array.size();
          IExpression[] values = new IExpression[size0 + size1];
          for (int i = 0; i < size0; i++) {
            values[i] = result.get(i);
          }
          for (int i = 0; i < size1; i++) {
            values[size0 + i] = array.get(i);
          }
          result = new Array(result.getType(), values);
        }
      } else {
        if (result == null) {
          result = new Array(operand);
        } else {
          result = result.append(operand);
        }
      }
    }

    return result;
  }
}
