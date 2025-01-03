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

import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;

/** Contains function */
@FunctionPlugin
public class ContainsFunction extends Function {
  public static final Function INSTANCE = new ContainsFunction();

  public ContainsFunction() {
    super(
        "CONTAINS",
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.STRING_STRING.or(OperandTypes.BINARY_BINARY),
        OperatorCategory.COMPARISON,
        "/docs/contains.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getOperand(0).getType();
    if (Types.isBinary(type)) {
      return new Call(BinaryContainsFunction.INSTANCE, call.getOperands());
    }

    return new Call(StringContainsFunction.INSTANCE, call.getOperands());
  }

  /** Contains string function */
  private static final class StringContainsFunction extends ContainsFunction {
    public static final ContainsFunction INSTANCE = new StringContainsFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      String value = operands[0].getValue(String.class);
      if (value == null) return null;

      String search = operands[1].getValue(String.class);
      if (search == null) return null;

      if (value.contains(search)) return Boolean.TRUE;

      return Boolean.FALSE;
    }
  }

  /** Contains binary function */
  private static final class BinaryContainsFunction extends ContainsFunction {
    public static final ContainsFunction INSTANCE = new BinaryContainsFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      byte[] value = operands[0].getValue(byte[].class);
      if (value == null) return null;

      byte[] search = operands[1].getValue(byte[].class);
      if (search == null) return null;

      if (search.length == 0) {
        return Boolean.FALSE;
      }

      outer:
      for (int i = 0; i < value.length - search.length + 1; i++) {
        for (int j = 0; j < search.length; j++) {
          if (value[i + j] != search[j]) {
            continue outer;
          }
        }
        return Boolean.TRUE;
      }
      return Boolean.FALSE;
    }
  }
}
