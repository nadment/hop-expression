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

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;

/**
 * Returns an array element at the given an index <br>
 * <strong>Syntax:</strong> <code>array[3]</code>
 */
public class ElementAtOperator extends Operator {

  public ElementAtOperator() {
    super(
        "ELEMENT_AT",
        "[]",
        200,
        true,
        ReturnTypes.ARRAY_ELEMENT,
        OperandTypes.ARRAY_NUMERIC,
        OperatorCategory.ARRAY,
        "/docs/element_at.html");
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    Array array = call.getOperand(0).asArray();
    Type type = Types.getLeastRestrictive(array);

    // Coerce values
    boolean coerced = false;
    List<IExpression> list = new ArrayList<>();
    for (IExpression operand : array) {
      if (Types.needToCast(operand, type)) {
        operand = Types.cast(operand, type);
        coerced = true;
      }
      list.add(operand);
    }

    if (coerced) {
      call.setOperand(0, new Array(ArrayType.of(type), list));
      call.inferReturnType();
    }

    return coerced;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Array array = operands[0].asArray();
    Long index = operands[1].getValue(Long.class);
    if (index == null) return null;

    int i = index.intValue();

    if (i < 0) i = array.size() + i + 1;

    if (i < 1 || i > array.size() || i > ArrayType.MAX_ARRAY_CARDINALITY) {
      throw new ExpressionException(ErrorCode.INVALID_ARRAY_INDEX, index);
    }

    return array.get(i - 1).getValue();
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, 0, 0);
    writer.append('[');
    operands[1].unparse(writer, 0, 0);
    writer.append(']');
  }
}
