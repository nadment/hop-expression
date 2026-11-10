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

import java.util.ArrayList;
import java.util.List;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeComparability;
import org.apache.hop.expression.type.TypeName;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Returns an array without the duplicate elements of an array, the first occurrence of each element
 * is kept in its original order.
 *
 * @see ArrayRemoveFunction
 */
@FunctionPlugin
@NullMarked
public class ArrayDistinctFunction extends Function {

  public static final Function INSTANCE = new ArrayDistinctFunction();

  public ArrayDistinctFunction() {
    super(
        "ARRAY_DISTINCT",
        ReturnTypes.ARG0,
        OperandTypes.ARRAY,
        OperatorCategory.ARRAY,
        "/docs/hop-user-manual/expression/array_distinct.html");
  }

  @Override
  protected boolean checkOperandTypes(Call call) {
    if (!super.checkOperandTypes(call)) {
      return false;
    }

    // Array elements should be comparable for equality
    Type elementType = call.getOperand(0).getType().getElementType();
    return elementType != null
        && (elementType.isName(TypeName.ANY)
            || elementType.getComparability() != TypeComparability.NONE);
  }

  @Override
  public @Nullable Object eval(IExpression[] operands) {
    Array array = operands[0].getValue(Array.class);
    if (array == null) return null;

    // The values are converted to the element type of the array to compare the values of elements
    // of different types, like INTEGER and NUMBER
    Type type = array.getType().getElementType();
    if (type == null) return null;
    Class<?> clazz = type.getName().getJavaClass();

    List<IExpression> elements = new ArrayList<>();
    List<Object> values = new ArrayList<>();
    boolean hasNull = false;
    for (IExpression element : array) {
      Object value = element.getValue(clazz);
      // The NULL elements are duplicates of each other, only the first one is kept
      if (value == null) {
        if (!hasNull) {
          hasNull = true;
          elements.add(element);
        }
        continue;
      }

      if (values.stream().noneMatch(kept -> type.compare(kept, value) == 0)) {
        values.add(value);
        elements.add(element);
      }
    }

    return new Array(array.getType(), elements);
  }
}
