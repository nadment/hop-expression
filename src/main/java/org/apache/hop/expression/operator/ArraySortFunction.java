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
import java.util.Comparator;
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
import org.apache.hop.expression.util.Pair;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Returns an array with the elements of an array sorted in ascending or descending order, the NULL
 * elements are placed at the end.
 */
@FunctionPlugin
@NullMarked
public class ArraySortFunction extends Function {

  public static final Function INSTANCE = new ArraySortFunction();

  public ArraySortFunction() {
    super(
        "ARRAY_SORT",
        ReturnTypes.ARG0,
        OperandTypes.ARRAY.or(OperandTypes.ARRAY_BOOLEAN),
        OperatorCategory.ARRAY,
        "/docs/hop-user-manual/expression/array_sort.html");
  }

  @Override
  public boolean checkOperandTypes(Call call) {
    if (!super.checkOperandTypes(call)) {
      return false;
    }

    // Array elements should be ordered
    Type elementType = call.getOperand(0).getType().getElementType();
    return elementType != null
        && (elementType.isName(TypeName.ANY)
            || elementType.getComparability() == TypeComparability.ALL);
  }

  @Override
  public @Nullable Object eval(IExpression[] operands) {
    Array array = operands[0].getValue(Array.class);
    if (array == null) return null;

    boolean ascending = true;
    if (operands.length == 2) {
      Boolean value = operands[1].getValue(Boolean.class);
      if (value == null) return null;
      ascending = value;
    }

    // The values are converted to the element type of the array to compare the values of elements
    // of different types, like INTEGER and NUMBER
    Type type = array.getType().getElementType();
    if (type == null) return null;
    Class<?> clazz = type.getName().getJavaClass();

    // Evaluate each element once, the values are compared several times by the sort
    List<Pair<IExpression, Object>> values = new ArrayList<>();
    List<IExpression> nulls = new ArrayList<>();
    for (IExpression element : array) {
      Object value = element.getValue(clazz);
      if (value == null) {
        nulls.add(element);
      } else {
        values.add(Pair.of(element, value));
      }
    }

    Comparator<Pair<IExpression, Object>> comparator =
        (left, right) -> type.compare(left.right(), right.right());
    values.sort(ascending ? comparator : comparator.reversed());

    // The NULL elements are placed at the end, whatever the order
    List<IExpression> elements = new ArrayList<>();
    values.forEach(pair -> elements.add(pair.left()));
    elements.addAll(nulls);
    return new Array(array.getType(), elements);
  }
}
