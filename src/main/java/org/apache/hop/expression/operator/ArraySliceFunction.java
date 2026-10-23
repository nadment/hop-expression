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
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Returns an array constructed from a specified subset of elements of the input array. */
@FunctionPlugin
@NullMarked
public class ArraySliceFunction extends Function {

  public static final Function INSTANCE = new ArraySliceFunction();

  public ArraySliceFunction() {
    super(
        "ARRAY_SLICE",
        ReturnTypes.ARRAY,
        OperandTypes.ARRAY_INTEGER_INTEGER,
        OperatorCategory.ARRAY,
        "/docs/hop-user-manual/expression/array_slice.html");
  }

  @Override
  public @Nullable Object eval(IExpression[] operands) {
    Array array = operands[0].getValue(Array.class);
    if (array == null) return null;
    Long fromLong = operands[1].getValue(Long.class);
    if (fromLong == null) return null;
    Long toLong = operands[2].getValue(Long.class);
    if (toLong == null) return null;

    int size = array.size();

    long from = fromLong;
    if (from < 0) {
      from = size + from;
    }

    long to = toLong;
    if (to < 0) {
      to = size + to;
    }

    // Clamp to the bounds of the array on long values, the conversion to int would wrap a value
    // out of the int range, so a slice partially out of the array keeps its elements inside
    from = Math.clamp(from, 0, size);
    to = Math.clamp(to, 0, size);

    if (to <= from) return Array.EMPTY;

    return array.slice((int) from, (int) to);
  }
}
