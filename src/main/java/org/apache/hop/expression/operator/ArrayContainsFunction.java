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
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;

/** Returns TRUE if the value is found in the array. */
@FunctionPlugin
public class ArrayContainsFunction extends Function {

  public static final Function INSTANCE = new ArrayContainsFunction();

  public ArrayContainsFunction() {
    super(
        "ARRAY_CONTAINS",
        ReturnTypes.ARRAY,
        OperandTypes.ARRAY_ANY,
        OperatorCategory.ARRAY,
        "/docs/array_contains.html");
  }

  @Override
  public boolean checkOperandTypes(Call call) {
    boolean success = super.checkOperandTypes(call);
    if (success) {
      Type arrayType = call.getOperand(0).getType().getElementType();
      Type elementType = call.getOperand(1).getType();

      // Check if the element is coercible to the array element type
      success = elementType.isCoercible(arrayType);
    }

    return success;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Array array = (Array) operands[0];
    IExpression element = operands[1];
    return array.contains(element);
  }
}
