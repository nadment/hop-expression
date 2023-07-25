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

import org.apache.hop.expression.Category;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * 
 */
@FunctionPlugin
public class BitSetFunction extends Function {

  public BitSetFunction() {
    super("BIT_SET", ReturnTypes.INTEGER, OperandTypes.NUMERIC_NUMERIC,
        Category.BITWISE, "/docs/bit_set.html");
  }

  @Override
  public Object eval(IExpression[] operands)
      throws Exception {
    Long value = operands[0].getValue(Long.class);
    if (value == null)
      return null;
    Long v1 = operands[1].getValue(Long.class);
    if (v1 == null)
      return null;

    int position = v1.intValue();
    if (position <= 0)
      return null;
    if (position > 64)
      return value;
    return value | (1L << position - 1);
  }

}
