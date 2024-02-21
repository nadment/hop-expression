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

import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Tuple;
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;

/**
 * Returns an array element at the given an index
 * <br>
 * <strong>Syntax:</strong> <code>array[3]</code>
 */
public class ElementAtOperator extends Operator {

  public ElementAtOperator() {
    super("ELEMENT_AT", "[]", 200, true, ReturnTypes.ARRAY_ELEMENT, OperandTypes.ARRAY_NUMERIC,
        OperatorCategory.ARRAY, "/docs/element_at.html");
  }
  
  @Override
  public Object eval(final IExpression[] operands) {
    Tuple tuple = operands[0].asTuple();
    Long index = operands[1].getValue(Long.class);
    if (index == null)
      return null;
    
    int i = index.intValue();
    
    if ( i<0 ) i = tuple.size()+i+1;
    
    
    if ( i<1 || i>tuple.size() || i > ArrayType.MAX_ARRAY_CARDINALITY) {
      throw new ExpressionException(ErrorCode.INVALID_ARRAY_INDEX, index);
    }
    
    return tuple.get(i-1).getValue();
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append('[');
    operands[1].unparse(writer);
    writer.append(']');
  }
}
