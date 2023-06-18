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
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;


/**
 * String concatenation operator '<code>||</code>'
 */
@FunctionPlugin
public class ConcatFunction extends Function {

  // Function
  public ConcatFunction() {
    super("CONCAT", ReturnTypes.FIRST_KNOWN,
        OperandTypes.or(OperandTypes.STRING_VARIADIC, OperandTypes.BINARY_VARIADIC),
        OperatorCategory.STRING, "/docs/concat.html");
  }

  // Operator
  public ConcatFunction(String name) {
    super("CONCAT", name, 110, true, ReturnTypes.FIRST_KNOWN,
        OperandTypes.or(OperandTypes.STRING_VARIADIC, OperandTypes.BINARY_VARIADIC),
        OperatorCategory.STRING, "/docs/concat.html");
  }
  
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
   
    // Combine chained CONCAT operator and remove NULL
    ArrayList<IExpression> operands = new ArrayList<>();    
    for (IExpression operand : getChainedOperands(call, true)) {
      if (operand.isNull()) continue;
      operands.add(operand);
    }
    
    switch (operands.size()) {
      case 0: // Nothing to concat
        return Literal.NULL;
      case 1: // Concat(X) => X
        return operands.get(0);
      default:
        return new Call(call.getOperator(), operands);
    }
  }

  
  @Override
  public Object eval(IExpression[] operands) throws Exception {

    Object firstNotNull = null;
    Object[] values = new Object[operands.length];
    int i = 0;
    for (IExpression operand : operands) {
      Object value = operand.getValue();
      if (firstNotNull == null && value != null)
        firstNotNull = value;
      values[i++] = value;
    }

    if (firstNotNull == null)
      return null;

    // Concat Binary
    if (firstNotNull instanceof byte[]) {
      try (ByteArrayOutputStream output = new ByteArrayOutputStream()) {
        for (Object value : values) {
          if (value != null) {
            output.write((byte[]) value);
          }
        }
        return output.toByteArray();
      } catch (IOException e) {
        throw new ExpressionException(ExpressionError.INTERNAL_ERROR, e.getMessage());
      }
    }

    // Concat String
    StringBuilder builder = new StringBuilder();
    for (IExpression operand : operands) {
      String value = operand.getValue(String.class);
      if (value != null)
        builder.append(value);
    }

    return builder.toString();
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    boolean concatFirst = true;
    for (IExpression operand : operands) {
      if (concatFirst)
        concatFirst = false;
      else
        writer.append("||");
      operand.unparse(writer);
    }
  }
}
