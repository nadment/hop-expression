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
import org.apache.hop.expression.Category;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import java.io.StringWriter;

/**
 * Converts a value of one data type into another data type
 * <code>CAST(value AS type [FORMAT format])</code>.
 * 
 * @see CastOperator
 * @see TryCastFunction
 */
@FunctionPlugin
public class CastFunction extends Function {

  public CastFunction() {
    this("CAST");
  }

  public CastFunction(String id) {
    super(id, ReturnTypes.CAST_OPERATOR, OperandTypes.CAST_OPERATOR, Category.CONVERSION,
        "/docs/cast.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Object value = operands[0].getValue();
    if (value == null)
      return null;

    Type type = operands[1].getValue(Type.class);

    String format = null;
    if (operands.length == 3) {
      format = operands[2].getValue(String.class);
    }

    return type.cast(value, format);
  }

  @Override
  public IExpression compile(final IExpressionContext context, Call call)
      throws ExpressionException {

    // Type type = call.inferenceType().getType();

    // Remove lossless cast
    IExpression operand = call.getOperand(0);
    if (call.getType().equals(operand.getType())) {
      return operand;
    }

    // // Translate to function
    // switch(type.getFamily()) {
    // case BOOLEAN:
    // return new Call(FunctionRegistry.getFunction("TO_BOOLEAN"), call.getOperand(0));
    // case NUMERIC:
    // //return new Call(FunctionRegistry.getFunction("TO_NUMBER"), call.getOperand(0));
    // break;
    // case TEMPORAL:
    // //return new Call(FunctionRegistry.getFunction("TO_DATE"), call.getOperand(0));
    // break;
    // case STRING:
    // //return new Call(FunctionRegistry.getFunction("TO_CHAR"), call.getOperand(0));
    // break;
    // case BINARY:
    // //return new Call(FunctionRegistry.getFunction("TO_BINARY"), call.getOperand(0));
    // break;
    // case JSON:
    // //return new Call(FunctionRegistry.getFunction("TO_JSON"), call.getOperand(0));
    // default:
    // break;
    // }

    return call;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append(this.getName());
    writer.append('(');
    operands[0].unparse(writer);
    writer.append(" AS ");
    writer.append(operands[1].toString());
    if (operands.length == 3) {
      writer.append(" FORMAT ");
      operands[2].unparse(writer);
    }
    writer.append(')');
  }
}
