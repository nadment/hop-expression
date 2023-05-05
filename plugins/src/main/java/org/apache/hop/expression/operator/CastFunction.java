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
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.Converter;
import org.apache.hop.expression.type.DataType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;

/**
 * Converts a value of one data type into another data type <code>CAST(value AS type [FORMAT format])</code>.
 * 
 * @see CastOperator
 */
@FunctionPlugin
public class CastFunction extends Function {

  public CastFunction() {
    this("CAST");
  }
  
  protected CastFunction(final String id) {
    super(id, ReturnTypes.CAST_OPERATOR, OperandTypes.CAST_OPERATOR,
        OperatorCategory.CONVERSION, "/docs/cast.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands) throws Exception {
    Object value = operands[0].getValue(context);
    if (value == null)
      return null;

    DataType type = operands[1].getValue(context, DataType.class);

    String format = null;
    if (operands.length == 3) {
      format = operands[2].getValue(context, String.class);
    }

    return cast(value, type, format);
  }
  
  protected Object cast(Object value, DataType type, String format) {
    return Converter.cast(value, type, format);
  }
  
  protected boolean isTry() {
    return false;  
  }
  
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    call.inferenceType(context);
    DataType type = call.getType();
    
    // Remove lossless cast
    IExpression operand = call.getOperand(0);
    if (call.getType().equals(operand.getType())) {
      return operand;
    }
    
    // Translate to function
    switch(type.getFamily()) {
      case BOOLEAN:
        return new Call(FunctionRegistry.getFunction(isTry() ? "TRY_TO_BOOLEAN":"TO_BOOLEAN"), call.getOperand(0));
      case NUMERIC:
        //return new Call(FunctionRegistry.getFunction(isTry() ? "TRY_TO_NUMBER":"TO_NUMBER"), call.getOperand(0));
        break;
      case TEMPORAL:
        //return new Call(FunctionRegistry.getFunction(isTry() ? "TRY_TO_DATE":"TO_DATE"), call.getOperand(0));
        break;
      case STRING:
        //return new Call(FunctionRegistry.getFunction(isTry() ? "TRY_TO_CHAR":"TO_CHAR"), call.getOperand(0));
        break;
      case BINARY:
        //return new Call(FunctionRegistry.getFunction(isTry() ? "TRY_TO_BINARY":"TO_BINARY"), call.getOperand(0));
        break;
      case JSON:
        //return new Call(FunctionRegistry.getFunction(isTry() ? "TRY_TO_JSON":"TO_JSON"), call.getOperand(0));
      default:
        break;
    } 
    
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
