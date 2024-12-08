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
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;

/**
 * Converts a value of one data type into another data type <code>
 * CAST(value AS type [FORMAT format])</code>.
 *
 * @see CastOperator
 * @see TryCastFunction
 */
@FunctionPlugin
public class CastFunction extends Function {

  public static final Function INSTANCE = new CastFunction();

  public CastFunction() {
    this("CAST");
  }

  public CastFunction(String id) {
    super(
        id,
        ReturnTypes.CAST_OPERATOR,
        OperandTypes.CAST,
        OperatorCategory.CONVERSION,
        "/docs/cast.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Object value = operands[0].getValue();
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

    // Cast null value
    if (call.getOperand(0).isNull()) {
      return new Literal(null, call.getType());
    }

    // When cast without format
    if (call.getOperandCount() == 2) {

      // Cast constant value
      if (call.getOperand(0).isConstant()) {
        return new Literal(call.getValue(), call.getType());
      }

      // Remove unnecessary chained cast CAST(CAST(x as type(10)) as type(5)) → CAST(x as type(5))
      if (call.getOperand(0).isOperator(Operators.CAST)) {
        Type toType = call.getType();
        Type fromType = call.getOperand(0).getType();
        if (Types.isLosslessCast(toType, fromType)) {
          return new Call(
              call.getOperator(), call(call.getOperand(0)).getOperand(0), Literal.of(toType));
        }
      }

      // Remove loss-less cast
      else if (Types.isLosslessCast(call.getOperand(0).getType(), call.getType())) {
        return call.getOperand(0);
      }

      // Use default format
      //      switch (call.getType().getId()) {
      //        case BINARY:
      //          String defaultBinaryFormat =
      //              context.getVariable(ExpressionContext.EXPRESSION_BINARY_FORMAT);
      //          return new Call(
      //              call.getOperator(),
      //              call.getOperand(0),
      //              call.getOperand(1),
      //              Literal.of(defaultBinaryFormat));
      //
      //        case DATE:
      //          String defaultDateFormat =
      // context.getVariable(ExpressionContext.EXPRESSION_DATE_FORMAT);
      //          return new Call(
      //              call.getOperator(),
      //              call.getOperand(0),
      //              call.getOperand(1),
      //              Literal.of(defaultDateFormat));
      //      }
    }

    // TODO: the CAST function and the :: operator should call the appropriate conversion function.

    return call;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append(this.getName());
    writer.append('(');
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append(" AS ");
    writer.append(operands[1].toString());
    if (operands.length == 3) {
      writer.append(" FORMAT ");
      operands[2].unparse(writer, 0, 0);
    }
    writer.append(')');
  }
}
