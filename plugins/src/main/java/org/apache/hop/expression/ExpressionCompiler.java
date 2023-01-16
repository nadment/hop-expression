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
package org.apache.hop.expression;

import org.apache.hop.expression.type.DataTypeName;
import org.apache.hop.expression.type.IOperandCountRange;
import org.apache.hop.expression.type.IOperandTypeChecker;
import java.util.ArrayList;
import java.util.List;

/* package */ class ExpressionCompiler implements IExpressionVisitor<IExpression> {

  @Override
  public IExpression apply(IExpressionContext context, Tuple tuple) {
    List<IExpression> elements = new ArrayList<>(tuple.size());
    for (IExpression expression : tuple) {
      elements.add(expression.accept(context, this));
    }
    return new Tuple(elements);
  }

  @Override
  public IExpression apply(final IExpressionContext context, final Call call) {


    // Check the number of operands expected
    Operator operator = call.getOperator();
    IOperandCountRange operandCountRange = operator.getOperandCountRange();
    if (!operandCountRange.isValid(call.getOperandCount())) {
      if (call.getOperandCount() < operandCountRange.getMin()) {
        // throw new
        // ExpressionException(ExpressionError.NOT_ENOUGH_ARGUMENT.message(operator.getId()));
      }
      if (call.getOperandCount() > operandCountRange.getMax()) {
        // throw new
        // ExpressionException(ExpressionError.TOO_MANY_ARGUMENT.message(operator.getId()));
      }
    }

    // Check operand types expected
    IOperandTypeChecker operandTypeChecker = operator.getOperandTypeChecker();
    if (!operandTypeChecker.checkOperandTypes(call)) {
      // throw new
      // ExpressionException(ExpressionError.ILLEGAL_ARGUMENT.message(operator.getName()));
    }

    // Replace arguments in User Defined Function by the operands of the call.
    if (call.getOperator() instanceof UserDefinedFunction) {
      UserDefinedFunction udf = (UserDefinedFunction) call.getOperator();

      try {
        IExpressionContext udfContext = new ExpressionContext(context, udf.createRowMeta());
        IExpression expression = ExpressionBuilder.compile(udfContext, udf.getSource());
        return expression.accept(context, new UserDefinedFunctionResolver(call.getOperands()));
      } catch (Exception e) {
        // throw new ExpressionException(ExpressionError.UDF_COMPILATION_ERROR, udf.getName());
      }
    }

    // Compile all operands
    List<IExpression> operands = new ArrayList<>();
    for (IExpression expression : call.getOperands()) {
      operands.add(expression.accept(context, this));
    }

    // Inference return type
    DataTypeName type = call.getOperator().getReturnTypeInference().getReturnType(context, call);

    return new Call(type, operator, operands);
  }

  @Override
  public IExpression apply(IExpressionContext context, Literal literal) {
    return literal;
  }

  @Override
  public IExpression apply(IExpressionContext context, Identifier identifier) {
    return identifier;
  }
}
