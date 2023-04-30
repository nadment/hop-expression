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
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;
import java.util.Stack;

/**
 * Logical disjunction <code>OR</code> operator.
 */
public class BoolOrOperator extends Operator {

  public BoolOrOperator() {
    super("BOOLOR", "OR", 180, true, ReturnTypes.BOOLEAN, OperandTypes.BOOLEAN_VARIADIC,
        OperatorCategory.LOGICAL, "/docs/boolor.html");
  }

  /**
   * Simplifies OR expressions whose answer can be determined without evaluating both sides.
   */
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    if (call.getOperand(0).is(Kind.LITERAL)) {
      Boolean value = call.getOperand(0).getValue(context, Boolean.class);
      if (value == Boolean.TRUE)
        return Literal.TRUE;
    }

    if (call.getOperand(1).is(Kind.LITERAL)) {
      Boolean value = call.getOperand(1).getValue(context, Boolean.class);
      if (value == Boolean.TRUE)
        return Literal.TRUE;
    }

    // X <> A OR X <> B => X IS NOT NULL or NULL
    
    // Remove duplicate
    // x OR x => x    
    // x OR y OR x => x OR y   
    Stack<IExpression> operands = this.getChainedOperands(call, false);
    if ( operands.size()==1 ) return operands.pop();
    IExpression operand  = operands.pop();
    while (!operands.isEmpty()) {      
      call = new Call(Operators.BOOLOR, operands.pop(), operand ); 
      call.inferenceType(context);  
      operand = call;
    }
    
    return operand;
  }
  
  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands) throws Exception {
    Boolean left = operands[0].getValue(context, Boolean.class);
    Boolean right = operands[1].getValue(context, Boolean.class);
    
    if (left == null) {
      if (!right)
        return null;
      return right;
    }
    if (right == null) {
      if (!left)
        return null;
      return left;
    }
    return Boolean.logicalOr(left, right);
  }
  
  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" OR ");
    operands[1].unparse(writer);
  }
}
