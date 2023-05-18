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
import org.apache.hop.expression.util.Pair;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;
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

    // Simplify trivial TRUE
    if (call.getOperand(0).isConstant()) {
      Boolean value = call.getOperand(0).getValue(context, Boolean.class);
      if (value == Boolean.TRUE)
        return Literal.TRUE;
    }
    if (call.getOperand(1).isConstant()) {
      Boolean value = call.getOperand(1).getValue(context, Boolean.class);
      if (value == Boolean.TRUE)
        return Literal.TRUE;
    }
   
    // Remove duplicate
    // x OR x => x    
    // x OR y OR x => x OR y   
    Stack<IExpression> conditions = this.getChainedOperands(call, false);
    
    final Map<Pair<IExpression,IExpression>, Call> equalTerms = new HashMap<>();
    final Map<Pair<IExpression,IExpression>, Call> notEqualTerms = new HashMap<>();
    final Map<Pair<IExpression,IExpression>, Call> lessThanTerms = new HashMap<>();
    final Map<Pair<IExpression,IExpression>, Call> greaterThanTerms = new HashMap<>();

    for (IExpression condition : conditions) {
      if (condition.is(Kind.CALL)) {
        call = condition.asCall();
        if (call.is(Operators.EQUAL)) {
          equalTerms.put(Pair.of(call.getOperand(0),call.getOperand(1)), call);
        }     
        if (call.is(Operators.NOT_EQUAL)) {
          notEqualTerms.put(Pair.of(call.getOperand(0),call.getOperand(1)), call);
        }     
        if (call.is(Operators.LESS_THAN)) {
          lessThanTerms.put(Pair.of(call.getOperand(0),call.getOperand(1)), call);
        }  
        if (call.is(Operators.GREATER_THAN)) {
          greaterThanTerms.put(Pair.of(call.getOperand(0),call.getOperand(1)), call);
        }  
      }
    }
    
    // Merge OR comparison
    for(Pair<IExpression,IExpression> pair: lessThanTerms.keySet()) {

      // "x < a OR x != a" to "x != a"  
      if ( notEqualTerms.containsKey(pair) ) {
        conditions.remove(lessThanTerms.get(pair));
      }
      
      // "x < a OR x = a" to "x <= a"
      if ( equalTerms.containsKey(pair) ) {
        conditions.remove(equalTerms.get(pair));
        conditions.remove(lessThanTerms.get(pair));
        conditions.add(new Call(Operators.LESS_THAN_OR_EQUAL, pair.getLeft(), pair.getRight()));
      }
      
      // "x < a OR x > a" to "x != a"      
      if ( greaterThanTerms.containsKey(pair) ) {
        conditions.remove(lessThanTerms.get(pair));
        conditions.remove(greaterThanTerms.get(pair));
        conditions.add(new Call(Operators.NOT_EQUAL, pair.getLeft(), pair.getRight()));
      }
    }      

    for(Pair<IExpression,IExpression> pair: greaterThanTerms.keySet()) {

      // "x > a OR x != a" to "x != a"  
      if ( notEqualTerms.containsKey(pair) ) {
        conditions.remove(greaterThanTerms.get(pair));
      }
      
      // "x > a OR x = a" to "x >= a"
      if ( equalTerms.containsKey(pair) ) {
        conditions.remove(equalTerms.get(pair));
        conditions.remove(greaterThanTerms.get(pair));
        conditions.add(new Call(Operators.GREATER_THAN_OR_EQUAL, pair.getLeft(), pair.getRight()));
      }
    }      
   
    
    // X <> A OR X <> B => X IS NOT NULL or NULL
    
    // Rebuild disjunctions if more than 1 condition    
    if ( conditions.size()==1 ) return conditions.pop();
    IExpression operand  = conditions.pop();
    while (!conditions.isEmpty()) {      
      call = new Call(Operators.BOOLOR, conditions.pop(), operand ); 
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
