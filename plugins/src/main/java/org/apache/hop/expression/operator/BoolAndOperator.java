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

import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;
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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Stack;

/**
 * Logical conjunction <code>AND</code> operator.
 */
public class BoolAndOperator extends Operator {

  public BoolAndOperator() {
    super("BOOLAND", "AND", 160, true, ReturnTypes.BOOLEAN, OperandTypes.BOOLEAN_BOOLEAN,
        OperatorCategory.LOGICAL, "/docs/booland.html");
  }

  /**
   * Simplifies AND expressions whose answer can be determined without evaluating both sides.
   */
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    boolean left = true;
    boolean right = true;

    if (call.getOperand(0).isConstant()) {
      Boolean value = call.getOperand(0).getValue(context, Boolean.class);
      if (value == Boolean.FALSE)
        left = false;
    }

    if (call.getOperand(1).isConstant()) {
      Boolean value = call.getOperand(1).getValue(context, Boolean.class);
      if (value == Boolean.FALSE)
        right = false;
    }

    // FALSE AND x => FALSE
    // x AND FALSE => FALSE
    if (!left || !right) {
      return Literal.FALSE;
    }

    // Remove duplicate predicate
    // x AND x => x    
    // x AND y AND x => x AND y   
    Stack<IExpression> conditions = this.getChainedOperands(call, false);


    //final Map<IExpression, Range<?>> ranges = new HashMap<>();
    final List<IExpression> nullTerms = new ArrayList<>();
    final List<IExpression> notNullTerms = new ArrayList<>();
    final List<IExpression> strongTerms = new ArrayList<>();
    final MultiValuedMap<IExpression, Pair<Call,Literal>> equalsLiterals = new ArrayListValuedHashMap<>();
    
    for (IExpression condition : conditions) {
      if (condition.is(Kind.CALL)) {
        call = (Call) condition;
        if (call.is(Operators.IS_NULL)) {
          nullTerms.add(call.getOperand(0));
        }
        if (call.is(Operators.IS_NOT_NULL)) {
          notNullTerms.add(call.getOperand(0));
        }
        if (call.is(Operators.EQUAL)) {
          if (call.getOperand(0).is(Kind.LITERAL)) {
            equalsLiterals.put(call.getOperand(1), Pair.of(call, (Literal) call.getOperand(0)));
          }
          if (call.getOperand(1).is(Kind.LITERAL)) {
            equalsLiterals.put(call.getOperand(0), Pair.of(call, (Literal) call.getOperand(1)));
          }
        }
        
//        if (call.is(Operators.LESS_THAN) && call.getOperand(0).isConstant() ) {
//          
//          this.processRange();
//          
//          Object value = call.getOperand(0).getValue(context);
//          if ( value instanceof Comparable ) {
//          Comparable<?> c = (Comparable<?>) value;
//          Range<?> range = ranges.get(call);
//          
//          if ( range==null) {                       
//            ranges.put(call, Range.lessThan(c));
//          }
//        }
     
        if (Operators.is(call, Operators.EQUAL, Operators.NOT_EQUAL, Operators.LESS_THAN, Operators.LESS_THAN_OR_EQUAL, Operators.LESS_THAN_OR_GREATER_THAN, Operators.GREATER_THAN, Operators.GREATER_THAN_OR_EQUAL)) {
          if (call.getOperand(0).is(Kind.IDENTIFIER)) {
            strongTerms.add(call.getOperand(0));
          }
          if (call.getOperand(1).is(Kind.IDENTIFIER)) {
            strongTerms.add(call.getOperand(1));
          }
        }
      }
    }

    // Simplify X=1 AND X=2 - not satisfiable
    for (IExpression reference : equalsLiterals.keySet()) {
        Collection<Pair<Call,Literal>> pairs = equalsLiterals.get(reference);              
        Literal literal = null;
        for (Pair<Call,Literal> pair : pairs ) {
          if ( literal==null ) {
            literal = pair.getRight();
          } 
          else if ( !literal.equals(pair.getRight()) ) {
            return Literal.FALSE;
          }
        }
    }
    
    // Simplify IS NULL(x) AND x < 5  - not satisfiable
    if (!Collections.disjoint(nullTerms, strongTerms)) {
      return Literal.FALSE;
    }      

    // Remove not necessary IS NOT NULL expressions "IS NOT NULL(x) AND x < 5" to "x < 5"
    for (IExpression operand : notNullTerms) {
      if (strongTerms.contains(operand)) {
        for (IExpression condition : conditions ) {
          if( condition.is(Operators.IS_NOT_NULL) && ((Call)condition).getOperand(0)==operand) { 
            conditions.remove(condition);
          }
        }
      }
    }
    
    // Rebuild conjunctions if more than 1 condition    
    IExpression expression  = conditions.pop();
    while (!conditions.isEmpty()) {      
      call = new Call(Operators.BOOLAND, conditions.pop(), expression); 
      call.inferenceType(context);  
      expression = call;
    }
    
    return expression;
  } 
  
//    private void processRange(Operator operator, Literal literal) {
//      
//    }
    
  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands) throws Exception {
    Boolean left = operands[0].getValue(context, Boolean.class);
    if (left == null) {
      return null;
    }
    Boolean right = operands[1].getValue(context, Boolean.class);
    if (right == null) {
      return null;
    }
    return Boolean.logicalAnd(left, right);
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" AND ");
    operands[1].unparse(writer);
  }
}
