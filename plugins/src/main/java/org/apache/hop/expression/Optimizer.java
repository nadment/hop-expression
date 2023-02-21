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
import org.apache.hop.expression.util.NumberFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Simplifies expressions:
 * 
 * 1. Simplifies expressions whose answer can be determined without evaluating both sides.
 * 2. Merge same expressions.
 * 3. Removes `Not` operator.
 */
public class Optimizer implements IExpressionVisitor<IExpression> {

  protected Optimizer() {
    super();
  }

  @Override
  public IExpression apply(IExpressionContext context, Call call) {
    try {
      Operator operator = call.getOperator();

      // Optimize all operands
      List<IExpression> operands = new ArrayList<>();
      for (IExpression expression : call.getOperands()) {
        operands.add(expression.accept(context, this));
      }
      call = new Call(operator, operands);

      // If operator is deterministic try to evaluate the call
      if (operator.isDeterministic()) {
        try {
          boolean literal = true;

          for (IExpression operand : call.getOperands()) {
            if (operand == null)
              continue;
            //
            if (operand instanceof Tuple) {
              for (IExpression expression : (Tuple) operand) {
                if (!expression.is(Kind.LITERAL)) {
                  literal = false;
                }
              }
            } else if (!operand.is(Kind.LITERAL)) {
              literal = false;
            }
          }
          if (literal) {
            Object value = call.getValue(context);
            return Literal.of(value);
          }
        } catch (Exception e) {
          // Ignore and continue
        }
      }

      // If operator is symmetrical reorganize operands
      if (operator.isSymmetrical()) {
        call = this.reorganizeSymmetrical(context, call);
      }

      IExpression expression = this.simplify(context, call);
      
      // Inference return type
      if ( expression.is(Kind.CALL) ) {
        expression = this.inferenceReturnType(context, (Call) expression);
      }

      return expression;
    } catch (Exception e) {
      return call;
    }
  }

  // Operator optimization
  protected IExpression simplify(IExpressionContext context, Call call) throws ExpressionException {

    if (call.is(Operators.BOOLNOT)) {
      return this.simplifyBoolNot(context, call);
    }
    if (call.is(Operators.BOOLOR)) {
      return this.simplifyBoolOr(context, call);
    }
    if (call.is(Operators.BOOLAND)) {
      return this.simplifyBoolAnd(context, call);
    }
    if (call.is(Operators.CONCAT)) {
      return this.simplifyConcat(context, call);
    }
    if (call.is(Operators.EXTRACT)) {
      return this.simplifyExtract(context, call);
    }
    if (call.is(Operators.NEGATIVE)) {
      return this.simplifyNegative(context, call);
    }
    if (call.is(Operators.ADD)) {
      return this.simplifyAdd(context, call);
    }
    if (call.is(Operators.SUBTRACT)) {
      return this.simplifySubtract(context, call);
    }
    if (call.is(Operators.MULTIPLY)) {
      return this.simplifyMultiply(context, call);
    }
    if (call.is(Operators.DIVIDE)) {
      return this.simplifyDivide(context, call);
    }
    if (call.is(Operators.IN)) {
      return this.simplifyIn(context, call);
    }
    if (call.is(Operators.LIKE)) {
      return this.simplifyLike(context, call);
    }
    // else if (call.is(Operators.EQUAL)) {
    // if (call.getOperand(0).equals(call.getOperand(1)) && !call.getOperand(0).isNullable() ) {
    // return Literal.TRUE;
    // }
    // }
    else if (call.is(FunctionRegistry.getFunction("EQUAL_NULL"))) {
      return this.simplifyEqualNull(context, call);
    }
    
    return call;
  }
  
  /**
   * Simplifies by removing unnecessary `Not` operator
   */
  protected IExpression simplifyBoolNot(IExpressionContext context, Call call) {

    IExpression operand = call.getOperand(0);

    // NOT(l > r) => l <= r
    if (operand.is(Operators.GREATER_THAN)) {
      return new Call(Operators.LESS_THAN_OR_EQUAL, ((Call) operand).getOperands());
    }
    // NOT(l >= r) => l < r
    if (operand.is(Operators.GREATER_THAN_OR_EQUAL)) {
      return new Call(Operators.LESS_THAN, ((Call) operand).getOperands());
    }
    // NOT(l < r) => l >= r
    if (operand.is(Operators.LESS_THAN)) {
      return new Call(Operators.GREATER_THAN_OR_EQUAL, ((Call) operand).getOperands());
    }
    // NOT(l <= r) => l > r
    if (operand.is(Operators.LESS_THAN_OR_EQUAL)) {
      return new Call(Operators.GREATER_THAN, ((Call) operand).getOperands());
    }
    // NOT(NOT(x)) => x
    if (operand.is(Operators.BOOLNOT)) {
      return ((Call) operand).getOperand(0);
    }
    // NOT(x IS TRUE) => x IS FALSE
    if (operand.is(Operators.IS_TRUE)) {
      return new Call(Operators.IS_FALSE, ((Call) operand).getOperands());
    }
    // NOT(x IS FALSE) => x IS TRUE
    if (operand.is(Operators.IS_FALSE)) {
      return new Call(Operators.IS_TRUE, ((Call) operand).getOperands());
    }
    // NOT(x IS NULL) => x IS NOT NULL
    if (operand.is(Operators.IS_NULL)) {
      return new Call(Operators.IS_NOT_NULL, ((Call) operand).getOperands());
    }
    // NOT(x IS NOT NULL) => x IS NULL
    if (operand.is(Operators.IS_NOT_NULL)) {
      return new Call(Operators.IS_NULL, ((Call) operand).getOperands());
    }

    return call;
  }

  /**
   * Simplifies AND expressions whose answer can be determined without evaluating both sides.
   */
  protected IExpression simplifyBoolAnd(IExpressionContext context, Call call)
      throws ExpressionException {
    boolean left = true;
    boolean right = true;

    if (call.getOperand(0).is(Kind.LITERAL)) {
      Boolean value = call.getOperand(0).getValue(context, Boolean.class);
      if (value == null)
        return Literal.NULL;
      if (value == Boolean.FALSE)
        left = false;
    }

    if (call.getOperand(1).is(Kind.LITERAL)) {
      Boolean value = call.getOperand(1).getValue(context, Boolean.class);
      if (value == null)
        return Literal.NULL;
      if (value == Boolean.FALSE)
        right = false;
    }

    // FALSE AND x => FALSE
    // x AND FALSE => FALSE
    if (!left || !right) {
      return Literal.FALSE;
    }

    // x AND x => x
    if (call.getOperand(0).equals(call.getOperand(1))) {
      return call.getOperand(0);
    }

    return call;
  }

  /**
   * Simplifies OR expressions whose answer can be determined without evaluating both sides.
   */
  protected IExpression simplifyBoolOr(IExpressionContext context, Call call)
      throws ExpressionException {

    if (call.getOperand(0).is(Kind.LITERAL)) {
      Boolean value = call.getOperand(0).getValue(context, Boolean.class);
      if (value == null)
        return call.getOperand(1);
      if (value == Boolean.TRUE)
        return Literal.TRUE;
    }

    if (call.getOperand(1).is(Kind.LITERAL)) {
      Boolean value = call.getOperand(1).getValue(context, Boolean.class);
      if (value == null)
        return call.getOperand(0);
      if (value == Boolean.TRUE)
        return Literal.TRUE;
    }

    // x OR x => x
    if (call.getOperand(0).equals(call.getOperand(1))) {
      return call.getOperand(0);
    }

    return call;
  }

  // -(-(x)) => x
  protected IExpression simplifyNegative(IExpressionContext context, Call call) {
    IExpression operand = call.getOperand(0);
    if (operand.is(Operators.NEGATIVE)) {
      return ((Call) operand).getOperand(0);
    }

    return call;
  }

  protected IExpression simplifyAdd(IExpressionContext context, Call call)
      throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // x+0 => x
    if (Literal.ZERO.equals(left)) {
      return right;
    }

    // Pull up literal
    if (left.is(Kind.LITERAL) && right.is(Operators.ADD)) {
      Call child = (Call) right;
      if (child.getOperand(0).is(Kind.LITERAL)) {
        IExpression expression = new Call(Operators.ADD, left, child.getOperand(0));
        Literal literal = Literal.of(expression.getValue(context));
        return new Call(Operators.ADD, literal, child.getOperand(1));
      }
    }

    return call;
  }

  /**
   * Simplify arithmetic subtract
   */
  protected IExpression simplifySubtract(IExpressionContext context, Call call)
      throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // x-0 => x
    if (Literal.ZERO.equals(right)) {
      return left;
    }
    // 0-x => -x
    if (Literal.ZERO.equals(left)) {
      return new Call(Operators.NEGATIVE, right);
    }

    // x-(-z) => x+z
    if (right.is(Operators.NEGATIVE)) {
      Call negative = (Call) right;
      return new Call(Operators.ADD, left, negative.getOperand(0));
    }
    return call;
  }

  /**
   * Simplify arithmetic multiply
   */
  protected IExpression simplifyMultiply(IExpressionContext context, Call call)
      throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // x*1 => x
    if (Literal.ONE.equals(left)) {
      return right;
    }

    // Pull up literal
    if (left.is(Kind.LITERAL) && right.is(Operators.MULTIPLY)) {
      Call child = (Call) right;
      if (child.getOperand(0).is(Kind.LITERAL)) {
        IExpression operation = new Call(Operators.MULTIPLY, left, child.getOperand(0));
        Literal literal = Literal.of(operation.getValue(context));
        return new Call(Operators.MULTIPLY, literal, child.getOperand(1));
      }
    }
    return call;
  }

  /**
   * Simplify arithmetic divide
   */
  protected IExpression simplifyDivide(IExpressionContext context, Call call)
      throws ExpressionException {
    IExpression right = call.getOperand(1);

    // x/1 => x
    if (Literal.ONE.equals(right)) {
      return call.getOperand(0);
    }
    return call;
  }

  protected IExpression simplifyConcat(IExpressionContext context, Call call) {

    if (call.is(Operators.CONCAT)) {
      ArrayList<IExpression> operands = new ArrayList<>();

      for (IExpression expression : call.getOperands()) {
        if (expression.is(Operators.CONCAT)) {
          Call childCall = (Call) expression;
          for (IExpression child : childCall.getOperands()) {
            if (!child.isNull()) {
              operands.add(child);
            }
          }
        } else if (!expression.isNull()) {
          operands.add(expression);
        }
      }

      // If only 1 expression, nothing to concat
      if (operands.size() == 1) {
        return operands.get(0);
      }

      return new Call(Operators.CONCAT, operands);
    }

    return call;
  }

  /**
   * Simplifies IN expressions list of elements.
   * 1. Remove duplicate expressions in list.
   * 2. Sort expressions on cost.
   */
  protected IExpression simplifyIn(IExpressionContext context, Call call)
      throws ExpressionException {
    List<IExpression> list = new ArrayList<>();

    IExpression value = call.getOperand(0);
    Tuple tuple = (Tuple) call.getOperand(1);
    
    // Remove null and duplicate element in list
    for (IExpression expression : tuple) {

      if (expression.isNull()) {
        continue;
      }

      // B in (A,B,C) return B=B;
      if ( value.equals(expression) ) {
        return new Call(Operators.EQUAL, value, expression);
      }
      
      // If this element is not present in new list then add it
      if (!list.contains(expression)) {
        list.add(expression);
      }
    }

    // Sort list on cost
    list.sort(Comparator.comparing(IExpression::getCost));

    return new Call(call.getOperator(), call.getOperand(0), new Tuple(list));
  }


  static final Pattern startsWith = Pattern.compile("^([^_%]+)%$");
  static final Pattern endsWith = Pattern.compile("^%([^_%]+)$");
  static final Pattern contains = Pattern.compile("^%([^_%]+)%$");
  static final Pattern equalTo = Pattern.compile("^[^_%]*$");

  /**
   * Simplifies LIKE expressions that do not need full regular expressions to evaluate the
   * condition. For example, when the expression is just checking to see if a string starts with a
   * given pattern.
   */
  protected IExpression simplifyLike(IExpressionContext context, Call call)
      throws ExpressionException {
    // Optimize NULL LIKE FIELD to NULL
    IExpression value = call.getOperand(0);
    if (value == Literal.NULL)
      return Literal.NULL;

    IExpression v1 = call.getOperand(1);
    if (v1.is(Kind.LITERAL)) {
      String pattern = v1.getValue(context, String.class);

      // Optimize FIELD LIKE NULL to NULL
      if (pattern == null)
        return Literal.NULL;

      if (call.getOperandCount() == 3) {
        String escape = call.getOperand(2).getValue(context, String.class);
        if (escape == null)
          return Literal.NULL;

        // For now don't optimize if special escape char
        return call;
      }

      // Optimize "x LIKE '%'" to "x = x"
      if ("%".equals(pattern)) {
        return new Call(Operators.EQUAL, value, value);
      }

      // Optimize the common case of FIELD LIKE '%foo%' to CONTAINS(FIELD,'foo')
      // Try contains before starts and ends
      if (contains.matcher(pattern).find()) {
        String search = pattern.replace("%", "");
        return new Call(FunctionRegistry.getFunction("CONTAINS"), value, Literal.of(search));
      }

      // Optimize the common case of FIELD LIKE 'foo%' to STARTSWITH(FIELD,'foo')
      if (startsWith.matcher(pattern).find()) {
        String search = pattern.replace("%", "");
        return new Call(FunctionRegistry.getFunction("STARTSWITH"), value, Literal.of(search));
      }

      // Optimize the common case of FIELD LIKE '%foo' to ENDSWITH(FIELD,'foo')
      if (endsWith.matcher(pattern).find()) {
        String search = pattern.replace("%", "");
        return new Call(FunctionRegistry.getFunction("ENDSWITH"), value, Literal.of(search));
      }

      // Optimize FIELD LIKE 'Hello' to FIELD='Hello'
      if (equalTo.matcher(pattern).find()) {
        String search = pattern.replace("%", "");
        return new Call(Operators.EQUAL, value, Literal.of(search));
      }
    }

    return call;
  }


  /**
   * Replace EXTRACT with the corresponding function only if without time zone
   */
  protected IExpression simplifyExtract(IExpressionContext context, Call call)
      throws ExpressionException {

    TimeUnit unit = call.getOperand(0).getValue(context, TimeUnit.class);
    Function function = FunctionRegistry.getFunction(unit.name());
    if (function != null && !(function instanceof UserDefinedFunction)) {
      return new Call(function, call.getOperand(1));
    }

    return call;
  }

  /**
   * Merge same expressions.
   */
  protected IExpression simplifyEqualNull(IExpressionContext context, Call call)
      throws ExpressionException {
    if (call.getOperand(0).equals(call.getOperand(1))) {
      return Literal.TRUE;
    }

    return call;
  }
  
  /**
   * TODO: Replace string pattern format operand with a compiled format
   */
  protected IExpression simplifyToNumber(IExpressionContext context, Call call) {
    NumberFormat format = NumberFormat.of("TM");

    try {
      if (call.getOperandCount() == 2) {
        String pattern = (String) call.getOperand(1).getValue(context);
        format = NumberFormat.of(pattern);
      }
    } catch (ExpressionException e) {

    }

    return new Call(call.getOperator(), call.getOperand(0), Literal.of(format));
  }

  /**
   * Reorganize symmetrical operator
   * 
   * <ul>
   * <li>Move low cost operand to the left</li>
   * <li>Go up an operand if low cost</li>
   * <li>Order identifier by name (only useful for test)</li>
   * </ul>
   */
  protected Call reorganizeSymmetrical(IExpressionContext context, Call call) {
    Operator operator = call.getOperator();
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Move low cost operand to the left
    if (left.getCost() > right.getCost()) {
      return new Call(operator, right, left);
    }

    // Order identifier by name
    if (left.is(Kind.IDENTIFIER) && right.is(Kind.IDENTIFIER)) {
      if (((Identifier) left).getName().compareTo(((Identifier) right).getName()) > 0) {
        return new Call(operator, right, left);
      }
    }

    if (right.is(operator)) {
      Call subCall = (Call) right;
      IExpression subLeft = subCall.getOperand(0);
      IExpression subRight = subCall.getOperand(1);

      // Go up left operand if low cost
      if (subLeft.getCost() < left.getCost()) {
        return new Call(operator, subLeft, new Call(operator, left, subRight));
      }

      // Order identifier by name
      if (left.is(Kind.IDENTIFIER) && subLeft.is(Kind.IDENTIFIER)) {
        if (((Identifier) left).getName().compareTo(((Identifier) subLeft).getName()) > 0) {
          return new Call(operator, subLeft, new Call(operator, left, subRight));
        }
      }
    }

    return call;
  }

  protected Call inferenceReturnType(IExpressionContext context, Call call) {
    Operator operator = call.getOperator();
    DataTypeName type = operator.getReturnTypeInference().getReturnType(context, call);
    return new Call(type, operator, call.getOperands());
  }

  @Override
  public IExpression apply(IExpressionContext context, Identifier identifier) {
    return identifier;
  }

  @Override
  public IExpression apply(IExpressionContext context, Tuple tuple) {
    List<IExpression> elements = new ArrayList<>(tuple.size());
    for (IExpression expression : tuple) {
      elements.add(expression.accept(context, this));
    }
    return new Tuple(elements);
  }

  @Override
  public IExpression apply(IExpressionContext context, Literal literal) {
    return literal;
  }
}
