/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.expression;

import org.apache.hop.core.util.TranslateUtil;
import org.apache.hop.expression.util.ExpressionUtil;
import java.io.StringWriter;
import java.util.Objects;

/**
 * Operators may be binary, unary, functions, special syntactic constructs like CASE ... WHEN ... END, or even internally generated constructs like implicit type conversions.
 * 
 * Operators have the precedence levels. An operator on higher levels is evaluated before an operator on a lower level
 */
public abstract class Operator implements Comparable<Operator> {
   
  /** The unique identifier of the operator/function. Ex. "COS" or "TRIM" */
  private final String id;

  /** The symbol of the operator or name of function. Ex. "id=TRUNCATE" but alias name is "TRUNC" */
  private final String name;

  /**
   * The precedence with which this operator binds to the expression to the left. This is less than
   * the right precedence if the operator is left-associative.
   */
  private final int leftPrecedence;

  /**
   * findDocumentionDescription
   * the left precedence if the operator is left-associative.
   */
  private final int rightPrecedence;

  private final boolean isDeterministic;

  private final String category;

  private final String documentationUrl;
  
  private final String documentation;
  
  private final String description;

  /**
   * Create a new operator for use in expressions.
   *
   * @param id The unique identifier of operator
   * @param name The symbol of the operator or name of function
   * @param precedence The precedence value of the operator
   * @param isLeftAssociative Set to true if the operator is left associative, false if it is right
   *        associative
   * @param isDeterministic Set to true if the operator always returns the same result for the same
   *        parameters
   * @param category The category to group operator
   */
  protected Operator(String id, String name, int precedence, boolean isLeftAssociative,
      boolean isDeterministic, String category, String documentationUrl) {
    this.id = Objects.requireNonNull(id);
    this.name = Objects.requireNonNull(name);
    this.leftPrecedence = leftPrecedence(precedence, isLeftAssociative);
    this.rightPrecedence = rightPrecedence(precedence, isLeftAssociative);
    this.isDeterministic = isDeterministic;
    this.category = TranslateUtil.translate(category, IExpression.class);
    this.documentationUrl = documentationUrl;    
    this.documentation = ExpressionUtil.loadDocumention(id, documentationUrl);
    this.description = ExpressionUtil.findDocumentionDescription(documentation);
  }

  protected Operator(String id, int precedence, boolean isLeftAssociative, boolean isDeterministic,
      String category, String documentationUrl) {
    this(id, id, precedence, isLeftAssociative, isDeterministic, category, documentationUrl);
  }

  private static int leftPrecedence(int precedence, boolean isLeftAssociative) {
    if (isLeftAssociative) {
      ++precedence;
    }
    return precedence;
  }

  private static int rightPrecedence(int precedence, boolean isLeftAssociative) {
    if (!isLeftAssociative) {
      ++precedence;
    }
    return precedence;
  }

  /**
   * The unique identifier of the operator
   */
  public String getId() {
    return id;
  }

  /**
   * The name of the operator
   */
  public String getName() {
    return name;
  }

  public int getLeftPrecedence() {
    return leftPrecedence;
  }

  public int getRightPrecedence() {
    return rightPrecedence;
  }

  /**
   * Whether the operator always returns the same result for the same parameters.
   *
   * @return true if it does
   */
  public boolean isDeterministic() {
    return isDeterministic;
  }
  
  /**
   * Returns whether a call to this operator is not sensitive to the operands order.
   * An operator is symmetrical if the call returns the same result when the operands are permuted.
   */
  public boolean isSymmetrical() {
    return false;
  }
  
  /**
   * Return type inference strategy.
   * @return
   */
  public IReturnTypeInference getReturnTypeInference() {
    return ReturnTypes.UNKNOWN;
  }
  
  public String getDocumentationUrl() {
    return this.documentationUrl;
  }

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof Operator) {
      Operator other = (Operator) obj;
      return id.equals(other.id) && name.equals(other.name);
    }
    return false;
  }

  /**
   * Check if it's the same operator.
   * @param other
   * @return 
   */
  public boolean is(Operator other) {
    if (other == null)
      return false;
    return id.equals(other.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, name);
  }

  /**
   * Get the category of operator
   * 
   * @return
   */
  public String getCategory() {
    return category;
  }

  /**
   * Get the description of operator
   * 
   * @return
   */
  public String getDescription() {
    return description;
  }

  /**
   * Check if the number of arguments is correct.
   *
   * @param len the number of arguments set
   * @throws error if not enough or too many arguments
   */
  protected void checkNumberOfArguments(IExpression[] operands) {    
  }

  public abstract Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException;

  public abstract void unparse(StringWriter writer, IExpression[] operands);

  @Override
  public int compareTo(Operator o) {
    // Compare with id
    int compare = id.compareTo(o.id);
    if (compare != 0)
      return compare;

    // Primary operator first and alias last
    if ( id.equals(this.name)) return 99;
    
    return name.compareTo(o.name);
  }
  
  public String getDocumentation() {
    return this.documentation;
  }
  
  @Override
  public String toString() {
    return id;
  } 
}
