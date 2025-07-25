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

import java.io.StringWriter;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.IReturnTypeInference;

/** A <code>Function</code> is a type of operator which has conventional function-call syntax. */
public abstract class Function extends Operator {

  /**
   * Creates a function.
   *
   * <p>Note that some function has specific syntax CAST, COUNT, EXTRACT, POSITION.
   *
   * @param id The unique identifier of the function
   */
  protected Function(
      String id,
      IReturnTypeInference returnTypeInference,
      IOperandTypeChecker operandTypeChecker,
      String category,
      String documentationUrl) {
    super(
        id,
        id,
        10,
        Associativity.LEFT,
        returnTypeInference,
        operandTypeChecker,
        category,
        documentationUrl);
  }

  /**
   * Creates a function.
   *
   * <p>Note that some function has specific syntax CAST, COUNT, EXTRACT, POSITION.
   *
   * @param id The unique identifier of the function
   * @param name The name of function
   */
  protected Function(
      String id,
      String name,
      IReturnTypeInference returnTypeInference,
      IOperandTypeChecker operandTypeChecker,
      String category,
      String documentationUrl) {
    super(
        id,
        name,
        10,
        Associativity.LEFT,
        returnTypeInference,
        operandTypeChecker,
        category,
        documentationUrl);
  }

  protected Function(
      String id,
      String name,
      int precedence,
      Associativity associativity,
      IReturnTypeInference returnTypeInference,
      IOperandTypeChecker operandTypeChecker,
      String category,
      String documentationUrl) {
    super(
        id,
        name,
        precedence,
        associativity,
        returnTypeInference,
        operandTypeChecker,
        category,
        documentationUrl);
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append(this.getName());
    writer.append('(');
    boolean first = true;
    for (IExpression operand : operands) {
      if (!first) writer.append(',');
      else first = false;
      operand.unparse(writer, 0, 0);
    }
    writer.append(')');
  }
}
