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
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.IReturnTypeInference;

/** Postfix unary operator, as in "x is null". */
public abstract class PostfixUnaryOperator extends Operator {

  protected PostfixUnaryOperator(
      String id,
      int precedence,
      Associativity associativity,
      IReturnTypeInference returnTypeInference,
      IOperandTypeChecker operandTypeChecker,
      String category,
      String documentationUrl) {
    super(
        id,
        precedence,
        associativity,
        returnTypeInference,
        operandTypeChecker,
        category,
        documentationUrl);
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append(' ');
    writer.append(getName());
  }
}
