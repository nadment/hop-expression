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
package org.apache.hop.expression.type;

import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;

/**
 * Operand type-checking strategy which checks operand type must be a literal with the specified
 * class.
 */
public class LiteralOperandTypeChecker implements ISingleOperandTypeChecker {
  private final Class<?> javaClass;
  // private final boolean allowNull;

  public LiteralOperandTypeChecker(Class<?> javaClass) {
    this.javaClass = javaClass;
  }

  @Override
  public boolean checkSingleOperandType(IExpression operand) {
    if (operand.is(Kind.LITERAL)) {
      Literal literal = (Literal) operand;
      if (javaClass.equals(literal.getJavaClass())) {
        return true;
      }
    }
    return false;
  }
}
