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
 * or implied. See the License for the specific language governing permissions and limitations
 * under the License.
 */
package org.apache.hop.expression;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import org.apache.hop.expression.type.ISingleOperandTypeChecker;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.TypeName;
import org.junit.jupiter.api.Test;

/** Tests a lambda with several parameters, called directly as no function accepts one yet. */
class LambdaTest extends ExpressionTest {

  /** Parses, validates with INTEGER parameters and compiles a lambda. */
  private Lambda compile(IExpressionContext context, String source) {
    Lambda lambda = (Lambda) new ExpressionParser(new ExpressionLexer(source)).parse();
    IntegerType[] types = new IntegerType[lambda.getParameterCount()];
    Arrays.fill(types, IntegerType.INTEGER);
    lambda.validate(context, types);
    return (Lambda) new ExpressionCompiler(context).compile(lambda);
  }

  /** A single-operand checker returns {@code true} if the operand is not valid. */
  @Test
  void operandTypeChecker() throws Exception {
    IExpressionContext context = createExpressionContext();
    Lambda lambda = compile(context, "(x, y) -> x + y");

    ISingleOperandTypeChecker checker = OperandTypes.lambda(2, TypeName.ANY);
    assertFalse(checker.checkSingleOperandType(lambda));
    assertFalse(OperandTypes.lambda(2, TypeName.NUMBER).checkSingleOperandType(lambda));

    // The number of parameters must match
    assertTrue(OperandTypes.lambda(1, TypeName.ANY).checkSingleOperandType(lambda));
    assertTrue(OperandTypes.lambda(3, TypeName.ANY).checkSingleOperandType(lambda));

    // The expression must return a type coercible to the expected type
    Lambda date = compile(context, "(x, y) -> DATE '2020-01-01'");
    assertTrue(OperandTypes.lambda(2, TypeName.BOOLEAN).checkSingleOperandType(date));

    // The operand must be a lambda
    assertTrue(checker.checkSingleOperandType(Literal.TRUE));
  }

  @Test
  void severalParameters() throws Exception {
    IExpressionContext context = createExpressionContext();

    Lambda lambda = compile(context, "(x, y) -> x * 10 + y");
    assertEquals(2, lambda.getParameterCount());
    // The operands are normalized by the compiler
    assertEquals("(x,y)->y+10*x", lambda.toString());
    assertTrue(lambda.isConstant());
    assertEquals(12L, lambda.apply(1L, 2L));
    assertEquals(30L, lambda.apply(Long.class, 3L, 0L));

    // The expression can reference the fields of the row
    lambda = compile(context, "(x, y, z) -> x + y + z + FIELD_INTEGER");
    assertFalse(lambda.isConstant());
    assertEquals(46L, lambda.apply(1L, 2L, 3L));
  }

  @Test
  void wrongNumberOfArguments() throws Exception {
    IExpressionContext context = createExpressionContext();
    Lambda lambda = compile(context, "(x, y) -> x + y");

    ExpressionException exception = assertThrows(ExpressionException.class, () -> lambda.apply(1L));
    assertEquals(ErrorCode.INTERNAL_ERROR, exception.getErrorCode());
    assertThrows(ExpressionException.class, () -> lambda.validate(context, IntegerType.INTEGER));
  }
}
