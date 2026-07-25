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
package org.apache.hop.expression.jit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionTest;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IRowExpressionContext;
import org.junit.jupiter.api.Test;

/**
 * Differential tests comparing the interpreted evaluation of an expression against the same
 * expression JIT-compiled by {@link JitCompiler}: the two must always agree, for every expression
 * covered by the prototype's code generation (arithmetic, comparison, boolean logic) as well as
 * for the interpreted fallback used for everything else.
 */
class JitCompilerTest extends ExpressionTest {

  /** Compiles {@code source} the normal (interpreted) way and asserts the JIT agrees on value. */
  private void assertSameResult(String source) throws Exception {
    IRowExpressionContext context = createExpressionContext();
    IExpression interpreted = IExpression.of(context, source);
    IExpression compiled = JitCompiler.tryCompile(interpreted);

    assertEquals(
        interpreted.getValue(),
        compiled.getValue(),
        () -> "JIT result differs from interpreted result for: " + source);
  }

  private void assertSameError(String source, ErrorCode errorCode) throws Exception {
    IRowExpressionContext context = createExpressionContext();
    IExpression interpreted = IExpression.of(context, source);
    IExpression compiled = JitCompiler.tryCompile(interpreted);

    ExpressionException interpretedError =
        assertThrows(ExpressionException.class, interpreted::getValue);
    ExpressionException compiledError = assertThrows(ExpressionException.class, compiled::getValue);

    assertEquals(errorCode, interpretedError.getErrorCode());
    assertEquals(errorCode, compiledError.getErrorCode());
  }

  @Test
  void integerArithmetic() throws Exception {
    assertSameResult("FIELD_INTEGER + 2");
    assertSameResult("FIELD_INTEGER - 2");
    assertSameResult("FIELD_INTEGER * 2");
    assertSameResult("FIELD_INTEGER + FIELD_INTEGER_ZERO");
    assertSameResult("NULL_INTEGER + FIELD_INTEGER");
    assertSameResult("FIELD_INTEGER + NULL_INTEGER");
  }

  @Test
  void numberArithmetic() throws Exception {
    assertSameResult("FIELD_BIGNUMBER + 1.5");
    assertSameResult("FIELD_BIGNUMBER - 1.5");
    assertSameResult("FIELD_BIGNUMBER * 2");
    assertSameResult("FIELD_BIGNUMBER / 2");
    assertSameResult("NULL_NUMBER + FIELD_BIGNUMBER");
    assertSameResult("FIELD_BIGNUMBER / FIELD_BIGNUMBER");
  }

  @Test
  void divisionByZero() throws Exception {
    assertSameError("FIELD_INTEGER / FIELD_INTEGER_ZERO", ErrorCode.DIVISION_BY_ZERO);
  }

  @Test
  void integerOverflow() throws Exception {
    assertSameError("9223372036854775807 + 1", ErrorCode.ARITHMETIC_OVERFLOW);
  }

  @Test
  void comparison() throws Exception {
    assertSameResult("FIELD_INTEGER > 10");
    assertSameResult("FIELD_INTEGER >= 40");
    assertSameResult("FIELD_INTEGER < 10");
    assertSameResult("FIELD_INTEGER <= 40");
    assertSameResult("FIELD_INTEGER = 40");
    assertSameResult("FIELD_INTEGER <> 40");
    assertSameResult("NULL_INTEGER = FIELD_INTEGER");
    assertSameResult("FIELD_STRING = 'TEST'");
  }

  @Test
  void booleanLogic() throws Exception {
    assertSameResult("FIELD_BOOLEAN_TRUE AND FIELD_BOOLEAN_FALSE");
    assertSameResult("FIELD_BOOLEAN_TRUE OR FIELD_BOOLEAN_FALSE");
    assertSameResult("NOT FIELD_BOOLEAN_TRUE");
    assertSameResult("FIELD_BOOLEAN_TRUE OR NULL_BOOLEAN");
    assertSameResult("FIELD_BOOLEAN_FALSE OR NULL_BOOLEAN");
    assertSameResult("NULL_BOOLEAN AND FIELD_BOOLEAN_FALSE");
    assertSameResult("NULL_BOOLEAN AND FIELD_BOOLEAN_TRUE");
  }

  @Test
  void compoundExpression() throws Exception {
    assertSameResult("(FIELD_INTEGER + 2) * 3 > 100");
    assertSameResult("FIELD_INTEGER + FIELD_INTEGER_ZERO = FIELD_INTEGER AND FIELD_BOOLEAN_TRUE");
  }

  @Test
  void unsupportedOperatorFallsBackToInterpreter() throws Exception {
    // UPPER() has no JIT code generation support: the whole call must be captured as a leaf
    // and evaluated through the interpreter, still wrapped by a compiled class since the outer
    // comparison operator is supported.
    assertSameResult("UPPER(FIELD_STRING) = 'TEST'");
  }

  @Test
  void compilesToRealGeneratedClass() throws Exception {
    IRowExpressionContext context = createExpressionContext();
    IExpression interpreted = IExpression.of(context, "FIELD_INTEGER + 2 > 10");
    IExpression compiled = JitCompiler.tryCompile(interpreted);

    assertInstanceOf(JitCompiledExpression.class, compiled);
    assertEquals(interpreted.getValue(), compiled.getValue());
  }

  @Test
  void literalIsNotCompiled() throws Exception {
    IRowExpressionContext context = createExpressionContext();
    IExpression interpreted = IExpression.of(context, "42");
    IExpression compiled = JitCompiler.tryCompile(interpreted);

    // A bare literal has nothing to gain from compilation: tryCompile must return it unchanged.
    assertEquals(interpreted, compiled);
  }
}
