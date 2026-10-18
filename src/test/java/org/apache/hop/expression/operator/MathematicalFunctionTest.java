/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression.operator;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

import ch.obermuhlner.math.big.BigDecimalMath;
import java.math.BigDecimal;
import java.util.Random;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.Evaluator;
import org.apache.hop.expression.ExpressionTest;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.IntervalType;
import org.apache.hop.expression.type.NumberType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.parallel.Execution;

@TestInstance(PER_CLASS)
@Execution(CONCURRENT)
class MathematicalFunctionTest extends ExpressionTest {
  @Test
  void Abs() throws Exception {
    // A boolean returns an integer
    evalEquals("ABS(TRUE)", 1L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("ABS(FIELD_BOOLEAN_TRUE)", 1L).returnType(IntegerType.INTEGER);

    // The absolute value of the smallest integer is out of the integer range
    evalFails("ABS(-9223372036854775807-1)", ErrorCode.ARITHMETIC_OVERFLOW);
    evalNull("TRY(ABS(-9223372036854775807-1))");
    evalEquals("ABS(-9223372036854775807)", 9223372036854775807L);
    evalEquals(
        "ABS(CAST(-9223372036854775807-1 AS NUMBER))", new BigDecimal("9223372036854775808"));
    // Documentation example
    evalEquals("ABS(-123)", 123L);
    evalEquals("Abs(0)", 0L).returnType(IntegerType.of(1, false));
    evalEquals("Abs(1)", 1L).returnType(IntegerType.of(1, false));
    evalEquals("Abs(-1)", 1L).returnType(IntegerType.of(1, false));
    evalEquals("Abs(FIELD_INTEGER)", 40L).returnType(IntegerType.of(12));
    evalEquals("Abs(FIELD_NUMBER)", 5.12D).returnType(NumberType.NUMBER);
    // A number keeps its precision and scale
    evalEquals("Abs(-1.5)", 1.5D).returnType(NumberType.of(2, 1, false));
    evalEquals("Abs(CAST(FIELD_NUMBER AS NUMBER(5,2)))", 5.12D).returnType(NumberType.of(5, 2));
    evalEquals("Abs(-1::INTEGER)", 1L);
    evalEquals("Abs(-1.12345679)", 1.12345679D);
    evalEquals(
        "Abs(-1.1234567912345679123456791234567912345)",
        new BigDecimal("1.1234567912345679123456791234567912345"));

    // Null handling
    evalNull("Abs(NULL_INTEGER)");
    evalNull("Abs(NULL_NUMBER)");
    evalNull("Abs(NULL_BIGNUMBER)");

    // Interval
    evalEquals("Abs(INTERVAL 5 YEARS)", Interval.of(5)).returnType(IntervalType.INTERVAL_NOT_NULL);
    evalEquals("Abs(INTERVAL -5 YEARS)", Interval.of(5)).returnType(IntervalType.INTERVAL_NOT_NULL);
    evalNull("Abs(NULL_STRINg::INTERVAL)").returnType(IntervalType.INTERVAL);
    // The absolute value of an interval with the smallest value is out of range
    evalFails("Abs(MAKE_INTERVAL(0,0,-9223372036854775807-1,0,0,0))", ErrorCode.INTERVAL_OVERFLOW);

    // Check operands
    evalFails("Abs()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Abs(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("Abs(FIELD_STRING)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Abs(FIELD_BINARY)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Abs(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Abs(FIELD_TIMESTAMP)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Abs(FIELD_INET)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Abs(FIELD_JSON)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Abs(", ErrorCode.MISSING_RIGHT_PARENTHESIS);

    optimize("ABS(-FIELD_INTEGER)");

    // Function repetition
    optimize("ABS(ABS(FIELD_INTEGER))", "ABS(FIELD_INTEGER)");
  }

  @Test
  void Pi() throws Exception {
    evalEquals("Pi()", PI).returnType(NumberType.NUMBER_NOT_NULL);

    // Check operands
    evalFails("Pi(123)", ErrorCode.TOO_MANY_ARGUMENTS);

    optimize("PI()", "3.1415926535897932384626433832795");
  }

  @Test
  void Div0() throws Exception {
    // Documentation example
    evalEquals("DIV0(6,0)", 0D).returnType(NumberType.of(7, 6, false));
    evalEquals("Div0(10,4)", 2.5D).returnType(NumberType.of(8, 6, false));
    evalEquals("Div0(FIELD_INTEGER,-100)", -0.4D);
    evalEquals("Div0(FIELD_INTEGER,0)", 0D);
    evalEquals("Div0(FIELD_INTEGER,2)", 20D).returnType(NumberType.of(18, 6));

    // Null handling
    evalNull("Div0(NULL_INTEGER,1)");
    evalNull("Div0(NULL_INTEGER,0)");
    evalNull("Div0(1,NULL_INTEGER)");

    evalFails("Div0()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Div0(40)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Div0(40,1,2)", ErrorCode.TOO_MANY_ARGUMENTS);

    // Arithmetic overflow when the integer part of a NUMBER exceeds 38 digits
    evalFails("Div0(1e37::NUMBER(38,0), 0.01)", ErrorCode.ARITHMETIC_OVERFLOW);
    evalFails("Div0(-1e30::NUMBER(38,0), 1e-10)", ErrorCode.ARITHMETIC_OVERFLOW);
    evalEquals("Div0(1e35::NUMBER(38,0), 0.01)", new BigDecimal("1e37"));

    // Normalize
    optimize("DIV0(FIELD_INTEGER,4)");

    // Simplify arithmetic DIV0(A,0) → 0 when A is not nullable
    optimize("DIV0(0,0)", "0");
    optimize("DIV0(PI(),0)", "0");

    // Simplify arithmetic with NULL
    optimizeNull("DIV0(NULL::INTEGER,FIELD_INTEGER)");
    optimizeNull("DIV0(FIELD_INTEGER,NULL::INTEGER)");

    // Simplify arithmetic DIV0(A,1) → A
    optimize("DIV0(FIELD_INTEGER,1)", "FIELD_INTEGER");

    // Simplify arithmetic DIV0(-A,-B) → DIV0(A,B)
    optimize("DIV0(-FIELD_NUMBER,-FIELD_INTEGER)", "DIV0(FIELD_NUMBER,FIELD_INTEGER)");
    optimize("DIV0(-FIELD_NUMBER,FIELD_INTEGER)", "DIV0(-FIELD_NUMBER,FIELD_INTEGER)");
    optimize("DIV0(FIELD_NUMBER,-FIELD_INTEGER)", "DIV0(FIELD_NUMBER,-FIELD_INTEGER)");
    optimize("DIV0(FIELD_NUMBER,FIELD_INTEGER)", "DIV0(FIELD_NUMBER,FIELD_INTEGER)");
  }

  @Test
  void Exp() throws Exception {
    evalEquals("EXP(0)", 1D);
    evalEquals("EXP(-1)", new BigDecimal("0.36787944117144232159552377016146"));
    evalEquals("Exp(1)", BigDecimalMath.exp(BigDecimal.ONE, Operator.MATH_CONTEXT))
        .returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Exp(2)", new BigDecimal("7.3890560989306502272304274605750"));

    // Null handling
    evalNull("Exp(NULL_INTEGER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Exp()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Exp(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("Exp(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);

    optimize("EXP(1)", "2.7182818284590452353602874713527");
  }

  @Test
  void Factorial() throws Exception {
    // The last factorial computed with an integer and the first one with a number
    evalEquals("FACTORIAL(20)", new BigDecimal("2432902008176640000"));
    evalEquals("FACTORIAL(21)", new BigDecimal("51090942171709440000"));
    // The decimal part of a number is truncated
    evalEquals("FACTORIAL(5.7)", 120D);
    evalEquals("FACTORIAL(-0.5)", 1D);
    evalEquals("Factorial(0)", BigDecimal.ONE);
    evalEquals("Factorial(1)", BigDecimal.ONE);
    evalEquals("Factorial(5)", new BigDecimal("120"));
    evalEquals("Factorial(10)", new BigDecimal("3628800"));
    evalEquals("Factorial(33)", new BigDecimal("8683317618811886495518194401280000000"));
    // The largest argument is 33, its factorial of 37 digits fits in the precision of a number
    evalFails("Factorial(34)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Factorial(40)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Factorial(-1)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    // The argument must not wrap
    evalFails("Factorial(4294967296)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Factorial(4294967301)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Factorial(-4294967295)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalNull("TRY(Factorial(34))");

    // Null handling
    evalNull("Factorial(NULL_INTEGER)");
    evalNull("Factorial(NULL_NUMBER)");

    // Check operands
    evalFails("Factorial()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Factorial(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("Factorial(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Factorial(-2)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
  }

  @Test
  void Power() throws Exception {
    evalEquals("Power(3,2)", BigDecimal.valueOf(9L)).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Power(100,0.5)", BigDecimal.valueOf(10L));
    evalEquals("Power(-4,2)", BigDecimal.valueOf(16L));
    evalEquals("Power(FIELD_INTEGER,0)", BigDecimal.ONE);
    evalEquals("Power(0,0)", BigDecimal.ONE);
    evalEquals("Power(99,0)", BigDecimal.ONE);
    evalEquals("Power(-2,1)", BigDecimal.valueOf(-2L));
    evalEquals("Power(2,2.5)", new BigDecimal("5.6568542494923801952067548968388"));
    evalEquals("Power(2,-3)", new BigDecimal("0.125")).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Power(2.000,-2)", new BigDecimal("0.25")).returnType(NumberType.NUMBER_NOT_NULL);

    // Null handling
    evalNull("Power(NULL_INTEGER,2)");
    evalNull("Power(NULL_NUMBER,2)");
    evalNull("Power(3,NULL_INTEGER)");
    evalNull("Power(3,NULL_NUMBER)");
    evalNull("Power(NULL_INTEGER,NULL_INTEGER)");

    // Check operands
    evalFails("Power()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Power(3)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Power(1,2,3)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("Power(FIELD_STRING,2)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Power(2,FIELD_STRING)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    // A negative base can only have an integer exponent
    evalEquals("Power(-2,3)", -8D);
    evalFails("Power(-4,0.5)", ErrorCode.CALL_FUNCTION_ERROR);
    // A zero base with a negative exponent is a division by zero
    evalFails("Power(0,-1)", ErrorCode.CALL_FUNCTION_ERROR);

    // Alias
    evalEquals("Pow(3,2)", 9D);

    optimize("POWER(FIELD_NUMBER,1)", "POWER(FIELD_NUMBER,1)");
    optimize("POWER(RANDOM(),1)", "RANDOM()");
  }

  @Test
  void Sign() throws Exception {
    evalEquals("Sign(0.3)", 1L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Sign(0)", 0L);
    evalEquals("Sign(-5)", -1L);

    // Null handling
    evalNull("Sign(NULL_INTEGER)").returnType(IntegerType.INTEGER);
    evalNull("Sign(NULL_NUMBER)").returnType(IntegerType.INTEGER);

    // Check operands
    evalFails("Sign()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Sign(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("Sign(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);

    // Simplify function repetition
    optimize("SIGN(SIGN(FIELD_INTEGER))", "SIGN(FIELD_INTEGER)");
  }

  @Test
  void Cbrt() throws Exception {
    evalEquals("Cbrt(0)", BigDecimal.ZERO);
    evalEquals("Cbrt(1)", BigDecimal.ONE);
    evalEquals("Cbrt(2)", new BigDecimal("1.2599210498948731647672106072782"));
    evalEquals("Cbrt(64)", BigDecimal.valueOf(4L));
    evalEquals("Cbrt(343)", BigDecimal.valueOf(7L));

    // Support negative value
    evalEquals("Cbrt(-8)", BigDecimal.valueOf(-2L));

    // Null handling
    evalNull("Cbrt(NULL_INTEGER)");
    evalNull("Cbrt(NULL_NUMBER)");

    // Check operands
    evalFails("Cbrt()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Cbrt(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("Cbrt(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Sqrt() throws Exception {
    evalEquals("Sqrt(9)", BigDecimal.valueOf(3L));

    // Null handling
    evalNull("Sqrt(NULL_INTEGER)");

    // Check operands
    evalFails("Sqrt()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Sqrt(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("Sqrt(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Sqrt(-5)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
  }

  @Test
  void Square() throws Exception {
    evalEquals("Square(1)", 1D);
    evalEquals("Square(-5)", 25D);
    evalEquals("Square(3.3)", new BigDecimal("3.3").multiply(new BigDecimal("3.3")));

    // Null handling
    evalNull("Square(NULL_INTEGER)");

    // Check operands
    evalFails("Square()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Square(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("Square(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Ceil() throws Exception {
    // Documentation example
    evalEquals("CEIL(28.05)", 29D);
    evalEquals("Ceil(1)", 1D).returnType(NumberType.of(1).withNullability(false));
    evalEquals("Ceil(125.9)", 126D).returnType(NumberType.of(4).withNullability(false));
    evalEquals("Ceil(0.4873)", 1D).returnType(NumberType.of(2).withNullability(false));
    evalEquals("Ceil(-0.1)", 0D).returnType(NumberType.of(2).withNullability(false));
    evalEquals("Ceil(-0.65)", 0D).returnType(NumberType.of(2).withNullability(false));
    evalEquals("Ceil(-42.8)", -42D).returnType(NumberType.of(3).withNullability(false));
    // Rounding a fraction can add a digit, the value fits in the type
    evalEquals("Ceil(9.5)", 10D).returnType(NumberType.of(2).withNullability(false));
    evalEquals("Ceil(999.9)", 1000D).returnType(NumberType.of(4).withNullability(false));
    evalEquals("CAST(Ceil(9.5) AS NUMBER(2))", 10D);
    evalEquals("Ceil(FIELD_INTEGER)", 40D);
    evalEquals("Ceil(FIELD_NUMBER)", -5D);
    evalEquals("Ceil(FIELD_BIGNUMBER)", new BigDecimal("123457"));

    // Null handling
    evalNull("Ceil(NULL_INTEGER)");
    evalNull("Ceil(NULL_NUMBER)");
    evalNull("Ceil(NULL_BIGNUMBER)");

    // Check operands
    evalFails("Ceil()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Ceil(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("Ceil('12x')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);

    // Function repetition
    optimize("CEIL(CEIL(FIELD_NUMBER))", "CEIL(FIELD_NUMBER)");
    optimize("CEIL(FLOOR(FIELD_NUMBER))", "FLOOR(FIELD_NUMBER)");
  }

  @Test
  void Floor() throws Exception {
    // Documentation example
    evalEquals("FLOOR(28.05)", 28D);
    evalEquals("Floor(1)", 1D).returnType(NumberType.of(1).withNullability(false));
    evalEquals("Floor(125.9)", 125D).returnType(NumberType.of(4).withNullability(false));
    evalEquals("Floor(0.4873)", 0D).returnType(NumberType.of(2).withNullability(false));
    evalEquals("Floor(-0.1)", -1D).returnType(NumberType.of(2).withNullability(false));
    evalEquals("Floor(-0.65)", -1D).returnType(NumberType.of(2).withNullability(false));
    evalEquals("Floor(-42.8)", -43D).returnType(NumberType.of(3).withNullability(false));
    // Rounding a fraction can add a digit, the value fits in the type
    evalEquals("Floor(-9.5)", -10D).returnType(NumberType.of(2).withNullability(false));
    evalEquals("Floor(FIELD_INTEGER)", 40D);
    evalEquals("Floor(FIELD_NUMBER)", -6D);
    evalEquals("Floor(FIELD_BIGNUMBER)", new BigDecimal("123456"));

    // Null handling
    evalNull("Floor(NULL_INTEGER)");
    evalNull("Floor(NULL_NUMBER)");
    evalNull("Floor(NULL_BIGNUMBER)");

    // Check operands
    evalFails("Floor()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Floor(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("Floor('12x')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);

    // Function repetition
    optimize("FLOOR(FLOOR(FIELD_NUMBER))", "FLOOR(FIELD_NUMBER)");
    optimize("FLOOR(CEIL(FIELD_NUMBER))", "CEIL(FIELD_NUMBER)");
  }

  @Test
  void Round() throws Exception {
    // The scale must not wrap
    evalEquals("ROUND(1.55, 4294967297)", 1.55D);
    evalEquals("ROUND(123.456, 2147483648)", 123.456D);
    evalEquals("ROUND(123.456, -4294967295)", 0D);
    evalEquals("ROUND(-123.456, -4294967295)", 0D);
    evalEquals("ROUND(0.00123, -4294967295)", 0D);
    // A scale just before the first integer digit can still round up
    evalEquals("ROUND(500, -3)", 1000D);
    evalEquals("ROUND(500, -4)", 0D);
    // Documentation examples, half up at any scale
    evalEquals("ROUND(28.05)", 28D);
    evalEquals("ROUND(28.05, 1)", 28.1D);
    evalEquals("ROUND(28.05, -1)", 30D);
    evalNull("ROUND(1.5, NULL_INTEGER)");
    evalEquals("Round(1)", 1D).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Round(2.5)", 3D).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Round(-2.5)", -3D).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Round(12.123456,2)", new BigDecimal("12.12"))
        .returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Round(12.123456,-1)", 10D).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Round(125.49)", 125D);
    evalEquals("Round(125.99)", 126D);
    evalEquals("Round(0.4873)", 0D);
    evalEquals("Round(-0.65)", -1D);
    evalEquals("Round(9223372036854775807,-1)", new BigDecimal("9223372036854775810"))
        .returnType(NumberType.NUMBER_NOT_NULL);

    // Null handling
    evalNull("Round(NULL_INTEGER)");
    evalNull("Round(NULL_NUMBER)");
    evalNull("Round(NULL_BIGNUMBER)");

    // Check operands
    evalFails("Round()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Round(1,2,3)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("Round('x')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);

    // Function repetition
    optimize("ROUND(ROUND(FIELD_NUMBER))", "ROUND(FIELD_NUMBER)");
    // Not with a scale in the inner call
    optimize("ROUND(ROUND(FIELD_NUMBER,1))", "ROUND(ROUND(FIELD_NUMBER,1))");
    evalEquals("ROUND(ROUND(FIELD_NUMBER,1))", -5D);
  }

  @Test
  void Truncate() throws Exception {
    // The scale must not wrap
    evalEquals("TRUNCATE(1.55, 4294967297)", 1.55D);
    evalEquals("TRUNCATE(123.456, 2147483648)", 123.456D);
    evalEquals("TRUNCATE(123.456, -4294967295)", 0D);
    evalEquals("TRUNCATE(-123.456, -4294967295)", 0D);
    evalEquals("TRUNCATE(1999.9, -3)", 1000D);
    evalEquals("TRUNCATE(1999.9, -4)", 0D);
    // Documentation examples
    evalEquals("TRUNCATE(28.52)", 28D);
    evalEquals("TRUNCATE(28.52, 1)", 28.5D);
    evalEquals("TRUNCATE(28.52, -1)", 20D);
    evalEquals("TRUNCATE(-9.6)", -9D);
    evalEquals("Truncate(-975.975)", -975D); // TODO: .returnType(NumberType.of(3));
    evalEquals("Truncate(-975.975,-1)", -970D);
    evalEquals("Truncate(-975.975, 0)", -975D);
    evalEquals("Truncate(-975.975, 2)", -975.97D); // .returnType(NumberType.of(3,2));
    evalEquals("Truncate(-975.975, 3)", -975.975D);
    evalEquals("Truncate(-975.975, 50)", -975.975D);
    evalEquals("Truncate(123.456, -2)", 100D);
    evalEquals("truncate(123456789012345678999.999,-2)", new BigDecimal("123456789012345678900"));

    // Null handling
    evalNull("Truncate(-975.975, NULL_INTEGER)");
    evalNull("Truncate(NULL_NUMBER, 2)");

    // Check operands
    evalFails("Truncate()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Truncate(1,2,3)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("Truncate('x')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);

    // Alias
    evalEquals("Trunc(123.456, -2)", 100D);
    evalEquals("Trunc(28.52)", 28D);

    optimize("TRUNC(TRUNCATE(FIELD_NUMBER))", "TRUNCATE(FIELD_NUMBER)");
  }

  @Test
  void Degrees() throws Exception {
    evalEquals("Degrees(Pi())", new BigDecimal("180")).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Degrees(Radians(50))", new BigDecimal("50")).returnType(NumberType.NUMBER_NOT_NULL);

    // Null handling
    evalNull("Degrees(NULL_INTEGER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Degrees()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Degrees(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Radians() throws Exception {
    evalEquals("Radians(180)", PI).returnType(NumberType.NUMBER_NOT_NULL);

    // Null handling
    evalNull("Radians(NULL_INTEGER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Radians()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("Radians(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Random() throws Exception {
    // A seed with the alias gives the same value
    evalEquals("Rand(180)", new Random(180).nextDouble());
    Assertions.assertFalse(FunctionRegistry.getFunction("RANDOM").isDeterministic());

    evalTrue("Random() between 0 and 1");

    /*
     Keep the same context
     Warning Random implementation is different on each JVM
    */
    Evaluator evaluator = new Evaluator(createExpressionContext(), "Random()");
    evaluator.returnType(NumberType.NUMBER_NOT_NULL);

    // Evaluate should execute
    Object value = evaluator.eval(Object.class);
    assertInstanceOf(BigDecimal.class, value);
    double randomValue = ((BigDecimal) value).doubleValue();
    assertTrue(0 <= randomValue && randomValue < 1);

    // Check operands
    evalFails("Random('test')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("Random(NULL_INTEGER)", ErrorCode.INVALID_NUMBER);
    evalFails("Random(1,2)", ErrorCode.TOO_MANY_ARGUMENTS);

    // Each call has its own generator, so a seed always gives the same sequence
    evalEquals("Random(180)", new Random(180).nextDouble());
    evalEquals("Random(180)", new Random(180).nextDouble());

    // Alias
    evalTrue("Rand()>0");

    // Optimize should do nothing
    optimize("RANDOM()", "RANDOM()");
  }
}
