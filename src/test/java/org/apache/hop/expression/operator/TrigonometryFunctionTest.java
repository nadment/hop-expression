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

import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

import java.math.BigDecimal;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionTest;
import org.apache.hop.expression.type.NumberType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.parallel.Execution;

@TestInstance(PER_CLASS)
@Execution(CONCURRENT)
class TrigonometryFunctionTest extends ExpressionTest {

  @Test
  void Acos() throws Exception {
    evalEquals("Acos(0)", new BigDecimal("1.5707963267948966"))
        .returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Acos(1)", BigDecimal.ZERO).returnType(NumberType.NUMBER_NOT_NULL);

    // Null handling
    evalNull("Acos(NULL_INTEGER)");
    evalNull("Acos(NULL_NUMBER)");

    // Check operands
    evalFails("Acos()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Acos(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Acos(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("Acos(2)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Acos(-2)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
  }

  @Test
  void Acosh() throws Exception {
    evalEquals("Acosh(1)", BigDecimal.ZERO).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Acosh(3)", new BigDecimal("1.7627471740390860504652186499596"))
        .returnType(NumberType.NUMBER_NOT_NULL);

    // Null handling
    evalNull("Acosh(NULL_INTEGER)").returnType(NumberType.NUMBER);
    evalNull("Acosh(NULL_NUMBER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Acosh()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Acosh(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Acosh(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Acoth() throws Exception {
    evalEquals("Acoth(2)", new BigDecimal("0.54930614433405484569762261846126"))
        .returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Acoth(3)", new BigDecimal("0.34657359027997265470861606072909"))
        .returnType(NumberType.NUMBER_NOT_NULL);

    // Null handling
    evalNull("Acoth(NULL_INTEGER)").returnType(NumberType.NUMBER);
    evalNull("Acoth(NULL_NUMBER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Acoth()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Acoth(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Acoth(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("Acoth(0.5)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Acoth(1)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("Acoth(-1)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
  }

  @Test
  void Asin() throws Exception {
    evalEquals("Asin(0)", BigDecimal.ZERO).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Asin(sin(0.5))", 0.5D);

    // Null handling
    evalNull("Asin(NULL_INTEGER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Asin()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Asin(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Asin(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Asinh() throws Exception {
    evalEquals("Asinh(asin(0.5))", new BigDecimal("0.50221898503461160828703900193479"))
        .returnType(NumberType.NUMBER_NOT_NULL);

    // Null handling
    evalNull("Asinh(NULL_INTEGER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Asinh()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Asinh(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Asinh(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Atan() throws Exception {
    evalEquals("Atan(0.5)", new BigDecimal("0.46364760900080611621425623146121"))
        .returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Atan(Tan(0.5))", 0.5);

    // Null handling
    evalNull("Atan(NULL_INTEGER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Atan()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Atan(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Atan(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Atan2() throws Exception {
    evalEquals("Atan2(0,3)", BigDecimal.ZERO).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Atan2(0,-3)", PI);

    // Null handling
    evalNull("Atan2(NULL_INTEGER,0)").returnType(NumberType.NUMBER);
    evalNull("Atan2(1,NULL_INTEGER)");

    // Check operands
    evalFails("Atan2()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Atan2(1)", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Atan2(1,2,3)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Atan2(FIELD_DATE,1)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Atanh() throws Exception {
    evalEquals("Atanh(0.2)", new BigDecimal("0.20273255405408219098900655773217"))
        .returnType(NumberType.NUMBER_NOT_NULL);

    // Null handling
    evalNull("Atanh(NULL_INTEGER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Atanh()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Atanh(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Atanh(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Cos() throws Exception {
    evalEquals("Cos(0)", BigDecimal.ONE).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Cos(1)", new BigDecimal("0.54030230586813971740093660744298"))
        .returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Cos(Pi())", -1D);

    // Null handling
    evalNull("Cos(NULL_NUMBER)");

    // Check operands
    evalFails("Cos()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Cos(0,1)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Cos(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Cosh() throws Exception {
    evalEquals("Cosh(1.234)", new BigDecimal("1.8630338016984225890736437502561"))
        .returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Cosh(0)", BigDecimal.ONE);

    // Null handling
    evalNull("Cosh(NULL_NUMBER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Cosh()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Cosh(0,1)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Cosh(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Sin() throws Exception {
    evalEquals("Sin(0)", BigDecimal.ZERO);
    evalEquals("Sin(1)", new BigDecimal("0.84147098480789650665250232163030"))
        .returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Sin(Pi()/2)", BigDecimal.ONE);

    // Null handling
    evalNull("Sin(NULL_NUMBER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Sin()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Sin(0,1)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Sin(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Sinh() throws Exception {
    evalEquals("Sinh(84.4)", new BigDecimal("2.2564425307670914188455367832027E+36"))
        .returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Sinh(0)", BigDecimal.ZERO);

    // Null handling
    evalNull("Sinh(NULL_NUMBER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Sinh()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Sinh(0,1)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Sinh(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Cot() throws Exception {
    evalEquals("Cot(1)", new BigDecimal("0.64209261593433070300641998659427"))
        .returnType(NumberType.NUMBER_NOT_NULL);

    // Null handling
    evalNull("Cot(NULL_NUMBER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Cot()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Cot(1,0)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Cot(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("Cot(0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
  }

  @Test
  void Csc() throws Exception {
    evalEquals("Csc(Pi()/2)", BigDecimal.ONE).returnType(NumberType.NUMBER_NOT_NULL);

    // Null handling
    evalNull("Csc(NULL_INTEGER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Csc()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Csc(1,0)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Csc(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("Csc(0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
  }

  @Test
  void Csch() throws Exception {
    evalEquals("Csch(1.5)", new BigDecimal("0.4696424405952245847295644318870206"))
        .returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Csch(Pi())", new BigDecimal("0.08658953753004694182845976975218431"))
        .returnType(NumberType.NUMBER_NOT_NULL);

    // Null handling
    evalNull("Csch(NULL_INTEGER)");

    // Check operands
    evalFails("Csch()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Csch(1,0)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Csch(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("Csch(0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
  }

  @Test
  void Sec() throws Exception {
    evalEquals("Sec(Pi())", -1D).returnType(NumberType.NUMBER_NOT_NULL);

    // Null handling
    evalNull("Sec(NULL_INTEGER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Sec()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Sec(1,0)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Sec(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("Sec(0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
  }

  @Test
  void Sech() throws Exception {
    evalEquals("Sech(0)", BigDecimal.ONE).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Sech(1)", new BigDecimal("0.6480542736638853995749773532261342"));

    // Null handling
    evalNull("Sech(NULL_INTEGER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Sech()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Sech(1,0)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Sech(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Tan() throws Exception {
    evalEquals("Tan(84.4)", new BigDecimal("-0.45017764606195051960412881423455"))
        .returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Tan(0)", BigDecimal.ZERO).returnType(NumberType.NUMBER_NOT_NULL);

    // Null handling
    evalNull("Tan(NULL_INTEGER)").returnType(NumberType.NUMBER);
    evalNull("Tan(NULL_NUMBER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Tan()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Tan(0,1)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Tan(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Tanh() throws Exception {
    evalEquals("Tanh(1.234)", new BigDecimal("0.84373566258933019391702000004355"))
        .returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Tanh(0)", BigDecimal.ZERO);

    // Null handling
    evalNull("Tanh(NULL_INTEGER)").returnType(NumberType.NUMBER);
    evalNull("Tanh(NULL_NUMBER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Tanh()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Tanh(0,1)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Tanh(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Ln() throws Exception {
    evalEquals("Ln(1)", 0D).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Ln(Exp(2.4))", 2.4D).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Ln(10)", new BigDecimal("2.3025850929940456840179914546844"))
        .returnType(NumberType.NUMBER_NOT_NULL);

    // Null handling
    evalNull("Ln(NULL_INTEGER)");
    evalNull("Ln(NULL_NUMBER)");

    // Check operands
    evalFails("Ln()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Ln(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Ln(0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
  }

  @Test
  void Log() throws Exception {
    evalEquals("Log(10,100)", 2D).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Log(10,1000)", 3D);

    // Null handling
    evalNull("Log(10,NULL_INTEGER)").returnType(NumberType.NUMBER);
    evalNull("Log(NULL_INTEGER,1)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Log()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Log(-2)", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Log(1,2,3)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Log(10, 'x')", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("Log(10,0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
  }

  @Test
  void Log10() throws Exception {
    evalEquals("Log10(10)", 1D).returnType(NumberType.NUMBER_NOT_NULL);
    evalEquals("Log10(1000)", 3D).returnType(NumberType.NUMBER_NOT_NULL);

    // Null handling
    evalNull("Log10(NULL_INTEGER)").returnType(NumberType.NUMBER);

    // Check operands
    evalFails("Log10()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Log10(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("Log10('x')", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("Log10(-1)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
  }
}
