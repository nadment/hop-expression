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

import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionTest;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.IntegerType;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.parallel.Execution;

@TestInstance(PER_CLASS)
@Execution(CONCURRENT)
class BitwiseFunctionTest extends ExpressionTest {
  @Test
  void BitClear() throws Exception {
    evalEquals("Bit_Clear(3,1)", 2L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Bit_Clear(3,4)", 3L);

    // Overflow has no impact on the result
    evalEquals("Bit_Clear(32,66)", 32L);

    // Null handling
    evalNull("Bit_Clear(123,0)").returnType(IntegerType.INTEGER);
    evalNull("Bit_Clear(123,-1)");
    evalNull("Bit_Clear(NULL_INTEGER,3)");
    evalNull("Bit_Clear(123, NULL_INTEGER)");

    // Check operands
    evalFails("Bit_Clear()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Bit_Clear(123)", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void BitShift() throws Exception {
    evalEquals("Bit_Shift(123,0)", 123L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Bit_Shift(1,4)", 16L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Bit_Shift(2,8)", 512L);
    evalEquals("Bit_Shift(6,-2)", 1L);
    evalEquals("Bit_Shift(16,-4)", 1L);
    evalEquals("Bit_Shift(10000,-3)", 1250L);
    evalEquals("Bit_Shift(1,63)", 0x8000000000000000L);
    // evalEquals("Bit_Shift(0x8000000000000000,-63)", 1);

    // Overflow
    evalEquals("Bit_Shift(1,64)", 0L);
    evalEquals("Bit_Shift(1,-64)", 0L);

    // Underflow
    evalEquals("Bit_Shift(1,-1)", 0L);

    // Null handling
    evalNull("Bit_Shift(NULL_INTEGER,3)").returnType(IntegerType.INTEGER);
    evalNull("Bit_Shift(123, NULL_INTEGER)");

    // TODO: Implicit cast number to integer no supported
    // evalFails("Bit_Shift(16.3,-4)", ErrorCode.ILLEGAL_ARGUMENT);
    // evalFails("Bit_Shift(16,2.1)", ErrorCode.ILLEGAL_ARGUMENT);

    // Check operands
    evalFails("Bit_Shift()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Bit_Shift(123)", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Bit_Shift('Bidon',3)", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("Bit_Shift(DATE '2022-11-25', 3)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void BitRotate() throws Exception {
    evalEquals("Bit_Rotate(123,0)", 123L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Bit_Rotate(1,4)", 16L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Bit_Rotate(16,-4)", 1L);
    evalEquals("Bit_Rotate(-9223372036854775807,2)", 6L);
    evalEquals("Bit_Rotate(6,-2)", -9223372036854775807L);
    evalEquals("Bit_Rotate(10000,-3)", 1250L);
    // Full rotate 64 bits
    evalEquals("Bit_Rotate(123456,64)", 123456L);
    evalEquals("Bit_Rotate(123456,128)", 123456L);
    evalEquals("Bit_Rotate(123456,-64)", 123456L);
    evalEquals("Bit_Rotate(123456,-128)", 123456L);

    // Null handling
    evalNull("Bit_Rotate(NULL_INTEGER,3)").returnType(IntegerType.INTEGER);
    evalNull("Bit_Rotate(123, NULL_INTEGER)");

    // Check operands
    evalFails("Bit_Rotate()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Bit_Rotate(123)", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void BitGet() throws Exception {
    evalTrue("Bit_Get(3,1)").returnType(BooleanType.BOOLEAN_NOT_NULL);
    evalTrue("Bit_Get(3,2)");
    evalFalse("Bit_Get(3,4)");

    // Overflow is always false
    evalFalse("Bit_Get(3,255)");

    // Null handling
    evalNull("Bit_Get(123,0)");
    evalNull("Bit_Get(123,-1)");
    evalNull("Bit_Get(NULL_INTEGER,3)");
    evalNull("Bit_Get(123, NULL_INTEGER)");

    // Check operands
    evalFails("Bit_Get()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Bit_Get(123)", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void BitCount() throws Exception {
    evalEquals("Bit_Count(FIELD_INTEGER)", 2L).returnType(IntegerType.INTEGER);
    evalEquals("Bit_Count(31)", 5L);
    evalEquals("Bit_Count(True)", 1L);

    // Null handling
    evalNull("Bit_Count(NULL_INTEGER)").returnType(IntegerType.INTEGER);

    // Check operands
    evalFails("Bit_Count()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Bit_Count(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
  }

  @Test
  void BitSet() throws Exception {
    evalEquals("Bit_Set(16,1)", 17L).returnType(IntegerType.INTEGER_NOT_NULL);
    evalEquals("Bit_Set(1,4)", 9L);
    evalEquals("Bit_Set(16,4)", 24L);
    evalEquals("Bit_Set(32,4)", 40L);

    // Overflow has no impact on the result
    evalEquals("Bit_Set(32,66)", 32L);

    // Null handling
    evalNull("Bit_Set(123,0)").returnType(IntegerType.INTEGER);
    evalNull("Bit_Set(123,-1)");
    evalNull("Bit_Set(NULL_INTEGER,3)");
    evalNull("Bit_Set(123, NULL_INTEGER)");

    // Check operands
    evalFails("Bit_Set()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("Bit_Set(123)", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Nested
  class BitNot {
    @Test
    void testWithInteger() throws Exception {
      evalEquals("BIT_NOT(1)", -2L).returnType(IntegerType.INTEGER_NOT_NULL);
      evalEquals("BIT_NOT(0)", -1L);
      evalEquals("BIT_NOT(4)", -5L);
      evalEquals("BIT_NOT(65504)", -65505L);
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("BIT_NOT(NULL_INTEGER)").returnType(IntegerType.INTEGER);
    }

    @Test
    void testOptimize() throws Exception {
      optimize("BIT_NOT(FIELD_INTEGER)");
      optimize("BIT_NOT(BIT_NOT(FIELD_INTEGER))", "FIELD_INTEGER");
    }

    @Test
    void testError() throws Exception {
      evalFails("BIT_NOT()", ErrorCode.NOT_ENOUGH_ARGUMENT);
      evalFails("BIT_NOT(1,2)", ErrorCode.TOO_MANY_ARGUMENT);
      evalFails("BIT_NOT(FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
    }
  }

  @Nested
  class BitAnd {
    @Test
    void testWithInteger() throws Exception {
      evalEquals("BIT_AND(3,2)", 2L).returnType(IntegerType.INTEGER_NOT_NULL);
      evalEquals("BIT_AND(100,2)", 0L).returnType(IntegerType.INTEGER_NOT_NULL);
      evalEquals("BIT_AND(BIT_AND(100,2),1)", 0L).returnType(IntegerType.INTEGER_NOT_NULL);

      // Data type propagation check
      evalEquals("BIT_AND(3,FIELD_INTEGER)", 0L).returnType(IntegerType.INTEGER);
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("BIT_AND(100,NULL_INTEGER)").returnType(IntegerType.INTEGER);
      evalNull("BIT_AND(NULL_INTEGER,100)").returnType(IntegerType.INTEGER);
      evalNull("BIT_AND(BINARY '0F000001',NULL_BINARY)").returnType(BinaryType.BINARY);
      evalNull("BIT_AND(NULL_BINARY,BINARY '0F000001')").returnType(BinaryType.BINARY);
    }

    @Test
    void testWithBinary() throws Exception {
      // Binary
      evalEquals("BIT_AND(BINARY '0F000001',BINARY '0000003')", new byte[] {0x00, 0x00, 0x00, 0x01})
          .returnType(BinaryType.BINARY_NOT_NULL);
      evalFails("BIT_AND(BINARY '0F',BINARY '0F01')", ErrorCode.INVALID_BITWISE_OPERANDS_SIZE);
    }

    @Test
    void testOptimize() throws Exception {
      // Nothing to simplify
      optimize("BIT_AND(4,FIELD_INTEGER)");
      optimize("BIT_AND(FIELD_INTEGER,4)", "BIT_AND(4,FIELD_INTEGER)");

      // Simplify constant
      optimize("BIT_AND(BIT_AND(BIT_AND(5,FIELD_INTEGER),5),5)", "BIT_AND(5,FIELD_INTEGER)");
      optimize("BIT_AND(123,0)", "0");
      optimize("BIT_AND(0,123)", "0");

      // Simplify BIT_AND(NULL,A) → NULL
      optimizeNull("BIT_AND(NULL::INTEGER,FIELD_INTEGER)");
      optimizeNull("BIT_AND(FIELD_INTEGER,NULL::INTEGER)");
      optimizeNull("BIT_AND(FIELD_BINARY,NULL::BINARY)");
      optimizeNull("BIT_AND(NULL::BINARY,FIELD_BINARY)");

      // Simplify BIT_AND(0,A) -> 0 (if A not nullable)
      optimize("BIT_AND(FIELD_INTEGER,0)", "BIT_AND(0,FIELD_INTEGER)");
      optimize("BIT_AND(0,FIELD_INTEGER)");

      // Simplify BIT_AND(A,A) → A
      optimize("BIT_AND(FIELD_INTEGER,FIELD_INTEGER)", "FIELD_INTEGER");
      optimize("BIT_AND(FIELD_BINARY,FIELD_BINARY)", "FIELD_BINARY");
    }

    @Test
    void testError() throws Exception {
      evalFails("BIT_AND(3)", ErrorCode.NOT_ENOUGH_ARGUMENT);
      evalFails("BIT_AND(1,2,3)", ErrorCode.TOO_MANY_ARGUMENT);
      evalFails("BIT_AND(FIELD_DATE,FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
    }
  }

  @Nested
  class BitOr {
    @Test
    void testWithInteger() throws Exception {
      evalEquals("BIT_OR(100,2)", 102L).returnType(IntegerType.INTEGER_NOT_NULL);
      evalEquals("BIT_OR(3,2)", 3L).returnType(IntegerType.INTEGER_NOT_NULL);
    }

    @Test
    void testWithBinary() throws Exception {
      evalEquals(
              "BIT_OR(BINARY '0F000001',BINARY '00000001')",
              new byte[] {(byte) 0x0F, (byte) 0x00, (byte) 0x00, (byte) 0x01})
          .returnType(BinaryType.BINARY_NOT_NULL);
      evalFails("BIT_OR(BINARY '0F',BINARY '0F01')", ErrorCode.INVALID_BITWISE_OPERANDS_SIZE);
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("BIT_OR(100,NULL_INTEGER)").returnType(IntegerType.INTEGER);
      evalNull("BIT_OR(NULL_INTEGER,100)").returnType(IntegerType.INTEGER);
      evalNull("BIT_OR(BINARY '0F000001',NULL_BINARY)").returnType(BinaryType.BINARY);
      evalNull("BIT_OR(NULL_BINARY,BINARY '0F000001')").returnType(BinaryType.BINARY);
    }

    @Test
    void testError() throws Exception {
      evalFails("BIT_OR(3)", ErrorCode.NOT_ENOUGH_ARGUMENT);
      evalFails("BIT_OR(1,2,3)", ErrorCode.TOO_MANY_ARGUMENT);
      evalFails("BIT_OR(FIELD_DATE,FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
    }

    @Test
    void testOptimize() throws Exception {
      // Nothing to simplify
      optimize("BIT_OR(FIELD_INTEGER,4)", "BIT_OR(4,FIELD_INTEGER)");

      // Simplify constant
      optimize("BIT_OR(BIT_OR(BIT_OR(1,FIELD_INTEGER),4),4)", "BIT_OR(5,FIELD_INTEGER)");

      // Simplify BIT_OR(NULL,A) → NULL
      optimizeNull("BIT_OR(NULL::INTEGER,FIELD_INTEGER)");
      optimizeNull("BIT_OR(FIELD_INTEGER,NULL::INTEGER)");
      optimizeNull("BIT_OR(NULL::BINARY,FIELD_BINARY)");
      optimizeNull("BIT_OR(FIELD_BINARY,NULL::BINARY)");

      // Simplify BIT_OR(A,A) → A
      optimize("BIT_OR(FIELD_INTEGER,FIELD_INTEGER)", "FIELD_INTEGER");
      optimize("BIT_OR(FIELD_BINARY,FIELD_BINARY)", "FIELD_BINARY");

      // Simplify BIT_OR(0,A) → A (even if A is null)
      optimize("BIT_OR(FIELD_INTEGER,0)", "FIELD_INTEGER");
      optimize("BIT_OR(0,FIELD_INTEGER)", "FIELD_INTEGER");
    }
  }

  @Nested
  class BitXor {
    @Test
    void testWithInteger() throws Exception {
      evalEquals("BIT_XOR(2,1)", 3L).returnType(IntegerType.INTEGER_NOT_NULL);
      evalEquals("BIT_XOR(2,2)", 0L).returnType(IntegerType.INTEGER_NOT_NULL);
      evalEquals("BIT_XOR(100,2)", 102L).returnType(IntegerType.INTEGER_NOT_NULL);
    }

    @Test
    void testWithBinary() throws Exception {

      // Binary
      evalEquals("BIT_XOR(BINARY 'FFFF',BINARY 'FFFF')", new byte[] {(byte) 0x00, (byte) 0x00})
          .returnType(BinaryType.BINARY_NOT_NULL);

      evalEquals("BIT_XOR(BINARY '0F01',BINARY 'F0FE')", new byte[] {(byte) 0xFF, (byte) 0xFF})
          .returnType(BinaryType.BINARY_NOT_NULL);
      evalFails("BIT_XOR(BINARY '0F',BINARY '0F01')", ErrorCode.INVALID_BITWISE_OPERANDS_SIZE);
    }

    @Test
    void testNullHandling() throws Exception {
      evalNull("BIT_XOR(100,NULL_INTEGER)").returnType(IntegerType.INTEGER);
      evalNull("BIT_XOR(NULL_INTEGER,100)").returnType(IntegerType.INTEGER);
      evalNull("BIT_XOR(BINARY '0F000001',NULL_BINARY)").returnType(BinaryType.BINARY);
      evalNull("BIT_XOR(NULL_BINARY,BINARY '0F000001')").returnType(BinaryType.BINARY);
    }

    @Test
    void testError() throws Exception {
      evalFails("BIT_XOR(3)", ErrorCode.NOT_ENOUGH_ARGUMENT);
      evalFails("BIT_XOR(1,2,3)", ErrorCode.TOO_MANY_ARGUMENT);
      evalFails("BIT_XOR(FIELD_DATE,FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
    }

    @Test
    void testOptimize() throws Exception {
      // Nothing to simplify
      optimize("BIT_XOR(FIELD_INTEGER,4)", "BIT_XOR(4,FIELD_INTEGER)");

      // Simplify constant
      optimize("BIT_XOR(BIT_XOR(BIT_XOR(1,FIELD_INTEGER),3),4)", "BIT_XOR(6,FIELD_INTEGER)");

      // Simplify BIT_XOR(NULL,A) → NULL
      optimizeNull("BIT_XOR(NULL::INTEGER,FIELD_INTEGER)");
      optimizeNull("BIT_XOR(FIELD_INTEGER,NULL::INTEGER)");
      optimizeNull("BIT_XOR(NULL::BINARY,FIELD_BINARY)");
      optimizeNull("BIT_XOR(FIELD_BINARY,NULL::BINARY)");

      // Simplify BIT_XOR(A,A) → 0 (if A is not nullable)
      optimize("BIT_XOR(FIELD_INTEGER,FIELD_INTEGER)", "BIT_XOR(FIELD_INTEGER,FIELD_INTEGER)");
      optimize("BIT_XOR(IFNULL(FIELD_INTEGER,4),IFNULL(FIELD_INTEGER,4))", "0");

      // Simplify BIT_XOR(0,A) → A (even if A is nullable)
      optimize("BIT_XOR(0,FIELD_INTEGER)", "FIELD_INTEGER");
      optimize("BIT_XOR(FIELD_INTEGER,0)", "FIELD_INTEGER");
    }
  }
}
