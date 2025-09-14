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
import org.apache.hop.expression.type.StringType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.parallel.Execution;

@TestInstance(PER_CLASS)
@Execution(CONCURRENT)
class CryptographicFunctionTest extends ExpressionTest {

  @Test
  void Crc32() throws Exception {
    evalEquals("CRC32('Apache Hop')", "dbb81b5e").returnType(StringType.STRING_NOT_NULL);
    evalEquals("CRC32(BINARY '123456789ABCDEF')", "2f720f20")
        .returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("CRC32(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("CRC32()", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void MD5() throws Exception {
    evalEquals("MD5('Test')", "0cbc6611f5540bd0809a388dc95a615b")
        .returnType(StringType.STRING_NOT_NULL);
    evalEquals(
            "MD5(BINARY '123456789ABCDEF123456789ABCDEF123456789ABCDEF123456789ABCDEF')",
            "99c415050a2cddbeb525670345ff0aee")
        .returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("MD5(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("MD5()", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void Sha1() throws Exception {
    evalEquals("SHA1('Test')", "640ab2bae07bedc4c163f679a746f7ab7fb5d1fa")
        .returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("SHA1(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("SHA1()", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void Sha224() throws Exception {
    evalEquals("SHA224('Test')", "c696f08d2858549cfe0929bb7b098cfa9b64d51bec94aa68471688e4")
        .returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("SHA224(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("SHA224()", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void Sha256() throws Exception {
    evalEquals("SHA256('Test')", "532eaabd9574880dbf76b9b8cc00832c20a6ec113d682299550d7a6e0f345e25")
        .returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("SHA256(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("SHA256()", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void Sha384() throws Exception {
    evalEquals(
            "SHA384('Test')",
            "7b8f4654076b80eb963911f19cfad1aaf4285ed48e826f6cde1b01a79aa73fadb5446e667fc4f90417782c91270540f3")
        .returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("SHA384(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("SHA384()", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }

  @Test
  void Sha512() throws Exception {
    evalEquals(
            "SHA512('Test')",
            "c6ee9e33cf5c6715a1d148fd73f7318884b41adcb916021e2bc0e800a5c5dd97f5142178f6ae88c8fdd98e1afb0ce4c8d2c54b5f37b30b7da1997bb33b0b8a31")
        .returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("SHA512(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("SHA512()", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }
}
