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
    evalFails("CRC32()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("CRC32('a','b')", ErrorCode.TOO_MANY_ARGUMENTS);

    // Binary value and characters encoded in UTF-8
    evalTrue("CRC32('Test') = CRC32(BINARY '54657374')");

    // Always 8 characters, with the leading zeros
    evalEquals("CRC32('')", "00000000");
    evalEquals("CRC32('v3')", "04444822");
    evalTrue("CRC32('é') = CRC32(BINARY 'C3A9')");
    evalNull("CRC32(NULL_BINARY)").returnType(StringType.STRING);

    // A value which is not a string or a binary is hashed as its text
    evalTrue("CRC32(123) = CRC32('123')");
    evalTrue("CRC32(FIELD_INTEGER) = CRC32('40')");
    evalTrue("CRC32(TRUE) = CRC32('TRUE')");
    evalTrue("CRC32(FIELD_DATE) = CRC32(CAST(FIELD_DATE AS STRING))");
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
    evalFails("MD5()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("MD5('a','b')", ErrorCode.TOO_MANY_ARGUMENTS);

    // Empty value, binary value and characters encoded in UTF-8
    evalEquals("MD5('')", "d41d8cd98f00b204e9800998ecf8427e");
    evalTrue("MD5('Test') = MD5(BINARY '54657374')");
    evalTrue("MD5('é') = MD5(BINARY 'C3A9')");
    evalNull("MD5(NULL_BINARY)").returnType(StringType.STRING);

    // A value which is not a string or a binary is hashed as its text
    evalTrue("MD5(123) = MD5('123')");
    evalTrue("MD5(FIELD_INTEGER) = MD5('40')");
    evalTrue("MD5(TRUE) = MD5('TRUE')");
    evalTrue("MD5(FIELD_DATE) = MD5(CAST(FIELD_DATE AS STRING))");
  }

  @Test
  void Sha1() throws Exception {
    evalEquals("SHA1('Test')", "640ab2bae07bedc4c163f679a746f7ab7fb5d1fa")
        .returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("SHA1(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("SHA1()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("SHA1('a','b')", ErrorCode.TOO_MANY_ARGUMENTS);

    // Empty value, binary value and characters encoded in UTF-8
    evalEquals("SHA1('')", "da39a3ee5e6b4b0d3255bfef95601890afd80709");
    evalTrue("SHA1('Test') = SHA1(BINARY '54657374')");
    evalTrue("SHA1('é') = SHA1(BINARY 'C3A9')");
    evalNull("SHA1(NULL_BINARY)").returnType(StringType.STRING);

    // A value which is not a string or a binary is hashed as its text
    evalTrue("SHA1(123) = SHA1('123')");
    evalTrue("SHA1(FIELD_INTEGER) = SHA1('40')");
    evalTrue("SHA1(TRUE) = SHA1('TRUE')");
    evalTrue("SHA1(FIELD_DATE) = SHA1(CAST(FIELD_DATE AS STRING))");
  }

  @Test
  void Sha224() throws Exception {
    evalEquals("SHA224('Test')", "3606346815fd4d491a92649905a40da025d8cf15f095136b19f37923")
        .returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("SHA224(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("SHA224()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("SHA224('a','b')", ErrorCode.TOO_MANY_ARGUMENTS);

    // Empty value, binary value and characters encoded in UTF-8
    evalEquals("SHA224('')", "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f");
    evalTrue("SHA224('Test') = SHA224(BINARY '54657374')");
    evalTrue("SHA224('é') = SHA224(BINARY 'C3A9')");
    evalNull("SHA224(NULL_BINARY)").returnType(StringType.STRING);

    // A value which is not a string or a binary is hashed as its text
    evalTrue("SHA224(123) = SHA224('123')");
    evalTrue("SHA224(FIELD_INTEGER) = SHA224('40')");
    evalTrue("SHA224(TRUE) = SHA224('TRUE')");
    evalTrue("SHA224(FIELD_DATE) = SHA224(CAST(FIELD_DATE AS STRING))");
  }

  @Test
  void Sha256() throws Exception {
    evalEquals("SHA256('Test')", "532eaabd9574880dbf76b9b8cc00832c20a6ec113d682299550d7a6e0f345e25")
        .returnType(StringType.STRING_NOT_NULL);

    // Null handling
    evalNull("SHA256(NULL_STRING)").returnType(StringType.STRING);

    // Check operands
    evalFails("SHA256()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("SHA256('a','b')", ErrorCode.TOO_MANY_ARGUMENTS);

    // Empty value, binary value and characters encoded in UTF-8
    evalEquals("SHA256('')", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
    evalTrue("SHA256('Test') = SHA256(BINARY '54657374')");
    evalTrue("SHA256('é') = SHA256(BINARY 'C3A9')");
    evalNull("SHA256(NULL_BINARY)").returnType(StringType.STRING);

    // A value which is not a string or a binary is hashed as its text
    evalTrue("SHA256(123) = SHA256('123')");
    evalTrue("SHA256(FIELD_INTEGER) = SHA256('40')");
    evalTrue("SHA256(TRUE) = SHA256('TRUE')");
    evalTrue("SHA256(FIELD_DATE) = SHA256(CAST(FIELD_DATE AS STRING))");
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
    evalFails("SHA384()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("SHA384('a','b')", ErrorCode.TOO_MANY_ARGUMENTS);

    // Empty value, binary value and characters encoded in UTF-8
    evalEquals(
        "SHA384('')",
        "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b");
    evalTrue("SHA384('Test') = SHA384(BINARY '54657374')");
    evalTrue("SHA384('é') = SHA384(BINARY 'C3A9')");
    evalNull("SHA384(NULL_BINARY)").returnType(StringType.STRING);

    // A value which is not a string or a binary is hashed as its text
    evalTrue("SHA384(123) = SHA384('123')");
    evalTrue("SHA384(FIELD_INTEGER) = SHA384('40')");
    evalTrue("SHA384(TRUE) = SHA384('TRUE')");
    evalTrue("SHA384(FIELD_DATE) = SHA384(CAST(FIELD_DATE AS STRING))");
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
    evalFails("SHA512()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("SHA512('a','b')", ErrorCode.TOO_MANY_ARGUMENTS);

    // Empty value, binary value and characters encoded in UTF-8
    evalEquals(
        "SHA512('')",
        "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e");
    evalTrue("SHA512('Test') = SHA512(BINARY '54657374')");
    evalTrue("SHA512('é') = SHA512(BINARY 'C3A9')");
    evalNull("SHA512(NULL_BINARY)").returnType(StringType.STRING);

    // A value which is not a string or a binary is hashed as its text
    evalTrue("SHA512(123) = SHA512('123')");
    evalTrue("SHA512(FIELD_INTEGER) = SHA512('40')");
    evalTrue("SHA512(TRUE) = SHA512('TRUE')");
    evalTrue("SHA512(FIELD_DATE) = SHA512(CAST(FIELD_DATE AS STRING))");
  }
}
