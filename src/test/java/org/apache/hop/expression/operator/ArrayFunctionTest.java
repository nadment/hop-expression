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
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.StringType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.parallel.Execution;

@TestInstance(PER_CLASS)
@Execution(CONCURRENT)
class ArrayFunctionTest extends ExpressionTest {

  @Test
  void Cardinality() throws Exception {
    evalEquals("Cardinality([1,FIELD_INTEGER,8+2])", 3L).returnType(IntegerType.INTEGER);

    // Array computed at evaluation
    evalEquals("CARDINALITY(STRING_TO_ARRAY(FIELD_STRING_DATE,'-'))", 3L);
    evalNull("CARDINALITY(STRING_TO_ARRAY(NULL_STRING,','))");
  }

  @Test
  void Array() throws Exception {
    optimize("ARRAY(1,2,3)", "[1,2,3]");
    optimize("ARRAY('sun','mon','tue')", "['sun','mon','tue']");

    // Construct an empty array
    optimize("ARRAY()", "[]");
  }

  @Test
  void Array_Prepend() throws Exception {
    optimize("ARRAY_PREPEND(0,[1,2,3])", "[0,1,2,3]");
    optimize("ARRAY_PREPEND('sun',['mon','tue'])", "['sun','mon','tue']");
    optimize("ARRAY_PREPEND(0,[])", "[0]");

    // Check operands
    evalFails("ARRAY_PREPEND()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("ARRAY_PREPEND('test',3)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_PREPEND(1,NULL)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_PREPEND([1,2,3],4)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_PREPEND('test',[1,2,3])", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Array_Append() throws Exception {
    optimize("ARRAY_APPEND([1,2,3],4)", "[1,2,3,4]");
    optimize("ARRAY_APPEND(['sun','mon'],'tue')", "['sun','mon','tue']");
    optimize("ARRAY_APPEND([],0)", "[0]");

    // Check operands
    evalFails("ARRAY_APPEND()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("ARRAY_APPEND('test',3)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_APPEND(NULL,1)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_APPEND(0,[1,2,3])", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_APPEND([1,2,3],'test')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Array_Contains() throws Exception {
    evalTrue("ARRAY_CONTAINS([1,2,3],2)");
    evalFalse("ARRAY_CONTAINS([1,2,3],0)");
    evalTrue("ARRAY_CONTAINS(['FR','BE','US'],'BE')");
    evalFalse("ARRAY_CONTAINS(['FR','BE','US'],'GB')");
    optimize("ARRAY_CONTAINS([FIELD_INTEGER,2],FIELD_INTEGER)").returnType(BooleanType.BOOLEAN);

    // Elements are compared by value
    evalTrue("ARRAY_CONTAINS([40,2],FIELD_INTEGER)");
    evalTrue("ARRAY_CONTAINS([FIELD_INTEGER,2],40)");
    evalFalse("ARRAY_CONTAINS([FIELD_INTEGER,2],3)");
    evalTrue("ARRAY_CONTAINS(STRING_TO_ARRAY(FIELD_STRING,','),'TEST')");

    // A null element never matches
    evalTrue("ARRAY_CONTAINS(['A',NULL_STRING],'A')");
    evalFalse("ARRAY_CONTAINS(['A',NULL_STRING],'B')");
    evalNull("ARRAY_CONTAINS(['A','B'],NULL_STRING)");

    // Check operands
    evalFails("ARRAY_CONTAINS()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("ARRAY_CONTAINS([1,2,3],0,2)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("ARRAY_CONTAINS(1,[1,2,3])", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_CONTAINS(NULL,1)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_CONTAINS([1,2,3], FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Array_Position() throws Exception {
    // TODO: return NOT NULL
    evalEquals("ARRAY_POSITION(['sun','mon','tue','wed','thu','fri','sat'],'mon')", 2L)
        .returnType(IntegerType.INTEGER);
    evalEquals("ARRAY_POSITION([1,2,3],2.0)", 2L).returnType(IntegerType.INTEGER);

    // Element is not found in the array
    evalEquals("ARRAY_POSITION([1,2,3],4)", 0L);
    // A null element never matches
    evalEquals("ARRAY_POSITION([1,NULL_INTEGER,FIELD_INTEGER],40)", 3L);
    evalEquals("ARRAY_POSITION([],4)", 0L);
    evalEquals("ARRAY_POSITION([1,2,3],2,10)", 0L);

    // Start position, the first position in the array is 1
    evalEquals("ARRAY_POSITION([1,2,1],1,2)", 3L);
    evalEquals("ARRAY_POSITION([1,2,3],3,3)", 3L);
    evalEquals("ARRAY_POSITION([1,2,3],2,4294967297)", 0L);
    evalEquals("ARRAY_POSITION([1,2,3],2,2147483648)", 0L);
    evalFails("ARRAY_POSITION([1,2,3],2,0)", ErrorCode.ARGUMENT_OUT_OF_RANGE);
    evalFails("ARRAY_POSITION([1,2,3],2,-1)", ErrorCode.ARGUMENT_OUT_OF_RANGE);

    // Function cannot be used to find the position of a NULL element.
    evalNull("ARRAY_POSITION([1,2,NULL_INTEGER,3],NULL_INTEGER)");
    evalNull("ARRAY_POSITION([1,2,3],2,NULL_INTEGER)");

    // TODO: The position of a sub-array in a multi-dimensional array
    // evalEquals("ARRAY_POSITION([[1,2,3], [4,5,6]], [4,5,6])",
    // 2L).returnType(IntegerType.INTEGER);

    // Check operands
    evalFails("ARRAY_POSITION()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("ARRAY_POSITION('test','test')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_POSITION(NULL,1)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_POSITION([1,2,3],'test')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Array_Slice() throws Exception {
    optimize("ARRAY_SLICE([1,2,3,4,5,6],0,2)", "[1,2]");

    optimize("ARRAY_SLICE(['FOO','APACHE','HOP','BAR'], 1, 3)", "['APACHE','HOP']");

    optimize("ARRAY_SLICE([1,2,3,4,5,6],0,-2)", "[1,2,3,4]");
    optimize("ARRAY_SLICE([0,1,2,3,4,5,6], -5, -3)", "[2,3]");

    optimize("ARRAY_SLICE([0,1,2,3,4,5,6], 10, 12)", "[]");
    optimize("ARRAY_SLICE([0,1,2,3,4,5,6], -10, -12)", "[]");

    // A slice partially out of the array keeps the elements inside
    optimize("ARRAY_SLICE([1,2,3], 1, 10)", "[2,3]");
    optimize("ARRAY_SLICE([1,2,3], -10, 2)", "[1,2]");
    optimize("ARRAY_SLICE([1,2,3], -10, 10)", "[1,2,3]");

    // A position out of the int range must not wrap
    optimize("ARRAY_SLICE([1,2,3], 4294967296, 4294967298)", "[]");
    optimize("ARRAY_SLICE([1,2,3], 0, 4294967298)", "[1,2,3]");
    optimize("ARRAY_SLICE([1,2,3], -4294967296, 2)", "[1,2]");

    // Null handling
    evalNull("ARRAY_SLICE(NULL, 2, 3)");
    evalNull("ARRAY_SLICE([1,2,3,4,5,6],NULL,3)");
    evalNull("ARRAY_SLICE([1,2,3,4,5,6],1,NULL)");

    // Check operands
    evalFails("ARRAY_SLICE([1,2,3,4,5,6],1)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
  }

  @Test
  void Array_To_String() throws Exception {
    evalEquals("ARRAY_TO_STRING(['Hello','world'],' ')", "Hello world")
        .returnType(StringType.STRING);
    evalEquals("ARRAY_TO_STRING([1.2,4,8+2],',')", "1.2,4,10").returnType(StringType.STRING);
    evalEquals("ARRAY_TO_STRING(['A',[4,8+2],'B'],'')", "A410B");

    // A null element is an empty string
    evalEquals("ARRAY_TO_STRING(['A',NULL_STRING,'B'],',')", "A,,B");
    evalEquals("ARRAY_TO_STRING([NULL_STRING,'B'],',')", ",B");

    // An element is cast to a string
    evalEquals(
        "ARRAY_TO_STRING([DATE '2024-01-02',TIMESTAMP '2024-01-02 10:20:30'],',')",
        "2024-01-02,2024-01-02 10:20:30");
    evalEquals("ARRAY_TO_STRING([FIELD_BOOLEAN_TRUE,FALSE],',')", "TRUE,FALSE");
    evalEquals("ARRAY_TO_STRING([FIELD_INTEGER,FIELD_NUMBER],';')", "40;-5.12");

    // Null handling
    evalNull("ARRAY_TO_STRING(['Hello','world'],NULL::STRING)");
    evalNull("ARRAY_TO_STRING(NULL,',')");

    // Check operands
    evalFails("ARRAY_TO_STRING([1,2,3])", ErrorCode.NOT_ENOUGH_ARGUMENTS);
  }
}
