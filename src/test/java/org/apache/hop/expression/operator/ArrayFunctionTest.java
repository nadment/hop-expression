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
    evalFails("ARRAY_PREPEND()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("ARRAY_PREPEND('test',3)", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("ARRAY_PREPEND([1,2,3],4)", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("ARRAY_PREPEND('test',[1,2,3])", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Array_Append() throws Exception {
    optimize("ARRAY_APPEND([1,2,3],4)", "[1,2,3,4]");
    optimize("ARRAY_APPEND(['sun','mon'],'tue')", "['sun','mon','tue']");
    optimize("ARRAY_APPEND([],0)", "[0]");

    // Check operands
    evalFails("ARRAY_APPEND()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("ARRAY_APPEND('test',3)", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("ARRAY_APPEND(0,[1,2,3])", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("ARRAY_APPEND([1,2,3],'test')", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Array_Contains() throws Exception {
    evalTrue("ARRAY_CONTAINS([1,2,3],2)");
    evalFalse("ARRAY_CONTAINS([1,2,3],0)");
    evalTrue("ARRAY_CONTAINS(['FR','BE','US'],'BE')");
    evalFalse("ARRAY_CONTAINS(['FR','BE','US'],'GB')");

    // Check operands
    evalFails("ARRAY_CONTAINS()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("ARRAY_CONTAINS([1,2,3],0,2)", ErrorCode.TOO_MANY_ARGUMENT);
    evalFails("ARRAY_CONTAINS(1,[1,2,3])", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("ARRAY_CONTAINS([1,2,3], FIELD_DATE)", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Array_Position() throws Exception {
    // TODO: return NOT NULL
    evalEquals("ARRAY_POSITION(['sun','mon','tue','wed','thu','fri','sat'],'mon')", 2L)
        .returnType(IntegerType.INTEGER);
    evalEquals("ARRAY_POSITION([1,2,3],2.0)", 2L).returnType(IntegerType.INTEGER);

    // Element is not found in the array
    evalEquals("ARRAY_POSITION([1,2,3],4)", 0L);
    evalEquals("ARRAY_POSITION([],4)", 0L);
    evalEquals("ARRAY_POSITION([1,2,3],2,10)", 0L);

    // Function cannot be used to find the position of a NULL element.
    evalNull("ARRAY_POSITION([1,2,NULL_INTEGER,3],NULL_INTEGER)");
    evalNull("ARRAY_POSITION([1,2,3],2,NULL_INTEGER)");

    // TODO: The position of a sub-array in a multi-dimensional array
    // evalEquals("ARRAY_POSITION([[1,2,3], [4,5,6]], [4,5,6])",
    // 2L).returnType(IntegerType.INTEGER);

    // Check operands
    evalFails("ARRAY_POSITION()", ErrorCode.NOT_ENOUGH_ARGUMENT);
    evalFails("ARRAY_POSITION('test','test')", ErrorCode.ILLEGAL_ARGUMENT);
    evalFails("ARRAY_POSITION([1,2,3],'test')", ErrorCode.ILLEGAL_ARGUMENT);
  }

  @Test
  void Array_Slice() throws Exception {
    optimize("ARRAY_SLICE([1,2,3,4,5,6],0,2)", "[1,2]");

    optimize("ARRAY_SLICE(['FOO','APACHE','HOP','BAR'], 1, 3)", "['APACHE','HOP']");

    optimize("ARRAY_SLICE([1,2,3,4,5,6],0,-2)", "[1,2,3,4]");
    optimize("ARRAY_SLICE([0,1,2,3,4,5,6], -5, -3)", "[2,3]");

    optimize("ARRAY_SLICE([0,1,2,3,4,5,6], 10, 12)", "[]");
    optimize("ARRAY_SLICE([0,1,2,3,4,5,6], -10, -12)", "[]");

    evalNull("ARRAY_SLICE(NULL, 2, 3)");
    evalNull("ARRAY_SLICE([1,2,3,4,5,6],NULL,3)");
    evalNull("ARRAY_SLICE([1,2,3,4,5,6],1,NULL)");
  }

  @Test
  void Array_To_String() throws Exception {
    evalEquals("ARRAY_TO_STRING(['Hello','world'],' ')", "Hello world")
        .returnType(StringType.STRING);
    evalEquals("ARRAY_TO_STRING([1.2,4,8+2],',')", "1.2,4,10").returnType(StringType.STRING);
    evalEquals("ARRAY_TO_STRING(['A',[4,8+2],'B'],'')", "A410B");

    // Check operands
    evalFails("ARRAY_TO_STRING([1,2,3])", ErrorCode.NOT_ENOUGH_ARGUMENT);
  }
}
