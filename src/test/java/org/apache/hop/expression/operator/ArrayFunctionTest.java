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
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.NumberType;
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
    optimize("ARRAY_PREPEND(FIELD_STRING,['A','B','C'])", "[FIELD_STRING,'A','B','C']");
    optimize(
        "ARRAY_PREPEND('A',STRING_TO_ARRAY(FIELD_STRING,','))",
        "ARRAY_PREPEND('A',STRING_TO_ARRAY(FIELD_STRING,','))");

    // The elements of the array are converted to the type of the prepended element
    optimize("ARRAY_PREPEND(2.5,[1,2])", "[2.5,1,2]");
    optimize(
            "ARRAY_PREPEND(FIELD_NUMBER,[FIELD_INTEGER,2])",
            "[FIELD_NUMBER,CAST(FIELD_INTEGER AS NUMBER),2]")
        .returnType(ArrayType.of(NumberType.NUMBER));

    // An INTEGER prepended to an array of NUMBER is coerced to NUMBER
    optimize("ARRAY_PREPEND(3,[1.5,2.5])", "[3,1.5,2.5]")
        .returnType(ArrayType.of(NumberType.of(2, 1, false)));
    optimize(
            "ARRAY_PREPEND(FIELD_INTEGER,[1.5,FIELD_NUMBER])",
            "[CAST(FIELD_INTEGER AS NUMBER),1.5,FIELD_NUMBER]")
        .returnType(ArrayType.of(NumberType.NUMBER));
    // The element prepended to an array returned by a function is coerced when it is prepended
    optimize(
            "ARRAY_PREPEND(FIELD_INTEGER,TRANSFORM([FIELD_NUMBER],x->x))",
            "ARRAY_PREPEND(FIELD_INTEGER,TRANSFORM([FIELD_NUMBER],x->x))")
        .returnType(ArrayType.of(NumberType.NUMBER));

    // The result is an array of the most general type of the elements and the prepended element
    optimize("ARRAY_PREPEND(0,[1,2,3])", "[0,1,2,3]")
        .returnType(ArrayType.of(IntegerType.of(1, false)));
    optimize("ARRAY_PREPEND(40,[1,2,3])", "[40,1,2,3]")
        .returnType(ArrayType.of(IntegerType.of(2, false)));
    evalEquals(
        "ARRAY_TO_STRING(TRANSFORM(ARRAY_PREPEND(FIELD_INTEGER,[1,2]),x->x+1),',')", "41,2,3");
    evalTrue("EXISTS(ARRAY_PREPEND(NULL_INTEGER,[1,2]),x->x IS NULL)");

    // Check operands
    evalFails("ARRAY_PREPEND()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("ARRAY_PREPEND('test',3)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_PREPEND(1,NULL)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_PREPEND([1,2,3],4)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_PREPEND('test',[1,2,3])", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_PREPEND(FIELD_STRING,[1])", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Array_Concat() throws Exception {
    optimize("ARRAY_CONCAT([1,2,3],[4,5])", "[1,2,3,4,5]");
    optimize("ARRAY_CONCAT(['sun','mon'],['tue'],['wed'])", "['sun','mon','tue','wed']");
    optimize("ARRAY_CONCAT([1,2,3])", "[1,2,3]");
    optimize("ARRAY_CONCAT([],[1])", "[1]");
    optimize("ARRAY_CONCAT([[1]],[[2]])", "[[1],[2]]");
    evalEquals("CARDINALITY(ARRAY_CONCAT([1,2,3],[4,5]))", 5L);

    // The result is an array of the most general type of the elements
    optimize("ARRAY_CONCAT([1,2],[40])", "[1,2,40]")
        .returnType(ArrayType.of(IntegerType.of(2, false)));
    optimize("ARRAY_CONCAT([1,2],[2.5])", "[1,2,2.5]");
    optimize("ARRAY_CONCAT([FIELD_INTEGER],[1])", "[FIELD_INTEGER,1]");
    // The elements are converted to the most general type of the elements
    optimize(
        "ARRAY_CONCAT([1,FIELD_INTEGER],[2.5])", "[1,CAST(FIELD_INTEGER AS NUMBER(13,1)),2.5]");

    // An array of INTEGER with an array of NUMBER, the integers are coerced to NUMBER
    optimize("ARRAY_CONCAT([1,2],[1.5,2.5])", "[1,2,1.5,2.5]")
        .returnType(ArrayType.of(NumberType.of(2, 1, false)));
    optimize(
            "ARRAY_CONCAT([1,FIELD_INTEGER],[1.5,FIELD_NUMBER])",
            "[1,CAST(FIELD_INTEGER AS NUMBER),1.5,FIELD_NUMBER]")
        .returnType(ArrayType.of(NumberType.NUMBER));
    optimize(
            "ARRAY_CONCAT([FIELD_NUMBER],[FIELD_INTEGER])",
            "[FIELD_NUMBER,CAST(FIELD_INTEGER AS NUMBER)]")
        .returnType(ArrayType.of(NumberType.NUMBER));
    optimize(
            "ARRAY_CONCAT([FIELD_INTEGER],[1.5],[FIELD_NUMBER])",
            "[CAST(FIELD_INTEGER AS NUMBER),1.5,FIELD_NUMBER]")
        .returnType(ArrayType.of(NumberType.NUMBER));
    // The elements of an array constructor are coerced at compile time, the elements of an array
    // returned by a function when the arrays are joined
    optimize(
            "ARRAY_CONCAT(TRANSFORM([FIELD_NUMBER],x->x),[1,FIELD_INTEGER])",
            "ARRAY_CONCAT(TRANSFORM([FIELD_NUMBER],x->x),[1,CAST(FIELD_INTEGER AS NUMBER)])")
        .returnType(ArrayType.of(NumberType.NUMBER));
    optimize(
            "ARRAY_CONCAT(TRANSFORM([FIELD_INTEGER],x->x),[1.5,FIELD_NUMBER])",
            "ARRAY_CONCAT(TRANSFORM([FIELD_INTEGER],x->x),[1.5,FIELD_NUMBER])")
        .returnType(ArrayType.of(NumberType.NUMBER));
    evalTrue("ARRAY_CONCAT(TRANSFORM([1,FIELD_INTEGER],x->x*2),[2.5])[1] = 2");
    evalTrue("ARRAY_APPEND(TRANSFORM([1,FIELD_INTEGER],x->x*2),2.5)[2] = 80");

    // Chained calls are combined
    optimize(
        "ARRAY_CONCAT(ARRAY_CONCAT(STRING_TO_ARRAY(FIELD_STRING,','),['A']),['B'])",
        "ARRAY_CONCAT(STRING_TO_ARRAY(FIELD_STRING,','),['A'],['B'])");
    evalEquals("ARRAY_TO_STRING(ARRAY_CONCAT(STRING_TO_ARRAY('a,b',','),['c']),',')", "a,b,c");

    // NULL values are ignored
    optimize("ARRAY_CONCAT([1,2],NULL)", "[1,2]");
    optimize("ARRAY_CONCAT(NULL,[1,2],NULL,[3])", "[1,2,3]");
    evalEquals("ARRAY_TO_STRING(ARRAY_CONCAT(STRING_TO_ARRAY(NULL_STRING,','),['c']),',')", "c");
    evalNull("ARRAY_CONCAT(STRING_TO_ARRAY(NULL_STRING,','),STRING_TO_ARRAY(NULL_STRING,','))");

    // Check operands
    evalFails("ARRAY_CONCAT()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("ARRAY_CONCAT([1,2,3],4)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_CONCAT('test',[1,2,3])", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_CONCAT([DATE '2020-01-01'],[1])", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Array_Append() throws Exception {
    optimize("ARRAY_APPEND([1,2,3],4)", "[1,2,3,4]");
    optimize("ARRAY_APPEND(['sun','mon'],'tue')", "['sun','mon','tue']");
    optimize("ARRAY_APPEND([],0)", "[0]");
    optimize("ARRAY_APPEND(['A','B','C'],FIELD_STRING)", "['A','B','C',FIELD_STRING]");
    optimize(
        "ARRAY_APPEND(STRING_TO_ARRAY(FIELD_STRING,','),'A')",
        "ARRAY_APPEND(STRING_TO_ARRAY(FIELD_STRING,','),'A')");

    // The elements of the array are converted to the type of the appended element
    optimize("ARRAY_APPEND([1,2],2.5)", "[1,2,2.5]");
    optimize(
            "ARRAY_APPEND([FIELD_INTEGER,2],FIELD_NUMBER)",
            "[CAST(FIELD_INTEGER AS NUMBER),2,FIELD_NUMBER]")
        .returnType(ArrayType.of(NumberType.NUMBER));

    // An INTEGER appended to an array of NUMBER is coerced to NUMBER
    optimize("ARRAY_APPEND([1.5,2.5],3)", "[1.5,2.5,3]")
        .returnType(ArrayType.of(NumberType.of(2, 1, false)));
    optimize(
            "ARRAY_APPEND([1.5,2.5],FIELD_INTEGER)",
            "[1.5,2.5,CAST(FIELD_INTEGER AS NUMBER(13,1))]")
        .returnType(ArrayType.of(NumberType.of(13, 1)));
    optimize(
            "ARRAY_APPEND([1.5,FIELD_NUMBER],FIELD_INTEGER)",
            "[1.5,FIELD_NUMBER,CAST(FIELD_INTEGER AS NUMBER)]")
        .returnType(ArrayType.of(NumberType.NUMBER));
    // The element appended to an array returned by a function is coerced when it is appended
    optimize(
            "ARRAY_APPEND(TRANSFORM([FIELD_NUMBER],x->x),FIELD_INTEGER)",
            "ARRAY_APPEND(TRANSFORM([FIELD_NUMBER],x->x),FIELD_INTEGER)")
        .returnType(ArrayType.of(NumberType.NUMBER));

    // The result is an array of the most general type of the elements and the appended element
    optimize("ARRAY_APPEND([1,2,3],4)", "[1,2,3,4]")
        .returnType(ArrayType.of(IntegerType.of(1, false)));
    optimize("ARRAY_APPEND([1,2,3],40)", "[1,2,3,40]")
        .returnType(ArrayType.of(IntegerType.of(2, false)));
    evalEquals(
        "ARRAY_TO_STRING(TRANSFORM(ARRAY_APPEND([1,2],FIELD_INTEGER),x->x+1),',')", "2,3,41");
    evalTrue("EXISTS(ARRAY_APPEND([1,2],NULL_INTEGER),x->x IS NULL)");

    // Check operands
    evalFails("ARRAY_APPEND()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("ARRAY_APPEND('test',3)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_APPEND(NULL,1)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_APPEND(0,[1,2,3])", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_APPEND([1,2,3],'test')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_APPEND([DATE '2020-01-01'],1)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
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

    // Elements and value of different numeric types
    evalTrue("ARRAY_CONTAINS([1,2],2.0)");
    evalTrue("ARRAY_CONTAINS([1.5,2],2)");
    evalFalse("ARRAY_CONTAINS([1.5,2],3)");
    evalTrue("ARRAY_CONTAINS([FIELD_NUMBER,40.0],FIELD_INTEGER)");
    evalFalse("ARRAY_CONTAINS([],1)");

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
    // Elements and value of different numeric types
    evalEquals("ARRAY_POSITION([1.5,2],2)", 2L);
    evalEquals("ARRAY_POSITION([FIELD_NUMBER,2],2)", 2L);

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

    // The result is an array of the type of the source array
    optimize("ARRAY_SLICE([1,2,3],0,2)", "[1,2]")
        .returnType(ArrayType.of(IntegerType.of(1, false)));
    evalEquals("ARRAY_TO_STRING(TRANSFORM(ARRAY_SLICE([1,2,3],0,2),x->x+1),',')", "2,3");

    // Null handling
    evalNull("ARRAY_SLICE(NULL, 2, 3)");
    evalNull("ARRAY_SLICE([1,2,3,4,5,6],NULL,3)");
    evalNull("ARRAY_SLICE([1,2,3,4,5,6],1,NULL)");

    // Check operands
    evalFails("ARRAY_SLICE([1,2,3,4,5,6],1)", ErrorCode.NOT_ENOUGH_ARGUMENTS);
  }

  @Test
  void Array_Distinct() throws Exception {
    optimize("ARRAY_DISTINCT([1,2,1,3,2])", "[1,2,3]");
    optimize("ARRAY_DISTINCT(['b','a','b'])", "['b','a']");
    optimize("ARRAY_DISTINCT([1,2.5,1.0])", "[1,2.5]");
    optimize("ARRAY_DISTINCT([])", "[]");

    // Elements are compared by value
    evalEquals("ARRAY_TO_STRING(ARRAY_DISTINCT([40,FIELD_INTEGER,2]),',')", "40,2");
    evalEquals("ARRAY_TO_STRING(ARRAY_DISTINCT(STRING_TO_ARRAY('b,a,b,c,a',',')),',')", "b,a,c");

    // The NULL elements are duplicates of each other
    evalEquals("CARDINALITY(ARRAY_DISTINCT([1,NULL_INTEGER,1,NULL_INTEGER]))", 2L);

    // Null handling
    evalNull("ARRAY_DISTINCT(STRING_TO_ARRAY(NULL_STRING,','))");

    // Check operands
    evalFails("ARRAY_DISTINCT()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("ARRAY_DISTINCT([1,2],[3])", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("ARRAY_DISTINCT('test')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Array_Sort() throws Exception {
    optimize("ARRAY_SORT([3,1,2])", "[1,2,3]");
    optimize("ARRAY_SORT([3,1,2],FALSE)", "[3,2,1]");
    optimize("ARRAY_SORT(['mon','tue','fri'])", "['fri','mon','tue']");
    optimize("ARRAY_SORT([2.5,1,2])", "[1,2,2.5]");
    optimize("ARRAY_SORT([])", "[]");

    // Elements are compared by value
    evalEquals("ARRAY_TO_STRING(ARRAY_SORT([50,FIELD_INTEGER,2]),',')", "2,40,50");
    evalEquals("ARRAY_TO_STRING(ARRAY_SORT(STRING_TO_ARRAY('b,c,a',',')),',')", "a,b,c");

    // The NULL elements are placed at the end, whatever the order
    evalEquals("ARRAY_POSITION(ARRAY_SORT([NULL_INTEGER,2,1]),1)", 1L);
    evalEquals("ARRAY_POSITION(ARRAY_SORT([NULL_INTEGER,1,2],FALSE),2)", 1L);
    evalEquals("ARRAY_POSITION(ARRAY_SORT([NULL_INTEGER,1,2],FALSE),1)", 2L);

    // Null handling
    evalNull("ARRAY_SORT(STRING_TO_ARRAY(NULL_STRING,','))");
    evalNull("ARRAY_SORT([1,2],NULL_BOOLEAN)");

    // Check operands
    evalFails("ARRAY_SORT()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("ARRAY_SORT([1,2],TRUE,TRUE)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("ARRAY_SORT('test')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_SORT([1,2],'test')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Array_Sort_Comparator() throws Exception {
    optimize("ARRAY_SORT([3,1,2],(a,b)->a-b)", "[1,2,3]");
    optimize("ARRAY_SORT([3,1,2], (p1, p2) -> p2 - p1)", "[3,2,1]");
    optimize("ARRAY_SORT(['ccc','a','bb'],(a,b)->LENGTH(a)-LENGTH(b))", "['a','bb','ccc']");
    optimize("ARRAY_SORT([],(a,b)->0)", "[]");
    optimize("ARRAY_SORT(STRING_TO_ARRAY(FIELD_STRING,','),(a,b)->LENGTH(a)-LENGTH(b))");

    // Only the sign of a number matters, not its decimal part
    optimize("ARRAY_SORT([1.5,0.2,1.2],(a,b)->a-b)", "[0.2,1.2,1.5]");

    // The sort is stable, the elements considered as equal keep their order
    optimize("ARRAY_SORT(['bb','a','cc'],(a,b)->LENGTH(a)-LENGTH(b))", "['a','bb','cc']");

    // The expression can reference the fields of the row, sorted by distance to 40
    evalEquals(
        "ARRAY_TO_STRING(ARRAY_SORT([10,50,30],(a,b)->ABS(a-FIELD_INTEGER)-ABS(b-FIELD_INTEGER)),',')",
        "50,30,10");

    // The NULL elements are not compared and placed at the end
    evalEquals("ARRAY_POSITION(ARRAY_SORT([NULL_INTEGER,1,2],(a,b)->b-a),1)", 2L);

    // A NULL result means equal elements
    evalEquals("ARRAY_TO_STRING(ARRAY_SORT([3,1,2],(a,b)->NULL_INTEGER),',')", "3,1,2");

    // Null handling
    evalNull("ARRAY_SORT(STRING_TO_ARRAY(NULL_STRING,','),(a,b)->LENGTH(a)-LENGTH(b))");

    // The comparator must return a number, not a BOOLEAN
    evalFails("ARRAY_SORT([3,1,2],(a,b)->a<b)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_SORT([3,1,2],(a,b)->'test')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);

    // The comparator must have two parameters
    evalFails("ARRAY_SORT([3,1,2],a->a)", ErrorCode.ILLEGAL_LAMBDA_PARAMETER_COUNT);
    evalFails("ARRAY_SORT([3,1,2],(a,b,c)->a-b)", ErrorCode.ILLEGAL_LAMBDA_PARAMETER_COUNT);
    evalFails("ARRAY_SORT([3,1,2],(a,b)->a-b,TRUE)", ErrorCode.TOO_MANY_ARGUMENTS);
  }

  @Test
  void Array_Remove() throws Exception {
    optimize("ARRAY_REMOVE([1,2,1,3],1)", "[2,3]");
    optimize("ARRAY_REMOVE(['a','b','a'],'a')", "['b']");
    optimize("ARRAY_REMOVE([1,2,3],4)", "[1,2,3]");
    optimize("ARRAY_REMOVE([1,2,3],2.0)", "[1,3]");
    optimize("ARRAY_REMOVE([1,1],1)", "[]");
    optimize("ARRAY_REMOVE([],1)", "[]");

    // Values of different numeric types
    optimize("ARRAY_REMOVE([1,2.5,3],2.5)", "[1,3]");
    optimize("ARRAY_REMOVE([1.5,2,3],2)", "[1.5,3]");

    // Elements are compared by value
    evalEquals("ARRAY_TO_STRING(ARRAY_REMOVE([40,FIELD_INTEGER,2],40),',')", "2");
    evalEquals("ARRAY_TO_STRING(ARRAY_REMOVE(STRING_TO_ARRAY('a,,b,',','),''),',')", "a,b");

    // A NULL element never matches and is kept
    evalEquals("CARDINALITY(ARRAY_REMOVE([1,NULL_INTEGER,2],1))", 2L);

    // Null handling
    evalNull("ARRAY_REMOVE([1,2,3],NULL_INTEGER)");
    evalNull("ARRAY_REMOVE([],NULL_INTEGER)");
    evalNull("ARRAY_REMOVE(STRING_TO_ARRAY(NULL_STRING,','),'a')");

    // Check operands
    evalFails("ARRAY_REMOVE()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("ARRAY_REMOVE([1,2,3])", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("ARRAY_REMOVE([1,2,3],1,2)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("ARRAY_REMOVE('test',1)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("ARRAY_REMOVE([1,2,3],'test')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Transform() throws Exception {
    optimize("TRANSFORM([1,2,3],x->x*2)", "[2,4,6]");
    optimize("TRANSFORM([1,2,3], x -> x + 1)", "[2,3,4]");
    optimize("TRANSFORM(['a','b'],s->UPPER(s))", "['A','B']");
    optimize("TRANSFORM([],x->x)", "[]");
    optimize("TRANSFORM(STRING_TO_ARRAY(FIELD_STRING,','),x->LOWER(x))");

    // The type of the elements can change
    evalEquals("ARRAY_TO_STRING(TRANSFORM([1,2],x->'#'||x),',')", "#1,#2");
    evalEquals("ARRAY_TO_STRING(TRANSFORM([1.5,2],x->CEIL(x)),',')", "2,2");

    // The body can reference the fields of the row
    evalEquals("ARRAY_TO_STRING(TRANSFORM([1,2],x->x+FIELD_INTEGER),',')", "41,42");
    evalEquals(
        "ARRAY_TO_STRING(TRANSFORM(STRING_TO_ARRAY('100,250',','),p->TO_NUMBER(p)*FIELD_INTEGER),',')",
        "4000,10000");

    // The parameter hides a field with the same name
    evalEquals("ARRAY_TO_STRING(TRANSFORM([1,2],FIELD_INTEGER->FIELD_INTEGER*10),',')", "10,20");

    // Nested lambdas
    evalEquals(
        "ARRAY_TO_STRING(TRANSFORM([1,2],x->ARRAY_TO_STRING(TRANSFORM([10,20],y->x+y),'+')),',')",
        "11+21,12+22");

    // A NULL element is passed to the lambda
    evalEquals("ARRAY_TO_STRING(TRANSFORM([1,NULL_INTEGER,3],x->COALESCE(x,0)),',')", "1,0,3");
    optimize("TRANSFORM([1,NULL,3], x -> COALESCE(x,0))", "[1,0,3]");

    // Null handling
    evalNull("TRANSFORM(STRING_TO_ARRAY(NULL_STRING,','),x->x)");

    // Check operands
    evalFails("TRANSFORM()", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("TRANSFORM([1,2])", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("TRANSFORM([1,2],x->x,1)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("TRANSFORM([1,2],3)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("TRANSFORM('test',x->x)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("TRANSFORM([1,2],x->y)", ErrorCode.UNRESOLVED_IDENTIFIER);
    evalFails("TRANSFORM([1,2],x->)", ErrorCode.MISSING_LAMBDA_EXPRESSION);
    evalFails("TRANSFORM([1,2],x->,1)", ErrorCode.MISSING_LAMBDA_EXPRESSION);
    evalFails("TRANSFORM([1,2],x->", ErrorCode.MISSING_LAMBDA_EXPRESSION);

    // The parameter can be enclosed in parentheses
    optimize("TRANSFORM([1,2],(x)->x+1)", "[2,3]");

    // The lambda must have a single parameter
    evalFails("TRANSFORM([1,2],(x,y)->x+y)", ErrorCode.ILLEGAL_LAMBDA_PARAMETER_COUNT);
    evalFails("FILTER([1,2],(x,y,z)->x>y)", ErrorCode.ILLEGAL_LAMBDA_PARAMETER_COUNT);
    // The number of parameters is checked before the expression, which is not validated
    evalFails("TRANSFORM([1,2],(x,y)->z)", ErrorCode.ILLEGAL_LAMBDA_PARAMETER_COUNT);
    evalFails("TRANSFORM('test',(x,y)->x)", ErrorCode.ILLEGAL_LAMBDA_PARAMETER_COUNT);
    // The expression of a lambda that can't be called with an element is not validated
    evalFails("TRANSFORM('test',x->z)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("TRANSFORM([1,2],(x,x)->x)", ErrorCode.DUPLICATE_LAMBDA_PARAMETER);
    evalFails("TRANSFORM([1,2],(x,y,x)->x)", ErrorCode.DUPLICATE_LAMBDA_PARAMETER);
    evalFails("TRANSFORM([1,2],(x,y)->)", ErrorCode.MISSING_LAMBDA_EXPRESSION);
    evalFails("TRANSFORM([1,2],()->1)", ErrorCode.SYNTAX_ERROR);

    // The parameter is only visible in the body
    evalFails("CARDINALITY(TRANSFORM([1,2],x->x))+x", ErrorCode.UNRESOLVED_IDENTIFIER);

    // A lambda is only allowed as the argument of a higher-order function
    evalFails("x->x", ErrorCode.LAMBDA_NOT_ALLOWED);
    evalFails("UPPER(x->x)", ErrorCode.LAMBDA_NOT_ALLOWED);
    evalFails("TRANSFORM(x->x,[1,2])", ErrorCode.LAMBDA_NOT_ALLOWED);
  }

  @Test
  void Filter() throws Exception {
    optimize("FILTER([1,-2,3],x->x>0)", "[1,3]");
    optimize("FILTER(['a','bb','ccc'],s->LENGTH(s)>1)", "['bb','ccc']");
    optimize("FILTER([1,2],x->x>5)", "[]");
    optimize("FILTER(STRING_TO_ARRAY(FIELD_STRING,','),x->x>'A')");

    // The body can reference the fields of the row
    evalEquals("CARDINALITY(FILTER([10,50,60],x->x>FIELD_INTEGER))", 2L);

    // An element is kept only if the condition is TRUE
    evalEquals("ARRAY_TO_STRING(FILTER([1,NULL_INTEGER,3],x->x>1),',')", "3");
    optimize("FILTER([1,NULL,3], x -> x > 1)", "[3]");

    // An element can be NULL
    evalEquals("CARDINALITY(FILTER([1,NULL,3],x->x IS NULL))", 1L);
    evalEquals("CARDINALITY(FILTER([1,NULL,3],x->x IS NOT NULL))", 2L);

    // A number is a condition, TRUE if not zero
    evalEquals("ARRAY_TO_STRING(FILTER([0,1,2],x->x),',')", "1,2");

    // Combined with TRANSFORM
    evalEquals(
        "ARRAY_TO_STRING(TRANSFORM(FILTER(STRING_TO_ARRAY('a,,b',','),x->x<>''),x->UPPER(x)),',')",
        "A,B");

    // Null handling
    evalNull("FILTER(STRING_TO_ARRAY(NULL_STRING,','),x->x='a')");

    // Check operands
    evalFails("FILTER([1,2])", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("FILTER([1,2],TRUE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("FILTER([1,2],x->DATE '2020-01-01')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("FILTER('test',x->TRUE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
  }

  @Test
  void Exists() throws Exception {
    evalTrue("EXISTS([1,2,3],x->x>2)").returnType(BooleanType.BOOLEAN);
    evalFalse("EXISTS([1,2,3],x->x>5)");
    evalTrue("EXISTS(ARRAY(0,NULL,2,3,NULL),x->x IS NULL)");
    evalTrue("EXISTS(['a','bb','ccc'],s->LENGTH(s)>2)");
    optimize("EXISTS([1,2,3],x->x>2)", "TRUE");
    optimize("EXISTS(STRING_TO_ARRAY(FIELD_STRING,','),x->x>'A')");

    // The body can reference the fields of the row
    evalTrue("EXISTS([10,50,60],x->x>FIELD_INTEGER)");

    // An empty array has no element
    evalFalse("EXISTS(FILTER([1,2],x->x>5),x->TRUE)");

    // A number is a condition, TRUE if not zero
    evalTrue("EXISTS([0,1],x->x)");
    evalFalse("EXISTS([0,0],x->x)");

    // Three-valued logic: TRUE wins over NULL, NULL wins over FALSE
    evalTrue("EXISTS([1,NULL_INTEGER,3],x->x>2)");
    evalTrue("EXISTS([3,NULL_INTEGER,1],x->x>2)");
    evalNull("EXISTS([1,NULL_INTEGER,3],x->x>5)");

    // Null handling
    evalNull("EXISTS(STRING_TO_ARRAY(NULL_STRING,','),x->x='a')");

    // Check operands
    evalFails("EXISTS([1,2])", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("EXISTS([1,2],x->x>1,3)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("EXISTS([1,2],TRUE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("EXISTS([1,2],x->DATE '2020-01-01')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("EXISTS('test',x->TRUE)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("EXISTS([1,2],(x,y)->x>y)", ErrorCode.ILLEGAL_LAMBDA_PARAMETER_COUNT);
  }

  @Test
  void Reduce() throws Exception {
    evalEquals("REDUCE([1,2,3],(acc,x)->acc+x)", 6L).returnType(IntegerType.INTEGER);
    evalEquals("REDUCE([3,7,5],(acc,x)->GREATEST(acc,x))", 7L);
    evalEquals("REDUCE(['a','b','c'],(acc,s)->acc||'-'||s)", "a-b-c").returnType(StringType.STRING);
    optimize("REDUCE([1,2,3],(acc,x)->acc+x)", "6");
    optimize("REDUCE(STRING_TO_ARRAY(FIELD_STRING,','),(acc,x)->acc||x)");

    // A single element is returned without evaluating the lambda
    evalEquals("REDUCE([5],(acc,x)->acc+x)", 5L);

    // With an initial value, the lambda is evaluated for each element
    evalEquals("REDUCE([1,2,3],(acc,x)->acc+x,100)", 106L);
    evalEquals("REDUCE([5],(acc,x)->acc+x,100)", 105L);

    // The initial value gives the type of the accumulator
    evalEquals("REDUCE([1,2,3],(acc,x)->acc||x,'')", "123");
    evalEquals("REDUCE([1,2,3],(acc,x)->acc+x*0.5,0.5)", new BigDecimal("3.5"))
        .returnType(NumberType.NUMBER);
    evalEquals("REDUCE([1,2,3],(acc,x)->acc+x*0.5,CAST(0 AS NUMBER))", new BigDecimal("3.0"));
    evalEquals("REDUCE(['a','bb','ccc'],(acc,s)->acc+LENGTH(s),0)", 6L);

    // The body can reference the fields of the row
    evalEquals("REDUCE([1,2],(acc,x)->acc+x,FIELD_INTEGER)", 43L);
    evalEquals("REDUCE([1,2],(acc,x)->acc+x*FIELD_INTEGER,0)", 120L);

    // A NULL element is passed to the lambda
    evalNull("REDUCE([1,NULL_INTEGER,3],(acc,x)->acc+x)");
    evalEquals("REDUCE([1,NULL_INTEGER,3],(acc,x)->acc+COALESCE(x,0))", 4L);
    evalEquals("REDUCE([NULL_INTEGER,2,3],(acc,x)->COALESCE(acc,0)+x)", 5L);
    evalEquals("REDUCE([1,2],(acc,x)->IF(acc IS NULL,x,acc+x),NULL_INTEGER)", 3L);

    // An empty array returns the initial value, or NULL
    evalEquals("REDUCE(FILTER([1,2],x->x>5),(acc,x)->acc+x,10)", 10L);
    evalNull("REDUCE(FILTER([1,2],x->x>5),(acc,x)->acc+x)");

    // Combined with TRANSFORM
    evalEquals("REDUCE(TRANSFORM([1,2,3],x->x*x),(acc,x)->acc+x)", 14L);

    // Null handling
    evalNull("REDUCE(STRING_TO_ARRAY(NULL_STRING,','),(acc,x)->acc||x)");

    // The lambda must return a value of the type of the accumulator
    evalFails("REDUCE([1,2,3],(acc,x)->acc+x*0.5)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("REDUCE([1,2,3],(acc,x)->DATE '2020-01-01')", ErrorCode.ILLEGAL_ARGUMENT_TYPE);

    // Check operands
    evalFails("REDUCE([1,2])", ErrorCode.NOT_ENOUGH_ARGUMENTS);
    evalFails("REDUCE([1,2],(acc,x)->acc+x,0,1)", ErrorCode.TOO_MANY_ARGUMENTS);
    evalFails("REDUCE([1,2],3)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("REDUCE('test',(acc,x)->acc)", ErrorCode.ILLEGAL_ARGUMENT_TYPE);
    evalFails("REDUCE([1,2],x->x)", ErrorCode.ILLEGAL_LAMBDA_PARAMETER_COUNT);
    evalFails("REDUCE([1,2],(acc,x)->y)", ErrorCode.UNRESOLVED_IDENTIFIER);
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
