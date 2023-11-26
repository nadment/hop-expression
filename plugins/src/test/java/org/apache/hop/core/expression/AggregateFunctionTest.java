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
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package org.apache.hop.core.expression;

import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.junit.Test;

public class AggregateFunctionTest extends ExpressionTest { 
  @Test
  public void Avg() throws Exception {
    returnType("AVG(FIELD_NUMBER)", NumberType.NUMBER);
  }

  @Test
  public void Count() throws Exception {
    evalFails("Count()");
    evalFails("Count(DISTINCT )");
    evalFails("Count(1,2)");

    returnType("Count(*)", IntegerType.INTEGER);

    optimize("COUNT(FIELD_INTEGER)");
    optimize("COUNT(*)");
    optimize("COUNT(DISTINCT FIELD_STRING)");
  }

  @Test
  public void CountIf() throws Exception {
    evalFails("CountIf()");
    evalFails("CountIf(FIELD_DATE)");
    evalFails("CountIf(1,2)");
    returnType("CountIf(FIELD_INTEGER>=10)", IntegerType.INTEGER);
    optimize("COUNTIF(FIELD_INTEGER>=10)","COUNTIF(10<=FIELD_INTEGER)");
  }

  @Test
  public void Sum() throws Exception {
    returnType("SUM(FIELD_INTEGER)", NumberType.NUMBER);
  }
  
  @Test
  public void Max() throws Exception {
    returnType("MAX(FIELD_STRING)", new StringType(1000));
    returnType("MAX(FIELD_INTEGER)", IntegerType.INTEGER);
    returnType("MAX(FIELD_NUMBER)", NumberType.NUMBER);
    returnType("MAX(FIELD_DATE)", DateType.DATE);
  }

  @Test
  public void Min() throws Exception {
    returnType("MIN(FIELD_STRING)", new StringType(1000));
    returnType("MIN(FIELD_INTEGER)", IntegerType.INTEGER);
    returnType("MIN(FIELD_NUMBER)", NumberType.NUMBER);
    returnType("MIN(FIELD_DATE)", DateType.DATE);
  }
  
  @Test
  public void Median() throws Exception {
    returnType("MEDIAN(FIELD_INTEGER)", NumberType.NUMBER);
  }

  @Test
  public void AnyValue() throws Exception {
    returnType("Any_Value(FIELD_DATE)", DateType.DATE);
  }
  
  @Test
  public void FirstValue() throws Exception {
    evalFails("FIRST_VALUE(FIELD_DATE) IGNORE");
    evalFails("FIRST_VALUE(FIELD_DATE) NULLS");
    
    optimize("FIRST_VALUE(FIELD_DATE) RESPECT NULLS", "FIRST_VALUE(FIELD_DATE)");
    optimize("FIRST_VALUE(FIELD_DATE) IGNORE NULLS");
    
    returnType("FIRST_VALUE(FIELD_DATE)", DateType.DATE);
  }
  
  @Test
  public void LastValue() throws Exception {
    evalFails("LAST_VALUE(FIELD_DATE) IGNORE");
    evalFails("LAST_VALUE(FIELD_DATE) NULLS");
    
    optimize("LAST_VALUE(FIELD_DATE) RESPECT NULLS", "LAST_VALUE(FIELD_DATE)");
    optimize("LAST_VALUE(FIELD_DATE) IGNORE NULLS");
    
    returnType("LAST_VALUE(FIELD_DATE)", DateType.DATE);
  }

  @Test
  public void NthValue() throws Exception {
    evalFails("NTH_VALUE(FIELD_DATE)");
    evalFails("NTH_VALUE(FIELD_DATE) IGNORE");
    evalFails("NTH_VALUE(FIELD_DATE) NULLS");
    
    optimize("NTH_VALUE(FIELD_DATE,2) RESPECT NULLS", "NTH_VALUE(FIELD_DATE,2)");
    optimize("NTH_VALUE(FIELD_DATE,2) IGNORE NULLS");
    
    returnType("NTH_VALUE(FIELD_DATE,3)", DateType.DATE);
  }
  
  @Test
  public void ListAgg() throws Exception {
    returnType("ListAGG(FIELD_STRING,',')", StringType.STRING);
  }  
  
  @Test
  public void Percentile() throws Exception {
    returnType("Percentile(FIELD_INTEGER,0.75)", NumberType.NUMBER);
  }

  @Test
  public void VarPop() throws Exception {
    returnType("Variance_Pop(FIELD_INTEGER)", NumberType.NUMBER);
  }
  
  public void VarSamp() throws Exception {
    returnType("Variance_Samp(FIELD_INTEGER)", NumberType.NUMBER);
  }

  @Test
  public void StdDevPop() throws Exception {
    returnType("StdDev_Pop(FIELD_INTEGER)", NumberType.NUMBER);
  }
  
  @Test
  public void StdDevSamp() throws Exception {
    returnType("StdDev_Samp(FIELD_INTEGER)", NumberType.NUMBER);
  }
}