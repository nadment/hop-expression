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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaBigNumber;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaNumber;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.ExpressionBuilder;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.junit.rules.RestoreHopEnvironment;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.ExternalResource;

public class OptimizerTest {

  @ClassRule
  public static RestoreHopEnvironment env = new RestoreHopEnvironment();

  @ClassRule
  public static ExternalResource getResource() {
      return new ExternalResource() {
          @Override
          protected void before() throws Throwable {
            FunctionRegistry.registerBuilInFunctions();
          }
      };
  }
  
  protected IExpression optimize(String e) throws Exception {
    IRowMeta rowMeta = new RowMeta();
    rowMeta.addValueMeta(new ValueMetaString("NAME"));
    rowMeta.addValueMeta(new ValueMetaString("SEX"));
    rowMeta.addValueMeta(new ValueMetaDate("BIRTHDATE"));
    rowMeta.addValueMeta(new ValueMetaInteger("AGE"));
    rowMeta.addValueMeta(new ValueMetaDate("DN"));
    rowMeta.addValueMeta(new ValueMetaBoolean("FLAG"));
    rowMeta.addValueMeta(new ValueMetaBoolean("VALUE_NULL"));
    rowMeta.addValueMeta(new ValueMetaInteger("YEAR"));
    rowMeta.addValueMeta(new ValueMetaString("FROM"));
    rowMeta.addValueMeta(new ValueMetaNumber("PRICE"));
    rowMeta.addValueMeta(new ValueMetaBigNumber("AMOUNT"));
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER SPACE"));
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER_UNDERSCORE"));
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER lower"));

    IExpressionContext context = new ExpressionContext(new Variables(), rowMeta);
    IExpression expression = ExpressionBuilder.compile(context, e);

    System.out.println("optimize (" + e + ") cost=" + expression.getCost() + " >>> " + expression);
    
    return expression;
  }

  protected void optimize(String e, String expected) throws Exception {
    assertEquals(expected, (String) optimize(e).toString());
  }

  protected void optimizeTrue(String e) throws Exception {
    assertTrue((Boolean) optimize(e).eval(null));
  }

  protected void optimizeFalse(String e) throws Exception {
    assertFalse((Boolean) optimize(e).eval(null));
  }

  protected void optimizeNull(String e) throws Exception {
    assertNull(optimize(e).eval(null));
  }

  @Test
  public void testSimplifyInRule() throws Exception {
    optimize("FIELD in (\"FIELD\", FIELD,1,2,1,null,FIELD,null)", "FIELD IN (1,2,FIELD)");
    optimizeNull("NULL in (\"FIELD\", FIELD,1,2,1,null,FIELD,null)");
  }

  @Test
  public void testSimplifyLikeRule() throws Exception {
    optimizeNull("FIELD LIKE NULL");
    optimizeNull("NULL LIKE FIELD");
    optimize("FIELD LIKE 'Hello'", "FIELD='Hello'");
    optimize("FIELD LIKE 'H%'", "STARTSWITH(FIELD,'H')");
    optimize("FIELD LIKE '%o'", "ENDSWITH(FIELD,'o')");
    optimize("FIELD LIKE '%Hello%'", "CONTAINS(FIELD,'Hello')");
  }

  @Test
  public void testSimplifyExtractRule() throws Exception {
    optimize("EXTRACT(YEAR FROM OrderDate)", "YEAR(OrderDate)");
    optimize("EXTRACT(ISOYEAR FROM OrderDate)", "ISOYEAR(OrderDate)");
    optimize("EXTRACT(MONTH FROM OrderDate)", "MONTH(OrderDate)");
    optimize("EXTRACT(QUARTER FROM OrderDate)", "QUARTER(OrderDate)");
    optimize("EXTRACT(DAY FROM OrderDate)", "DAY(OrderDate)");
    optimize("EXTRACT(HOUR FROM OrderDate)", "HOUR(OrderDate)");
    optimize("EXTRACT(MINUTE FROM OrderDate)", "MINUTE(OrderDate)");
    optimize("EXTRACT(SECOND FROM OrderDate)", "SECOND(OrderDate)");
    optimize("EXTRACT(WEEK FROM OrderDate)", "WEEK(OrderDate)");
    optimize("EXTRACT(ISOWEEK FROM OrderDate)", "ISOWEEK(OrderDate)");
    optimize("EXTRACT(DAYOFYEAR FROM OrderDate)", "DAYOFYEAR(OrderDate)");
    optimize("EXTRACT(DAYOFWEEK FROM OrderDate)", "DAYOFWEEK(OrderDate)");
    optimize("EXTRACT(ISODAYOFWEEK FROM OrderDate)", "ISODAYOFWEEK(OrderDate)");
  }

  @Test
  public void testSimplifyBooleanRule() throws Exception {
    optimizeFalse("not true");
    optimizeTrue("not false");
    optimizeTrue("not not true");
    optimizeFalse("not not false");
    optimize("not(FIELD>5)", "FIELD<=5");
    optimize("not(FIELD>=5)", "FIELD<5");
    optimize("not(FIELD<5)", "FIELD>=5");
    optimize("not(FIELD<=5)", "FIELD>5");

    optimizeTrue("true or true");
    optimizeTrue("true or false");
    optimizeTrue("false or true");
    optimizeFalse("false or false");
    optimizeTrue("true or null");
    optimizeTrue("null or true");
    optimizeNull("null or null");
    optimizeTrue("FIELD or true");
    optimize("FIELD or false", "FIELD");
    optimizeTrue("true or FIELD");
    optimize("FIELD or FIELD", "FIELD");

    optimizeTrue("true and true");
    optimizeFalse("true and false");
    optimizeFalse("false and true");
    optimizeFalse("false and false");
    optimizeNull("true and null");
    optimizeNull("null and true");
    optimize("FIELD and FIELD", "FIELD");
    
    // TODO: optimize("(A IS NOT NULL OR B) AND A IS NOT NULL","A IS NOT NULL");
  }

  @Test
  public void testDeterministicRule() throws Exception {
    optimize("CONCAT('TES','T')", "'TEST'");

    optimize("'A'||'B'", "'AB'");

    optimize("3+1+1+1+1+1+1+1+1+1+1+1", "14");
    optimize("3+1+2", "6");
    optimize("3+1*2", "5");
    optimize("(3+1)*2", "8");
    optimize("-(10+2)", "-12");
    optimize("-(0)", "0");

    optimize("NOT (FIELD IS TRUE)", "FIELD IS FALSE");
    optimize("NOT (FIELD IS NOT TRUE)", "FIELD IS TRUE");
    optimize("NOT (FIELD IS FALSE)", "FIELD IS TRUE");
    optimize("NOT (FIELD IS NOT FALSE)", "FIELD IS FALSE");
    optimize("NOT (FIELD IS NOT NULL)", "FIELD IS NULL");
    optimize("NOT (FIELD IS NULL)", "FIELD IS NOT NULL");

    optimize("-(-FIELD)", "FIELD");
    optimize("false and true or FIELD", "FIELD");
    optimizeFalse("false and FIELD");

    optimizeTrue("null is null");
    optimizeTrue("true is true");
    optimizeTrue("false is false");
    optimizeFalse("true is false");
    optimizeTrue("false is not true");
    optimizeTrue("true is not false");
    optimizeFalse("true is null");
    optimizeFalse("false is null");
    optimizeFalse("null is true");
    optimizeFalse("null is false");

    optimizeTrue("25>=12");
    optimizeTrue("25>=12 and 14<15");

    optimize("AGE between 3 and (5+1)");
    optimizeFalse("2 between 3 and (5+1)");

    optimize("Cast('2021-02-08' as DATE)", "DATE '2021-02-08'");

    optimize("null=null");
    optimizeTrue("'25' in ('1','25','66')");
    optimizeTrue("25.8 between 18 and 32");
    optimizeTrue("Trim(' test ')='test'");
    optimize("Day(Date '2019-02-15')", "15");
    optimize("Day(Date(2019,2,15))", "15");
  }

  @Test
  public void testCombineConcatsRule() throws Exception {
    // Same syntax but cost reduced
    optimize("'A'||FIELD1||FIELD2||'C'", "'A'||FIELD1||FIELD2||'C'");
    optimize("'A'||FIELD1||NULL||'C'", "'A'||FIELD1||'C'");
    optimize("CONCAT('A',CONCAT(FIELD1,CONCAT(FIELD2,'C')||'D'))", "'A'||FIELD1||FIELD2||'C'||'D'");
    optimize("NULL||CONCAT(FIELD1,NULL)","FIELD1");
  }

  @Test
  public void testArithmeticRule() throws Exception {

    optimize("AGE+0", "AGE");
    optimize("0+AGE", "AGE");
    optimize("AGE-0", "AGE");
    optimize("0-AGE", "-AGE");
    optimize("Z-(0-AGE)", "AGE+Z");
    optimize("AGE*1", "AGE");
    optimize("1.0*AGE", "AGE");

    optimize("AGE*3*2", "6*AGE");
    optimize("3*(AGE*1)*1*(2*5)", "30*AGE");
    optimize("1+AGE+3+FIELD+5*2", "14+AGE+FIELD");
    optimize("AGE+3+1", "4+AGE");
    optimize("4+AGE+1", "5+AGE");
    optimize("4*AGE*0.5", "2.0*AGE");
    optimize("AGE/1", "AGE");
    optimize("AGE/1.0", "AGE");
  }

  @Test
  public void testChainedCast() throws Exception {
    //optimize("CAST(CAST(CAST(123456 AS INTEGER) AS NUMBER) AS BIGNUMBER)", "123456");
  }
}
