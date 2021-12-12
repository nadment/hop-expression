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
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.optimizer.Optimizer;
import org.apache.hop.junit.rules.RestoreHopEnvironment;
import org.junit.ClassRule;
import org.junit.Test;

public class OptimizerTest {

  @ClassRule
  public static RestoreHopEnvironment env = new RestoreHopEnvironment();

  protected IExpression optimize(String e) throws Exception {
    ExpressionContext context = new ExpressionContext(new Variables());

    IExpression expression = ExpressionParser.parse(e);

    Optimizer optimizer = new Optimizer();
    IExpression optimized = optimizer.optimize(context, expression);

    int gain = expression.getCost() - optimized.getCost();

    System.out.println("(" + gain + ") optimize(" + e + ")[" + expression.getCost()
        + "] >>> optimized(" + optimized + ")[" + optimized.getCost() + "]");
    return optimized;
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
  public void simplifyInRule() throws Exception {
    optimize("FIELD in (\"FIELD\", FIELD,1,2,1,null,FIELD,null)", "FIELD IN (1,2,NULL,FIELD)");
  }

  @Test
  public void simplifyLikeRule() throws Exception {
    optimizeNull("FIELD LIKE NULL");
    optimizeNull("NULL LIKE FIELD");
    optimize("FIELD LIKE 'Hello'", "FIELD='Hello'");
    optimize("FIELD LIKE 'H%'", "STARTSWITH(FIELD,'H')");
    optimize("FIELD LIKE '%o'", "ENDSWITH(FIELD,'o')");
    optimize("FIELD LIKE '%Hello%'", "CONTAINS(FIELD,'Hello')");
  }

  @Test
  public void simplifyExtractRule() throws Exception {
    optimize("EXTRACT(YEAR FROM OrderDate)", "YEAR(OrderDate)");
    optimize("EXTRACT(MONTH FROM OrderDate)", "MONTH(OrderDate)");
    optimize("EXTRACT(QUARTER FROM OrderDate)", "QUARTER(OrderDate)");
    optimize("EXTRACT(DAY FROM OrderDate)", "DAY(OrderDate)");
    optimize("EXTRACT(HOUR FROM OrderDate)", "HOUR(OrderDate)");
    optimize("EXTRACT(MINUTE FROM OrderDate)", "MINUTE(OrderDate)");
    optimize("EXTRACT(SECOND FROM OrderDate)", "SECOND(OrderDate)");
    optimize("EXTRACT(WEEK FROM OrderDate)", "WEEK(OrderDate)");
    optimize("EXTRACT(DAYOFYEAR FROM OrderDate)", "DAYOFYEAR(OrderDate)");
    optimize("EXTRACT(DAYOFWEEK FROM OrderDate)", "DAYOFWEEK(OrderDate)");
  }

  @Test
  public void simplifyBooleanRule() throws Exception {
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
    optimizeTrue("\"FIELD\" or true");
    optimizeTrue("true or FIELD");
    optimize("FIELD or FIELD", "FIELD");

    optimizeTrue("true and true");
    optimizeFalse("true and false");
    optimizeFalse("false and true");
    optimizeFalse("false and false");
    optimizeNull("true and null");
    optimizeNull("null and true");
    optimize("FIELD and FIELD", "FIELD");
  }

  @Test
  public void deterministicRule() throws Exception {


    optimize("CONCAT('TES','T')", "'TEST'");

    optimize("'A'||'B'", "'AB'");

    optimize("3+1", "4");
    optimize("3+1+2", "6");
    optimize("3+1*2", "5");
    optimize("(3+1)*2", "8");
    optimize("-(10+2)", "-12");
    optimize("-(0)", "0");

    optimize("not ( not \"FIELD\" is NULL )");
    optimize("-(-\"FIELD\")");
    optimize("false and true or \"FIELD\"");
    optimize("false and \"FIELD\"");

    optimizeTrue("null is null");
    optimizeTrue("true is true");
    optimizeTrue("false is false");
    optimizeFalse("true is false");
    optimizeFalse("true is null");
    optimizeFalse("false is null");
    optimizeFalse("null is true");
    optimizeFalse("null is false");

    optimizeTrue("25>=12");
    optimizeTrue("25>=12 and 14<15");

    optimize("AGE between 3 and (5+1)");
    optimizeFalse("2 between 3 and (5+1)");


    optimize("null=null");
    optimizeTrue("'25' in ('1','25','66')");
    optimizeTrue("25.8 between 18 and 32");
    optimizeTrue("Trim(' test ')='test'");
    optimize("DayOfMonth(Date '2019-02-15')", "15");
    optimize("DayOfMonth(Date(2019,2,15))", "15");
  }

  @Test
  public void combineConcatsRule() throws Exception {
    // Same syntax but cost reduced
    optimize("'A'||FIELD1||FIELD2||'C'", "'A'||FIELD1||FIELD2||'C'");
    optimize("CONCAT('A',CONCAT(FIELD1,CONCAT(FIELD2,'C')))", "'A'||FIELD1||FIELD2||'C'");
  }

  @Test
  public void permutationRule() throws Exception {
    // optimize("1+AGE+3+FIELD+5","8+AGE+FIELD");
    optimize("AGE+3+1", "4+AGE");
    // TODO: optimize("4*AGE*0.5","2.0*AGE");
  }
}
