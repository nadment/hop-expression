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

public class BaseOptimizerTest {

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
  public void test() throws Exception {
    //optimize("1+AGE+3"); // TODO: operator swap
    optimize("EXTRACT(YEAR FROM OrderDate)", "YEAR(OrderDate)");
    // optimize("AGE+3+1");
  }
}
