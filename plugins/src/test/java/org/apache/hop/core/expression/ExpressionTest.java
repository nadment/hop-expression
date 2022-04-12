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
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.hop.expression.DatePart;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.operator.Concat;
import org.junit.Test;
import java.math.BigDecimal;
import java.math.MathContext;

public class ExpressionTest extends BaseExpressionTest { 

  @Test
  public void Empty() throws Exception {
    evalNull("");
    evalNull(" ");
    evalNull("\t");
    evalNull("\n");    
  }
    
  @Test
  public void Comment() throws Exception {
    evalTrue(" // Test line comment \n  true ");
    evalTrue(" /* Test block comment */  true ");
    evalTrue(" true /* Test block comment */");
    evalTrue("/*\n * Comment on multi line\n *\n */ True");
    evalTrue(
        "/*\n * Comment on multi line \n  with nesting: /* nested block comment */ *\n */   True");

    // Single line comment
    evalTrue("// Single line comment\nTrue");
    evalTrue("-- Single line comment\nTrue");
    evalTrue("-- Single line comment\rTrue");

    // Multi line comment
    evalTrue("/* Line 1\n * Line 2 */ True");

    // Empty
    evalNull("-- Single line comment\n");
    
    // Syntax error
    evalFails(" /");
    evalFails("/*   True");
    evalFails("/   True");
    evalFails("/*   True*");
    evalFails("/* /* nested block comment */    True");
  }

  @Test
  public void CarriageReturnAndLineFeed() throws Exception {
    evalTrue(" \rTrue");
    evalTrue(" \nTrue");
  }
  
  @Test
  public void DatePart() throws Exception {
    assertTrue(DatePart.exist("MONTH"));
    assertEquals(DatePart.HOUR, DatePart.of("HOUR"));
    assertEquals(DatePart.DECADE, DatePart.of("DECADE"));
    assertEquals(DatePart.CENTURY, DatePart.of("CENTURY"));
    assertEquals(DatePart.QUARTER, DatePart.of("quarter"));
    assertEquals(DatePart.DAY, DatePart.of("d"));
    assertEquals(DatePart.DAY, DatePart.of("dd"));
    assertEquals(DatePart.DAY, DatePart.of("dayofmonth"));
    assertEquals(DatePart.HOUR, DatePart.of("HOUR"));
    assertEquals(DatePart.HOUR, DatePart.of("HH"));
    assertNotEquals(DatePart.HOUR, DatePart.MINUTE);
    assertNotEquals(DatePart.of("HOUR"), null);
    assertThrows(IllegalArgumentException.class, () -> DatePart.of("NOP"));
  }

  @Test
  public void Operator() throws Exception {
    assertEquals("Mathematical", Operators.MULTIPLY.getCategory());
    assertEquals(Operators.CONCAT, new Concat());
    assertNotEquals(Operators.CONCAT, Operators.EQUAL);
    assertTrue(Operators.CONCAT.is(FunctionRegistry.getFunction("CONCAT")));
    assertFalse(Operators.CONCAT.is(null));
    assertNotNull(Operators.CONCAT.getDescription());
    assertNotEquals(Operators.CONCAT, null);
    assertNotEquals(Operators.CONCAT, FunctionRegistry.getFunction("CONCAT"));

    // FIXME: Don't work on github
    // assertNotNull(OperatorRegistry.CONCAT.getDocumentation());
    assertNotNull(Operators.CONCAT.getDocumentationUrl());
    assertTrue(
        FunctionRegistry.getFunction("TRUNCATE").is(FunctionRegistry.getFunction("TRUNC")));
  }

  @Test
  public void precedenceAndAssociativity() throws Exception {

    assertEquals(51, Operators.MULTIPLY.getLeftPrecedence());
    assertEquals(50, Operators.MULTIPLY.getRightPrecedence());

    // Arithmetic
    evalEquals("3*5/2", ((3 * 5) / 2d));
    evalEquals("9/3*3", (9 / 3) * 3);
    evalEquals("1 + 2 * 3 * 4 + 5", ((1 + ((2 * 3) * 4)) + 5));
    evalEquals("1-2+3*4/5/6-7", 1 - 2 + 3 * 4d / 5d / 6d - 7);
    evalEquals("10*2+1", 21);
    evalEquals("1+10*2", 21);
    evalEquals("10*(2+1)", 30);
    evalEquals("30/(5+5)", 3);
    evalEquals("42%(3+2)", 2);
    evalEquals("1-2+3*4/5/6-7", (((1d - 2d) + (((3d * 4d) / 5d) / 6d)) - 7d));
    evalEquals("Age-(10+3*10+50-2*25)", 0);

      
    // NOT has higher precedence than AND, which has higher precedence than OR
    evalTrue("NOT false AND NOT false");
    evalTrue("NOT 5 = 5 OR NOT 'Test' = 'X' AND NOT 5 = 4");

    // Equals (=) has higher precedence than NOT "NOT (1=1)"
    evalTrue("NOT 2 = 1");

    // IS NULL has higher precedence than NOT
    evalFalse("NOT VALUE_NULL IS NULL");

    // IS NULL has lower precedence than comparison (1 = 1) IS NULL
    evalFalse("1 = 1 is null");
    evalTrue(" 3 > 5 IS FALSE");

    // BETWEEN, IN, LIKE have higher precedence than comparison
    // evalFalse("5 between 4>=4 and 6<=6");
  }

  @Test
  public void SyntaxError() throws Exception {
    evalFails("'T'||'T");
    evalFails("\"T\"||\"T");
    evalFails("9!7");
    evalFails("9+(");
    evalFails("9+*(");
    evalFails("9:");
    evalFails("Left||'X'");
   // evalFails("Date ");
    evalFails("Extract(");
    evalFails(")+1");
    evalFails("'missing end");
    evalFails("Year(");
    evalFails("Year(2020");
    evalFails("Year)");
    evalFails("Year()");
    evalFails("Year(()");
    evalFails("Today())");
    evalFails("Date '2022-05-01' AT TIME");
    evalFails("Date '2022-05-01' AT TIME ZONE");
    evalFails("Date '2022-05-01' AT TIME ZONE 'XYZ'");
    evalFails("Year(1,2)");
    evalFails("TRUE AND");
    evalFails("5 BETWEEN 4 AND");
    evalFails("5 BETWEEN 4 OR");
    evalFails("case when 1=1 then 1 else 0");
    evalFails("case when 1=1 then 1 else  end ");
    evalFails("case 1 when 1  else 0 end");
    evalFails("Cast(3 as NILL)");
    evalFails("Cast(3 as )");
    evalFails("Cast(3 as");
    evalFails("1 in ()    ");
    evalFails("1 in (,2,3)");
    evalFails("1 in (1,2,3");
    evalFails("1 in (1,,3)");
    evalFails("1 in (1,2,)");
    evalFails("0xABCDEFg");
    evalFails("Date '2020-20-28'");
  }

  @Test
  public void CoercionImplicit() throws Exception {
    // Coercion Number to Boolean
    evalTrue("true = 1");
    evalTrue("false = 0");
    evalTrue("true OR 0");
    evalFalse("false AND 0");
    
    // String to Number
    evalEquals("2*'1.23'", 2.46);
        
    // Integer to BigNumber
    evalEquals("'-1e-3'::BigNumber * 2", new BigDecimal("-2e-3", MathContext.DECIMAL128));    
    // Number to BigNumber
    evalEquals("'-1e-3'::BigNumber * 0.5", new BigDecimal("-5e-4", MathContext.DECIMAL128));
    
    evalEquals(" 4 + 4 || '2' ", "82");    
    evalEquals(" '8' || 1 + 1", 82);
  }

  @Test
  public void CoercionExplicit() throws Exception {
    // Cast String to Boolean
    evalTrue("'1'::Boolean=true");
    evalTrue("'On'::Boolean=true");
    evalTrue("'Y'::Boolean=true");
    evalTrue("true = 'Y'::Boolean");
    evalTrue("'Yes'::Boolean=true");
    evalTrue("true = 'Yes'::Boolean");
    evalTrue("'T'::Boolean=true");
    evalTrue("'TRUE'::Boolean=true");
    evalTrue("true = 'True'::Boolean");

    evalTrue("'0'::Boolean=false");
    evalTrue("'N'::Boolean=false");
    evalTrue("'NO'::Boolean=false");
    evalTrue("'OFF'::Boolean=false");
    evalTrue("'F'::Boolean=false");
    evalTrue("'FALSE'::Boolean=false");
    
    // String to BigNumber
    evalEquals("' -1e-3 '::BigNumber", new BigDecimal("-1e-3", MathContext.DECIMAL128));    

  }
}


