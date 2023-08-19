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
import org.apache.hop.expression.Call;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.Identifier;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.operator.ConcatFunction;
import org.junit.Test;

public class SyntaxTest extends ExpressionTest { 

  @Test
  public void EmptyOrNull() throws Exception {
    // Empty source return NULL
    evalNull("");
    evalNull(" ");
    evalNull("\t");
    evalNull("\n");
        
    evalFails(null);
  }
    
  @Test
  public void Comment() throws Exception {
    evalTrue(" // Test line comment \n  true ");
    evalTrue(" /* Test block comment */  true ");
    evalTrue(" true /* Test block comment */");
    evalTrue("/*\n * Comment on multi line\n *\n */ True");
    evalTrue("/*\n * Comment on multi line \n  with nesting: /* nested block comment */ *\n */   True");

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
  public void Tab() throws Exception {
    evalTrue(" \n\tTrue");
    evalTrue(" \nTrue\t\r");
  }
  
  @Test
  public void CarriageReturnAndLineFeed() throws Exception {
    evalTrue(" \rTrue");
    evalTrue(" \n\tTrue");
    evalTrue(" \nTrue\n\r");
  }
   
  @Test
  public void Operator() throws Exception {
    assertEquals("Mathematical", Operators.MULTIPLY.getCategory());
    assertEquals(Operators.CONCAT, new ConcatFunction("||"));
    
    // Primary operator first and alias last
    assertTrue(Operators.CONCAT.compareTo(new ConcatFunction())>0);
    assertEquals(51, Operators.MULTIPLY.getLeftPrecedence());
    assertEquals(50, Operators.MULTIPLY.getRightPrecedence());   
    assertEquals("CONCAT", Operators.CONCAT.toString());
    assertNotEquals(Operators.CONCAT, Operators.EQUAL);
    assertTrue(Operators.CONCAT.is(FunctionRegistry.getFunction("CONCAT")));
    assertFalse(Operators.CONCAT.is(null));
    assertFalse(Operators.CONCAT.isAggregate());
    assertNotNull(Operators.CONCAT.getDescription());
    assertNotEquals(Operators.CONCAT, null);
    //assertNotNull(Operators.CONCAT.getDocumentation());
    assertNotNull(Operators.CONCAT.getDocumentationUrl());
    
    assertTrue(FunctionRegistry.getFunction("TRUNCATE").is(FunctionRegistry.getFunction("TRUNC")));
    assertTrue(FunctionRegistry.getFunction("COUNT").isAggregate());
  }

  @Test
  public void as() throws Exception {
    assertTrue(optimize("ABS(FIELD_INTEGER)").asCall() instanceof Call);
    assertTrue(optimize("FIELD_INTEGER").asIdentifier() instanceof Identifier);
    assertTrue(optimize("123").asLiteral() instanceof Literal);   
        
    assertThrows(UnsupportedOperationException.class, () -> optimize("123").asCall());
    assertThrows(UnsupportedOperationException.class, () -> optimize("ABS(FIELD_INTEGER)").asIdentifier());
    assertThrows(UnsupportedOperationException.class, () -> optimize("FIELD_INTEGER").asLiteral());
  }
  
  @Test
  public void precedenceAndAssociativity() throws Exception {

    // Arithmetic
    evalEquals("3*5/2",  3*5/2d);
    evalEquals("9/3*3", 9L/3L*3L);
    evalEquals("1 + 2 * 3 * 4 + 5", 1 + 2 * 3 * 4 + 5L);        
    evalEquals("1-2+3*4/5/6-7", 1 - 2 + 3 * 4d / 5d / 6d - 7);
    evalEquals("10*2+1", 21L);
    evalEquals("8+5-2*8", 8+5-2*8L);
    evalEquals("8+(5-2)*8",8L+(5-2)*8L);
    evalEquals("1+10*2", 1+10*2L);
    evalEquals("10*(2+1)", 30L);
    evalEquals("30/(5+5)", 3L);
    evalEquals("42%(3+2)", 2L);
    evalEquals("1-2+3*4/5/6-7", (((1d - 2d) + (((3d * 4d) / 5d) / 6d)) - 7d));
    evalEquals("FIELD_INTEGER-(10+3*10+50-2*25)", 0L);
      
    // NOT has higher precedence than AND, which has higher precedence than OR
    evalTrue("NOT false AND NOT false");
    evalTrue("NOT 5 = 5 OR NOT 'Test' = 'X' AND NOT 5 = 4");

    // Equals (=) has higher precedence than NOT "NOT (1=1)"
    evalTrue("NOT 2 = 1");

    // IS NULL has higher precedence than NOT
    evalFalse("NOT NULL_BOOLEAN IS NULL");

    // IS NULL has lower precedence than comparison (1 = 1) IS NULL
    evalFalse("1 = 1 is null");
    evalTrue(" 3 > 5 IS FALSE");

    // BETWEEN, IN, LIKE have higher precedence than comparison
    //evalFalse("5 between 4>=4 and 6<=6");
    
    // The cast operator has higher precedence than the unary minus (negation) operator,
    // so the statement is interpreted as -(0.0::NUMBER::BOOLEAN)
    evalFails("-0.0::NUMBER::BOOLEAN");    
    evalFalse("(-0.0::NUMBER)::BOOLEAN");
  }

  @Test
  public void SyntaxError() throws Exception {
    
    // Single quote for string
    evalFails("'T'||'T");
    
    // Double quote for identifier
    evalFails("\"T\"||\"T");
    evalFails("\"");
    evalFails(" \" ");    
    evalFails(" \"");
    evalFails("\"\"");
    
    evalFails("9!7");
    evalFails("9+(");
    evalFails("9+*(");
    evalFails("9:");
    evalFails("*9");
    evalFails("DATE '2023-01-01'||'X'");
    evalFails("Date ");
    evalFails("Timestamp ");
    evalFails("Extract(");
    evalFails("3*(1+2");
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
    evalFails("5 BETWEEN  AND 7");
    evalFails("5 BETWEEN 4 OR 6");
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
    
    assertThrows(ExpressionException.class, () -> eval("0xABCDEFg"));
  }


}