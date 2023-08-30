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
import static org.junit.Assert.assertNotEquals;
import org.apache.hop.expression.Identifier;
import org.junit.Test;

public class IdentifierTest extends ExpressionTest {
   
  @Test
  public void test() throws Exception {
    Identifier identifier1 = new Identifier(0, "FIELD_STRING");
    Identifier identifier2 = new Identifier(10, "FIELD_STRING");
    Identifier identifier3 = new Identifier("FIELD_STRING");
    identifier3.validate(this.createExpressionContext(true));
    assertEquals("FIELD_STRING", identifier1.getName());
    assertEquals(identifier1, identifier2);
    assertEquals(identifier1.hashCode(), identifier2.hashCode());
    assertNotEquals(identifier1,null);    
    assertNotEquals(identifier1,identifier3);
  }
  
  @Test
  public void eval() throws Exception {
    evalEquals("FIELD_INTEGER%2", 0L);
    evalEquals(" \t\n\"FIELD_INTEGER\"%2", 0L);
    evalEquals("\"IDENTIFIER SPACE\"", "SPACE");
    evalEquals("\"IDENTIFIER_UNDERSCORE\"", "UNDERSCORE");
    evalEquals("\"IDENTIFIER lower\"", "lower");
    
    evalFails("BIDON||'X'");
    evalFails("ABS(FIELD_STRING)");
    evalFails("SIN(FIELD_STRING)");
    evalFails("CAST(FIELD_STRING as INTEGER)");
  }  
  
  @Test
  public void escape() throws Exception {
    // Reserved word
    evalEquals("Upper(\"STRING\")", "PARIS");
    // Field name like a function name
    evalEquals("\"YEAR\"", 2020L);
  }
  
  @Test
  public void write() throws Exception {
    optimize("FIELD_STRING");
    // Reserved word
    optimize("\"CASE\"");    
    // Data type name
    optimize("\"STRING\"");
    // Time unit name
    optimize("\"CENTURY\"");
    // Function name
    optimize("\"YEAR\"");
    // Contains space
    optimize("Trim(\"IDENTIFIER SPACE\")","TRIM(\"IDENTIFIER SPACE\")");
  }
}


