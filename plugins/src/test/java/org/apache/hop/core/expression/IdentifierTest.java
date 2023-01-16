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
import org.apache.hop.expression.Identifier;
import org.junit.Test;

public class IdentifierTest extends BaseExpressionTest {
 
  @SuppressWarnings("unlikely-arg-type")
  @Test
  public void test() throws Exception {
    Identifier identifier = new Identifier("NAME");
    assertEquals("NAME", identifier.getName());
    assertFalse(identifier.equals("NAME"));
    assertEquals(new Identifier("NAME"), identifier);
    assertEquals(new Identifier("NAME").hashCode(), identifier.hashCode());
    assertNotEquals(identifier,null);
  }
  
  @Test
  public void eval() throws Exception {
    evalEquals("FIELD_INTEGER%2", 0);
    evalEquals(" \t\n\"FIELD_INTEGER\"%2", 0);
    evalEquals("\"IDENTIFIER SPACE\"", "SPACE");
    evalEquals("\"IDENTIFIER_UNDERSCORE\"", "UNDERSCORE");
    evalEquals("\"IDENTIFIER lower\"", "lower");

    evalFails("\"");
    evalFails(" \" ");    
    evalFails(" \"");
    evalFails("\"\"");
  }  
  
  @Test
  public void escape() throws Exception {
    // Reserved word
    evalEquals("Upper(\"FROM\")", "PARIS");
    // DatePart
    evalEquals("\"YEAR\"", 2020);
  }
  
  @Test
  public void write() throws Exception {
    writeEquals("IDENTIFIER");
    // Reserved word
    writeEquals("\"CASE\"");
    writeEquals("\"LIKE\"");
    // Data type name
    writeEquals("\"NUMBER\"");
    // Date part name
    writeEquals("\"CENTURY\"");
    // Function name
    writeEquals("\"YEAR\"");
    writeEquals("\"UPPER\"");
    // Contains space
    writeEquals("Trim(\"IDENTIFIER SPACE\")","TRIM(\"IDENTIFIER SPACE\")");
  }
}


