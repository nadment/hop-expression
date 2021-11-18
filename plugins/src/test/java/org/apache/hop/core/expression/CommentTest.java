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
import org.apache.hop.expression.Literal;
import org.junit.Test;

public class CommentTest extends BaseExpressionTest {



  @Test
  public void String() throws Exception {
    assertEquals("Test", Literal.of("Test").eval(null));
    assertEquals(Literal.of("Test"), Literal.of("Test"));

    // Single line comment
    evalTrue("// Single line comment\nTrue");
    evalTrue("-- Single lioe comment\nTrue");
    
    // Multi line comment
    evalTrue("/* Line 1\n * Line 2 */ True");
  }


}

