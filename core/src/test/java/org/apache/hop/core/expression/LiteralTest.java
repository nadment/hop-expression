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
import static org.junit.Assert.assertNull;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.junit.Test;
import java.math.BigDecimal;
import java.time.Instant;

public class LiteralTest {

  @Test
  public void Null() throws Exception {
    assertEquals(Kind.LITERAL, Literal.UNKNOWN.getKind());
    assertEquals(Literal.UNKNOWN, Literal.of(null));
    assertEquals(Literal.UNKNOWN, new Literal(null));
    assertNull(Literal.UNKNOWN.eval(null));
  }

  @Test
  public void String() throws Exception {
    assertEquals("Test", Literal.of("Test").eval(null));
    assertEquals(Literal.of("Test"), Literal.of("Test"));
  }

  @Test
  public void Boolean() throws Exception {
    assertEquals(Boolean.TRUE, Literal.TRUE.eval(null));
    assertEquals(Boolean.FALSE, Literal.FALSE.eval(null));
    assertEquals(Literal.TRUE, Literal.of(true));
    assertEquals(Literal.FALSE, Literal.of(false));

    assertEquals("TRUE", Operator.coerceToString(Literal.TRUE));
    assertEquals("FALSE",Operator.coerceToString(Literal.FALSE));
  }

  @Test
  public void Integer() throws Exception {
    assertEquals(Literal.ZERO, Literal.of(0L));
    assertEquals(Literal.ONE, Literal.of(1L));
    assertEquals("-123456", Literal.of(-123456L).toString());
  }

  @Test
  public void Number() throws Exception {
    assertEquals(Literal.ZERO, Literal.of(0D));
    assertEquals(Literal.ONE, Literal.of(1D));
    assertEquals(Math.PI, Literal.PI.eval(null));
    assertEquals("-123456.789", Literal.of(-123456.789D).toString());
  }

  @Test
  public void BigNumber() throws Exception {
    assertEquals(Literal.ZERO, Literal.of(BigDecimal.ZERO));
    assertEquals(Literal.ONE, Literal.of(BigDecimal.ONE));
    assertEquals("-123456.789", Literal.of(BigDecimal.valueOf(-123456.789)).toString());
  }

  @Test
  public void Date() throws Exception {
    Instant instant = Instant.ofEpochMilli(0);
    assertEquals(instant, Literal.of(instant).eval(null));
    assertEquals(Literal.of(instant), Literal.of(instant));   
  }
}

