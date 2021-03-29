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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertThrows;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.value.Value;
import org.apache.hop.expression.value.ValueBigNumber;
import org.apache.hop.expression.value.ValueBoolean;
import org.apache.hop.expression.value.ValueDate;
import org.apache.hop.expression.value.ValueInteger;
import org.apache.hop.expression.value.ValueNumber;
import org.apache.hop.expression.value.ValueString;
import org.junit.Test;
import java.math.BigDecimal;
import java.time.Instant;

public class ValueTest {

  @Test
  public void Null() throws Exception {
    assertTrue(Value.NULL.isNull());
    assertEquals(Value.NULL.getKind(),Kind.VALUE);
    assertNull(Value.NULL.getObject());
    assertNull(Value.NULL.toString());
    assertTrue(Value.NULL.equals(Value.NULL));
    assertTrue(Value.NULL.add(ValueNumber.ONE).isNull());
    assertTrue(Value.NULL.subtract(ValueNumber.ONE).isNull());
    assertTrue(Value.NULL.multiply(ValueNumber.ONE).isNull());
    assertTrue(Value.NULL.divide(ValueNumber.ONE).isNull());
    assertTrue(Value.NULL.remainder(ValueNumber.ONE).isNull());
    assertTrue(Value.NULL.power(ValueNumber.ONE).isNull());
  }

  @Test
  public void String() throws Exception {
    assertEquals(ValueString.of(null), Value.NULL);
    assertEquals(ValueString.of("Test").toString(), "Test");
    assertTrue(ValueString.of("Test").isString());
    assertThrows(ExpressionException.class, () -> { ValueString.of("Test").signum(); });
  }

  @Test
  public void Boolean() throws Exception {

    assertEquals(ValueBoolean.TRUE, ValueBoolean.of(true));
    assertEquals(ValueBoolean.FALSE, ValueBoolean.of(false));
    assertEquals(ValueBoolean.TRUE.toString(), "TRUE");
    assertEquals(ValueBoolean.FALSE.toString(), "FALSE");
    assertEquals(ValueBoolean.TRUE.getObject(), Boolean.TRUE);
    assertEquals(ValueBoolean.FALSE.getObject(), Boolean.FALSE);

    assertEquals(ValueBoolean.TRUE.toInteger(), 1L);
    assertEquals(ValueBoolean.FALSE.toInteger(), 0L);
    assertEquals(ValueBoolean.TRUE.toNumber(), 1D, 0.0000001);
    assertEquals(ValueBoolean.FALSE.toNumber(), 0D, 0.0000001);
    assertEquals(ValueBoolean.TRUE.toBigNumber(), BigDecimal.ONE);
    assertEquals(ValueBoolean.FALSE.toBigNumber(), BigDecimal.ZERO);
    assertThrows(ExpressionException.class, () -> {
      ValueBoolean.TRUE.toDate();
    });

    assertTrue(ValueBoolean.TRUE.isBoolean());
    assertTrue(ValueBoolean.FALSE.isBoolean());
    assertFalse(ValueBoolean.TRUE.isNull());
    assertFalse(ValueBoolean.FALSE.isNull());

    assertEquals(ValueBoolean.TRUE.negate(), ValueBoolean.FALSE);
    assertEquals(ValueBoolean.FALSE.negate(), ValueBoolean.TRUE);

    assertNotEquals(ValueBoolean.TRUE, ValueBoolean.FALSE);
    assertTrue(ValueBoolean.TRUE.equals(ValueBoolean.of(true)));
    assertTrue(ValueBoolean.TRUE.compare(ValueBoolean.FALSE) > 0);
    assertTrue(ValueBoolean.FALSE.compare(ValueBoolean.TRUE) < 0);
    assertTrue(ValueBoolean.FALSE.compare(ValueBoolean.FALSE) == 0);

  }

  @Test
  public void Integer() throws Exception {
    assertEquals(ValueInteger.of((Long) null), Value.NULL);
    assertEquals(ValueInteger.of(0L), ValueInteger.ZERO);
    assertEquals(ValueInteger.of(1L), ValueInteger.ONE);
    assertEquals(ValueInteger.of(Integer.valueOf(1)), ValueInteger.ONE);
    assertEquals(ValueInteger.of(2L), ValueInteger.of(2));
    assertEquals(ValueInteger.of(2L).toInteger(), 2L);
    assertEquals(ValueInteger.of(2L).toNumber(), 2D, 0.0000001);
    assertEquals(ValueInteger.of(2L).toBigNumber(), BigDecimal.valueOf(2));
    assertEquals(ValueInteger.of(-2L).toString(), "-2");
    assertFalse(ValueInteger.ZERO.toBoolean());
    assertTrue(ValueInteger.ONE.toBoolean());
    
    assertTrue(ValueInteger.ZERO.isNumeric());
    assertTrue(ValueInteger.ZERO.isInteger());
    assertFalse(ValueInteger.ZERO.isNumber());
    assertFalse(ValueInteger.ZERO.isBigNumber());    

    assertTrue(ValueInteger.of(123).equals(ValueInteger.of(123)));
    assertTrue(ValueInteger.ONE.compare(ValueInteger.ZERO) > 0);
    assertEquals(ValueInteger.ONE.negate(),ValueInteger.of(-1));    
    assertTrue(ValueInteger.ONE.add(Value.NULL).isNull());
    assertTrue(ValueInteger.ONE.subtract(Value.NULL).isNull());
    assertTrue(ValueInteger.ONE.multiply(Value.NULL).isNull());
    assertTrue(ValueInteger.ONE.divide(Value.NULL).isNull());
    assertTrue(ValueInteger.ONE.remainder(Value.NULL).isNull());

    assertThrows(ExpressionException.class, () -> { ValueInteger.of(Long.MIN_VALUE).negate(); });
    assertThrows(ExpressionException.class, () -> { ValueInteger.ONE.divide(ValueInteger.ZERO); });
    assertThrows(ExpressionException.class, () -> { ValueInteger.ONE.remainder(ValueInteger.ZERO); });
    
    assertNotEquals(ValueInteger.ZERO, ValueBigNumber.ZERO);
  }

  @Test
  public void Number() throws Exception {
    assertEquals(ValueNumber.of(null), Value.NULL);
    assertEquals(ValueNumber.of(0D), ValueNumber.ZERO);
    assertEquals(ValueNumber.of(1D), ValueNumber.ONE);
    assertEquals(ValueNumber.of(Math.PI), ValueNumber.PI);
    assertFalse(ValueNumber.ZERO.toBoolean());
    assertTrue(ValueNumber.ONE.toBoolean());
    assertEquals(ValueNumber.of(123.456).toInteger(), 123);
    assertEquals(ValueNumber.of(123.456).toNumber(), 123.456, 0.0000001);
    // assertEquals(ValueNumber.ZERO.toBigNumber(), BigDecimal.ZERO);
    // assertEquals(ValueNumber.ONE.toBigNumber(), BigDecimal.ONE);
    //assertEquals(ValueNumber.of(-0.1230).toString(), "-.123");
    assertEquals(ValueNumber.of(-2.1230).toString(), "-2.123");
    
    assertFalse(ValueNumber.ZERO.isInteger());
    assertTrue(ValueNumber.ZERO.isNumber());
    assertTrue(ValueNumber.ZERO.isNumeric());
    assertFalse(ValueNumber.ZERO.isBigNumber());
    
    assertTrue(ValueNumber.of(123.456).equals(ValueNumber.of(123.456)));
    assertTrue(ValueNumber.ONE.compare(ValueNumber.ZERO) > 0);
    assertTrue(ValueNumber.of(-2.1230).add(Value.NULL).isNull());
    assertEquals(ValueNumber.of(-2.1230).add(ValueInteger.of(1)).toNumber(), -1.123, 0.0000001);
    assertEquals(ValueNumber.of(-2.1230).subtract(ValueInteger.of(1)).toNumber(), -3.123, 0.0000001);

    
    assertTrue(ValueNumber.ONE.add(Value.NULL).isNull());
    assertTrue(ValueNumber.ONE.subtract(Value.NULL).isNull());
    assertTrue(ValueNumber.ONE.multiply(Value.NULL).isNull());
    assertTrue(ValueNumber.ONE.divide(Value.NULL).isNull());
    assertTrue(ValueNumber.ONE.remainder(Value.NULL).isNull());
    
    assertThrows(ExpressionException.class, () -> { ValueNumber.ONE.divide(ValueNumber.ZERO); });
    assertThrows(ExpressionException.class, () -> { ValueNumber.ONE.remainder(ValueNumber.ZERO); });

    
  }

  @Test
  public void BigNumber() throws Exception {
    assertEquals(ValueBigNumber.of(null), Value.NULL);
    
    assertEquals(ValueBigNumber.of(BigDecimal.ZERO), ValueBigNumber.ZERO);
    assertEquals(ValueBigNumber.of(BigDecimal.ONE), ValueBigNumber.ONE);
    assertEquals(ValueBigNumber.of(1L), ValueBigNumber.ONE);
    //assertEquals(ValueBigNumber.of(1.0D), ValueBigNumber.ONE);
    assertEquals(ValueBigNumber.of(BigDecimal.valueOf(2)),
        ValueBigNumber.of(BigDecimal.valueOf(2)));
    assertFalse(ValueBigNumber.ZERO.toBoolean());
    assertTrue(ValueBigNumber.ONE.toBoolean());
    assertEquals(ValueBigNumber.ZERO.toBigNumber(), BigDecimal.ZERO);
    assertEquals(ValueBigNumber.ONE.toBigNumber(), BigDecimal.ONE);
    assertNotEquals(ValueNumber.ZERO, ValueBigNumber.ZERO);
    assertNotEquals(ValueBigNumber.ZERO, ValueBigNumber.ONE);

    assertTrue(ValueBigNumber.ZERO.isBigNumber());
    assertTrue(ValueBigNumber.ZERO.isNumeric());

    assertTrue(ValueBigNumber.of(BigDecimal.valueOf(123E-28))
        .equals(ValueBigNumber.of(BigDecimal.valueOf(123E-28))));
    assertTrue(ValueBigNumber.ONE.compare(ValueBigNumber.ZERO) > 0);
    assertEquals(ValueBigNumber.ONE.negate(),ValueBigNumber.of(-1));
    assertTrue(ValueBigNumber.ONE.add(Value.NULL).isNull());
    // assertEquals(ValueBigNumber.ZERO.add(ValueNumber.of(5D)),ValueBigNumber.of(5));
    
    assertThrows(ExpressionException.class, () -> { ValueBigNumber.ONE.divide(ValueBigNumber.ZERO); });
    assertThrows(ExpressionException.class, () -> { ValueBigNumber.ONE.remainder(ValueBigNumber.ZERO); });

  }

  @Test
  public void Date() throws Exception {
    assertEquals(ValueDate.of(null), Value.NULL);
    assertThrows(ExpressionException.class, () -> { ValueDate.of(Instant.now()).signum(); });
  }
}

