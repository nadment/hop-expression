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
import static org.junit.Assert.assertThrows;
import org.apache.hop.expression.DataType;
import org.junit.Test;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;

public class DataTypeTest extends BaseExpressionTest {
    
  @Test
  public void of() throws Exception {
    assertEquals(DataType.BOOLEAN, DataType.of("BOOLEAN"));
    assertEquals(DataType.BOOLEAN, DataType.of("Boolean"));
    assertEquals(DataType.STRING, DataType.of("STRING") );
    assertEquals(DataType.STRING, DataType.of("String") );
    assertEquals(DataType.DATE, DataType.of("DATE") );
    assertEquals(DataType.NUMBER, DataType.of("NUMBER") );
    assertEquals(DataType.BIGNUMBER, DataType.of("BIGNUMBER") );
    assertEquals(DataType.BINARY, DataType.of("BINARY") );
    assertEquals(DataType.INTEGER, DataType.of("INTEGER") );
    assertThrows(IllegalArgumentException.class, () -> DataType.of("NOP") );
  }
  
  @Test
  public void from() throws Exception {
    assertEquals(DataType.UNKNOWN, DataType.from(null) );
    assertEquals(DataType.BOOLEAN, DataType.from(true));
    assertEquals(DataType.STRING, DataType.from("") );
    assertEquals(DataType.INTEGER, DataType.from(1L));
    assertEquals(DataType.NUMBER, DataType.from(1D));
    assertEquals(DataType.BIGNUMBER, DataType.from(BigDecimal.ONE) );
    assertEquals(DataType.BINARY, DataType.from(new byte[] {0x78}));
    assertEquals(DataType.DATE,
        DataType.from(ZonedDateTime.of(LocalDateTime.now(), ZoneId.of("Asia/Ho_Chi_Minh")))
        );
    
    assertThrows(IllegalArgumentException.class, () -> DataType.from(Float.class) );
    assertThrows(IllegalArgumentException.class, () -> DataType.from(Date.class) );
  }
  
  @Test
  public void javaClass() throws Exception {
    assertEquals(Boolean.class, DataType.BOOLEAN.getJavaClass() );
    assertEquals(Long.class, DataType.INTEGER.getJavaClass() );
    assertEquals(Double.class, DataType.NUMBER.getJavaClass() );
    assertEquals(BigDecimal.class, DataType.BIGNUMBER.getJavaClass() );
    assertEquals(String.class, DataType.STRING.getJavaClass() );
    assertEquals(ZonedDateTime.class, DataType.DATE.getJavaClass() );
    assertEquals(byte[].class, DataType.BINARY.getJavaClass() );
  }

}

