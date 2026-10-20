/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.apache.hop.pipeline.transforms.expression;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;
import java.util.TimeZone;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaTimestamp;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.RowExpressionContext;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Isolated;

@Isolated
class ExpressionTransformTest {

  @Test
  void dateAndTimestampFieldsAreWrittenUnchanged() throws Exception {
    // The default time zone is global, restore it so that the other tests are not affected
    TimeZone defaultTimeZone = TimeZone.getDefault();
    try {
      // A time zone ahead of UTC, where an instant read in UTC would shift the date
      ZoneId paris = ZoneId.of("Europe/Paris");
      TimeZone.setDefault(TimeZone.getTimeZone(paris));

      IRowMeta rowMeta = new RowMeta();
      rowMeta.addValueMeta(new ValueMetaDate("FIELD_DATE"));
      rowMeta.addValueMeta(new ValueMetaTimestamp("FIELD_TIMESTAMP"));
      RowExpressionContext context = new RowExpressionContext(new Variables(), rowMeta);

      Date date = Date.from(LocalDate.of(2019, 4, 23).atStartOfDay(paris).toInstant());
      Timestamp timestamp = Timestamp.valueOf(LocalDateTime.of(2019, 4, 23, 10, 30, 15, 123));
      context.setRow(new Object[] {date, timestamp});

      IExpression dateExpression = IExpression.of(context, "FIELD_DATE");
      IExpression timestampExpression = IExpression.of(context, "FIELD_TIMESTAMP");

      assertEquals(date, ExpressionTransform.processValue(dateExpression, rowMeta.getValueMeta(0)));
      assertEquals(
          timestamp,
          ExpressionTransform.processValue(timestampExpression, rowMeta.getValueMeta(1)));
    } finally {
      TimeZone.setDefault(defaultTimeZone);
    }
  }
}
