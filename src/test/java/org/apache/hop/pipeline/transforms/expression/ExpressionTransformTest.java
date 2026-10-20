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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;
import org.apache.hop.core.HopEnvironment;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.row.value.ValueMetaTimestamp;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.engines.local.LocalPipelineEngine;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Isolated;

@Isolated
class ExpressionTransformTest {

  /** Serves one input row and keeps the output row, without the row sets of a pipeline. */
  static final class OneRowExpressionTransform extends ExpressionTransform {
    private Object[] input;
    private Object[] output;

    OneRowExpressionTransform(
        PipelineMeta pipelineMeta, TransformMeta transformMeta, Object[] input) {
      super(
          transformMeta,
          (ExpressionMeta) transformMeta.getTransform(),
          new ExpressionData(),
          0,
          pipelineMeta,
          new LocalPipelineEngine(pipelineMeta));
      this.input = input;
    }

    @Override
    public Object[] getRow() {
      Object[] row = input;
      input = null;
      return row;
    }

    @Override
    public void putRow(IRowMeta rowMeta, Object[] row) {
      output = row;
    }
  }

  private static ExpressionField createField(String name, String expression) {
    return createField(name, expression, "Integer");
  }

  private static ExpressionField createField(String name, String expression, String type) {
    ExpressionField field = new ExpressionField();
    field.setName(name);
    field.setExpression(expression);
    field.setType(type);
    return field;
  }

  /** Creates the transform with the input row {@code A = 21}. */
  private static OneRowExpressionTransform createTransform(ExpressionField... fields)
      throws Exception {
    IRowMeta inputRowMeta = new RowMeta();
    inputRowMeta.addValueMeta(new ValueMetaInteger("A"));
    return createTransform(inputRowMeta, new Object[] {21L}, fields);
  }

  private static OneRowExpressionTransform createTransform(
      IRowMeta inputRowMeta, Object[] input, ExpressionField... fields) throws Exception {
    HopEnvironment.init();

    ExpressionMeta meta = new ExpressionMeta();
    meta.setDefault();
    meta.getFields().addAll(List.of(fields));

    // The transform must be in the pipeline before it is created
    PipelineMeta pipelineMeta = new PipelineMeta();
    TransformMeta transformMeta = new TransformMeta("expression", meta);
    pipelineMeta.addTransform(transformMeta);

    OneRowExpressionTransform transform =
        new OneRowExpressionTransform(pipelineMeta, transformMeta, input);
    transform.setInputRowMeta(inputRowMeta);
    assertTrue(transform.init());
    return transform;
  }

  @Test
  void replacedFieldIsReadWithItsInputType() throws Exception {
    IRowMeta inputRowMeta = new RowMeta();
    inputRowMeta.addValueMeta(new ValueMetaString("AMOUNT"));
    OneRowExpressionTransform transform =
        createTransform(
            inputRowMeta,
            new Object[] {"12.5"},
            createField("AMOUNT", "TO_NUMBER(AMOUNT)", "Number"));

    assertTrue(transform.processRow());
    assertEquals(0, transform.getErrors());
    assertEquals(12.5D, transform.output[0]);
  }

  @Test
  void resultOfAnotherExpressionIsNotAvailable() throws Exception {
    OneRowExpressionTransform transform =
        createTransform(createField("R", "A*2"), createField("S", "R+1"));

    // R is not an input field, the second expression doesn't compile
    assertFalse(transform.processRow());
    assertEquals(1, transform.getErrors());
  }

  @Test
  void replacedFieldKeepsItsInputValueForTheOtherExpressions() throws Exception {
    OneRowExpressionTransform transform =
        createTransform(createField("A", "A*2"), createField("B", "A+1"));

    assertTrue(transform.processRow());
    assertEquals(42L, transform.output[0]);
    assertEquals(22L, transform.output[1]);
  }

  @Test
  void fieldWithoutNameIsIgnored() throws Exception {
    OneRowExpressionTransform transform =
        createTransform(createField(null, "1+1"), createField("R", "A*2"));

    assertTrue(transform.processRow());
    assertEquals(0, transform.getErrors());
    assertEquals(42L, transform.output[1]);
  }

  @Test
  void fieldComputedTwiceFails() throws Exception {
    OneRowExpressionTransform transform =
        createTransform(createField("R", "A*2"), createField("R", "A*3"));

    assertFalse(transform.processRow());
    assertEquals(1, transform.getErrors());
  }

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
