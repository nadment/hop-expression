/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.hop.pipeline.transforms.aggregate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Date;
import java.util.Deque;
import java.util.List;
import org.apache.hop.core.Const;
import org.apache.hop.core.HopEnvironment;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.engines.local.LocalPipelineEngine;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.junit.jupiter.api.Test;

class AggregateTransformTest {

  /** Serves the input rows and keeps the output rows and the errors, without a pipeline. */
  static final class TestAggregate extends AggregateTransform {
    private final Deque<Object[]> input;
    private final List<Object[]> outputRows = new ArrayList<>();
    private final List<String> errors = new ArrayList<>();
    private boolean outputDone;

    TestAggregate(
        PipelineMeta pipelineMeta,
        TransformMeta transformMeta,
        LocalPipelineEngine pipeline,
        List<Object[]> input) {
      super(
          transformMeta,
          (AggregateMeta) transformMeta.getTransform(),
          new AggregateData(),
          0,
          pipelineMeta,
          pipeline);
      this.input = new ArrayDeque<>(input);
    }

    @Override
    public Object[] getRow() {
      return input.poll();
    }

    @Override
    public void putRow(IRowMeta rowMeta, Object[] row) {
      outputRows.add(row);
    }

    @Override
    public void logError(String message) {
      errors.add(message);
    }

    @Override
    public void setOutputDone() {
      outputDone = true;
    }
  }

  private static AggregateField aggregate(String name, String expression) {
    AggregateField field = new AggregateField();
    field.setName(name);
    field.setExpression(expression);
    return field;
  }

  /** Creates and initializes the transform with its input rows. */
  private static TestAggregate create(
      AggregateMeta meta, IRowMeta inputRowMeta, List<Object[]> rows) throws Exception {
    HopEnvironment.init();

    PipelineMeta pipelineMeta = new PipelineMeta();
    TransformMeta transformMeta = new TransformMeta("aggregate", meta);
    pipelineMeta.addTransform(transformMeta);
    TestAggregate transform =
        new TestAggregate(pipelineMeta, transformMeta, new LocalPipelineEngine(pipelineMeta), rows);
    transform.setInputRowMeta(inputRowMeta);
    assertTrue(transform.init());
    return transform;
  }

  /** Returns an aggregate transform counting the rows, which always gives back a result row. */
  private static AggregateMeta countAlwaysGivingBackOneRow() {
    AggregateMeta meta = new AggregateMeta();
    meta.setDefault();
    meta.setAlwaysGivingBackOneRow(true);
    meta.getAggregateFields().add(aggregate("COUNT", "COUNT(*)"));
    return meta;
  }

  private static IRowMeta integerField(String name) {
    IRowMeta rowMeta = new RowMeta();
    rowMeta.addValueMeta(new ValueMetaInteger(name));
    return rowMeta;
  }

  @Test
  void noInputRowGivesBackOneRow() throws Exception {
    TestAggregate transform =
        create(countAlwaysGivingBackOneRow(), integerField("A"), new ArrayList<>());

    assertFalse(transform.processRow());

    assertEquals(1, transform.outputRows.size());
    assertEquals(0L, transform.outputRows.getFirst()[0]);
  }

  @Test
  void batchCompleteDoesNotAddEmptyRow() throws Exception {
    List<Object[]> rows = new ArrayList<>();
    rows.add(new Object[] {1L});
    TestAggregate transform = create(countAlwaysGivingBackOneRow(), integerField("A"), rows);

    // The batch writes its group, the end of the input finds an empty map
    assertTrue(transform.processRow());
    transform.batchComplete();
    assertFalse(transform.processRow());

    assertEquals(1, transform.outputRows.size());
    assertEquals(1L, transform.outputRows.getFirst()[0]);
  }

  @Test
  void aggregateNamedLikeGroupField() throws Exception {
    AggregateMeta meta = new AggregateMeta();
    meta.setDefault();
    meta.getGroupFields().add(new GroupField("D"));
    meta.getAggregateFields().add(aggregate("D", "COUNT(*)"));

    IRowMeta inputRowMeta = new RowMeta();
    inputRowMeta.addValueMeta(new ValueMetaDate("D"));
    Date date = new Date(0);
    List<Object[]> rows = new ArrayList<>();
    rows.add(new Object[] {date});
    TestAggregate transform = create(meta, inputRowMeta, rows);

    // The aggregate is written with its own type, not the type of the group field
    assertTrue(transform.processRow());
    assertFalse(transform.processRow());

    assertEquals(0, transform.getErrors());
    assertEquals(1, transform.outputRows.size());
    assertEquals(date, transform.outputRows.getFirst()[0]);
    assertEquals(1L, transform.outputRows.getFirst()[1]);
  }

  @Test
  void errorWhileCalculatingAggregate() throws Exception {
    AggregateMeta meta = new AggregateMeta();
    meta.setDefault();
    meta.getAggregateFields().add(aggregate("TOTAL", "SUM(TO_NUMBER(S))"));

    IRowMeta inputRowMeta = new RowMeta();
    inputRowMeta.addValueMeta(new ValueMetaString("S"));
    List<Object[]> rows = new ArrayList<>();
    rows.add(new Object[] {"abc"});
    TestAggregate transform = create(meta, inputRowMeta, rows);

    // The error stops the transform, its message is logged without placeholder
    assertFalse(transform.processRow());
    assertEquals(1, transform.getErrors());
    assertTrue(transform.outputDone);
    assertEquals(1, transform.errors.size());
    String error = transform.errors.getFirst();
    assertFalse(error.contains("{"), error);

    // The message identifies the aggregate with its field and its expression
    assertTrue(error.endsWith(": TOTAL=SUM(TO_NUMBER(S))"), error);
  }

  @Test
  void errorWhileCreatingAggregate() throws Exception {
    AggregateMeta meta = new AggregateMeta();
    meta.setDefault();
    meta.getAggregateFields().add(aggregate("P25", "PERCENTILE(A, 25)"));

    List<Object[]> rows = new ArrayList<>();
    rows.add(new Object[] {1L});
    TestAggregate transform = create(meta, integerField("A"), rows);

    // The percentage is out of range, only the message of the error is logged
    assertFalse(transform.processRow());
    assertEquals(1, transform.getErrors());
    assertEquals(1, transform.errors.size());
    String error = transform.errors.getFirst();
    assertTrue(error.startsWith("Argument 2 is out of range"), error);
    assertTrue(error.endsWith(": P25=PERCENTILE(A, 25)"), error);
  }

  @Test
  void allNullsAreZeroOnlyForNumbersAndStrings() throws Exception {
    HopEnvironment.init();

    AggregateMeta meta = new AggregateMeta();
    meta.setDefault();
    meta.getAggregateFields().add(aggregate("TOTAL", "SUM(A)"));
    meta.getAggregateFields().add(aggregate("FIRST", "MIN(D)"));

    PipelineMeta pipelineMeta = new PipelineMeta();
    TransformMeta transformMeta = new TransformMeta("aggregate", meta);
    pipelineMeta.addTransform(transformMeta);
    LocalPipelineEngine pipeline = new LocalPipelineEngine(pipelineMeta);
    pipeline.setVariable(Const.HOP_AGGREGATION_ALL_NULLS_ARE_ZERO, "Y");

    IRowMeta inputRowMeta = new RowMeta();
    inputRowMeta.addValueMeta(new ValueMetaInteger("A"));
    inputRowMeta.addValueMeta(new ValueMetaDate("D"));
    List<Object[]> rows = new ArrayList<>();
    rows.add(new Object[] {null, null});
    TestAggregate transform = new TestAggregate(pipelineMeta, transformMeta, pipeline, rows);
    transform.setInputRowMeta(inputRowMeta);
    assertTrue(transform.init());

    assertTrue(transform.processRow());
    assertFalse(transform.processRow());

    // The NULL sum is zero, a date has no zero value and stays NULL
    assertEquals(0, transform.getErrors());
    assertEquals(1, transform.outputRows.size());
    assertEquals(BigDecimal.ZERO, transform.outputRows.getFirst()[0]);
    assertNull(transform.outputRows.getFirst()[1]);
  }

  @Test
  void groupFieldNotFound() throws Exception {
    HopEnvironment.init();

    AggregateMeta meta = new AggregateMeta();
    meta.setDefault();
    meta.getGroupFields().add(new GroupField("UNKNOWN"));
    meta.getAggregateFields().add(aggregate("TOTAL", "SUM(A)"));

    PipelineMeta pipelineMeta = new PipelineMeta();
    TransformMeta transformMeta = new TransformMeta("aggregate", meta);
    pipelineMeta.addTransform(transformMeta);

    IRowMeta inputRowMeta = new RowMeta();
    inputRowMeta.addValueMeta(new ValueMetaInteger("A"));
    List<Object[]> rows = new ArrayList<>();
    rows.add(new Object[] {1L});
    TestAggregate transform =
        new TestAggregate(pipelineMeta, transformMeta, new LocalPipelineEngine(pipelineMeta), rows);
    transform.setInputRowMeta(inputRowMeta);
    assertTrue(transform.init());

    assertFalse(transform.processRow());

    // The error names the field, the message is found in the resources
    assertEquals(1, transform.getErrors());
    assertEquals(List.of("Grouping field [UNKNOWN] couldn't be found!"), transform.errors);
    assertTrue(transform.outputDone);
  }
}
