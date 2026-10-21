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

package org.apache.hop.pipeline.transforms.where;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import org.apache.hop.core.HopEnvironment;
import org.apache.hop.core.IRowSet;
import org.apache.hop.core.QueueRowSet;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.pipeline.PipelineHopMeta;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.engines.local.LocalPipelineEngine;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.apache.hop.pipeline.transforms.dummy.DummyMeta;
import org.junit.jupiter.api.Test;

class WhereTest {

  /** Serves the input rows and keeps the rows sent to each row set, without a pipeline. */
  static final class TestWhere extends Where {
    private final Deque<Object[]> input;
    private final IRowSet trueTarget = new QueueRowSet();
    private final IRowSet falseTarget = new QueueRowSet();
    private final List<IRowSet> rowSets = new ArrayList<>();
    private final List<Object[]> mainOutputRows = new ArrayList<>();

    TestWhere(PipelineMeta pipelineMeta, TransformMeta transformMeta, List<Object[]> input) {
      super(
          transformMeta,
          (WhereMeta) transformMeta.getTransform(),
          new WhereData(),
          0,
          pipelineMeta,
          new LocalPipelineEngine(pipelineMeta));
      this.input = new ArrayDeque<>(input);
    }

    @Override
    public Object[] getRow() {
      return input.poll();
    }

    @Override
    public IRowSet findOutputRowSet(String from, int fromCopy, String to, int toCopy) {
      return switch (to) {
        case "true" -> trueTarget;
        case "false" -> falseTarget;
        default -> null;
      };
    }

    @Override
    public void putRowTo(IRowMeta rowMeta, Object[] row, IRowSet rowSet) {
      rowSets.add(rowSet);
    }

    @Override
    public void putRow(IRowMeta rowMeta, Object[] row) {
      mainOutputRows.add(row);
    }
  }

  /**
   * Creates the transform with the condition {@code A > 10}, with or without TRUE and FALSE
   * targets, and the input rows of the field {@code A}.
   */
  private static TestWhere createTransform(boolean withTargets, Long... values) throws Exception {
    HopEnvironment.init();

    WhereMeta meta = new WhereMeta();
    meta.setCondition("A > 10");
    if (withTargets) {
      meta.setTrueTransformName("true");
      meta.setFalseTransformName("false");
    }

    // The transform must be in the pipeline before it is created
    PipelineMeta pipelineMeta = new PipelineMeta();
    TransformMeta transformMeta = new TransformMeta("where", meta);
    TransformMeta trueMeta = new TransformMeta("true", new DummyMeta());
    TransformMeta falseMeta = new TransformMeta("false", new DummyMeta());
    pipelineMeta.addTransform(transformMeta);
    pipelineMeta.addTransform(trueMeta);
    pipelineMeta.addTransform(falseMeta);
    meta.searchInfoAndTargetTransforms(pipelineMeta.getTransforms());

    List<Object[]> rows = new ArrayList<>();
    for (Long value : values) {
      rows.add(new Object[] {value});
    }
    IRowMeta inputRowMeta = new RowMeta();
    inputRowMeta.addValueMeta(new ValueMetaInteger("A"));
    TestWhere transform = new TestWhere(pipelineMeta, transformMeta, rows);
    transform.setInputRowMeta(inputRowMeta);
    assertTrue(transform.init());

    // The hops are added after the creation of the transform, which looks up their row sets
    pipelineMeta.addPipelineHop(new PipelineHopMeta(transformMeta, trueMeta));
    pipelineMeta.addPipelineHop(new PipelineHopMeta(transformMeta, falseMeta));
    return transform;
  }

  @Test
  void rowsAreSentToTrueAndFalseTargets() throws Exception {
    TestWhere transform = createTransform(true, 20L, 5L);

    assertTrue(transform.processRow());
    assertTrue(transform.processRow());

    assertEquals(0, transform.getErrors());
    assertEquals(0, transform.mainOutputRows.size());
    assertEquals(2, transform.rowSets.size());
    assertSame(transform.trueTarget, transform.rowSets.get(0));
    assertSame(transform.falseTarget, transform.rowSets.get(1));
  }

  @Test
  void nullConditionIsFalse() throws Exception {
    TestWhere transform = createTransform(true, (Long) null);

    assertTrue(transform.processRow());

    assertEquals(0, transform.getErrors());
    assertEquals(1, transform.rowSets.size());
    assertSame(transform.falseTarget, transform.rowSets.getFirst());
  }

  @Test
  void rowsWhereConditionIsTrueAreSentToMainOutputWithoutTarget() throws Exception {
    TestWhere transform = createTransform(false, 20L, 5L, null);

    assertTrue(transform.processRow());
    assertTrue(transform.processRow());
    assertTrue(transform.processRow());

    // Only the first row is kept, the false and NULL conditions are filtered
    assertEquals(0, transform.getErrors());
    assertEquals(0, transform.rowSets.size());
    assertEquals(1, transform.mainOutputRows.size());
    assertEquals(20L, transform.mainOutputRows.getFirst()[0]);
  }
}
