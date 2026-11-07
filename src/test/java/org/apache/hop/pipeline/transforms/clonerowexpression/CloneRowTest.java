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

package org.apache.hop.pipeline.transforms.clonerowexpression;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import org.apache.hop.core.HopEnvironment;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaNumber;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.engines.local.LocalPipelineEngine;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.junit.jupiter.api.Test;

class CloneRowTest {

  /** Serves the input rows and keeps the output rows, without a pipeline. */
  static final class TestCloneRow extends CloneRow {
    private final Deque<Object[]> input;
    private final List<Object[]> output = new ArrayList<>();

    TestCloneRow(PipelineMeta pipelineMeta, TransformMeta transformMeta, List<Object[]> input) {
      super(
          transformMeta,
          (CloneRowMeta) transformMeta.getTransform(),
          new CloneRowData(),
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
    public void putRow(IRowMeta rowMeta, Object[] row) {
      output.add(row);
    }
  }

  /**
   * Creates the transform with an expression of the number of clones, and a row with the integer
   * field {@code COUNT} at 2 and the number field {@code AMOUNT} at 2.7.
   */
  private static TestCloneRow createTransform(String nrClones) throws Exception {
    HopEnvironment.init();

    CloneRowMeta meta = new CloneRowMeta();
    meta.setNrClones(nrClones);

    // The transform must be in the pipeline before it is created
    PipelineMeta pipelineMeta = new PipelineMeta();
    TransformMeta transformMeta = new TransformMeta("clone", meta);
    pipelineMeta.addTransform(transformMeta);

    IRowMeta inputRowMeta = new RowMeta();
    inputRowMeta.addValueMeta(new ValueMetaInteger("COUNT"));
    inputRowMeta.addValueMeta(new ValueMetaNumber("AMOUNT"));
    List<Object[]> rows = new ArrayList<>();
    rows.add(new Object[] {2L, 2.7D});

    TestCloneRow transform = new TestCloneRow(pipelineMeta, transformMeta, rows);
    transform.setInputRowMeta(inputRowMeta);
    assertTrue(transform.init());
    return transform;
  }

  @Test
  void clonesTheRowWithAnIntegerNumberOfClones() throws Exception {
    TestCloneRow transform = createTransform("COUNT");
    transform.processRow();

    // The original row and its 2 clones
    assertEquals(3, transform.output.size());
  }

  @Test
  void failsWithAnInvalidNumberOfClones() throws Exception {
    // The message gives the expression that can't be compiled
    TestCloneRow transform = createTransform("COUNT +");
    HopException exception = assertThrows(HopException.class, transform::processRow);
    assertTrue(
        exception.getMessage().contains("Unable to compile expression 'COUNT +'"),
        exception.getMessage());
  }

  @Test
  void failsWithANumberOfClonesOfTypeNumber() throws Exception {
    // A number is not coerced to an integer, 2.7 would be truncated to 2 clones
    TestCloneRow transform = createTransform("AMOUNT");
    HopException exception = assertThrows(HopException.class, transform::processRow);
    assertTrue(exception.getMessage().contains("NUMBER"), exception.getMessage());
    assertEquals(0, transform.output.size());

    // Cast to an integer, the number is rounded to 3 clones
    transform = createTransform("CAST(AMOUNT AS INTEGER)");
    transform.processRow();
    assertEquals(4, transform.output.size());
  }
}
