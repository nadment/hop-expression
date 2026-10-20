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

package org.apache.hop.pipeline.transforms.route;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
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
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.engines.local.LocalPipelineEngine;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.junit.jupiter.api.Test;

class RouteTransformTest {

  /** Serves the input rows and keeps the rows sent to each row set, without a pipeline. */
  static final class TestRouteTransform extends RouteTransform {
    private final Deque<Object[]> input;
    private final IRowSet target = new QueueRowSet();
    private final List<IRowSet> rowSets = new ArrayList<>();

    TestRouteTransform(
        PipelineMeta pipelineMeta, TransformMeta transformMeta, List<Object[]> input) {
      super(
          transformMeta,
          (RouteMeta) transformMeta.getTransform(),
          new RouteData(),
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
    public IRowSet findOutputRowSet(String targetTransform) {
      return "target".equals(targetTransform) ? target : null;
    }

    @Override
    public void putRowTo(IRowMeta rowMeta, Object[] row, IRowSet rowSet) {
      rowSets.add(rowSet);
    }
  }

  /** Creates the transform with one route, and the input rows of the field {@code A}. */
  private static TestRouteTransform createTransform(String targetTransformName, Long... values)
      throws Exception {
    HopEnvironment.init();

    Route route = new Route();
    route.setCondition("A > 10");
    route.setTransformName(targetTransformName);
    RouteMeta meta = new RouteMeta();
    meta.getRoutes().add(route);

    // The transform must be in the pipeline before it is created
    PipelineMeta pipelineMeta = new PipelineMeta();
    TransformMeta transformMeta = new TransformMeta("route", meta);
    pipelineMeta.addTransform(transformMeta);

    List<Object[]> rows = new ArrayList<>();
    for (Long value : values) {
      rows.add(new Object[] {value});
    }
    IRowMeta inputRowMeta = new RowMeta();
    inputRowMeta.addValueMeta(new ValueMetaInteger("A"));
    TestRouteTransform transform = new TestRouteTransform(pipelineMeta, transformMeta, rows);
    transform.setInputRowMeta(inputRowMeta);
    assertTrue(transform.init());
    return transform;
  }

  @Test
  void missingTargetTransformStopsWithError() throws Exception {
    TestRouteTransform transform = createTransform("unknown", 20L);

    // The error is logged, no exception is thrown
    assertFalse(transform.processRow());
    assertEquals(1, transform.getErrors());
    assertEquals(0, transform.rowSets.size());
  }

  @Test
  void rowMatchingNoConditionIsIgnoredWithoutDefaultTarget() throws Exception {
    TestRouteTransform transform = createTransform("target", 5L, 20L);

    assertTrue(transform.processRow());
    assertTrue(transform.processRow());

    // Only the second row is sent, to the target of the route
    assertEquals(0, transform.getErrors());
    assertEquals(1, transform.rowSets.size());
    assertSame(transform.target, transform.rowSets.getFirst());
  }
}
