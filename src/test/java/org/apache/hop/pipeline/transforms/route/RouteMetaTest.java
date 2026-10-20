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
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.apache.hop.pipeline.transform.TransformSerializationTestUtil;
import org.apache.hop.pipeline.transform.stream.IStream;
import org.apache.hop.pipeline.transforms.dummy.DummyMeta;
import org.junit.jupiter.api.Test;

public class RouteMetaTest {

  @Test
  public void testSerialization() throws Exception {
    RouteMeta meta =
        TransformSerializationTestUtil.testSerialization("/route-transform.xml", RouteMeta.class);

    Route route = meta.getRoutes().getFirst();
    assertEquals("FIELD_BOOLEAN is TRUE", route.getCondition());
    assertEquals("Condition 1", route.getTransformName());
    assertEquals(4, meta.getRoutes().size());
    assertEquals("Condition default", meta.getDefaultTargetTransformName());
  }

  @Test
  public void testCopy() {
    Route route = new Route();
    route.setCondition("A > 10");
    route.setTransformName("target");
    RouteMeta meta = new RouteMeta();
    meta.getRoutes().add(route);
    meta.setDefaultTargetTransformName("default");

    RouteMeta copy = new RouteMeta(meta);

    assertEquals(1, copy.getRoutes().size());
    assertNotSame(route, copy.getRoutes().getFirst());
    assertEquals("A > 10", copy.getRoutes().getFirst().getCondition());
    assertEquals("target", copy.getRoutes().getFirst().getTransformName());
    assertEquals("default", copy.getDefaultTargetTransformName());
    // The source is not modified
    assertEquals(1, meta.getRoutes().size());
  }

  @Test
  public void testCheckWithoutDefaultTarget() {
    PipelineMeta pipelineMeta = new PipelineMeta();
    pipelineMeta.addTransform(new TransformMeta("target", new DummyMeta()));

    Route route = new Route();
    route.setCondition("1 = 1");
    route.setTransformName("target");
    RouteMeta meta = new RouteMeta();
    meta.getRoutes().add(route);

    List<ICheckResult> remarks = new ArrayList<>();
    meta.check(
        remarks,
        pipelineMeta,
        null,
        new RowMeta(),
        new String[] {"input"},
        null,
        null,
        new Variables(),
        null);

    // The default target is optional
    assertEquals(0, remarks.size());
  }

  @Test
  public void testConvertIOMetaToTransformNames() {
    RouteMeta meta = new RouteMeta();
    for (String transformName : new String[] {"a", null, "c"}) {
      Route route = new Route();
      route.setCondition("1 = 1");
      route.setTransformName(transformName);
      meta.getRoutes().add(route);
    }
    meta.setDefaultTargetTransformName("d");

    // The target transforms of the route 3 and of the default target are renamed
    List<IStream> streams = meta.getTransformIOMeta().getTargetStreams();
    assertEquals(4, streams.size());
    streams.get(2).setTransformMeta(new TransformMeta("c2", new DummyMeta()));
    streams.get(3).setTransformMeta(new TransformMeta("d2", new DummyMeta()));

    meta.convertIOMetaToTransformNames();

    // A route without target transform still has its stream
    assertEquals("c2", meta.getRoutes().get(2).getTransformName());
    assertEquals("d2", meta.getDefaultTargetTransformName());
  }

  @Test
  public void testHandleNewTargetStream() {
    RouteMeta meta = new RouteMeta();
    IStream newTargetStream = meta.getOptionalStreams().getFirst();
    newTargetStream.setTransformMeta(new TransformMeta("target", new DummyMeta()));

    meta.handleStreamSelection(newTargetStream);

    assertEquals(1, meta.getRoutes().size());
    assertEquals("target", meta.getRoutes().getFirst().getTransformName());
    // The description of the new stream gives the number of the route
    String description = meta.getTransformIOMeta().getTargetStreams().getLast().getDescription();
    assertTrue(description.contains(" 1 "), description);
  }

  @Test
  public void testCheckConditionNotBoolean() {
    PipelineMeta pipelineMeta = new PipelineMeta();
    pipelineMeta.addTransform(new TransformMeta("target", new DummyMeta()));

    RouteMeta meta = new RouteMeta();
    // A Boolean, a number converted to a Boolean, and a string that can fail to be converted
    for (String condition : new String[] {"1 = 1", "1", "'abc'"}) {
      Route route = new Route();
      route.setCondition(condition);
      route.setTransformName("target");
      meta.getRoutes().add(route);
    }

    List<ICheckResult> remarks = new ArrayList<>();
    meta.check(
        remarks,
        pipelineMeta,
        null,
        new RowMeta(),
        new String[] {"input"},
        null,
        null,
        new Variables(),
        null);

    // Only the string condition of the route 3 is reported
    assertEquals(1, remarks.size());
    assertEquals(ICheckResult.TYPE_RESULT_ERROR, remarks.getFirst().getType());
    assertTrue(remarks.getFirst().getText().startsWith("Route 3 "));
    assertTrue(remarks.getFirst().getText().contains("STRING"));
  }
}
