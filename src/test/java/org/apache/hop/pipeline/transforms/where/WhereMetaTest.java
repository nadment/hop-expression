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

package org.apache.hop.pipeline.transforms.where;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import org.apache.hop.core.HopEnvironment;
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.core.xml.XmlHandler;
import org.apache.hop.metadata.serializer.memory.MemoryMetadataProvider;
import org.apache.hop.pipeline.PipelineHopMeta;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.apache.hop.pipeline.transform.TransformSerializationTestUtil;
import org.apache.hop.pipeline.transform.stream.IStream;
import org.apache.hop.pipeline.transforms.dummy.DummyMeta;
import org.junit.jupiter.api.Test;

public class WhereMetaTest {

  /** Selects the target of a stream for a new hop, like Hop GUI. */
  private static void addHop(
      PipelineMeta pipelineMeta, TransformMeta from, TransformMeta to, int streamIndex) {
    IStream stream = from.getTransform().getTransformIOMeta().getTargetStreams().get(streamIndex);
    stream.setTransformMeta(to);
    from.getTransform().handleStreamSelection(stream);
    pipelineMeta.addPipelineHop(new PipelineHopMeta(from, to));
  }

  @Test
  public void testTargetsAddedInGuiAreSavedAndKept() throws Exception {
    HopEnvironment.init();
    WhereMeta meta = new WhereMeta();
    meta.setCondition("1=1");
    PipelineMeta pipelineMeta = new PipelineMeta();
    TransformMeta where = new TransformMeta("where", meta);
    TransformMeta trueTarget = new TransformMeta("true", new DummyMeta());
    TransformMeta falseTarget = new TransformMeta("false", new DummyMeta());
    pipelineMeta.addTransform(where);
    pipelineMeta.addTransform(trueTarget);
    pipelineMeta.addTransform(falseTarget);
    addHop(pipelineMeta, where, trueTarget, 0);
    addHop(pipelineMeta, where, falseTarget, 1);

    // The targets are saved
    String xml = pipelineMeta.getXml(new Variables());
    assertTrue(xml.contains("<send_true_to>true</send_true_to>"));
    assertTrue(xml.contains("<send_false_to>false</send_false_to>"));

    // The targets are kept when Hop GUI looks up the transforms again
    meta.searchInfoAndTargetTransforms(pipelineMeta.getTransforms());
    assertSame(
        trueTarget, meta.getTransformIOMeta().findTargetStream(trueTarget).getTransformMeta());
    assertSame(
        falseTarget, meta.getTransformIOMeta().findTargetStream(falseTarget).getTransformMeta());

    // The targets follow a renamed transform
    falseTarget.setName("renamed");
    assertEquals("renamed", meta.getFalseTransformName());

    // The targets are found when the pipeline is loaded
    PipelineMeta loaded =
        new PipelineMeta(
            XmlHandler.loadXmlString(xml, PipelineMeta.XML_TAG), new MemoryMetadataProvider());
    WhereMeta loadedMeta = (WhereMeta) loaded.findTransform("where").getTransform();
    assertNotNull(loadedMeta.getTransformIOMeta().findTargetStream(loaded.findTransform("true")));
    assertNotNull(loadedMeta.getTransformIOMeta().findTargetStream(loaded.findTransform("false")));
  }

  @Test
  public void testTargetRemovedWithTransform() {
    WhereMeta meta = new WhereMeta();
    meta.setFalseTransformName("false");
    TransformMeta falseTarget = new TransformMeta("false", new DummyMeta());

    meta.searchInfoAndTargetTransforms(List.of(falseTarget));
    assertSame(falseTarget, meta.getTransformIOMeta().getTargetStreams().get(1).getTransformMeta());

    // The transform is deleted
    meta.searchInfoAndTargetTransforms(List.of());
    assertNull(meta.getTransformIOMeta().getTargetStreams().get(1).getTransformMeta());
  }

  @Test
  public void testSerialization() throws Exception {
    WhereMeta meta =
        TransformSerializationTestUtil.testSerialization("/where-transform.xml", WhereMeta.class);

    assertEquals("Day(FIELD_DATE)%2=1", meta.getCondition());
  }

  /** Returns the remarks of the check about the type of the condition. */
  private static List<ICheckResult> checkType(String condition) {
    IRowMeta prev = new RowMeta();
    prev.addValueMeta(new ValueMetaString("NAME"));
    WhereMeta meta = new WhereMeta();
    meta.setCondition(condition);

    List<ICheckResult> remarks = new ArrayList<>();
    meta.check(
        remarks, null, null, prev, new String[] {"input"}, null, null, new Variables(), null);
    return remarks.stream().filter(remark -> remark.getText().contains("BOOLEAN")).toList();
  }

  @Test
  public void testCheckConditionNotBoolean() {
    // A Boolean and a number converted to a Boolean are valid
    assertEquals(0, checkType("NAME = 'abc'").size());
    assertEquals(0, checkType("1").size());

    // A string can fail to be converted to a Boolean
    List<ICheckResult> remarks = checkType("NAME");
    assertEquals(1, remarks.size());
    assertEquals(ICheckResult.TYPE_RESULT_ERROR, remarks.getFirst().getType());
    assertTrue(remarks.getFirst().getText().contains("STRING"));
  }

  @Test
  public void testCheckWithoutInputFields() {
    WhereMeta meta = new WhereMeta();
    meta.setCondition("NAME = 'abc'");

    // The check does not fail without the fields of the input stream
    List<ICheckResult> remarks = new ArrayList<>();
    meta.check(remarks, null, null, null, new String[0], null, null, new Variables(), null);

    assertTrue(
        remarks.stream()
            .anyMatch(
                remark -> remark.getText().equals("No input received from other transforms")));
  }

  @Test
  public void testCheckWithoutTarget() {
    IRowMeta prev = new RowMeta();
    prev.addValueMeta(new ValueMetaString("NAME"));
    WhereMeta meta = new WhereMeta();
    meta.setCondition("NAME = 'abc'");

    List<ICheckResult> remarks = new ArrayList<>();
    meta.check(
        remarks, null, null, prev, new String[] {"input"}, null, null, new Variables(), null);

    // Without TRUE or FALSE target, the rows are sent to the main output
    assertEquals(0, remarks.size());
  }

  @Test
  public void testMainOutput() {
    assertTrue(new WhereMeta().getTransformIOMeta().isOutputProducer());
  }

  @Test
  public void testRemoveHop() {
    WhereMeta meta = new WhereMeta();
    TransformMeta output = new TransformMeta("output", new DummyMeta());
    TransformMeta falseTarget = new TransformMeta("false", new DummyMeta());

    // Removing a hop of the main output without target transform changes nothing
    assertFalse(meta.cleanAfterHopFromRemove(output));

    // Only the FALSE target is chosen, the TRUE stream has no transform
    meta.setFalseTransformName("false");
    meta.searchInfoAndTargetTransforms(List.of(output, falseTarget));
    assertFalse(meta.cleanAfterHopFromRemove(output));
    assertEquals("false", meta.getFalseTransformName());

    // Removing the hop to the FALSE target removes the target
    assertTrue(meta.cleanAfterHopFromRemove(falseTarget));
    assertNull(meta.getFalseTransformName());
  }

  @Test
  public void testTargetsKeptAfterDialog() {
    WhereMeta meta = new WhereMeta();
    meta.setTrueTransformName("true");
    meta.setFalseTransformName("false");
    TransformMeta trueTarget = new TransformMeta("true", new DummyMeta());
    TransformMeta falseTarget = new TransformMeta("false", new DummyMeta());
    List<TransformMeta> transforms = List.of(trueTarget, falseTarget);
    meta.searchInfoAndTargetTransforms(transforms);

    // Hop GUI resets the Input/Output metadata after the dialog and looks up the transforms again
    meta.resetTransformIoMeta();
    meta.searchInfoAndTargetTransforms(transforms);

    assertSame(trueTarget, meta.getTransformIOMeta().getTargetStreams().get(0).getTransformMeta());
    assertSame(falseTarget, meta.getTransformIOMeta().getTargetStreams().get(1).getTransformMeta());
  }

  @Test
  public void testTargetMovedToOtherResult() {
    // The TRUE target is read from the metadata
    WhereMeta meta = new WhereMeta();
    meta.setTrueTransformName("target");
    TransformMeta target = new TransformMeta("target", new DummyMeta());
    List<TransformMeta> transforms = List.of(target);
    meta.searchInfoAndTargetTransforms(transforms);

    // The transform is chosen for the FALSE result, it is removed from the TRUE result
    IStream falseStream = meta.getTransformIOMeta().getTargetStreams().get(1);
    falseStream.setTransformMeta(target);
    meta.handleStreamSelection(falseStream);

    // The TRUE result is not found again when Hop GUI looks up the transforms
    meta.searchInfoAndTargetTransforms(transforms);
    assertNull(meta.getTrueTransformName());
    assertEquals("target", meta.getFalseTransformName());
  }
}
