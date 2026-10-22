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

package org.apache.hop.workflow.actions.loop;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.apache.hop.core.HopEnvironment;
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.Result;
import org.apache.hop.core.annotations.Action;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.plugins.ActionPluginType;
import org.apache.hop.core.plugins.PluginRegistry;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.metadata.api.IHopMetadataProvider;
import org.apache.hop.metadata.serializer.memory.MemoryMetadataProvider;
import org.apache.hop.workflow.WorkflowHopMeta;
import org.apache.hop.workflow.WorkflowMeta;
import org.apache.hop.workflow.action.ActionMeta;
import org.apache.hop.workflow.action.IAction;
import org.apache.hop.workflow.actions.setvariable.SetVariableAction;
import org.apache.hop.workflow.actions.setvariable.SetVariableDefinition;
import org.apache.hop.workflow.actions.setvariable.SetVariableScope;
import org.apache.hop.workflow.actions.start.ActionStart;
import org.apache.hop.workflow.config.WorkflowRunConfiguration;
import org.apache.hop.workflow.engine.IWorkflowEngine;
import org.apache.hop.workflow.engines.local.LocalWorkflowEngine;
import org.apache.hop.workflow.engines.local.LocalWorkflowRunConfiguration;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class LoopActionTest {

  @TempDir static Path folder;

  /** The workflow executed by the loop, which increments the variable COUNTER. */
  private static String filename;

  /** The metadata with the local run configuration. */
  private static IHopMetadataProvider metadataProvider;

  @BeforeAll
  static void init() throws Exception {
    HopEnvironment.init();

    // The plugins of this project are not found by the scan of the plugins
    PluginRegistry.getInstance()
        .registerPluginClass(
            SetVariableAction.class.getName(), ActionPluginType.class, Action.class);

    LocalWorkflowRunConfiguration engineConfiguration = new LocalWorkflowRunConfiguration();
    engineConfiguration.setEnginePluginId("Local");
    metadataProvider = new MemoryMetadataProvider();
    metadataProvider
        .getSerializer(WorkflowRunConfiguration.class)
        .save(new WorkflowRunConfiguration("local", "", null, engineConfiguration, true));

    // Start -> COUNTER = COUNTER + 1, in the current and the root workflow
    SetVariableAction increment = new SetVariableAction("increment");
    increment
        .getVariableDefinitions()
        .add(
            new SetVariableDefinition("COUNTER", "${COUNTER} + 1", SetVariableScope.ROOT_WORKFLOW));
    WorkflowMeta workflowMeta = createWorkflow("increment", increment);

    filename = folder.resolve("increment.hwf").toString();
    Files.writeString(Path.of(filename), workflowMeta.getXml(new Variables()));
  }

  /** Creates a workflow with a start action followed by the action. */
  private static WorkflowMeta createWorkflow(String name, IAction action) {
    WorkflowMeta workflowMeta = new WorkflowMeta();
    workflowMeta.setName(name);
    ActionMeta start = new ActionMeta(new ActionStart("start"));
    ActionMeta next = new ActionMeta(action);
    workflowMeta.addAction(start);
    workflowMeta.addAction(next);
    WorkflowHopMeta hop = new WorkflowHopMeta(start, next);
    hop.setUnconditional(true);
    workflowMeta.addWorkflowHop(hop);
    return workflowMeta;
  }

  /** Executes a workflow with the loop, the variable COUNTER starts with the value. */
  private static IWorkflowEngine<WorkflowMeta> executeLoop(String condition, String value)
      throws HopException {
    LoopAction loop = new LoopAction("loop", "");
    loop.setFilename(filename);
    loop.setRunConfigurationName("local");
    loop.setCondition(condition);
    loop.getParameters().add(new Parameter("COUNTER", value));

    IWorkflowEngine<WorkflowMeta> workflow = new LocalWorkflowEngine(createWorkflow("main", loop));
    workflow.setMetadataProvider(metadataProvider);
    workflow.startExecution();
    return workflow;
  }

  @Test
  void loopWhileConditionIsTrue() throws Exception {
    IWorkflowEngine<WorkflowMeta> workflow = executeLoop("${COUNTER} < 3", "0");

    // The workflow is executed 3 times, each execution starts with the previous COUNTER
    assertTrue(workflow.getResult().isResult());
    assertEquals(0, workflow.getResult().getNrErrors());
    assertEquals("3", workflow.getVariable("COUNTER"));
  }

  @Test
  void loopWithConditionFalseAtStart() throws Exception {
    IWorkflowEngine<WorkflowMeta> workflow = executeLoop("${COUNTER} < 3", "5");

    // The workflow is never executed, the loop succeeds
    assertTrue(workflow.getResult().isResult());
    assertEquals(0, workflow.getResult().getNrErrors());
    assertNull(workflow.getVariable("COUNTER"));
  }

  /** Returns the remarks of the check about the type of the condition. */
  private static List<ICheckResult> checkType(String condition) {
    LoopAction action = new LoopAction();
    action.setCondition(condition);

    List<ICheckResult> remarks = new ArrayList<>();
    action.check(remarks, null, new Variables(), null);
    return remarks.stream().filter(remark -> remark.getText().contains("BOOLEAN")).toList();
  }

  @Test
  void checkConditionNotBoolean() {
    // A Boolean and a number converted to a Boolean are valid
    assertEquals(0, checkType("1 = 1").size());
    assertEquals(0, checkType("1").size());

    // A string can fail to be converted to a Boolean
    List<ICheckResult> remarks = checkType("'abc'");
    assertEquals(1, remarks.size());
    assertEquals(ICheckResult.TYPE_RESULT_ERROR, remarks.getFirst().getType());
    assertTrue(remarks.getFirst().getText().contains("STRING"));
  }

  @Test
  void cloneCopiesParameters() {
    LoopAction action = new LoopAction();
    action.getParameters().add(new Parameter("P1", "V1"));

    LoopAction clone = (LoopAction) action.clone();
    assertNotSame(action.getParameters(), clone.getParameters());
    assertNotSame(action.getParameters().getFirst(), clone.getParameters().getFirst());
    assertEquals("P1", clone.getParameters().getFirst().getName());
    assertEquals("V1", clone.getParameters().getFirst().getValue());

    // A change of the clone does not change the original
    clone.getParameters().getFirst().setValue("V2");
    clone.getParameters().add(new Parameter("P2", "V3"));
    assertEquals("V1", action.getParameters().getFirst().getValue());
    assertEquals(1, action.getParameters().size());
  }

  @Test
  void executeWithoutCondition() {
    LoopAction action = new LoopAction();
    action.setFilename("loop.hwf");

    HopException e = assertThrows(HopException.class, () -> action.execute(new Result(), 0));
    assertTrue(e.getMessage().contains("Loop condition is missing"));
  }
}
