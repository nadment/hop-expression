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

package org.apache.hop.workflow.actions.setvariable;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.util.ArrayList;
import java.util.List;
import org.apache.hop.core.HopEnvironment;
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.Result;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.i18n.BaseMessages;
import org.junit.jupiter.api.Test;

class SetVariableActionTest {

  /** Keeps the errors logged by the action. */
  static final class TestSetVariableAction extends SetVariableAction {
    private final List<String> errors = new ArrayList<>();

    TestSetVariableAction() {
      super("set");
    }

    @Override
    public void logError(String message) {
      errors.add(message);
    }
  }

  @Test
  void defaultNameIsFound() {
    assertEquals(
        "Set variables expression",
        BaseMessages.getString(SetVariableAction.PKG, "SetVariableAction.Name.Default"));
  }

  @Test
  void checkMissingScope() throws Exception {
    HopEnvironment.init();

    SetVariableAction action = new SetVariableAction("set");
    action.getVariableDefinitions().add(new SetVariableDefinition("A", "'abc'", null));

    List<ICheckResult> remarks = new ArrayList<>();
    action.check(remarks, null, new Variables(), null);

    assertEquals(1, remarks.size());
    assertEquals(ICheckResult.TYPE_RESULT_ERROR, remarks.getFirst().getType());
    assertEquals("Scope for variable [A] is missing", remarks.getFirst().getText());
  }

  @Test
  void executeMissingScope() throws Exception {
    HopEnvironment.init();

    TestSetVariableAction action = new TestSetVariableAction();
    action.getVariableDefinitions().add(new SetVariableDefinition("A", "'abc'", null));
    action
        .getVariableDefinitions()
        .add(new SetVariableDefinition("B", "'xyz'", SetVariableScope.ROOT_WORKFLOW));

    Result result = action.execute(new Result(), 0);

    // The variable without scope is reported, the next variables are set
    assertFalse(result.getResult());
    assertEquals(1, result.getNrErrors());
    assertEquals(
        List.of(
            "Error set variable [A] with expression ['abc']: Scope for variable [A] is missing"),
        action.errors);
    assertEquals("xyz", action.getVariable("B"));
  }

  @Test
  void copiesDoNotShareDefinitions() {
    SetVariableAction action = new SetVariableAction("set");
    SetVariableDefinition definition =
        new SetVariableDefinition("A", "'abc'", SetVariableScope.CURRENT_WORKFLOW);
    action.getVariableDefinitions().add(definition);

    for (SetVariableAction copy :
        List.of(new SetVariableAction(action), (SetVariableAction) action.clone())) {
      assertNotSame(action.getVariableDefinitions(), copy.getVariableDefinitions());
      SetVariableDefinition copied = copy.getVariableDefinitions().getFirst();
      assertNotSame(definition, copied);
      assertEquals("A", copied.getName());
      assertEquals("'abc'", copied.getExpression());
      assertSame(SetVariableScope.CURRENT_WORKFLOW, copied.getScope());

      // A change of the copy does not change the action
      copied.setExpression("'xyz'");
      copy.getVariableDefinitions().clear();
      assertEquals(1, action.getVariableDefinitions().size());
      assertEquals("'abc'", definition.getExpression());
    }
  }
}
