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
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.variables.Variables;
import org.junit.jupiter.api.Test;

class LoopActionTest {

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
}
