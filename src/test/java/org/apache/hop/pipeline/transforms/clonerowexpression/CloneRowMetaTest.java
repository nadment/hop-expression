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
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaNumber;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.ExpressionExtension;
import org.apache.hop.junit.rules.RestoreHopEnvironmentExtension;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith({RestoreHopEnvironmentExtension.class, ExpressionExtension.class})
class CloneRowMetaTest {

  /** Returns the errors of the check of a number of clones, with or without the input fields. */
  private static List<ICheckResult> checkErrors(String nrClones, IRowMeta prev) {
    CloneRowMeta meta = new CloneRowMeta();
    meta.setNrClones(nrClones);

    List<ICheckResult> remarks = new ArrayList<>();
    meta.check(
        remarks, null, null, prev, new String[] {"input"}, null, null, new Variables(), null);
    return remarks.stream()
        .filter(remark -> remark.getType() == ICheckResult.TYPE_RESULT_ERROR)
        .toList();
  }

  private static IRowMeta inputFields() {
    IRowMeta prev = new RowMeta();
    prev.addValueMeta(new ValueMetaString("NAME"));
    prev.addValueMeta(new ValueMetaInteger("COUNT"));
    prev.addValueMeta(new ValueMetaNumber("AMOUNT"));
    return prev;
  }

  @Test
  void checkValidNrClones() {
    // An integer constant, field or expression is valid
    assertEquals(List.of(), checkErrors("3", inputFields()));
    assertEquals(List.of(), checkErrors("COUNT", inputFields()));
    assertEquals(List.of(), checkErrors("LENGTH(NAME) + COUNT", inputFields()));
    // A number converted to an integer is valid
    assertEquals(List.of(), checkErrors("CAST(AMOUNT AS INTEGER)", inputFields()));
  }

  @Test
  void checkNrClonesNotInteger() {
    // A number is not coerced to an integer, it could lose its decimal part
    List<ICheckResult> errors = checkErrors("AMOUNT", inputFields());
    assertEquals(1, errors.size());
    assertTrue(errors.getFirst().getText().contains("NUMBER"));

    // A string can't be converted to an integer
    errors = checkErrors("NAME", inputFields());
    assertEquals(1, errors.size());
    assertTrue(errors.getFirst().getText().contains("STRING"));
  }

  @Test
  void checkInvalidNrClones() {
    // An unknown field or a syntax error is reported with the message of the expression
    List<ICheckResult> errors = checkErrors("UNKNOWN_FIELD", inputFields());
    assertEquals(1, errors.size());
    assertTrue(errors.getFirst().getText().contains("UNKNOWN_FIELD"));

    assertEquals(1, checkErrors("COUNT +", inputFields()).size());
  }

  @Test
  void checkValidSettingGivesNoRemark() {
    // Only the errors and the warnings are reported
    CloneRowMeta meta = new CloneRowMeta();
    meta.setNrClones("COUNT");
    meta.setAddCloneFlag(true);
    meta.setCloneFlagField("FLAG");
    meta.setAddCloneNum(true);
    meta.setCloneNumField("NUM");

    List<ICheckResult> remarks = new ArrayList<>();
    meta.check(
        remarks,
        null,
        null,
        inputFields(),
        new String[] {"input"},
        null,
        null,
        new Variables(),
        null);
    assertEquals(List.of(), remarks);
  }

  @Test
  void checkNrClonesWithoutInputFields() {
    // Without the fields of the input stream, the expression is not checked
    assertEquals(List.of(), checkErrors("UNKNOWN_FIELD", null));
  }
}
