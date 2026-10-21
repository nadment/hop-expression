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
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import org.apache.hop.core.HopEnvironment;
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.variables.Variables;
import org.junit.jupiter.api.Test;

class AggregateMetaTest {

  @Test
  void variablesAreResolvedInOutputFields() throws Exception {
    HopEnvironment.init();

    AggregateMeta meta = new AggregateMeta();
    meta.setDefault();
    AggregateField field = new AggregateField();
    field.setName("TOTAL");
    field.setExpression("SUM(${FIELD})");
    meta.getAggregateFields().add(field);

    IRowMeta rowMeta = new RowMeta();
    rowMeta.addValueMeta(new ValueMetaInteger("AMOUNT"));
    Variables variables = new Variables();
    variables.setVariable("FIELD", "AMOUNT");

    meta.getFields(rowMeta, "aggregate", null, null, variables, null);

    // The aggregate is not skipped, the transform finds its field
    assertEquals(1, rowMeta.size());
    assertEquals("TOTAL", rowMeta.getValueMeta(0).getName());
  }

  /** Returns the error remarks of the check of an aggregate expression. */
  private static List<ICheckResult> check(String expression, IRowMeta prev) throws Exception {
    HopEnvironment.init();

    AggregateMeta meta = new AggregateMeta();
    meta.setDefault();
    AggregateField field = new AggregateField();
    field.setName("TOTAL");
    field.setExpression(expression);
    meta.getAggregateFields().add(field);

    Variables variables = new Variables();
    variables.setVariable("FIELD", "AMOUNT");

    List<ICheckResult> remarks = new ArrayList<>();
    meta.check(remarks, null, null, prev, new String[] {"input"}, null, null, variables, null);
    return remarks.stream()
        .filter(remark -> remark.getType() == ICheckResult.TYPE_RESULT_ERROR)
        .toList();
  }

  private static IRowMeta amount() {
    IRowMeta prev = new RowMeta();
    prev.addValueMeta(new ValueMetaInteger("AMOUNT"));
    return prev;
  }

  @Test
  void checkValidExpressions() throws Exception {
    assertEquals(0, check("SUM(AMOUNT)", amount()).size());
    assertEquals(0, check("COUNT(*)", amount()).size());
    assertEquals(0, check("SUM(${FIELD})", amount()).size());
  }

  @Test
  void checkInvalidExpressions() throws Exception {
    // A syntax error, an unknown field and an expression that is not an aggregate
    for (String expression :
        List.of("SUM(AMOUNT", "SUM(UNKNOWN)", "AMOUNT + 1", "SUM(AMOUNT) + 1")) {
      List<ICheckResult> remarks = check(expression, amount());
      assertEquals(1, remarks.size(), expression);
      assertTrue(remarks.getFirst().getText().startsWith("Aggregate expression [TOTAL] : "));
    }
  }

  @Test
  void checkEmptyExpression() throws Exception {
    List<ICheckResult> remarks = check("", amount());
    assertEquals(1, remarks.size());
    assertEquals("Aggregate expression [TOTAL] is missing", remarks.getFirst().getText());
  }

  @Test
  void checkDuplicateNames() throws Exception {
    HopEnvironment.init();

    AggregateMeta meta = new AggregateMeta();
    meta.setDefault();
    meta.getGroupFields().add(new GroupField("AMOUNT"));
    AggregateField count = new AggregateField();
    count.setName("amount");
    count.setExpression("COUNT(*)");
    meta.getAggregateFields().add(count);
    AggregateField total = new AggregateField();
    total.setName("TOTAL");
    total.setExpression("SUM(AMOUNT)");
    meta.getAggregateFields().add(total);
    meta.getAggregateFields().add(total);

    List<ICheckResult> remarks = new ArrayList<>();
    meta.check(
        remarks, null, null, amount(), new String[] {"input"}, null, null, new Variables(), null);

    // The names are not case-sensitive, like in the row structure
    assertEquals(
        List.of(
            "Field [amount] is written several times", "Field [TOTAL] is written several times"),
        remarks.stream().map(ICheckResult::getText).toList());
  }

  @Test
  void checkWithoutInputFields() throws Exception {
    // The expressions are not compiled without the fields of the input stream
    assertEquals(0, check("SUM(AMOUNT)", null).size());
  }
}
