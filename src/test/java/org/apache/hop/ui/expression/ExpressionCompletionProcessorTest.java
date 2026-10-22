/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.ui.expression;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.variables.Variables;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.junit.jupiter.api.Test;

class ExpressionCompletionProcessorTest {

  @Test
  void fieldsNotKnownYet() {
    // The supplier returns null while the fields are computed, no field is proposed
    ExpressionCompletionProcessor processor =
        new ExpressionCompletionProcessor(new Variables(), () -> null, ExpressionMode.ROW);
    List<ICompletionProposal> proposals = new ArrayList<>();
    processor.computeIdentifierProposals(proposals, "A", 0, 1, false);
    assertTrue(proposals.isEmpty());
  }

  @Test
  void fieldsReadWhenProposed() {
    // The supplier is read for each completion, the fields can be known after the processor
    AtomicInteger reads = new AtomicInteger();
    ExpressionCompletionProcessor processor =
        new ExpressionCompletionProcessor(
            new Variables(),
            () -> {
              reads.incrementAndGet();
              return new RowMeta();
            },
            ExpressionMode.UDF);
    processor.computeIdentifierProposals(new ArrayList<>(), "A", 0, 1, false);
    processor.computeIdentifierProposals(new ArrayList<>(), "A", 0, 1, false);
    assertEquals(2, reads.get());
  }

  @Test
  void startsWithIgnoreCase() {
    assertTrue(ExpressionCompletionProcessor.startsWithIgnoreCase("ABS", "ab"));
    assertTrue(ExpressionCompletionProcessor.startsWithIgnoreCase("ABS", ""));
    assertFalse(ExpressionCompletionProcessor.startsWithIgnoreCase("AB", "ABS"));
  }
}
