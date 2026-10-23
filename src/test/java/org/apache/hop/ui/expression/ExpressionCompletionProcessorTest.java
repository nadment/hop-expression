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
import org.apache.hop.expression.ExpressionExtension;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.junit.rules.RestoreHopEnvironmentExtension;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith({RestoreHopEnvironmentExtension.class, ExpressionExtension.class})
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

  /** Returns the text inserted for a function with a | at the caret. */
  private static String complete(String name, boolean hasParenthesis) {
    Function function = FunctionRegistry.getFunction(name);
    String text = ExpressionCompletionProcessor.getFunctionReplacement(name, hasParenthesis);
    int caret =
        ExpressionCompletionProcessor.getFunctionCursorPosition(name, function, hasParenthesis);
    return text.substring(0, caret) + "|" + text.substring(caret);
  }

  @Test
  void functionInsertedWithParentheses() {
    // The caret is between the parentheses, or after them for a function without argument
    assertEquals("LEFT(|)", complete("LEFT", false));
    assertEquals("COALESCE(|)", complete("COALESCE", false));
    assertEquals("PI()|", complete("PI", false));
    // A function already followed by its parenthesis is inserted without parentheses
    assertEquals("LEFT|", complete("LEFT", true));
    assertEquals("PI|", complete("PI", true));
  }

  /** Returns the data types and the time units proposed for a prefix. */
  private static List<String> typesAndTimeUnits(String prefix) {
    ExpressionCompletionProcessor processor =
        new ExpressionCompletionProcessor(new Variables(), () -> null, ExpressionMode.ROW);
    List<ICompletionProposal> proposals = new ArrayList<>();
    processor.computeTypeAndTimeUnitProposals(proposals, prefix, 0, prefix.length());
    return proposals.stream().map(ICompletionProposal::getDisplayString).sorted().toList();
  }

  @Test
  void typeAndTimeUnitProposals() {
    // A data type, like CAST(value AS NUMBER), the prefix is not case-sensitive
    assertEquals(List.of("NUMBER"), typesAndTimeUnits("nu"));
    assertEquals(List.of("STRING"), typesAndTimeUnits("STR"));
    // A time unit with its main name, like EXTRACT(YEAR FROM date), not its aliases
    assertEquals(List.of("YEAR"), typesAndTimeUnits("YEA"));
    assertEquals(
        List.of("MICROSECOND", "MILLENNIUM", "MILLISECOND", "MINUTE", "MONTH"),
        typesAndTimeUnits("M"));
    // A data type that is a reserved word, like DATE, is proposed with the reserved words
    assertFalse(typesAndTimeUnits("DAT").contains("DATE"));
    // The internal data types are not proposed
    assertTrue(typesAndTimeUnits("AN").isEmpty());
    assertTrue(typesAndTimeUnits("UNK").isEmpty());
    assertTrue(typesAndTimeUnits("ENU").isEmpty());
    assertFalse(typesAndTimeUnits("AR").contains("ARRAY"));
  }

  @Test
  void startsWithIgnoreCase() {
    assertTrue(ExpressionCompletionProcessor.startsWithIgnoreCase("ABS", "ab"));
    assertTrue(ExpressionCompletionProcessor.startsWithIgnoreCase("ABS", ""));
    assertFalse(ExpressionCompletionProcessor.startsWithIgnoreCase("AB", "ABS"));
  }
}
