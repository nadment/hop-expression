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
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.variables.DescribedVariable;
import org.junit.jupiter.api.Test;

class ExpressionLabelProviderTest {

  private final ExpressionLabelProvider provider = new ExpressionLabelProvider();

  @Test
  void toolTipOfField() {
    ValueMetaString meta = new ValueMetaString("A<B");
    meta.setComments("Price < 10 & <b>bold");

    String html = provider.getToolTipText(meta);

    // The texts are escaped
    assertTrue(html.contains("<h1>A&lt;B</h1>"));
    assertTrue(html.contains("Price &lt; 10 &amp; &lt;b&gt;bold"));
    assertFalse(html.contains("<b>"));
    assertWellFormed(html);
  }

  @Test
  void toolTipOfVariable() {
    DescribedVariable variable = new DescribedVariable("VAR", "value", "Use <VAR> & more");

    String html = provider.getToolTipText(variable);

    assertTrue(html.contains("<h1>VAR</h1>"));
    assertTrue(html.contains("Use &lt;VAR&gt; &amp; more"));
    assertWellFormed(html);

    // Without description
    assertTrue(provider.getToolTipText(new DescribedVariable("VAR", null, null)).contains("-"));
  }

  @Test
  void toolTipOfOther() {
    assertNull(provider.getToolTipText("TEXT"));
  }

  /** Each opened div is closed. */
  private static void assertWellFormed(String html) {
    assertEquals(html.split("<div", -1).length, html.split("</div>", -1).length);
    assertTrue(html.endsWith("</body></html>"));
  }
}
