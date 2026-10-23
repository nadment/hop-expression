/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.ui.expression;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.lang.reflect.Proxy;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.ExpressionExtension;
import org.apache.hop.expression.type.Types;
import org.apache.hop.junit.rules.RestoreHopEnvironmentExtension;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith({RestoreHopEnvironmentExtension.class, ExpressionExtension.class})
class ExpressionTextHoverTest {

  private IRowMeta rowMeta;
  private IVariables variables;

  @BeforeEach
  void setUp() {
    rowMeta = new RowMeta();
    IValueMeta name = new ValueMetaString("NAME", 50, -1);
    name.setOrigin("Input");
    rowMeta.addValueMeta(name);
    rowMeta.addValueMeta(new ValueMetaInteger("MY FIELD"));
    rowMeta.addValueMeta(new ValueMetaString("UPPER"));

    variables = new Variables();
    variables.setVariable("PATH", "/tmp");
    variables.setVariable("a.b.c", "x");
  }

  /** A viewer of the source, the only method used by the hover is getDocument. */
  private static ITextViewer viewer(String source) {
    IDocument document = new Document(source);
    return (ITextViewer)
        Proxy.newProxyInstance(
            ITextViewer.class.getClassLoader(),
            new Class<?>[] {ITextViewer.class},
            (proxy, method, args) -> "getDocument".equals(method.getName()) ? document : null);
  }

  private String hover(String source, int offset, boolean variablesOnly) {
    ExpressionTextHover hover = new ExpressionTextHover(variables, () -> rowMeta, variablesOnly);
    ITextViewer viewer = viewer(source);
    IRegion region = hover.getHoverRegion(viewer, offset);
    return (region == null) ? null : hover.getHoverInfo(viewer, region);
  }

  private String fieldInfo(String name) {
    IValueMeta valueMeta = rowMeta.searchValueMeta(name);
    String info = valueMeta.getName() + ": " + Types.createType(valueMeta);
    return (valueMeta.getOrigin() == null) ? info : info + "\nOrigin: " + valueMeta.getOrigin();
  }

  @Test
  void field() {
    assertEquals(fieldInfo("NAME"), hover("NAME || 'x'", 1, false));
    // The type of the field in an expression
    assertEquals("NAME: STRING(50)\nOrigin: Input", hover("NAME || 'x'", 3, false));
    // A quoted identifier, the mouse on a quote or inside
    assertEquals(fieldInfo("MY FIELD"), hover("\"MY FIELD\" + 1", 0, false));
    assertEquals(fieldInfo("MY FIELD"), hover("\"MY FIELD\" + 1", 4, false));
    assertEquals(fieldInfo("MY FIELD"), hover("\"MY FIELD\" + 1", 9, false));
    assertEquals(fieldInfo("MY FIELD"), hover("1 + \"MY FIELD\"", 6, false));
    // Not a field, or the fields are not known yet
    assertNull(hover("OTHER || 'x'", 1, false));
    assertNull(hover("NAME + 1", 5, false));
    rowMeta = null;
    assertNull(hover("NAME || 'x'", 1, false));
  }

  @Test
  void functionOrField() {
    // A word followed by a left parenthesis is a function, otherwise a field like UPPER
    assertNotEquals(fieldInfo("UPPER"), hover("UPPER(NAME)", 1, false));
    assertEquals(fieldInfo("UPPER"), hover("UPPER || NAME", 1, false));
  }

  @Test
  void variable() {
    assertEquals("${PATH} = /tmp", hover("${PATH} || 'x'", 3, false));
    assertEquals("${PATH} = /tmp", hover("${PATH} || 'x'", 0, false));
    assertEquals("${PATH} = /tmp", hover("${PATH} || 'x'", 6, false));
    // A name with dots, and a variable not defined
    assertEquals("${a.b.c} = x", hover("${a.b.c}", 4, false));
    assertEquals("${NOPE} is not defined", hover("${NOPE}", 3, false));
    // In a string, only the variables are shown
    assertEquals("${PATH} = /tmp", hover("'${PATH}/NAME'", 4, true));
    assertNull(hover("'${PATH}/NAME'", 10, true));
  }
}
