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
 */
package org.apache.hop.ui.expression;

import java.text.ParseException;
import java.util.ArrayList;

import org.apache.hop.expression.ExpressionScanner;
import org.apache.hop.expression.Token;
import org.apache.hop.ui.core.gui.GuiResource;
import org.eclipse.swt.custom.LineStyleEvent;
import org.eclipse.swt.custom.LineStyleListener;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Color;

public class ExpressionSyntaxHighlighter implements LineStyleListener {

  private Color black;
  private Color red;
  private Color green;
  private Color darkgreen;
  private Color blue;
  private Color indigo;
  private Color purple;
  private Color orange;

  public ExpressionSyntaxHighlighter() {
    black = GuiResource.getInstance().getColorBlack();
    red = GuiResource.getInstance().getColorRed();
    darkgreen = GuiResource.getInstance().getColor(10, 93, 0);
    green = GuiResource.getInstance().getColor(8, 154, 0);
    blue = GuiResource.getInstance().getColorBlue();
    purple = GuiResource.getInstance().getColor(255, 0, 255);
    orange = GuiResource.getInstance().getColorOrange();
    indigo = GuiResource.getInstance().getColorIndigo();
  }

  @Override
  public void lineGetStyle(LineStyleEvent event) {
    StyledText styledText = ((StyledText) event.widget);

    try {
      ArrayList<StyleRange> ranges = new ArrayList<>();
      ExpressionScanner scanner = new ExpressionScanner(styledText.getText());

      for (Token token = scanner.tokenize(); token != null; token = scanner.tokenize()) {
        ranges.add(new StyleRange(token.start(), token.length(), getColor(token), null));
      }

      event.styles = ranges.toArray(new StyleRange[0]);
    } catch (ParseException e) {
      // Ignore
    }
  }

  public Color getColor(Token token) {
    switch (token.id()) {
      case COMMENT:
        return darkgreen;
      case IDENTIFIER:
        return black;
      case FUNCTION:
        return purple;
      case VARIABLE:
        return red;
      case DATATYPE:
      case DATEPART:
        return indigo;
      case LITERAL_STRING:
        return green;
      case LITERAL_NUMBER:
      case LITERAL_BINARY_BIT:
      case LITERAL_BINARY_HEX:
        return orange;
      default:
        return blue;
    }
  }
}
