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

import org.eclipse.jface.fieldassist.IControlContentAdapter;
import org.eclipse.jface.fieldassist.IControlContentAdapter2;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Control;

/**
 * Content Adapter for StyledText widget
 *
 * @author Nicolas ADMENT
 */
public class StyledTextContentAdapter implements IControlContentAdapter, IControlContentAdapter2 {

  @Override
  public String getControlContents(Control control) {
    return ((StyledText) control).getText();
  }

  @Override
  public void setControlContents(Control control, String text, int cursorPosition) {
    StyledText styledText = (StyledText) control;
    styledText.setText(text);
    styledText.setSelection(cursorPosition, cursorPosition);
  }

  @Override
  public void insertControlContents(Control control, String text, int cursorPosition) {
    Point selection = ((StyledText) control).getSelection();
    ((StyledText) control).insert(text);
    // Insert will leave the cursor at the end of the inserted StyledText. If this
    // is not what we wanted, reset the selection.
    if (cursorPosition <= text.length()) {
      ((StyledText) control)
          .setSelection(selection.x + cursorPosition, selection.x + cursorPosition);
    }
  }

  @Override
  public int getCursorPosition(Control control) {
    return ((StyledText) control).getCaretOffset();
  }

  @Override
  public Rectangle getInsertionBounds(Control control) {
    StyledText StyledText = (StyledText) control;
    int caretOrigin = StyledText.getCaretOffset();
    // We fudge the y pixels due to problems with getCaretLocation
    // See https://bugs.eclipse.org/bugs/show_bug.cgi?id=52520
    return new Rectangle(
        caretOrigin + StyledText.getClientArea().x,
        0 + StyledText.getClientArea().y,
        1,
        StyledText.getLineHeight());
  }

  @Override
  public void setCursorPosition(Control control, int position) {
    ((StyledText) control).setSelection(new Point(position, position));
  }

  @Override
  public Point getSelection(Control control) {
    return ((StyledText) control).getSelection();
  }

  @Override
  public void setSelection(Control control, Point range) {
    ((StyledText) control).setSelection(range);
  }
}
