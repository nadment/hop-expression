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
 */
public class StyledTextContentAdapter implements IControlContentAdapter, IControlContentAdapter2 {

  private final StyledText styledText;
  
  public StyledTextContentAdapter(StyledText styledText) {
    this.styledText = styledText;
}
  
  @Override
  public String getControlContents(Control control) {
    return styledText.getText();
  }

  @Override
  public void setControlContents(Control control, String text, int cursorPosition) {    
    styledText.setText(text);
    styledText.setSelection(cursorPosition, cursorPosition);
  }

  @Override
  public void insertControlContents(Control control, String text, int cursorPosition) {
    Point selection = styledText.getSelection();
    styledText.insert(text);
    // Insert will leave the cursor at the end of the inserted StyledText. If this
    // is not what we wanted, reset the selection.
    if (cursorPosition <= text.length()) {
      styledText.setSelection(selection.x + cursorPosition, selection.x + cursorPosition);
    }
  }

  @Override
  public int getCursorPosition(Control control) {
    return styledText.getCaretOffset();
  }

  @Override
  public Rectangle getInsertionBounds(Control control) {   
    int caretOrigin = styledText.getCaretOffset();
    // We fudge the y pixels due to problems with getCaretLocation
    // See https://bugs.eclipse.org/bugs/show_bug.cgi?id=52520
    return new Rectangle(
        caretOrigin + styledText.getClientArea().x,
        0 + styledText.getClientArea().y,
        1,
        styledText.getLineHeight());
  }

  @Override
  public void setCursorPosition(Control control, int position) {
    styledText.setSelection(new Point(position, position));
  }

  @Override
  public Point getSelection(Control control) {
    return styledText.getSelection();
  }

  @Override
  public void setSelection(Control control, Point range) {
    styledText.setSelection(range);
  }
}
