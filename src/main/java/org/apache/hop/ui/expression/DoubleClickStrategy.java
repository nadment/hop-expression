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

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.ITextViewer;
import org.jspecify.annotations.NullMarked;

@NullMarked
public class DoubleClickStrategy implements ITextDoubleClickStrategy {

  public void doubleClicked(ITextViewer viewer) {
    int position = viewer.getSelectedRange().x;

    if (position < 0) return;

    if (!selectVariable(viewer, position) && !selectIdentifier(viewer, position)) {
      selectWord(viewer, position);
    }
  }

  protected boolean selectVariable(ITextViewer viewer, int position) {
    IDocument document = viewer.getDocument();

    try {
      int start = position;
      while (start >= 0) {
        char c = document.getChar(start);
        if (isLineBreak(c) || c == '}') return false;
        if (c == '$') break;
        start--;
      }

      // A variable starts with "${"
      if (start < 0 || document.getChar(start + 1) != '{') return false;

      // Search the end after "${", the position can be on the '$'
      int end = Math.max(position, start + 2);
      int length = document.getLength();
      while (end < length) {
        char c = document.getChar(end);
        if (isLineBreak(c) || c == '$') return false;
        if (c == '}') break;
        end++;
      }
      if (document.getChar(end) != '}') return false;

      viewer.setSelectedRange(start, end - start + 1);
    } catch (BadLocationException x) {
      return false;
    }
    return true;
  }

  protected boolean selectIdentifier(ITextViewer viewer, int position) {
    IDocument document = viewer.getDocument();

    try {
      int start = position;
      while (start >= 0) {
        char c = document.getChar(start);
        if (isLineBreak(c) || c == ']') return false;
        if (c == '[') break;
        start--;
      }

      if (start < 0) return false;

      int end = position;
      int length = document.getLength();
      while (end < length) {
        char c = document.getChar(end);
        if (isLineBreak(c) || c == '[') return false;
        if (c == ']') break;
        end++;
      }
      if (document.getChar(end) != ']') return false;

      viewer.setSelectedRange(start, end - start + 1);
    } catch (BadLocationException x) {
      return false;
    }
    return true;
  }

  protected boolean selectWord(ITextViewer viewer, int position) {
    IDocument document = viewer.getDocument();
    try {
      // The word includes the first character of the document
      int start = position;
      while (start > 0 && Character.isJavaIdentifierPart(document.getChar(start - 1))) {
        start--;
      }

      int end = position;
      int length = document.getLength();
      while (end < length && Character.isJavaIdentifierPart(document.getChar(end))) {
        end++;
      }

      viewer.setSelectedRange(start, end - start);

    } catch (BadLocationException x) {
      return false;
    }

    return true;
  }

  /** Checks if a character ends a line, '\n' or '\r'. */
  private static boolean isLineBreak(char c) {
    return c == '\n' || c == '\r';
  }
}
