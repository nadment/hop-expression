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

package org.apache.hop.ui.expression;

import org.apache.hop.ui.core.gui.GuiResource;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposalExtension6;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.jface.viewers.StyledString.Styler;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.TextStyle;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * A completion proposal of the expression editor with a colored label: the name is followed by
 * details written in gray, like the arguments of a function or the type of a field.
 *
 * <pre>{@code
 * StyledString label = new StyledString("ABS");
 * label.append("( value )", ExpressionCompletionProposal.DETAIL_STYLER);
 * new ExpressionCompletionProposal("ABS()", offset, length, 4, image, "ABS", label, description);
 * }</pre>
 */
@NullMarked
public class ExpressionCompletionProposal
    implements ICompletionProposal, ICompletionProposalExtension6 {

  /** The style of the details that follow a name, written in gray. */
  public static final Styler DETAIL_STYLER =
      new Styler() {
        @Override
        public void applyStyles(TextStyle textStyle) {
          // The color is read when the label is drawn, not when the proposal is created
          textStyle.foreground = GuiResource.getInstance().getColorGray();
        }
      };

  /** The style of the name of a function. */
  public static final Styler FUNCTION_STYLER =
      new Styler() {
        @Override
        public void applyStyles(TextStyle textStyle) {
          textStyle.foreground = ExpressionEditorConfiguration.getFunctionColor();
        }
      };

  /** The style of the name of a field. */
  public static final Styler FIELD_STYLER =
      new Styler() {
        @Override
        public void applyStyles(TextStyle textStyle) {
          textStyle.foreground = ExpressionEditorConfiguration.getFieldColor();
        }
      };

  /** The style of a reserved word. */
  public static final Styler KEYWORD_STYLER =
      new Styler() {
        @Override
        public void applyStyles(TextStyle textStyle) {
          textStyle.foreground = ExpressionEditorConfiguration.getKeywordColor();
        }
      };

  /** The style of a reserved literal, a data type or a time unit. */
  public static final Styler LITERAL_STYLER =
      new Styler() {
        @Override
        public void applyStyles(TextStyle textStyle) {
          textStyle.foreground = ExpressionEditorConfiguration.getLiteralColor();
        }
      };

  /** The style of the name of a variable. */
  public static final Styler VARIABLE_STYLER =
      new Styler() {
        @Override
        public void applyStyles(TextStyle textStyle) {
          textStyle.foreground = GuiResource.getInstance().getColorBlack();
        }
      };

  /** The style of the name of a deprecated variable. */
  public static final Styler DEPRECATED_STYLER =
      new Styler() {
        @Override
        public void applyStyles(TextStyle textStyle) {
          VARIABLE_STYLER.applyStyles(textStyle);
          textStyle.strikeout = true;
        }
      };

  private final String replacement;
  private final int offset;
  private final int length;
  private final int cursorPosition;
  private final @Nullable Image image;
  private final String displayString;
  private final StyledString styledDisplayString;
  private final @Nullable String additionalInfo;

  /**
   * Creates a proposal.
   *
   * @param replacement the text inserted in the document
   * @param offset the offset of the text to replace
   * @param length the length of the text to replace
   * @param cursorPosition the position of the caret after the insertion, relative to the offset
   * @param image the image of the proposal, or {@code null}
   * @param displayString the name of the proposal, used to sort the proposals
   * @param styledDisplayString the colored label of the proposal
   * @param additionalInfo the description displayed beside the proposals, or {@code null}
   */
  public ExpressionCompletionProposal(
      String replacement,
      int offset,
      int length,
      int cursorPosition,
      @Nullable Image image,
      String displayString,
      StyledString styledDisplayString,
      @Nullable String additionalInfo) {
    this.replacement = replacement;
    this.offset = offset;
    this.length = length;
    this.cursorPosition = cursorPosition;
    this.image = image;
    this.displayString = displayString;
    this.styledDisplayString = styledDisplayString;
    this.additionalInfo = additionalInfo;
  }

  @Override
  public void apply(IDocument document) {
    try {
      document.replace(offset, length, replacement);
    } catch (BadLocationException e) {
      // The document changed since the proposal was computed, nothing is inserted
    }
  }

  @Override
  public Point getSelection(IDocument document) {
    return new Point(offset + cursorPosition, 0);
  }

  @Override
  public @Nullable String getAdditionalProposalInfo() {
    return additionalInfo;
  }

  @Override
  public String getDisplayString() {
    return displayString;
  }

  @Override
  public StyledString getStyledDisplayString() {
    return styledDisplayString;
  }

  @Override
  public @Nullable Image getImage() {
    return image;
  }

  @Override
  public @Nullable IContextInformation getContextInformation() {
    return null;
  }
}
