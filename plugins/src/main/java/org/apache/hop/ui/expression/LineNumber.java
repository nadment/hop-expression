package org.apache.hop.ui.expression;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.Bullet;
import org.eclipse.swt.custom.LineStyleEvent;
import org.eclipse.swt.custom.LineStyleListener;
import org.eclipse.swt.custom.ST;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.GlyphMetrics;
import org.eclipse.swt.widgets.Display;

public class LineNumber implements LineStyleListener {

  private StyledText text;

  public LineNumber(final StyledText control) {
    this.text = control;
  }

  @Override
  public void lineGetStyle(final LineStyleEvent event) {
    // Set the line number
    event.bulletIndex = text.getLineAtOffset(event.lineOffset);

    int width = 24;
    int lineCount = text.getLineCount();
    if (lineCount > 99) width = (int) ((Math.floor(Math.log10(lineCount)) + 1) * 12);

    // Set the style, 12 pixels wide for each digit
    StyleRange style = new StyleRange();
    style.metrics = new GlyphMetrics(0, 0, width);
    style.foreground = Display.getCurrent().getSystemColor(SWT.COLOR_GRAY);

    // Create and set the bullet
    event.bullet = new Bullet(ST.BULLET_NUMBER, style);
  }
}
