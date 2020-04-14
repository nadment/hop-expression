package org.apache.hop.ui.expression;

import java.util.ArrayList;

import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionScanner;
import org.apache.hop.expression.ExpressionToken;
import org.apache.hop.ui.core.gui.GUIResource;
import org.eclipse.swt.custom.LineStyleEvent;
import org.eclipse.swt.custom.LineStyleListener;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Color;

public class ExpressionSyntaxHighlighter implements LineStyleListener {

	//private Color GREY;
	private Color BLACK;
	private Color RED;
	private Color GREEN;
	private Color DARK_GREEN;
	private Color BLUE;
	private Color PURPLE;
	private Color ORANGE;

	public ExpressionSyntaxHighlighter() {
		BLACK = GUIResource.getInstance().getColor(0, 0, 0);
		//GREY = GUIResource.getInstance().getColor(80, 80, 80);
		RED = GUIResource.getInstance().getColor(255, 0, 0);
		DARK_GREEN = GUIResource.getInstance().getColor(10, 93, 0);
		GREEN = GUIResource.getInstance().getColor(8, 154, 0);
		BLUE = GUIResource.getInstance().getColor(0, 0, 255);
		PURPLE = GUIResource.getInstance().getColor(255, 0, 255);
		ORANGE = GUIResource.getInstance().getColor(240, 94, 35);
	}

	@Override
	public void lineGetStyle(LineStyleEvent event) {
		StyledText styledText = ((StyledText) event.widget);

		try {
			ArrayList<StyleRange> ranges = new ArrayList<StyleRange>();
			ExpressionScanner scanner = new ExpressionScanner(styledText.getText());

			for (ExpressionToken token = scanner.tokenize(); token != null; token = scanner.tokenize()) {
				ranges.add(new StyleRange(token.getStart(), token.getLength(), getColor(token), null));
			}

			event.styles = ranges.toArray(new StyleRange[0]);
		} catch (ExpressionException e) {
			// Ignore
		}
	}

	public Color getColor(ExpressionToken token) {
		switch (token.getId()) {
		case COMMENT:
			return DARK_GREEN;
		case IDENTIFIER:
			return BLACK;
		case FUNCTION:
			return PURPLE;
		case VARIABLE:
			return RED;
		case LITERAL_TEXT:
			return GREEN;
		case LITERAL_NUMBER:
		case LITERAL_BITNUMBER:
		case LITERAL_HEXNUMBER:
			return ORANGE;
		default:
			return BLUE;
		}
	}
}
