package org.apache.hop.ui.expression;

import java.util.ArrayList;

import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Scanner;
import org.apache.hop.expression.Token;
import org.apache.hop.ui.core.gui.GuiResource;
import org.eclipse.swt.custom.LineStyleEvent;
import org.eclipse.swt.custom.LineStyleListener;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Color;

public class ExpressionSyntaxHighlighter implements LineStyleListener {

//	private Color BRUN;
	private Color BLACK;
	private Color RED;
	private Color GREEN;
	private Color DARK_GREEN;	
	private Color BLUE;
	private Color INDIGO;
	private Color PURPLE;
	private Color ORANGE;

	public ExpressionSyntaxHighlighter() {
		BLACK = GuiResource.getInstance().getColor(0, 0, 0);
	//	BRUN = GUIResource.getInstance().getColor(106,62,62);
		RED = GuiResource.getInstance().getColor(255, 0, 0);
		DARK_GREEN = GuiResource.getInstance().getColor(10, 93, 0);
		GREEN = GuiResource.getInstance().getColor(8, 154, 0);
		BLUE = GuiResource.getInstance().getColor(0, 0, 255);
		PURPLE = GuiResource.getInstance().getColor(255, 0, 255);
		ORANGE = GuiResource.getInstance().getColor(240, 94, 35);
		INDIGO = GuiResource.getInstance().getColor(149,58,145);
	}

	@Override
	public void lineGetStyle(LineStyleEvent event) {
		StyledText styledText = ((StyledText) event.widget);
		
		try {
			ArrayList<StyleRange> ranges = new ArrayList<StyleRange>();
			Scanner scanner = new Scanner(styledText.getText());

			for (Token token = scanner.tokenize(); token != null; token = scanner.tokenize()) {
				ranges.add(new StyleRange(token.getStart(), token.getLength(), getColor(token), null));
			}

			event.styles = ranges.toArray(new StyleRange[0]);
		} catch (ExpressionException e) {
			// Ignore
		}
	}

	public Color getColor(Token token) {
		switch (token.getId()) {
		case COMMENT:
			return DARK_GREEN;
		case IDENTIFIER:
			return BLACK;
		case FUNCTION:
			return PURPLE;
		case VARIABLE:
			return RED;
		case DATATYPE:
		case DATEPART:
			return INDIGO;		
		case LITERAL_STRING:
			return GREEN;
		case LITERAL_NUMBER:
		case LITERAL_BINARY_BIT:
		case LITERAL_BINARY_HEX:
			return ORANGE;
		default:
			return BLUE;
		}
	}
}
