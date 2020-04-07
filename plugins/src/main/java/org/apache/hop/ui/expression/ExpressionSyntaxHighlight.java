package org.apache.hop.ui.expression;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.apache.hop.core.database.SqlScriptStatement;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.OperatorRegistry;
import org.apache.hop.ui.core.gui.GUIResource;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.LineStyleEvent;
import org.eclipse.swt.custom.LineStyleListener;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Color;

public class ExpressionSyntaxHighlight implements LineStyleListener {
	
	static final String[] KEYWORDS = { "%", "+", "-", "*", "^", "/", "=", "<>", "!=", "<", "<=", ">", ">=", "(", ")",
			"||", "AS", "AND", "BETWEEN", "CASE", "CAST", "DATE", "ELSE", "END", "ESCAPE", "FALSE", "IN", "IS", "LIKE",
			"NOT", "NULL", "OR", "THEN", "TRUE", "WHEN", "XOR" };
// TODO: Java 9
//	private static final Set<String> KEYWORDS = Set.of( "%", "+", "-", "*", "**", "/", "<", "<=", ">", ">=", "(", ")", "||", "AND", "BETWEEN", "CASE", "DATE", "ELSE",
//			"END", "ESCAPE", "FALSE", "IN", "IS", "LIKE", "NOT", "NULL", "OR", "THEN", "TRUE", "WHEN" ,"XOR");

	
	Scanner scanner = new Scanner();
	int[] tokenColors;
	Color[] colors;
	Vector<int[]> blockComments = new Vector<int[]>();

	public static final int EOF = -1;
	public static final int EOL = 10;

	public static final int WORD = 0;
	public static final int WHITE = 1;
	public static final int KEY = 2;
	public static final int COMMENT = 3; // single line comment: //
	public static final int STRING = 5;
	public static final int OTHER = 6;
	public static final int NUMBER = 7;
	public static final int FUNCTION = 8;

	public static final int MAXIMUM_TOKEN = 9;

	private List<SqlScriptStatement> scriptStatements;

	public ExpressionSyntaxHighlight() {
		initializeColors();
		scriptStatements = new ArrayList<SqlScriptStatement>();
		scanner = new Scanner();
	}

	Color getColor(int type) {
		if (type < 0 || type >= tokenColors.length) {
			return null;
		}
		return colors[tokenColors[type]];
	}

	boolean inBlockComment(int start, int end) {
		for (int i = 0; i < blockComments.size(); i++) {
			int[] offsets = blockComments.elementAt(i);
			// start of comment in the line
			if ((offsets[0] >= start) && (offsets[0] <= end)) {
				return true;
			}
			// end of comment in the line
			if ((offsets[1] >= start) && (offsets[1] <= end)) {
				return true;
			}
			if ((offsets[0] <= start) && (offsets[1] >= end)) {
				return true;
			}
		}
		return false;
	}

	void initializeColors() {

		colors = new Color[] { GUIResource.getInstance().getColor(0, 0, 0), // black
				GUIResource.getInstance().getColor(255, 0, 0), // red
				GUIResource.getInstance().getColor(63, 127, 95), // green
				GUIResource.getInstance().getColor(0, 0, 255), // blue
				GUIResource.getInstance().getColor(255, 0, 255) // SQL Functions / Rose
		};

		tokenColors = new int[MAXIMUM_TOKEN];
		tokenColors[WORD] = 0;
		tokenColors[WHITE] = 0;
		tokenColors[KEY] = 3;
		tokenColors[COMMENT] = 2;
		tokenColors[STRING] = 1;
		tokenColors[OTHER] = 0;
		tokenColors[NUMBER] = 0;
		tokenColors[FUNCTION] = 4;
	}

	/**
	 * Event.detail line start offset (input) Event.text line text (input)
	 * LineStyleEvent.styles Enumeration of StyleRanges, need to be in order.
	 * (output) LineStyleEvent.background line background color (output)
	 */
	public void lineGetStyle(LineStyleEvent event) {
		Vector<StyleRange> styles = new Vector<StyleRange>();
		int token;
		StyleRange lastStyle;

		if (inBlockComment(event.lineOffset, event.lineOffset + event.lineText.length())) {
			styles.addElement(new StyleRange(event.lineOffset, event.lineText.length() + 4, colors[1], null));
			event.styles = new StyleRange[styles.size()];
			styles.copyInto(event.styles);
			return;
		}
		scanner.setRange(event.lineText);
		String xs = ((StyledText) event.widget).getText();
		if (xs != null) {
			parseBlockComments(xs);
		}
		token = scanner.nextToken();
		while (token != EOF) {
			if (token != OTHER) {
				if ((token == WHITE) && (!styles.isEmpty())) {
					int start = scanner.getStartOffset() + event.lineOffset;
					lastStyle = styles.lastElement();
					if (lastStyle.fontStyle != SWT.NORMAL) {
						if (lastStyle.start + lastStyle.length == start) {
							// have the white space take on the style before it to minimize font style
							// changes
							lastStyle.length += scanner.getLength();
						}
					}
				} else {
					Color color = getColor(token);
					if (color != colors[0]) { // hardcoded default foreground color, black
						StyleRange style = new StyleRange(scanner.getStartOffset() + event.lineOffset,
								scanner.getLength(), color, null);
						// if ( token == KEY ) {
						// style.fontStyle = SWT.BOLD;
						// }
						if (styles.isEmpty()) {
							styles.addElement(style);
						} else {
							lastStyle = styles.lastElement();
							if (lastStyle.similarTo(style) && (lastStyle.start + lastStyle.length == style.start)) {
								lastStyle.length += style.length;
							} else {
								styles.addElement(style);
							}
						}
					}
				}
			}
			token = scanner.nextToken();
		}

		// See which backgrounds to color...
		//
		if (scriptStatements != null) {
			for (SqlScriptStatement statement : scriptStatements) {
				// Leave non-executed statements alone.
				//
				StyleRange styleRange = new StyleRange();
				styleRange.start = statement.getFromIndex();
				styleRange.length = statement.getToIndex() - statement.getFromIndex();

				if (statement.isComplete()) {
					if (statement.isOk()) {
						// GUIResource.getInstance().getColor(63, 127, 95), // green

						styleRange.background = GUIResource.getInstance().getColor(244, 238, 224); // honey dew
					} else {
						styleRange.background = GUIResource.getInstance().getColor(250, 235, 215); // Antique White
					}
				} else {
					styleRange.background = GUIResource.getInstance().getColorWhite();
				}

				styles.add(styleRange);
			}
		}

		event.styles = new StyleRange[styles.size()];
		styles.copyInto(event.styles);
	}

	public void parseBlockComments(String text) {
		blockComments = new Vector<int[]>();
		StringReader buffer = new StringReader(text);
		int ch;
		boolean blkComment = false;
		int cnt = 0;
		int[] offsets = new int[2];
		boolean done = false;

		try {
			while (!done) {
				switch (ch = buffer.read()) {
				case -1: {
					if (blkComment) {
						offsets[1] = cnt;
						blockComments.addElement(offsets);
					}
					done = true;
					break;
				}
				case '/': {
					ch = buffer.read();
					if ((ch == '*') && (!blkComment)) {
						offsets = new int[2];
						offsets[0] = cnt;
						blkComment = true;
						cnt++;
					} else {
						cnt++;
					}
					cnt++;
					break;
				}
				case '*': {
					if (blkComment) {
						ch = buffer.read();
						cnt++;
						if (ch == '/') {
							blkComment = false;
							offsets[1] = cnt;
							blockComments.addElement(offsets);
						}
					}
					cnt++;
					break;
				}
				default: {
					cnt++;
					break;
				}
				}
			}
		} catch (IOException e) {
			// ignore errors
		}
	}

	/**
	 * A simple fuzzy scanner for Java
	 */
	public class Scanner {
		protected Map<String, Integer> fgKeys = null;
		protected Map<?, ?> fgFunctions = null;
		protected Map<String, Integer> kfKeys = null;
		protected Map<?, ?> kfFunctions = null;

		protected StringBuilder fBuffer = new StringBuilder();
		protected String fDoc;
		protected int fPos;
		protected int fEnd;
		protected int fStartToken;
		protected boolean fEofSeen = false;

		public Scanner() {
			initialize();
		}

		/**
		 * Returns the ending location of the current token in the document.
		 */
		public final int getLength() {
			return fPos - fStartToken;
		}

		/**
		 * Initialize the lookup table.
		 */
		void initialize() {
			fgKeys = new Hashtable<String, Integer>();
			Integer k = new Integer(KEY);
			for (String keyword : KEYWORDS) {
				fgKeys.put(keyword, k);
			}

			kfKeys = new Hashtable<String, Integer>();
			Integer f = new Integer(FUNCTION);
			for (Function function : OperatorRegistry.getFunctions()) {
				kfKeys.put(function.getName(), f);
			}
		}

		/**
		 * Returns the starting location of the current token in the document.
		 */
		public final int getStartOffset() {
			return fStartToken;
		}

		/**
		 * Returns the next lexical token in the document.
		 */
		public int nextToken() {
			int c;
			fStartToken = fPos;
			while (true) {
				switch (c = read()) {
				case EOF:
					return EOF;
				case '/': // comment
					c = read();
					if (c == '/') {
						while (true) {
							c = read();
							if ((c == EOF) || (c == EOL)) {
								unread(c);
								return COMMENT;
							}
						}
					} else {
						unread(c);
					}
					return OTHER;
				case '-': // comment
					c = read();
					if (c == '-') {
						while (true) {
							c = read();
							if ((c == EOF) || (c == EOL)) {
								unread(c);
								return COMMENT;
							}
						}
					} else {
						unread(c);
					}
					return OTHER;
				case '\'': // char const
					for (;;) {
						c = read();
						switch (c) {
						case '\'':
							return STRING;
						case EOF:
							unread(c);
							return STRING;
						case '\\':
							c = read();
							break;
						default:
							break;
						}
					}

				case '"': // string
					for (;;) {
						c = read();
						switch (c) {
						case '"':
							return STRING;
						case EOF:
							unread(c);
							return STRING;
						case '\\':
							c = read();
							break;
						default:
							break;
						}
					}

				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
					do {
						c = read();
					} while (Character.isDigit((char) c));
					unread(c);
					return NUMBER;
				default:
					if (Character.isWhitespace((char) c)) {
						do {
							c = read();
						} while (Character.isWhitespace((char) c));
						unread(c);
						return WHITE;
					}
					if (Character.isJavaIdentifierStart((char) c)) {
						fBuffer.setLength(0);
						do {
							fBuffer.append((char) c);
							c = read();
						} while (Character.isJavaIdentifierPart((char) c));
						unread(c);
						Integer i = fgKeys.get(fBuffer.toString());
						if (i != null) {
							return i.intValue();
						}
						i = kfKeys.get(fBuffer.toString());
						if (i != null) {
							return i.intValue();
						}
						return WORD;
					}
					return OTHER;
				}
			}
		}

		/**
		 * Returns next character.
		 */
		protected int read() {
			if (fPos <= fEnd) {
				return fDoc.charAt(fPos++);
			}
			return EOF;
		}

		public void setRange(String text) {
			fDoc = text.toUpperCase();
			fPos = 0;
			fEnd = fDoc.length() - 1;
		}

		protected void unread(int c) {
			if (c != EOF) {
				fPos--;
			}
		}
	}

}