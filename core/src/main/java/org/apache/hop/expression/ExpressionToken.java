package org.apache.hop.expression;

import java.util.Collection;

public class ExpressionToken {

	/**
	 * Enumerates the possible types of {@link ExpressionToken}.
	 */

	public static enum Id {

		AS,

		/**
		 * Variable "${var}" if scanner is used without variable environment substitution.
		 */
		VARIABLE,
		
		/**
		 * The bitwise AND operator "&".
		 */
		BITWISE_AND("&"),

		/**
		 * The bitwise NOT operator "~".
		 */
		BITWISE_NOT("~"),

		/**
		 * The bitwise OR operator "|".
		 */
		BITWISE_OR("|"),

		/**
		 * The bitwise exclusive OR operator "^".
		 */
		BITWISE_XOR("^"),

		/**
		 * Case when operator
		 */
		CASE,

		/**
		 * Cast operator
		 */
		CAST,

		/**
		 * Concat operator <code>||<code>
		 */
		CONCAT("||"),

		/**
		 * Contains operator <code>=~<code>
		 */
		CONTAINS("=~"),

		/**
		 * Comment
		 */
		COMMENT,

		/**
		 * Comma separator
		 */
		COMMA(","),

		/**
		 * Left parenthesis
		 */
		LPARENTHESIS("("),

		/**
		 * Right parenthesis
		 */
		RPARENTHESIS(")"),


		/**
		 * Literal number.
		 */
		LITERAL_NUMBER,
		
		/** Literal hex number 0x1234567890ABCDEF */
		LITERAL_HEXNUMBER,

		/** Literal bit number 0b1101010101 */
		LITERAL_BITNUMBER,

		/**
		 * Literal string.
		 */
		LITERAL_TEXT,

		/**
		 * The "DATE" word for litteral date.
		 */
		DATE,
		/**
		 * The "TIMESTAMP" word for litteral timesamp.
		 */
		TIMESTAMP,
		
		/**
		 * Identifier
		 */
		IDENTIFIER,

		/**
		 * Function
		 */
		FUNCTION,



		/**
		 * The arithmetic division operator, "/".
		 */
		DIVIDE("/"),

		/**
		 * The arithmetic multiplication operator, "*".
		 */
		MULTIPLY("*"),

		/**
		 * ESCAPE word for like operator
		 */
		ESCAPE,

		/**
		 * The arithmetic power operator, "**".
		 */
		POWER("**"),

		/**
		 * The arithmetic remainder operator, "MOD" (and "%" in some dialects).
		 */
		MODULUS("%"),

		/**
		 * The arithmetic unary plus (positive) operator "+" or the arithmetic addition operator "+".
		 */
		PLUS("+"),

		/**
		 * The arithmetic unary minus (negative) operator "-" or the arithmetic subtract operator "-".
		 */
		MINUS("-"),

		/**
		 * The "IN" operator.
		 */
		IN,

		/**
		 * The "BETWEEN" operator.
		 */
		BETWEEN,

		/**
		 * The less-than operator "&lt;".
		 */
		LESS_THAN("<"),

		/**
		 * The greater-than operator "&gt;".
		 */
		GREATER_THAN(">"),

		/**
		 * The less-than-or-equal operator "&lt;=".
		 */
		LESS_THAN_OR_EQUAL("<="),

		/**
		 * The greater-than-or-equal operator "&gt;=".
		 */
		GREATER_THAN_OR_EQUAL(">="),

		/**
		 * The equals operator "=".
		 */
		EQUAL("="),

		/**
		 * Compares whether two expressions are equal.
		 * 
		 * The function is NULL-safe, meaning it treats NULLs as known values for
		 * comparing equality. Note that this is different from the EQUAL comparison
		 * operator (=), which treats NULLs as unknown values.
		 */
		EQUAL_NULL,

		/**
		 * The not-equals operator, "&#33;=".
		 */
		NOT_EQUAL("!="),

		/**
		 * The not-equals operator "&lt;&gt;".
		 */
		LESS_THAN_OR_GREATER_THAN("<>"),

		/**
		 * The logical "OR" operator.
		 */
		OR,

		/**
		 * The logical "XOR" operator.
		 */
		XOR,

		/**
		 * The logical "AND" operator or keyword for BEETWEN value1 "AND" value2 .
		 */
		AND,

		/**
		 * The "LIKE" operator.
		 */
		LIKE,

		/**
		 * The logical "NOT" operator.
		 */
		NOT,

		/**
		 * The literal value "NULL".
		 */
		NULL,

		/**
		 * The "IS" operator.
		 */
		IS,

		/**
		 * The literal value "TRUE".
		 */
		TRUE,

		/**
		 * The literal value "FALSE".
		 */
		FALSE,

		ELSE, THEN, END, WHEN,

		/**
		 * The value type "BOOLEAN" for CAST operator
		 */
		BOOLEAN,
		/**
		 * The value type "STRING" for CAST operator
		 */				
		STRING,
		/**
		 * The value type "INTEGER" for CAST operator
		 */				
		INTEGER,
		/**
		 * The value type "NUMBER" for CAST operator
		 */				
		NUMBER,
		/**
		 * The value type "BIGNUMBER" for CAST operator
		 */				
		BIGNUMBER,
		/**
		 * The value type "BINARY" for CAST operator
		 */				
		BINARY;

		private final String source;

		Id() {
			this.source = name();
		}

		Id(final String source) {
			this.source = source;
		}

		@Override
		public String toString() {
			return source;
		}
	}

	private final Id id;

	private final String text;

	// The position of the first character of this Token.
	private final int start;
	private final int end;

	protected ExpressionToken(Id id, int start, int end) {
		this(id, start, end, null);
	}

	protected ExpressionToken(Id id, int start, int end, String text) {
		this.id = id;
		this.start = start;
		this.end = end;
		this.text = text;
	}

	public boolean is(Id id) {
		return this.id == id;
	}

	/**
	 * Returns whether this {@code Type} belongs to a given category.
	 *
	 * @param category Category
	 * @return Whether this kind belongs to the given category
	 */
	public boolean is(Collection<Id> ids) {
		return ids.contains(this.id);
	}

	public String toString() {
		String s = (text == null) ? id.toString() : (id + "(" + text + ")");

		return "[" + start + ":" + end + "]" + s;
	}

	public Id getId() {
		return id;
	}

	public int getStart() {
		return start;
	}

	public int getEnd() {
		return end;
	}

	public int getLength() {
		return end-start;
	}

	
	public String getText() {
		return text;
	}

}