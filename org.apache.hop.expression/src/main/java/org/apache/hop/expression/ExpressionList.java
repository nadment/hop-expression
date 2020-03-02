package org.apache.hop.expression;

import java.io.StringWriter;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * Immutable list of expression.
 */
public class ExpressionList extends Expression implements Iterable<Expression> {

	/**
	 * Iterator implementation used to efficiently expose contents of an
	 * ExpressionList as read-only iterator.
	 */
	public class ExpressionIterator implements Iterator<Expression> {

		private int index;

		public ExpressionIterator() {
			index = 0;
		}

		@Override
		public boolean hasNext() {
			return index < list.length;
		}

		@Override
		public Expression next() {
			if (index >= list.length) {
				throw new NoSuchElementException();
			}
			return list[index++];
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}
	}

	/**
	 * An immutable, empty ExpressionList.
	 */
	public static final ExpressionList EMPTY = new ExpressionList() {
	};

	private final Expression[] list;

	public ExpressionList() {
		super();
		this.list = new Expression[0];
	}

	public ExpressionList(Expression... expressions) {
		this.list = expressions;
	}

	public ExpressionList(List<Expression> expressions) {
		this.list = expressions.toArray(new Expression[0]);
	}

	public Expression get(int index) {
		return list[index];
	}

	public boolean isEmpty() {
		return list.length == 0;
	}

	@Override
	public boolean isConstant() {
		for (Expression e : list) {
			if (!e.isConstant()) {
				return false;
			}
		}
		return true;
	}

	public int size() {
		return list.length;
	}

	public Expression[] toArray() {
		return list;
	}

	@Override
	public Value eval(ExpressionContext context) throws ExpressionException {
		throw new ExpressionException("ExpressionException.ExpressionListNotEvaluable");
	}

	public void unparse(StringWriter writer, int leftPrec, int rightPrec) {

		writer.append('(');
		boolean first = true;
		for (Expression expression : list) {
			if (first)
				first = false;
			else {
				writer.append(',');
			}
			expression.unparse(writer, 2, 3);
		}
		// if (parenthese)
		writer.append(')');
	}

	@Override
	public Iterator<Expression> iterator() {
		return new ExpressionIterator();
	}
}
