package org.apache.hop.expression;

import java.math.BigDecimal;
import java.util.Date;
import java.util.Objects;

import org.apache.hop.core.exception.HopValueException;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.i18n.BaseMessages;

public class RowExpressionContext extends DefaultExpressionContext {

	protected static final Class<?> PKG = Expression.class; // for i18n purposes
	
	private IRowMeta rowMeta;
	private Object[] row;

	public RowExpressionContext(IRowMeta rowMeta) {
		this.rowMeta = Objects.requireNonNull(rowMeta);
	}

	public void setRow(Object[] row) {
		this.row = row;
	}

	public Value resolve(String name) throws ExpressionException {

		int index = rowMeta.indexOfValue(name);
		if (index < 0)
			throw new ExpressionException(BaseMessages.getString(PKG,"Expression.FieldNotFound", name));

		IValueMeta valueMeta = rowMeta.getValueMeta(index);
		try {
			switch (valueMeta.getType()) {
			case IValueMeta.TYPE_BOOLEAN:
				Boolean value = rowMeta.getBoolean(row, index);
				if (value == null)
					return Value.NULL;

				return Value.of(value);
			case IValueMeta.TYPE_DATE:
			case IValueMeta.TYPE_TIMESTAMP:
				// No getTimestamp from RowMeta ???
				Date date = rowMeta.getDate(row, index);
				if (date == null)
					return Value.NULL;

			//	LocalDateTime dt = LocalDateTime.ofInstant(date.toInstant(), getZone());
				return Value.of(date.toInstant());
			case IValueMeta.TYPE_STRING:
				String string = rowMeta.getString(row, index);
				return Value.of(string);
			case IValueMeta.TYPE_INTEGER:
				Long integer = rowMeta.getInteger(row, index);
				return Value.of(integer);
			case IValueMeta.TYPE_NUMBER:
				Double number = rowMeta.getNumber(row, index);
				return Value.of(number);
			case IValueMeta.TYPE_BIGNUMBER:
				BigDecimal bignumber = rowMeta.getBigNumber(row, index);
				return Value.of(bignumber);
			case IValueMeta.TYPE_BINARY:
				byte[] binary = rowMeta.getBinary(row, index);
				return Value.of(binary);

			}
		} catch (HopValueException e) {
			throw new ExpressionException("Error field value " + e.toString());
		}

		return null;
	}
}
