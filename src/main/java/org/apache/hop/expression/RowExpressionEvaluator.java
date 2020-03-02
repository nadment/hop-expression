package org.apache.hop.expression;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.Objects;

import org.apache.hop.core.exception.HopValueException;
import org.apache.hop.core.row.RowMetaInterface;
import org.apache.hop.core.row.ValueMetaInterface;

public class RowExpressionEvaluator extends DefaultExpressionContext {

	private RowMetaInterface rowMeta;
	private Object[] row;

	public RowExpressionEvaluator(RowMetaInterface rowMeta) {
		this.rowMeta = Objects.requireNonNull(rowMeta);
	}

	public void setRow(Object[] row) {
		this.row = row;
	}

	public Value resolve(String name) throws ExpressionException {

		int index = rowMeta.indexOfValue(name);
		if (index < 0)
			throw new ExpressionException("ExpressionException.FieldNotFound", name);

		ValueMetaInterface valueMeta = rowMeta.getValueMeta(index);
		try {
			switch (valueMeta.getType()) {
			case ValueMetaInterface.TYPE_BOOLEAN:
				Boolean value = rowMeta.getBoolean(row, index);
				if (value == null)
					return Value.NULL;

				return Value.of(value);
			case ValueMetaInterface.TYPE_DATE:
			case ValueMetaInterface.TYPE_TIMESTAMP:
				// No getTimestamp from RowMeta ???
				Date date = rowMeta.getDate(row, index);
				if (date == null)
					return Value.NULL;

				LocalDateTime dt = LocalDateTime.ofInstant(date.toInstant(), getZone());
				return Value.of(dt);
			case ValueMetaInterface.TYPE_STRING:
				String string = rowMeta.getString(row, index);
				return Value.of(string);
			case ValueMetaInterface.TYPE_INTEGER:
				Long integer = rowMeta.getInteger(row, index);
				return Value.of(integer);
			case ValueMetaInterface.TYPE_NUMBER:
				Double number = rowMeta.getNumber(row, index);
				return Value.of(number);
			case ValueMetaInterface.TYPE_BIGNUMBER:
				BigDecimal bignumber = rowMeta.getBigNumber(row, index);
				return Value.of(bignumber);
			case ValueMetaInterface.TYPE_BINARY:
				byte[] binary = rowMeta.getBinary(row, index);
				return Value.of(binary);

			}
		} catch (HopValueException e) {
			throw new ExpressionException("Error field value " + e.toString());
		}

		return null;
	}
}
