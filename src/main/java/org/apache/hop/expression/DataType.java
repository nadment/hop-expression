package org.apache.hop.expression;

/**
 * Enumeration of the data type which can be used to construct a expression.
 * 
 * The order of enum declaration is important to be usable with
 * <code>compareTo</code> method.
 * 
 * If values need to be converted to match the other operands data type, the
 * value with the lower order is converted to the value with the higher order.
 */
public enum DataType {
	/**
	 * A unknown data type
	 */
	NONE,
	
	/**
	 * Unlimited length text
	 */
	STRING,
	
	/**
	 * Boolean value (true or false)
	 */
	BOOLEAN,
	
	/**
	 * Signed long (64-bit) integer
	 */
	INTEGER,
	
	/** 
	 * Double precision floating point number
	 */
	NUMBER,
	
	/** 
	 * Unlimited precision number
	 */
	BIGNUMBER,
	
	/** 
	 * Date-time value with nanosecond precision
	 */
	DATE,
	
	/**
	 * A binary type can be images, sounds, videos, and other types of binary data
	 */
	BINARY;

	private static final DataType[] values = values();

	public static DataType valueOf(int ordinal) {
		return values[ordinal];
	}
}