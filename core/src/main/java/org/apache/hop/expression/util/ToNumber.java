package org.apache.hop.expression.util;

import java.math.BigDecimal;

/**
 * Emulates Oracle's TO_NUMBER(number) function.
 *
 * <p>
 * <table border="1">
 * <th>
 * 	<td>Input</td>
 * 	<td>Output</td>
 * </th>
 * <tr>
 * 	<td>9</td>
 * 	<td>Value with the specified number of digits</td>
 * </tr>
 * <tr>
 * 	<td>0</td>
 * 	<td>Value with leading zeros</td>
 * </tr> 
 * <tr>
 * <tr>
 * 	<td>. (periode)</td>
 * 	<td>Decimal point</td>
 * </tr>
 * <tr>
 * 	<td>, (comma)</td>
 * 	<td>Group (thousand) separator</td>
 * </tr> 
 * <tr>  
 * <tr> 
 * 	<td>G</td>
 * 	<td>Grouping separator.</td>
 * </tr>
 * 
 * </table>
 * 
 * 
 */
public class ToNumber {

	private ToNumber() {
		// Utility class
	}
	
	public static BigDecimal toNumber(String string, String format) {
		
		
		return null;
	}
}
