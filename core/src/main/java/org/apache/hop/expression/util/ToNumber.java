package org.apache.hop.expression.util;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.ParseException;

/**
 * Emulates Oracle's TO_NUMBER(number) function.
 *
 * <p>
 * <table border="1">
 * <th>
 * <td>Input</td>
 * <td>Output</td></th>
 * <tr>
 * <td>9</td>
 * <td>Value with the specified number of digits</td>
 * </tr>
 * <tr>
 * <td>0</td>
 * <td>Value with leading zeros</td>
 * </tr>
 * <tr>
 * <tr>
 * <td>. (periode)</td>
 * <td>Decimal point</td>
 * </tr>
 * <tr>
 * <td>, (comma)</td>
 * <td>Group (thousand) separator</td>
 * </tr>
 * <tr>
 * <tr>
 * <td>G</td>
 * <td>Grouping separator.</td>
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

	public static BigDecimal toNumber(String source, String format) {

		DecimalFormat parser = new DecimalFormat( pattern(format) );
		parser.setParseBigDecimal(true);
		try {
			return  (BigDecimal) parser.parse(source);
		} catch (ParseException e) {
		}
		
		
		return null;
	}

	public static String pattern(String format) {
		int pos = 0;
		StringBuilder output = new StringBuilder();

		while (pos < format.length()) {
			char c = format.charAt(pos);

			switch (c) {
			case ' ':
				break;

			case '0':
			case '.':
			case ',':
				output.append(c);
				break;

			case '9':
				output.append('#');
				break;

			case 'D':
				output.append('.');
				break;
			case 'G':
				output.append(',');

			
			}

			pos++;
		}

		return output.toString();
	}
}
