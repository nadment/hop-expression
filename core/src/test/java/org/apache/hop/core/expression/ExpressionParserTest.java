package org.apache.hop.core.expression;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

import org.junit.Test;

public class ExpressionParserTest extends ExpressionTest {

	@Test
	public void comment() throws Exception {
		evalTrue(" // Test \n  true ");
		evalTrue(" /** Test */  true ");
		evalTrue("/**\n * Comment on multi line\n *\n */	True");
	}

	@Test
	public void literalDate() throws Exception {
		evalEquals("Date '2019-02-25'", LocalDate.of(2019, 2, 25));
		evalEquals("Date '28-02-25'", LocalDate.of(28, 2, 25));
		evalEquals("Date '2028-Feb-25'", LocalDate.of(2028, 2, 25));
	}

	@Test
	public void literalTime() throws Exception {
		evalEquals("Time '23:48:59'", LocalDateTime.of(1900, 1, 1, 23, 48, 59));
		// TODO: evalEquals("Time '01:05'", LocalDateTime.of(1900, 1, 1, 23, 48, 59));
		// TODO: evalEquals("Time '10:30 am'", LocalDateTime.of(1900, 1, 1, 23, 48, 59));
		// TODO: evalEquals("Time '06:25:15 PM'", LocalDateTime.of(1900, 1, 1, 23, 48, 59));
	}

	// @Test
	public void literalTimestamp() throws Exception {
		evalEquals("Timestamp '2019-02-25 23:59'", LocalDateTime.of(2019, 2, 25, 23, 59, 00));
		evalEquals("Timestamp '2019-02-25 23:59:59'", LocalDateTime.of(2019, 2, 25, 23, 59, 59));
		evalEquals("Timestamp '2019-01-01 15:28:59'", LocalDateTime.of(2019, 1, 1, 15, 28, 59));
	}

	@Test
	public void literalString() throws Exception {

		// Single quote
		evalTrue("'test'='test'");
		evalEquals("'te''st'", "te'st");
		evalEquals("'te\"st'", "te\"st");

		// Double quote
		//evalEquals("\"te'st\"", "te'st");
		//evalEquals("\"te\"\"st\"", "te\"st");

		// Escape tab
		evalEquals("'\\t'", "\t");

		// Escape backspace
		evalEquals("'\\b'", "\b");

		// Escape form feed
		evalEquals("'\\f'", "\f");

		// Escape newline
		evalEquals("'\\n'", "\n");

		// Escape carriage return
		evalEquals("'\\r'", "\r");

		// Escape 16 bit unicode
		evalEquals("'\\u20AC'", "€");

		// Escape 32 bit unicode
		evalEquals("'\\U000020AC'", "€");

	}

	@Test
	public void literalNumeric() throws Exception {

		// Hexadecimal
		evalEquals("0xff", 255);
		evalEquals("0xfE", 254);
		evalEquals("0x0F", 15);
		evalFails("0X0F");
		evalFails("0X0FG");

		// Binary
		evalEquals("0b10", 2);
		evalEquals("0b00000010", 2);
		evalFails("0B010101");
		evalFails("0B010201");

		// Integer
		evalEquals("-9223372036854775818", Long.MIN_VALUE);
		evalEquals("9223372036854775807", Long.MAX_VALUE);

		// Big number
		evalEquals("2.3E2", new BigDecimal("2.3E2"));
		evalEquals("-2.3E-2", new BigDecimal("-2.3E-2"));
		evalEquals("-2.3e-2", new BigDecimal("-2.3E-2"));
		evalEquals("12345678901234567890123456789012345678901234567890",
				new BigDecimal("12345678901234567890123456789012345678901234567890"));

		evalFails("2E2E2");
		evalFails("2E-2.2");
		evalFails("-2.3EE-2");
		evalFails("-2.3E--2");
	}

	@Test
	public void reservedWord() throws Exception {
		evalEquals("Upper([FROM])", "PARIS");
	}

	@Test
	public void syntaxError() throws Exception {
		evalFails("'T'||'T");
		evalFails("\"T\"||\"T");
		evalFails("9!7");
		evalFails("9+(");
		evalFails("9+*(");
		evalFails("Year(");
		evalFails("Year)");
		evalFails("Year()");
		evalFails("Year(()");
		evalFails("Year())");
		evalFails("Year(1,2)");
		evalFails("TRUE AND");
		evalFails("5 BETWEEN 4 AND");
		evalFails("5 BETWEEN 4 OR");
		evalFails("case when 1=1 then 1 else 0");
		evalFails("case when 1=1 then 1 else  end ");
		evalFails("case 1 when 1  else 0 end");
		evalFails("1 in ()    ");
		evalFails("1 in (,2,3)");
		evalFails("1 in (1,2,3");
		evalFails("1 in (1,,3)");
		evalFails("1 in (1,2,)");

		// evalFails("Date '2020-20-28'");
		// evalEquals("-4**2",-16);
	}

}
