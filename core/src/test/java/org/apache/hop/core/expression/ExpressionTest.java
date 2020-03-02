package org.apache.hop.core.expression;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Date;

import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.Expression;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.expression.ExpressionParserException;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.expression.Value;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class ExpressionTest {

	private RowExpressionContext context;

	public RowExpressionContext getContext() {
		return context;
	}

	@Before
	public void setupOnce() throws Exception {

		IVariables variables = new Variables();
		variables.setVariable("TEST", "12345");

		IRowMeta rowMeta = new RowMeta();
		rowMeta.addValueMeta(new ValueMetaString("NAME"));
		rowMeta.addValueMeta(new ValueMetaString("SEX"));
		rowMeta.addValueMeta(new ValueMetaInteger("AGE"));
		rowMeta.addValueMeta(new ValueMetaDate("DN"));
		rowMeta.addValueMeta(new ValueMetaBoolean("FLAG"));
		rowMeta.addValueMeta(new ValueMetaBoolean("NULLIS"));
		rowMeta.addValueMeta(new ValueMetaInteger("YEAR"));
		rowMeta.addValueMeta(new ValueMetaString("FROM"));

		Object[] row = new Object[8];
		row[0] = "TEST";
		row[1] = "F";
		row[2] = 40L;
		row[3] = new Date();
		row[4] = true;
		row[5] = null;
		row[6] = 2020L;
		row[7] = "Paris";

		context = new RowExpressionContext(rowMeta);
		context.setRow(row);
	}

	protected Value eval(String e) throws Exception {

		Expression expression = ExpressionParser.parse(e);

		return expression.eval(context);
	}

	protected void evalNull(String e) throws Exception {
		assertTrue(eval(e).isNull());
	}

	protected void evalTrue(String e) throws Exception {
		assertTrue(eval(e).toBoolean());
	}

	protected void evalFalse(String e) throws Exception {
		assertFalse(eval(e).toBoolean());
	}

	protected void evalEquals(String e, String expected) throws Exception {
		assertEquals(expected, eval(e).toString());
	}

	protected void evalEquals(String e, Long expected) throws Exception {
		assertEquals(expected.longValue(), eval(e).toInteger());
	}

	protected void evalEquals(String e, double expected) throws Exception {
		Value value = eval(e);
		assertEquals(expected, value.toNumber(), 0);
	}

	protected void evalEquals(String e, BigDecimal expected) throws Exception {
		Value value = eval(e);
		assertEquals(expected, value.toBigNumber());
	}

	protected void evalEquals(String e, Instant expected) throws Exception {
		assertEquals(expected, eval(e).toDate());
	}

	protected void evalEquals(String e, LocalTime expected) throws Exception {
		assertEquals(Instant.from(expected), eval(e).toDate());
	}

	protected void evalEquals(String e, LocalDate expected) throws Exception {
		assertEquals(expected.atStartOfDay(ZoneId.of("UTC")).toInstant(), eval(e).toDate());
	}

	protected void evalEquals(String e, LocalDateTime expected) throws Exception {
		assertEquals(expected.atZone(ZoneId.of("UTC")).toInstant(), eval(e).toDate());
	}

	protected void evalFails(String e) {
		try {
			eval(e);
			Assert.fail("Syntax should be invalid");
		}
		 catch (ExpressionParserException ex) {
			 System.out.print(ex.getSource());
			 System.out.print(" >>> ");
			 System.err.println(ex.toString());
			}
		catch (ExpressionException ex) {
			System.out.println(e + " >>> Evaluation exception  " + ex.toString());
		}
	 catch (Exception ex) {
			Assert.fail("Uncatched exception " + ex.getClass());
		 }
	}

	@Test
	public void parser() throws Exception {
		// evalTrue("'give me 30% discount' like '%30!%%' escape '!'");
		// evalFalse("'AA' like '_'");
		// evalEquals("0xF+0", 15);
		//evalEquals("Date '2019-02-25'-5/(60*24)", LocalDateTime.of(2019, 2, 24, 23, 55, 0));
		//evalEquals("Date '2019-02-25'-0.5", LocalDateTime.of(2019, 2, 24, 12, 0, 0));
		//evalFails("0X0F");
		//evalFails("9!7");
		//evalFails("Year()");
		//evalFails("LPad('test',-8)");
		//evalEquals("LPad('test',3,'*')", "tes");
		//evalFails("Ceil('x')");
		//evalFails("Date(2020,13,22)");
		//evalEquals("To_Char(0.1,'999.99')", " .10");
		//evalEquals("To_Char(0,'9999')", " 0");
		//evalFails("To_Char(12,'MI99')");
		///evalEquals("TO_CHAR(7,'9999PR')","    7 ");
		//evalEquals("TO_CHAR(-7,'9999PR')","   <7>");
		//evalEquals("TO_CHAR(-7,'$99')", " -XXX7");
		//evalEquals("TO_CHAR(0,'99.99')", "   .00");
		//evalEquals("TO_CHAR(0,'9999')", "    0");
		//evalEquals("TO_CHAR(123.456,'9.9EEEE')", "  1.2E+02");

	}

}
