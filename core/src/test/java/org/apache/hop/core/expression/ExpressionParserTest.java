package org.apache.hop.core.expression;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.RowMetaInterface;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.variables.VariableSpace;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.Expression;
import org.apache.hop.expression.RowExpressionEvaluator;
import org.apache.hop.expression.Value;
import org.junit.Assert;
import org.junit.Test;

public class ExpressionParserTest {

	protected Value eval(String source) throws Exception {		
		Expression expression = Expression.parse(source);

		VariableSpace variables = new Variables();
		variables.setVariable("TEST", "12345");

		RowMetaInterface rowMeta = new RowMeta();
		rowMeta.addValueMeta(new ValueMetaString("NOM"));
		rowMeta.addValueMeta(new ValueMetaString("SEXE"));
		rowMeta.addValueMeta(new ValueMetaInteger("AGE"));
		rowMeta.addValueMeta(new ValueMetaDate("DN"));
		rowMeta.addValueMeta(new ValueMetaBoolean("FLAG"));
		rowMeta.addValueMeta(new ValueMetaBoolean("NULLIS"));

		Object[] row = new Object[6];
		row[0] = "TEST";
		row[1] = "F";
		row[2] = 40L;
		row[3] = new Date();
		row[4] = true;
		row[5] = null;

		RowExpressionEvaluator evaluator = new RowExpressionEvaluator(rowMeta);
		evaluator.setRow(row);
		System.out.println(source+"\t>>> " + expression.toString() );
		return expression.eval(evaluator);
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
		assertEquals(expected, eval(e));
	}

	protected void evalEquals(String e, Long expected) throws Exception {
		assertEquals(expected, eval(e).toInteger(), 0);
	}

	protected void evalEquals(String e, double expected) throws Exception {
		assertEquals(expected, eval(e).toNumber(), 0);
	}

	protected void evalEquals(String e, LocalDate expected) throws Exception {
		Date date = Date.from(expected.atStartOfDay(ZoneId.of("GMT")).toInstant());
		assertEquals(date, eval(e).toDate());
	}

	protected void evalFails(String e) {
		System.out.print("Eval: " + e);

		try {
			eval(e);
			Assert.fail("Syntax should be invalid");
		} catch (Exception ex) {
			System.out.println("\t>>> "+ex.getMessage());
		}
	}

	@Test
	public void TEST() throws Exception {
		// evaluateEquals("Add_Months(Date '2019-01-15',1)", LocalDate.of(2019,
		// Month.FEBRUARY,15));
		// evaluateEquals("CONCAT('TES','T')","TEST");
		// evaluateTrue("CONTAINS(NOM,'ES')");
		// evaluateTrue("NOM =~ 'ES'");
		// evaluateEquals("30/(2)",15);
		//evalEquals("10**2", 100);
		//evalEquals("Pi()", Math.PI);
		//evalTrue("2.000 = 2.00");
		//evalEquals("40/10",4);
		//evalTrue("NULLIS is null");
		evalFails(" 'T' | 'T' ");
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
		evalFails("Cast(123 as Nill)");
		//evalFails("Date '2020-20-28'");
		//evalEquals("-4**2",-16);
	}

}
