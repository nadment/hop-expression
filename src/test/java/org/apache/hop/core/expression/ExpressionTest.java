package org.apache.hop.core.expression;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.time.LocalDate;
import java.time.LocalDateTime;
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

public class ExpressionTest {

	protected Value eval(String e) throws Exception {
		
		Expression expression = Expression.parse(e);

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
		
		//DefaultExpressionContext contextOptimize = new DefaultExpressionContext();	
		//Expression result =  expression.optimize(contextOptimize);

		RowExpressionEvaluator context = new RowExpressionEvaluator(rowMeta);
		context.setRow(row);
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
		Value value =  eval(e);
		assertEquals(expected, value.toNumber(),0);
	}

	protected void evalEquals(String e, LocalDate expected) throws Exception {
		assertEquals(expected.atStartOfDay(), eval(e).toDate());
	}
	
	protected void evalEquals(String e, LocalDateTime expected) throws Exception {
		assertEquals(expected, eval(e).toDate());
	}
	
	protected void evalFails(String e) {
		try {
			eval(e);
			Assert.fail("Syntax should be invalid");
		} catch (Exception ex) {
		}
	}

	@Test
	public void parser() throws Exception {
		//evalFails("Year())");
		//evalFails("Year(()");
		//evalFails("Year(1,2)");
		//evalEquals("10**2+5",105);

		//evalTrue("Cast(0 as Boolean)");

		
		//evalEquals("1+2*3",7);
		//evalEquals("(1+2)*3",9);
		//evalEquals("5+10^2",105);		
	}


}
