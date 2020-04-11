package org.apache.hop.core.expression;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.apache.hop.expression.DefaultExpressionContext;
import org.apache.hop.expression.Expression;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Value;
import org.junit.Test;

public class OptimizerTest {

	protected Expression optimize(String e) throws Exception {

		IExpression expression = ExpressionParser.parse(e);

		DefaultExpressionContext context = new DefaultExpressionContext();
		Expression result = ((Expression) expression).optimize(context);

		System.out.println("optimize(" + e + ") >>> " + result);
		return result;
	}

	protected void optimize(String e, String expected) throws Exception {
		assertEquals(expected, ((Value) optimize(e)).toString());
	}

	protected void optimize(String e, long expected) throws Exception {
		assertEquals(expected, ((Value) optimize(e)).toInteger());
	}

	protected void optimizeTrue(String e) throws Exception {
		assertTrue(((Value) optimize(e)).toBoolean());
	}

	protected void optimizeFalse(String e) throws Exception {
		assertFalse(((Value) optimize(e)).toBoolean());
	}

	protected void optimizeNull(String e) throws Exception {
		assertTrue(((Value) optimize(e)).isNull());
	}

	@Test
	public void test() throws Exception {

		optimize("'A'||'B'", "AB");
		optimize("CONCAT('TES','T')", "TEST");

		optimize("3+1", 4L);
		optimize("3+1+2", 6L);
		optimize("3+1*2", 5L);
		optimize("(3+1)*2", 8L);
		optimize("-(10+2)", -12L);
		optimize("-(0)", 0L);
		
		optimize("1+AGE+3"); // TODO:
		optimize("not ( not [FIELD] is NULL )");
		optimize("-(-[FIELD])");
		optimize("false and true or [FIELD]");
		optimize("false and [FIELD]");

		optimizeFalse("not true");
		optimizeTrue("not false");
		optimizeFalse("true and false");
		optimizeTrue("true or false");
		optimizeNull("true and null");
		optimizeTrue("true is true");
		optimizeTrue("false is false");
		optimizeFalse("true is false");

		optimizeTrue("25>=12");
		optimizeTrue("25>=12 and 14<15");

		optimize("AGE between 3 and (5+1)");
		optimizeFalse("2 between 3 and (5+1)");

		optimize("AGE+3+1");
		optimize("null=null");
		optimizeTrue("'25' in ('1','25','66')");
		optimizeTrue("25.8 between 18 and 32");
		optimizeTrue("Trim(' test ')='test'");
		optimize("Day_Of_Month(Date '2019-02-15')",15L);
	}
}
