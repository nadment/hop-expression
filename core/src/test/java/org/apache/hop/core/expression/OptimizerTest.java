package org.apache.hop.core.expression;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.expression.DefaultExpressionContext;
import org.apache.hop.expression.Expression;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.expression.Value;
import org.junit.Test;

public class OptimizerTest {
		
	protected Expression optimize(String e) throws Exception {
		
		IExpression expression = Expression.parse(e);

		DefaultExpressionContext context = new DefaultExpressionContext();	
		Expression result =  ((Expression)expression).optimize(context);
		
		System.out.println("optimize("+e+") >>> "+result);
		return result;
	}
	
	protected Value eval(Expression expression) throws Exception {
		
		IRowMeta rowMeta = new RowMeta();

		Object[] row = new Object[0];

		RowExpressionContext context = new RowExpressionContext(rowMeta);
		context.setRow(row);
		return expression.eval(context);
	}
	
	
	protected void optimizeEquals(String e, String expected) throws Exception {
		assertEquals(expected, eval(optimize(e)).toString());
	}

	protected void optimizeTrue(String e) throws Exception {
		assertTrue(eval(optimize(e)).toBoolean());
	}
	protected void optimizeFalse(String e) throws Exception {
		assertFalse(eval(optimize(e)).toBoolean());
	}
	
	protected void optimizeNull(String e) throws Exception {
		assertTrue(eval(optimize(e)).isNull());
	}
	
	@Test
	public void test() throws Exception {
		
		optimizeEquals("'A'||'B'","AB");
		optimizeEquals("CONCAT('TES','T')","TEST");
		
		optimize("3+1");
		optimize("3+1+2");
		optimize("3+1*2");
		optimize("(3+1)*2");		
		optimize("-(10+2)");
		optimize("1+AGE+3"); // TODO:
		
		
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
		optimize("2 between 3 and (5+1)");
	
		
		optimize("AGE+3+1");
		optimize("null=null");
		optimizeTrue("'25' in ('1','25','66')");
		optimize("25 between 18 and 32");
		optimizeTrue("Trim(' test ')='test'");
		optimize("Day_Of_Month(Date '2019-02-15')");
	}
}
