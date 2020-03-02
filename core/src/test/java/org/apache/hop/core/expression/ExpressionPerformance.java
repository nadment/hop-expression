package org.apache.hop.core.expression;

import java.util.Date;

import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.expression.Expression;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.expression.Value;
import org.junit.Test;

public class ExpressionPerformance {

	
	public void perf(String e)  {
		Expression expression = ExpressionParser.parse("NOM||left(to_char(AGE+5,'000'),2)");

		IRowMeta rowMeta = new RowMeta();
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

		RowExpressionContext context = new RowExpressionContext(rowMeta);
		context.setRow(row);
				
		long cycle = 10000000;
		long startTime = System.currentTimeMillis();
		for (long i = cycle; i > 0; i--) {
			Value result = expression.eval(context);
		}

		long endTime = System.currentTimeMillis();
		long duration = endTime - startTime;

		System.out.println("Performance(\""+e+"\") Duration for " + cycle + " cycles = " + duration);
	}
	
	
	@Test
	public void performance() {

		//perf("NOM||left(to_char(AGE+5,'000'),2)");
		//perf("Date '2020-05-06'");
		//perf("To_DATE('2020-FEB-06','YYYY-MM-DD'");

	} 
}
