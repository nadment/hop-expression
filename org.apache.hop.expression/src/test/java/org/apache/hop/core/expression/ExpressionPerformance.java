package org.apache.hop.core.expression;

import java.util.Date;

import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.RowMetaInterface;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.expression.Expression;
import org.apache.hop.expression.RowExpressionEvaluator;
import org.apache.hop.expression.Value;
import org.junit.Test;

public class ExpressionPerformance {

	@Test
	public void perf() throws Exception {
		System.out.println("Started performance test");

		long cycle = 1000000;
		long startTime = System.currentTimeMillis();

		Expression expression = Expression.parse("NOM||left(to_char(AGE+5,'000'),2)");

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
		System.out.println(expression.toString());

		for (long i = cycle; i > 0; i--) {
			Value result = expression.eval(evaluator);
		}

		long endTime = System.currentTimeMillis();

		long duration = endTime - startTime;

		System.out.println("Performance test finished");
		System.out.println("Duration for " + cycle + " cycles = " + duration);
	}
}
