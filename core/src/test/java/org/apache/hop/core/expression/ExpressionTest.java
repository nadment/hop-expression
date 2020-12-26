/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.core.expression;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Month;
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
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.expression.Value;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class ExpressionTest {

	private RowExpressionContext context;

	public IExpressionContext getContext() {
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
	

	protected Value eval(String source) throws Exception {
		Expression expression = ExpressionParser.parse(source);
		return expression.eval(context);
	}

	protected void evalNull(String source) throws Exception {
		assertTrue(eval(source).isNull());
	}

	protected void evalTrue(String source) throws Exception {
		assertTrue(eval(source).toBoolean());
	}

	protected void evalFalse(String source) throws Exception {
		assertFalse(eval(source).toBoolean());
	}

	protected void evalEquals(String source, String expected) throws Exception {
		assertEquals(expected, eval(source).toString());
	}

	protected void evalEquals(String source, Long expected) throws Exception {
		assertEquals(expected.longValue(), eval(source).toInteger());
	}

	protected void evalEquals(String source, double expected) throws Exception {
		Value value = eval(source);
		assertEquals(expected, value.toNumber(), 0);
	}

	protected void evalEquals(String source, BigDecimal expected) throws Exception {
		Value value = eval(source);
		assertEquals(expected, value.toBigNumber());
	}

	protected void evalEquals(String source, Instant expected) throws Exception {
		assertEquals(expected, eval(source).toDate());
	}

	protected void evalEquals(String source, LocalTime expected) throws Exception {
		assertEquals(Instant.from(expected), eval(source).toDate());
	}

	protected void evalEquals(String source, LocalDate expected) throws Exception {
		assertEquals(expected.atStartOfDay(ZoneId.of("UTC")).toInstant(), eval(source).toDate());
	}

	protected void evalEquals(String source, LocalDateTime expected) throws Exception {
		assertEquals(expected.atZone(ZoneId.of("UTC")).toInstant(), eval(source).toDate());
	}

	protected void evalFails(String source) {
		try {
			eval(source);
			Assert.fail("Syntax should be invalid");
		} catch (ExpressionParserException ex) {
			System.out.print(ex.getSource());
			System.out.print(" >>> ");
			System.err.println(ex.toString());
		} catch (ExpressionException ex) {
			System.out.println(source + " >>> " + ex.toString());
		} catch (Exception ex) {
			Assert.fail("Uncatched exception " + ex.getClass());
		}
	}

	@Test
	public void parser() throws Exception {
		//evalEquals("TO_CHAR(-01.5, '$90.99MI')","1.5-"); 
		evalEquals("TO_CHAR(12923)", "12923");
		evalEquals("TO_CHAR(0.45)", ".45");
		
		
		//evalEquals("TO_CHAR(-7,'99MI')", " 7-");

		// evalEquals("TO_CHAR(12,'S99')", "+12");
		// evalEquals("TO_NUMBER('12,345,678', '999G999G999')", 12345678);
		// evalEquals("To_Date('01/2/52','DD/MM/RRRR')", LocalDate.of(1952, 2, 1));
		// evalEquals("To_Date('01/2/0001','DD/MM/RRRR')", LocalDate.of(2001, 2, 1));

//		evalEquals("To_Date('01/02/-100','DD/MM/SYYYY')", LocalDate.of(-100, 2, 1));
		// evalEquals("To_Date('01/II/2020','DD/RM/YYYY')", LocalDate.of(2020, 2, 1));
		// evalEquals("To_Date('2020,feb,25','YYYY,MON,DD')", LocalDate.of(2020, 2,
		// 25));
		// evalEquals("To_Date('2009-12-24 11:00:00 PM','YYYY-MM-DD HH12:MI:SS AM')",
		// LocalDateTime.of(2009, 12, 24, 23, 0, 0));
		// evalEquals("To_Date('01/February/2020','DD/MON/YYYY')", LocalDate.of(2020, 2,
		// 1));
		// evalTrue("'give me 30% discount' like '%30!%%' escape '!'");
		// evalFalse("'AA' like '_'");
		// evalEquals("0xF+0", 15);
		// evalEquals("Date '2019-02-25'-5/(60*24)", LocalDateTime.of(2019, 2, 24, 23,
		// 55, 0));
		// evalEquals("Date '2019-02-25'-0.5", LocalDateTime.of(2019, 2, 24, 12, 0, 0));
		// evalFails("0X0F");
		// evalFails("9!7");
		// evalFails("Year()");
		// evalFails("LPad('test',-8)");
		// evalEquals("LPad('test',3,'*')", "tes");
		// evalFails("Ceil('x')");
		// evalFails("Date(2020,13,22)");
		// evalEquals("To_Char(0.1,'999.99')", " .10");
		// evalEquals("To_Char(0,'9999')", " 0");
		// evalFails("To_Char(12,'MI99')");
		/// evalEquals("TO_CHAR(7,'9999PR')"," 7 ");
		// evalEquals("TO_CHAR(-7,'9999PR')"," <7>");
		// evalEquals("TO_CHAR(-7,'$99')", " -XXX7");
		// evalEquals("TO_CHAR(0,'99.99')", " .00");
		// evalEquals("TO_CHAR(0,'9999')", " 0");
		// evalEquals("TO_CHAR(123.456,'9.9EEEE')", " 1.2E+02");

	}

}
