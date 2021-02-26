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
import java.text.DecimalFormatSymbols;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Currency;
import java.util.Date;
import java.util.Locale;
import org.apache.commons.lang.StringUtils;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.expression.ExpressionParserException;
import org.apache.hop.expression.IExpression;
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
	
	protected Value eval(String s) throws Exception {
		IExpression expression = ExpressionParser.parse(s);
		return expression.eval(context);
	}

	protected void evalNull(String s) throws Exception {
		assertTrue(eval(s).isNull());
	}

	protected void evalTrue(String s) throws Exception {
		assertTrue(eval(s).toBoolean());
	}

	protected void evalFalse(String s) throws Exception {
		assertFalse(eval(s).toBoolean());
	}

	protected void evalEquals(String s, String expected) throws Exception {
		assertEquals(expected, eval(s).toString());
	}

	protected void evalEquals(String s, Long expected) throws Exception {
		assertEquals(expected.longValue(), eval(s).toInteger());
	}

	protected void evalEquals(String s, double expected) throws Exception {
		Value value = eval(s);
		assertEquals(expected, value.toNumber(), 0.001);
	}

	protected void evalEquals(String s, BigDecimal expected) throws Exception {
		Value value = eval(s);
		assertEquals(expected, value.toBigNumber());
	}

	protected void evalEquals(String s, Instant expected) throws Exception {
		assertEquals(expected, eval(s).toDate());
	}

	protected void evalEquals(String s, LocalTime expected) throws Exception {
		assertEquals(Instant.from(expected), eval(s).toDate());
	}

	protected void evalEquals(String s, LocalDate expected) throws Exception {
		assertEquals(expected.atStartOfDay(ZoneId.of("UTC")).toInstant(), eval(s).toDate());
	}

	protected void evalEquals(String s, LocalDateTime expected) throws Exception {
		assertEquals(expected.atZone(ZoneId.of("UTC")).toInstant(), eval(s).toDate());
	}

	protected void evalFails(String s) {
		try {
			eval(s);
			Assert.fail("Syntax or result should be invalid");
		} catch (ExpressionParserException ex) {
			System.out.print(ex.getSource());
			System.out.print(" >>> ");
			System.err.println(ex.toString());
		} catch (ExpressionException ex) {
			System.out.println(s + " >>> " + ex.toString());
		} catch (Exception ex) {
			Assert.fail("Uncatched exception " + ex.getClass());
		}
	}

	@Test
	public void parser() throws Exception {
	     Locale locale = this.getContext().getLocale();
	     DecimalFormatSymbols symbols = DecimalFormatSymbols.getInstance(locale);
	     Currency currency = symbols.getCurrency();
	     
	      //evalEquals("TO_NUMBER('0.4-','99.99MI')", -0.4);
	        //evalEquals("TO_NUMBER(' 0.5 ','99.99PR')", 0.5);
	     evalEquals("TO_NUMBER('12,345,678', '999G999G999')", 12_345_678);
	        //evalEquals("TO_NUMBER('<0.5>','99.99PR')", -0.5);
	        
	      //evalEquals("TO_CHAR(12,'$99')", " $12");
	     //evalEquals("TO_CHAR(123.456,'9.9EEEE')", "  1.2E+02");
	      //evalEquals("TO_CHAR(11,'FMRN')", "XI");  
	      //evalEquals("TO_CHAR(123,'XX')", " 7B");
	      //evalEquals("TO_CHAR(124,'FM$99')", "###");
	      //evalEquals("TO_CHAR(12345.567,'9,999')", "######");
	     
	     //evalEquals("TO_CHAR(-7,'99$')", " -7" + currency.getSymbol());

	}

}
