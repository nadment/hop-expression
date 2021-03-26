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
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Value;
import org.junit.Test;

public class OptimizerTest {

	protected IExpression optimize(String e) throws Exception {

		IExpression expression = ExpressionParser.parse(e);

		ExpressionContext context = new ExpressionContext();
		IExpression result = expression.optimize(context);
		
		int optimized = expression.getCost()-result.getCost();
				
		//System.out.println("("+optimized+") optimize(" + e + ")["+expression.getCost()+"] >>> optimized(" + result+")["+result.getCost()+"]");
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
		
		optimize("1+AGE+3"); // TODO: operator swap
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
		optimize("DayOfMonth(Date '2019-02-15')",15L);
		optimize("DayOfMonth(Date(2019,2,15))",15L);
	}
}
