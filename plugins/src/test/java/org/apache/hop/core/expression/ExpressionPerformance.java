/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.core.expression;

import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.expression.IExpression;
import org.apache.hop.junit.rules.RestoreHopEnvironment;
import org.junit.ClassRule;
import org.junit.Test;
import java.text.ParseException;
import java.util.Date;

public class ExpressionPerformance {

  @ClassRule
  public static RestoreHopEnvironment env = new RestoreHopEnvironment();

  public void perf(String e) throws ParseException {
    IExpression expression = ExpressionParser.parse(e);

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

    ExpressionContext context = new ExpressionContext(new Variables(), rowMeta);
    context.setRow(row);

    long cycle = 10000000;
    long startTime = System.currentTimeMillis();
    for (long i = cycle; i > 0; i--) {
      @SuppressWarnings("unused")
      Object result = expression.eval(context);
    }

    long endTime = System.currentTimeMillis();
    long duration = endTime - startTime;

    System.out
        .println("Performance(\"" + e + "\") Duration for " + cycle + " cycles = " + duration);
  }

  @Test
  public void performance() throws ParseException {
    perf("AGE>10");
    // perf("NOM||left(to_char(AGE+5,'000'),2)");
    // perf("NOM||left(to_char(AGE+5,'000'),2)");
    // perf("Date '2020-05-06'");
    // perf("To_DATE('2020-FEB-06','YYYY-MM-DD'");

  }
}
