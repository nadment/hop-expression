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
import org.apache.hop.core.row.value.ValueMetaBigNumber;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaNumber;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.optimizer.Optimizer;
import org.apache.hop.junit.rules.RestoreHopEnvironment;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.TearDown;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;
import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Date;

public class ExpressionBenchmark {  
  /**
   * @param args
   * @throws InterruptedException
   * @throws RunnerException
   */
  public static void main(String[] args) throws InterruptedException, RunnerException {
    RestoreHopEnvironment env = new RestoreHopEnvironment();
    Options opt = new OptionsBuilder()
            .include(ExpressionBenchmark.class.getSimpleName())
            .forks(2)
            .build();
    new Runner(opt).run();       
}
  
  @State(Scope.Thread)
  public static class MyState {
      @Setup(Level.Trial)
      public void doSetup() {

      }

      @TearDown(Level.Trial)
      public void doTearDown() {
          System.out.println("Do TearDown");
      }

      public ExpressionContext context = createExpressionContext();
  }
  
  protected static ExpressionContext createExpressionContext()  {

    IVariables variables = new Variables();
    variables.setVariable("TEST", "12345");

    IRowMeta rowMeta = new RowMeta();
    rowMeta.addValueMeta(new ValueMetaString("NAME"));
    rowMeta.addValueMeta(new ValueMetaString("SEX"));
    rowMeta.addValueMeta(new ValueMetaDate("BIRTHDATE"));
    rowMeta.addValueMeta(new ValueMetaInteger("AGE"));
    rowMeta.addValueMeta(new ValueMetaDate("DN"));
    rowMeta.addValueMeta(new ValueMetaBoolean("FLAG"));
    rowMeta.addValueMeta(new ValueMetaBoolean("VALUE_NULL"));
    rowMeta.addValueMeta(new ValueMetaInteger("YEAR"));
    rowMeta.addValueMeta(new ValueMetaString("FROM"));
    rowMeta.addValueMeta(new ValueMetaNumber("PRICE"));
    rowMeta.addValueMeta(new ValueMetaBigNumber("AMOUNT"));
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER SPACE"));
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER_UNDERSCORE"));
    rowMeta.addValueMeta(new ValueMetaString("IDENTIFIER lower"));

    Calendar calendar = Calendar.getInstance();
    calendar.set(1981, 5, 23);
    
    Object[] row = new Object[14];
    row[0] = "TEST";
    row[1] = "F";
    row[2] = calendar.getTime();
    row[3] = 40L;
    row[4] = new Date();
    row[5] = true;
    row[6] = null;
    row[7] = 2020L;
    row[8] = "Paris";
    row[9] = -5.12D;
    row[10] = BigDecimal.valueOf(123456.789);
    row[11] = "SPACE";
    row[12] = "UNDERSCORE";
    row[13] = "lower";

    ExpressionContext context = new ExpressionContext(variables, rowMeta);
    context.setRow(row);
   
    return context;
  }

  protected Object eval(String source, ExpressionContext context) throws Exception {
    IExpression expression = ExpressionParser.parse(source);
    Optimizer optimizer = new Optimizer();
    expression = optimizer.optimize(context, expression);    
    return expression.eval(context);
  }

  @Benchmark @BenchmarkMode(Mode.AverageTime)
  public void performance(MyState state) throws Exception {
    eval("AGE>10 or AMOUNT between 123000 and 200200", state.context);
  }
}
