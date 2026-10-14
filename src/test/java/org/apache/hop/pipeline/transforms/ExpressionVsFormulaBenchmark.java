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
package org.apache.hop.pipeline.transforms;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Random;
import java.util.concurrent.TimeUnit;
import org.apache.hop.core.HopEnvironment;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaBigNumber;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaFactory;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaNumber;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.pipeline.Pipeline;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.engines.local.LocalPipelineEngine;
import org.apache.hop.pipeline.transform.BaseTransform;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.apache.hop.pipeline.transforms.expression.ExpressionData;
import org.apache.hop.pipeline.transforms.expression.ExpressionField;
import org.apache.hop.pipeline.transforms.expression.ExpressionMeta;
import org.apache.hop.pipeline.transforms.expression.ExpressionTransform;
import org.apache.hop.pipeline.transforms.formula.Formula;
import org.apache.hop.pipeline.transforms.formula.FormulaData;
import org.apache.hop.pipeline.transforms.formula.FormulaMeta;
import org.apache.hop.pipeline.transforms.formula.FormulaMetaFunction;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;
import org.openjdk.jmh.runner.options.TimeValue;

/**
 * Compares the cost of one row processed by the Expression transform and by the Formula transform
 * (Apache POI based) for equivalent calculations.
 *
 * <p>The real transforms are used ({@code processRow()}), only the row sets are bypassed: {@code
 * getRow()} serves rows from an in-memory pool and {@code putRow()} keeps the last output row, so
 * the measure is not polluted by the threading and the buffering of the pipeline engine.
 */
@State(Scope.Benchmark)
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
public class ExpressionVsFormulaBenchmark {

  /** One output field, computed with the same logic by both transforms. */
  record Calculation(String name, int type, String expression, String formula) {}

  /** The similar cases to compare. */
  public enum Scenario {
    INTEGER_ARITHMETIC(
        new Calculation("R1", IValueMeta.TYPE_INTEGER, "AGE*2+SCORE", "[AGE]*2+[SCORE]")),
    NUMBER_ARITHMETIC(
        new Calculation("R2", IValueMeta.TYPE_NUMBER, "PRICE*1.5-10", "[PRICE]*1.5-10")),
    CONCAT(new Calculation("R3", IValueMeta.TYPE_STRING, "NAME||'-'||SEX", "[NAME]&\"-\"&[SEX]")),
    LOGIC(
        new Calculation(
            "R4", IValueMeta.TYPE_BOOLEAN, "AGE>18 AND SCORE<500", "AND([AGE]>18,[SCORE]<500)")),
    CONDITION(
        new Calculation(
            "R5",
            IValueMeta.TYPE_STRING,
            "CASE WHEN AMOUNT>5000 THEN 'HIGH' ELSE 'LOW' END",
            "IF([AMOUNT]>5000,\"HIGH\",\"LOW\")")),
    STRING_FUNCTION(
        new Calculation(
            "R6", IValueMeta.TYPE_STRING, "UPPER(LEFT(NAME,3))", "UPPER(LEFT([NAME],3))")),
    DATE_FUNCTION(
        new Calculation("R7", IValueMeta.TYPE_INTEGER, "YEAR(BIRTHDATE)", "YEAR([BIRTHDATE])")),
    /** All the calculations above in a single transform, like a typical real-life usage. */
    ALL();

    private final Calculation[] calculations;

    Scenario(Calculation... calculations) {
      this.calculations = calculations;
    }

    List<Calculation> getCalculations() {
      if (calculations.length > 0) {
        return List.of(calculations);
      }
      List<Calculation> all = new ArrayList<>();
      for (Scenario scenario : values()) {
        all.addAll(List.of(scenario.calculations));
      }
      return all;
    }
  }

  /** Size of the pool of input rows, a power of 2 to cycle with a mask. */
  private static final int ROW_COUNT = 1024;

  @Param public Scenario scenario;

  private RowFeed expressionFeed;
  private RowFeed formulaFeed;
  private BaseTransform<?, ?> expressionTransform;
  private BaseTransform<?, ?> formulaTransform;

  /** Serves the input rows of a transform and keeps its last output row. */
  static final class RowFeed {
    private final Object[][] rows;
    private int index;
    private Object[] output;

    RowFeed(Object[][] rows) {
      this.rows = rows;
    }

    Object[] next() {
      return rows[index++ & (ROW_COUNT - 1)];
    }
  }

  static final class FeedExpressionTransform extends ExpressionTransform {
    private final RowFeed feed;

    FeedExpressionTransform(
        TransformMeta transformMeta,
        ExpressionMeta meta,
        PipelineMeta pipelineMeta,
        Pipeline pipeline,
        RowFeed feed) {
      super(transformMeta, meta, new ExpressionData(), 0, pipelineMeta, pipeline);
      this.feed = feed;
    }

    @Override
    public Object[] getRow() {
      return feed.next();
    }

    @Override
    public void putRow(IRowMeta rowMeta, Object[] row) {
      feed.output = row;
    }
  }

  static final class FeedFormula extends Formula {
    private final RowFeed feed;

    FeedFormula(
        TransformMeta transformMeta,
        FormulaMeta meta,
        PipelineMeta pipelineMeta,
        Pipeline pipeline,
        RowFeed feed) {
      super(transformMeta, meta, new FormulaData(), 0, pipelineMeta, pipeline);
      this.feed = feed;
    }

    @Override
    public Object[] getRow() {
      return feed.next();
    }

    @Override
    public void putRow(IRowMeta rowMeta, Object[] row) {
      feed.output = row;
    }
  }

  @Setup
  public void setup() throws Exception {
    // Also registers the functions of the Expression plugin
    HopEnvironment.init();

    IRowMeta rowMeta = createRowMeta();
    Object[][] rows = createRows();
    List<Calculation> calculations = scenario.getCalculations();

    // Expression transform
    ExpressionMeta expressionMeta = new ExpressionMeta();
    expressionMeta.setDefault();
    for (Calculation calculation : calculations) {
      ExpressionField field = new ExpressionField();
      field.setName(calculation.name());
      field.setExpression(calculation.expression());
      field.setType(ValueMetaFactory.getValueMetaName(calculation.type()));
      expressionMeta.getFields().add(field);
    }
    expressionFeed = new RowFeed(rows);
    PipelineMeta pipelineMeta = new PipelineMeta();
    TransformMeta transformMeta = new TransformMeta("expression", expressionMeta);
    pipelineMeta.addTransform(transformMeta);
    expressionTransform =
        new FeedExpressionTransform(
            transformMeta,
            expressionMeta,
            pipelineMeta,
            new LocalPipelineEngine(pipelineMeta),
            expressionFeed);

    // Formula transform
    FormulaMeta formulaMeta = new FormulaMeta();
    formulaMeta.setDefault();
    for (Calculation calculation : calculations) {
      formulaMeta
          .getFormulas()
          .add(
              new FormulaMetaFunction(
                  calculation.name(),
                  calculation.formula(),
                  calculation.type(),
                  -1,
                  -1,
                  "",
                  false));
    }
    formulaFeed = new RowFeed(rows);
    pipelineMeta = new PipelineMeta();
    transformMeta = new TransformMeta("formula", formulaMeta);
    pipelineMeta.addTransform(transformMeta);
    formulaTransform =
        new FeedFormula(
            transformMeta,
            formulaMeta,
            pipelineMeta,
            new LocalPipelineEngine(pipelineMeta),
            formulaFeed);

    for (BaseTransform<?, ?> transform : List.of(expressionTransform, formulaTransform)) {
      transform.setInputRowMeta(rowMeta);
      if (!transform.init()) {
        throw new IllegalStateException("Unable to init transform " + transform.getTransformName());
      }
    }

    checkSameResults(rowMeta.size(), calculations);
  }

  /** Make sure both transforms compute the same values, otherwise the comparison is meaningless. */
  private void checkSameResults(int inputSize, List<Calculation> calculations) throws HopException {
    for (int i = 0; i < ROW_COUNT; i++) {
      expressionTransform.processRow();
      formulaTransform.processRow();
      for (int j = 0; j < calculations.size(); j++) {
        Object expected = expressionFeed.output[inputSize + j];
        Object actual = formulaFeed.output[inputSize + j];
        boolean same =
            (expected instanceof Double d1 && actual instanceof Double d2)
                ? Math.abs(d1 - d2) < 1e-9
                : expected.equals(actual);
        if (!same) {
          throw new IllegalStateException(
              String.format(
                  "%s differs: expression=%s formula=%s",
                  calculations.get(j).name(), expected, actual));
        }
      }
    }
  }

  private static IRowMeta createRowMeta() {
    IRowMeta rowMeta = new RowMeta();
    rowMeta.addValueMeta(new ValueMetaString("NAME"));
    rowMeta.addValueMeta(new ValueMetaString("SEX"));
    rowMeta.addValueMeta(new ValueMetaDate("BIRTHDATE"));
    rowMeta.addValueMeta(new ValueMetaInteger("AGE"));
    rowMeta.addValueMeta(new ValueMetaInteger("SCORE"));
    rowMeta.addValueMeta(new ValueMetaNumber("PRICE"));
    rowMeta.addValueMeta(new ValueMetaBigNumber("AMOUNT"));
    rowMeta.addValueMeta(new ValueMetaBoolean("FLAG"));
    return rowMeta;
  }

  /** Random but reproducible input rows, without null values. */
  private static Object[][] createRows() {
    String[] names = {"Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia", "Miller"};
    Random random = new Random(42);
    Calendar calendar = Calendar.getInstance();
    Object[][] rows = new Object[ROW_COUNT][];
    for (int i = 0; i < ROW_COUNT; i++) {
      calendar.clear();
      // At noon, as Expression reads the dates in UTC and the Formula in the local time zone
      calendar.set(1950 + random.nextInt(60), random.nextInt(12), 1 + random.nextInt(28), 12, 0);
      rows[i] =
          new Object[] {
            names[random.nextInt(names.length)],
            random.nextBoolean() ? "F" : "M",
            calendar.getTime(),
            (long) random.nextInt(100),
            (long) random.nextInt(1000),
            Math.round(random.nextDouble() * 100_000) / 100.0,
            BigDecimal.valueOf(random.nextInt(1_000_000), 2),
            random.nextBoolean()
          };
    }
    return rows;
  }

  @Benchmark
  public Object expression() throws HopException {
    expressionTransform.processRow();
    return expressionFeed.output;
  }

  @Benchmark
  public Object formula() throws HopException {
    formulaTransform.processRow();
    return formulaFeed.output;
  }

  /**
   * Run the benchmark.
   *
   * @param args not used
   * @throws RunnerException if the benchmark fails
   */
  public static void main(String[] args) throws RunnerException {
    Options opt =
        new OptionsBuilder()
            .include(ExpressionVsFormulaBenchmark.class.getSimpleName())
            .warmupIterations(3)
            .warmupTime(TimeValue.seconds(3))
            .measurementIterations(5)
            .measurementTime(TimeValue.seconds(5))
            .forks(1)
            .build();
    new Runner(opt).run();
  }
}
