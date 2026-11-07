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
package org.apache.hop.expression;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Objects;
import java.util.Random;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.junit.rules.RestoreHopEnvironmentExtension;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Differential test of the simplification rules applied when an expression is compiled.
 *
 * <p>Random expressions are generated over a few fields. For each row, the expression compiled with
 * the fields, simplified by the rules that apply to a non-constant operand like {@code x AND NOT
 * x}, must give the same result as the same expression with the values of the row written as typed
 * literals, which is folded to a constant operator by operator.
 *
 * <p>The seed is fixed, a failure is reproducible and reports the expression and the row.
 */
@ExtendWith({RestoreHopEnvironmentExtension.class, ExpressionExtension.class})
class SimplifierDifferentialTest {

  private static final long SEED = 20261107L;
  private static final int EXPRESSIONS = 2000;
  private static final int ROWS_PER_EXPRESSION = 20;
  private static final int MAX_DEPTH = 4;

  private static final String[] FIELDS = {"B1", "B2", "I1", "I2", "S1"};
  private static final Object[] BOOLEANS = {Boolean.TRUE, Boolean.FALSE, null};
  private static final Object[] INTEGERS = {-1L, 0L, 1L, 2L, null};
  private static final Object[] STRINGS = {"a", "b", "", null};
  private static final String[] INTEGER_LITERALS = {"-1", "0", "1", "2"};
  private static final String[] COMPARISONS = {"=", "<>", "<", "<=", ">", ">="};

  private final Random random = new Random(SEED);

  /** The outcome of an evaluation: a value, or the error code of the failure. */
  private record Outcome(Object value, ErrorCode error) {

    /** Compiles and evaluates the source, an internal error fails the test with the source. */
    static Outcome of(IExpressionContext context, String source) {
      try {
        return of(compile(context, source));
      } catch (ExpressionException e) {
        return new Outcome(null, e.getErrorCode());
      }
    }

    /** Evaluates the expression, an internal error fails the test with the expression. */
    static Outcome of(IExpression expression) {
      try {
        return new Outcome(expression.getValue(), null);
      } catch (ExpressionException e) {
        return new Outcome(null, e.getErrorCode());
      } catch (RuntimeException e) {
        throw new AssertionError("Internal error evaluating: " + expression, e);
      }
    }

    /** Compares the values, a number regardless of its Java class and scale. */
    boolean isSame(Outcome other) {
      if (value instanceof Number a && other.value instanceof Number b) {
        return new BigDecimal(a.toString()).compareTo(new BigDecimal(b.toString())) == 0;
      }
      return Objects.equals(value, other.value) && error == other.error;
    }

    @Override
    public String toString() {
      return (error == null) ? String.valueOf(value) : "error " + error;
    }
  }

  @Test
  void simplifiedExpressionGivesSameResultAsConstantFolding() {
    RowMeta rowMeta = new RowMeta();
    rowMeta.addValueMeta(new ValueMetaBoolean("B1"));
    rowMeta.addValueMeta(new ValueMetaBoolean("B2"));
    rowMeta.addValueMeta(new ValueMetaInteger("I1"));
    rowMeta.addValueMeta(new ValueMetaInteger("I2"));
    rowMeta.addValueMeta(new ValueMetaString("S1"));
    RowExpressionContext context = new RowExpressionContext(new Variables(), rowMeta);

    int compiled = 0;
    for (int i = 0; i < EXPRESSIONS; i++) {
      String source = bool(MAX_DEPTH);

      IExpression expression;
      try {
        expression = compile(context, source);
      } catch (ExpressionException e) {
        // An expression rejected when compiled, like a type that can't be inferred, is not compared
        continue;
      }
      compiled++;

      for (int r = 0; r < ROWS_PER_EXPRESSION; r++) {
        Object[] row = {
          pick(BOOLEANS), pick(BOOLEANS), pick(INTEGERS), pick(INTEGERS), pick(STRINGS)
        };
        context.setRow(row);

        Outcome simplified = Outcome.of(expression);
        String constantSource = substitute(source, row);
        Outcome folded = Outcome.of(context, constantSource);

        if (!simplified.isSame(folded)) {
          fail(
              String.format(
                  "Expression #%d: %s%n  simplified: %s%n  row: %s%n  simplified result: %s%n"
                      + "  constant: %s%n  constant result: %s",
                  i, source, expression, Arrays.toString(row), simplified, constantSource, folded));
        }
      }
    }

    // Guard against a generator that only produces rejected expressions
    assertTrue(compiled > EXPRESSIONS / 2, "Too few compiled expressions: " + compiled);
  }

  /**
   * Compiles the source, an internal error fails the test with the source.
   *
   * @throws ExpressionException if the source is rejected, like a type that can't be inferred
   */
  private static IExpression compile(IExpressionContext context, String source) {
    try {
      return IExpression.of(context, source);
    } catch (ExpressionException e) {
      throw e;
    } catch (RuntimeException e) {
      throw new AssertionError("Internal error compiling: " + source, e);
    }
  }

  /** Replaces each field with the value of the row, as a literal of the type of the field. */
  private static String substitute(String source, Object[] row) {
    String result = source;
    for (int i = 0; i < FIELDS.length; i++) {
      result = result.replaceAll("\\b" + FIELDS[i] + "\\b", literal(row[i], i));
    }
    return result;
  }

  private static String literal(Object value, int field) {
    String type =
        switch (field) {
          case 0, 1 -> "BOOLEAN";
          case 2, 3 -> "INTEGER";
          default -> "STRING";
        };
    String text =
        switch (value) {
          case null -> "NULL";
          case String str -> "'" + str + "'";
          default -> value.toString();
        };
    return "CAST(" + text + " AS " + type + ")";
  }

  /** Generates a boolean expression. */
  private String bool(int depth) {
    if (depth == 0 || random.nextInt(4) == 0) {
      return switch (random.nextInt(8)) {
        case 0 -> "B1";
        case 1 -> "B2";
        case 2 -> "TRUE";
        case 3 -> "FALSE";
        case 4 -> "NULL";
        case 5 -> "I1 " + pick(COMPARISONS) + " " + pick(INTEGER_LITERALS);
        case 6 -> "S1 = 'a'";
        default -> "S1 IS NULL";
      };
    }
    int d = depth - 1;
    return switch (random.nextInt(20)) {
      case 0, 1, 2 -> "(" + bool(d) + " AND " + bool(d) + ")";
      case 3, 4, 5 -> "(" + bool(d) + " OR " + bool(d) + ")";
      case 6 -> "NOT " + bool(d);
      case 7 -> "(" + bool(d) + " IS " + pick("TRUE", "NOT TRUE", "FALSE", "NOT FALSE") + ")";
      case 8 -> "(" + pick(bool(d), integer(d)) + " IS " + pick("NULL", "NOT NULL") + ")";
      case 9 -> "(" + integer(d) + " " + pick(COMPARISONS) + " " + integer(d) + ")";
      case 10 -> "(" + integer(d) + " IS " + pick("", "NOT ") + "DISTINCT FROM " + integer(d) + ")";
      case 11 ->
          "("
              + integer(d)
              + pick(" IN (", " NOT IN (")
              + pick(INTEGER_LITERALS)
              + ","
              + pick(INTEGER_LITERALS)
              + "))";
      case 12 ->
          "("
              + integer(d)
              + " BETWEEN "
              + pick(INTEGER_LITERALS)
              + " AND "
              + pick(INTEGER_LITERALS)
              + ")";
      case 13 -> "COALESCE(" + bool(d) + "," + bool(d) + ")";
      case 14 -> "NVL2(" + pick(bool(d), integer(d)) + "," + bool(d) + "," + bool(d) + ")";
      case 15 -> "IF(" + bool(d) + "," + bool(d) + "," + bool(d) + ")";
      case 16 ->
          "CASE WHEN "
              + bool(d)
              + " THEN "
              + bool(d)
              + " WHEN "
              + bool(d)
              + " THEN "
              + bool(d)
              + " ELSE "
              + bool(d)
              + " END";
      case 17 -> "(" + bool(d) + " = " + bool(d) + ")";
      case 18 -> "NULLIF(" + bool(d) + "," + bool(d) + ")";
      default -> "(" + bool(d) + " XOR " + bool(d) + ")";
    };
  }

  /** Generates an integer expression, with small values that can't overflow. */
  private String integer(int depth) {
    if (depth == 0 || random.nextInt(3) == 0) {
      return switch (random.nextInt(4)) {
        case 0 -> "I1";
        case 1 -> "I2";
        case 2 -> "NULL";
        default -> pick(INTEGER_LITERALS);
      };
    }
    int d = depth - 1;
    return switch (random.nextInt(14)) {
      case 0, 1 -> "(" + integer(d) + " + " + integer(d) + ")";
      case 2 -> "(" + integer(d) + " - " + integer(d) + ")";
      case 3 -> "(" + integer(d) + " * " + integer(d) + ")";
      case 4 -> "-(" + integer(d) + ")";
      case 5 -> "COALESCE(" + integer(d) + "," + integer(d) + ")";
      case 6 -> "IF(" + bool(d) + "," + integer(d) + "," + integer(d) + ")";
      case 7 -> "CASE WHEN " + bool(d) + " THEN " + integer(d) + " ELSE " + integer(d) + " END";
      case 8 ->
          "CASE "
              + integer(d)
              + " WHEN "
              + pick(INTEGER_LITERALS)
              + " THEN "
              + integer(d)
              + " ELSE "
              + integer(d)
              + " END";
      case 9 -> "ABS(" + integer(d) + ")";
      case 10 -> pick("GREATEST(", "LEAST(") + integer(d) + "," + integer(d) + ")";
      case 11 -> "NULLIF(" + integer(d) + "," + integer(d) + ")";
      case 12 -> "ZEROIFNULL(" + integer(d) + ")";
      default -> "NVL2(" + integer(d) + "," + integer(d) + "," + integer(d) + ")";
    };
  }

  @SafeVarargs
  private <T> T pick(T... values) {
    return values[random.nextInt(values.length)];
  }
}
