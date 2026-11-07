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
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Arrays;
import java.util.Date;
import java.util.Objects;
import java.util.Random;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaBigNumber;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.junit.rules.RestoreHopEnvironmentExtension;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Differential test of the simplification rules applied when an expression is compiled.
 *
 * <p>Random expressions are generated over a few fields of type boolean, integer, number, string
 * and date. For each row, the expression compiled with the fields, simplified by the rules that
 * apply to a non-constant operand like {@code x AND NOT x}, must give the same result as the same
 * expression with the values of the row written as typed literals, which is folded to a constant
 * operator by operator.
 *
 * <p>A division is always by {@code NULLIF(y,0)}: an error like a division by zero may be raised or
 * not depending on the order in which the simplified predicates are evaluated.
 *
 * <p>The seed is fixed, a failure is reproducible and reports the expression and the row.
 */
@ExtendWith({RestoreHopEnvironmentExtension.class, ExpressionExtension.class})
class SimplifierDifferentialTest {

  private static final long SEED = 20261107L;
  private static final int EXPRESSIONS = 3000;
  private static final int ROWS_PER_EXPRESSION = 20;
  private static final int MAX_DEPTH = 4;

  /** A field of the row, with the values it takes. */
  private record Field(String name, IValueMeta valueMeta, String type, Object[] values) {}

  private static final Field[] FIELDS = {
    new Field("B1", new ValueMetaBoolean("B1"), "BOOLEAN", new Object[] {true, false, null}),
    new Field("B2", new ValueMetaBoolean("B2"), "BOOLEAN", new Object[] {true, false, null}),
    new Field("I1", new ValueMetaInteger("I1"), "INTEGER", new Object[] {-1L, 0L, 1L, 2L, null}),
    new Field("I2", new ValueMetaInteger("I2"), "INTEGER", new Object[] {-1L, 0L, 1L, 2L, null}),
    new Field(
        "N1",
        new ValueMetaBigNumber("N1"),
        "NUMBER",
        new Object[] {
          new BigDecimal("1.5"), new BigDecimal("-0.25"), BigDecimal.ZERO, new BigDecimal("2"), null
        }),
    new Field("S1", new ValueMetaString("S1"), "STRING", new Object[] {"a", "b", "ab", "", null}),
    new Field(
        "D1",
        new ValueMetaDate("D1"),
        "DATE",
        new Object[] {
          LocalDate.of(2020, 1, 31), LocalDate.of(2024, 2, 29), LocalDate.of(1999, 12, 31), null
        })
  };

  private static final String[] INTEGER_LITERALS = {"-1", "0", "1", "2"};
  private static final String[] NUMBER_LITERALS = {"1.5", "-0.25", "0.0", "2.75"};
  private static final String[] STRING_LITERALS = {"'a'", "'b'", "'ab'", "''"};
  private static final String[] DATE_LITERALS = {
    "DATE '2020-01-31'", "DATE '2024-02-29'", "DATE '2000-01-01'"
  };
  private static final String[] LIKE_PATTERNS = {"'a%'", "'%b'", "'_'", "'%'", "'a_'"};
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
    for (Field field : FIELDS) {
      rowMeta.addValueMeta(field.valueMeta());
    }
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
        Object[] row = new Object[FIELDS.length];
        for (int f = 0; f < FIELDS.length; f++) {
          row[f] = pick(FIELDS[f].values());
        }
        context.setRow(toHopRow(row));

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
      result = result.replaceAll("\\b" + FIELDS[i].name() + "\\b", literal(row[i], FIELDS[i]));
    }
    return result;
  }

  private static String literal(Object value, Field field) {
    String text =
        switch (value) {
          case null -> "NULL";
          case String str -> "'" + str + "'";
          case LocalDate date -> "DATE '" + date + "'";
          default -> value.toString();
        };
    return "CAST(" + text + " AS " + field.type() + ")";
  }

  /**
   * Converts the values to the values stored in a Hop row. A date is converted when the row is
   * built, at midnight in the time zone of the test, which is set before each test.
   */
  private static Object[] toHopRow(Object[] row) {
    Object[] result = row.clone();
    for (int i = 0; i < result.length; i++) {
      if (result[i] instanceof LocalDate date) {
        result[i] = Date.from(date.atStartOfDay(ZoneId.systemDefault()).toInstant());
      }
    }
    return result;
  }

  /** Generates a boolean expression. */
  private String bool(int depth) {
    if (depth == 0 || random.nextInt(4) == 0) {
      return switch (random.nextInt(11)) {
        case 0 -> "B1";
        case 1 -> "B2";
        case 2 -> "TRUE";
        case 3 -> "FALSE";
        case 4 -> "NULL";
        case 5 -> "I1 " + pick(COMPARISONS) + " " + pick(INTEGER_LITERALS);
        case 6 -> "N1 " + pick(COMPARISONS) + " " + pick(NUMBER_LITERALS);
        case 7 -> "S1 = 'a'";
        case 8 -> "S1 LIKE " + pick(LIKE_PATTERNS);
        case 9 -> "D1 " + pick(COMPARISONS) + " " + pick(DATE_LITERALS);
        default -> pick("S1", "D1", "N1") + " IS NULL";
      };
    }
    int d = depth - 1;
    return switch (random.nextInt(30)) {
      case 0, 1, 2 -> "(" + bool(d) + " AND " + bool(d) + ")";
      case 3, 4, 5 -> "(" + bool(d) + " OR " + bool(d) + ")";
      case 6 -> "NOT " + bool(d);
      case 7 -> "(" + bool(d) + " IS " + pick("TRUE", "NOT TRUE", "FALSE", "NOT FALSE") + ")";
      case 8 -> "(" + any(d) + " IS " + pick("NULL", "NOT NULL") + ")";
      case 9 -> "(" + integer(d) + " " + pick(COMPARISONS) + " " + integer(d) + ")";
      case 10 -> "(" + number(d) + " " + pick(COMPARISONS) + " " + number(d) + ")";
      case 11 -> "(" + string(d) + " " + pick(COMPARISONS) + " " + string(d) + ")";
      case 12 -> "(" + date(d) + " " + pick(COMPARISONS) + " " + date(d) + ")";
      case 13 -> "(" + integer(d) + " " + pick(COMPARISONS) + " " + number(d) + ")";
      case 14 -> "(" + integer(d) + " IS " + pick("", "NOT ") + "DISTINCT FROM " + integer(d) + ")";
      case 15 ->
          "("
              + integer(d)
              + pick(" IN (", " NOT IN (")
              + pick(INTEGER_LITERALS)
              + ","
              + pick(INTEGER_LITERALS)
              + "))";
      case 16 ->
          "("
              + string(d)
              + pick(" IN (", " NOT IN (")
              + pick(STRING_LITERALS)
              + ","
              + pick(STRING_LITERALS)
              + "))";
      case 17 ->
          "("
              + pick(integer(d), number(d))
              + " BETWEEN "
              + pick(INTEGER_LITERALS)
              + " AND "
              + pick(NUMBER_LITERALS)
              + ")";
      case 18 ->
          "(" + date(d) + " BETWEEN " + pick(DATE_LITERALS) + " AND " + pick(DATE_LITERALS) + ")";
      case 19 -> "(" + string(d) + pick(" LIKE ", " NOT LIKE ") + pick(LIKE_PATTERNS) + ")";
      case 20 -> pick("STARTSWITH(", "ENDSWITH(", "CONTAINS(") + string(d) + "," + string(d) + ")";
      case 21 -> "COALESCE(" + bool(d) + "," + bool(d) + ")";
      case 22 -> "NVL2(" + any(d) + "," + bool(d) + "," + bool(d) + ")";
      case 23 -> "IF(" + bool(d) + "," + bool(d) + "," + bool(d) + ")";
      case 24 ->
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
      case 25 -> "(" + bool(d) + " = " + bool(d) + ")";
      case 26 -> "NULLIF(" + bool(d) + "," + bool(d) + ")";
      case 27 -> "(" + string(d) + " IS " + pick("", "NOT ") + "DISTINCT FROM " + string(d) + ")";
      case 28 -> "(" + date(d) + " IS " + pick("", "NOT ") + "DISTINCT FROM " + date(d) + ")";
      default -> "(" + bool(d) + " XOR " + bool(d) + ")";
    };
  }

  /** Generates an expression of any type. */
  private String any(int depth) {
    return switch (random.nextInt(5)) {
      case 0 -> bool(depth);
      case 1 -> integer(depth);
      case 2 -> number(depth);
      case 3 -> string(depth);
      default -> date(depth);
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
    return switch (random.nextInt(22)) {
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
      case 13 -> "NVL2(" + integer(d) + "," + integer(d) + "," + integer(d) + ")";
      case 14 -> "LENGTH(" + string(d) + ")";
      case 15 -> "INSTR(" + string(d) + "," + string(d) + ")";
      case 16 -> pick("YEAR(", "MONTH(", "DAY(") + date(d) + ")";
      case 17 -> "DAYS_BETWEEN(" + date(d) + "," + date(d) + ")";
      case 18 -> "SIGN(" + number(d) + ")";
      case 19 -> "CAST(" + number(d) + " AS INTEGER)";
      case 20 -> "(" + integer(d) + " % NULLIF(" + integer(d) + ",0))";
      default -> "MOD(" + integer(d) + ",NULLIF(" + integer(d) + ",0))";
    };
  }

  /** Generates a number expression, with small values that can't overflow. */
  private String number(int depth) {
    if (depth == 0 || random.nextInt(3) == 0) {
      return switch (random.nextInt(3)) {
        case 0 -> "N1";
        case 1 -> "NULL";
        default -> pick(NUMBER_LITERALS);
      };
    }
    int d = depth - 1;
    return switch (random.nextInt(18)) {
      case 0, 1 -> "(" + number(d) + " + " + pick(number(d), integer(d)) + ")";
      case 2 -> "(" + pick(number(d), integer(d)) + " - " + number(d) + ")";
      case 3 -> "(" + number(d) + " * " + pick(number(d), integer(d)) + ")";
      case 4 -> "(" + number(d) + " / NULLIF(" + pick(number(d), integer(d)) + ",0))";
      case 5 -> "-(" + number(d) + ")";
      case 6 -> pick("ABS(", "CEIL(", "FLOOR(", "ROUND(", "TRUNCATE(") + number(d) + ")";
      case 7 -> pick("ROUND(", "TRUNCATE(") + number(d) + "," + pick("0", "1") + ")";
      case 8 -> "COALESCE(" + number(d) + "," + number(d) + ")";
      case 9 -> "IF(" + bool(d) + "," + number(d) + "," + number(d) + ")";
      case 10 -> "CASE WHEN " + bool(d) + " THEN " + number(d) + " ELSE " + number(d) + " END";
      case 11 -> pick("GREATEST(", "LEAST(") + number(d) + "," + number(d) + ")";
      case 12 -> "NULLIF(" + number(d) + "," + number(d) + ")";
      case 13 -> "ZEROIFNULL(" + number(d) + ")";
      case 14 -> "NVL2(" + any(d) + "," + number(d) + "," + number(d) + ")";
      case 15 -> "CAST(" + integer(d) + " AS NUMBER)";
      case 16 -> "(" + number(d) + " % NULLIF(" + number(d) + ",0))";
      default -> "NULLIFZERO(" + number(d) + ")";
    };
  }

  /** Generates a string expression. */
  private String string(int depth) {
    if (depth == 0 || random.nextInt(3) == 0) {
      return switch (random.nextInt(3)) {
        case 0 -> "S1";
        case 1 -> "NULL";
        default -> pick(STRING_LITERALS);
      };
    }
    int d = depth - 1;
    return switch (random.nextInt(17)) {
      case 0, 1 -> "CONCAT(" + string(d) + "," + string(d) + ")";
      case 2 -> "(" + string(d) + " || " + string(d) + ")";
      case 3 -> pick("UPPER(", "LOWER(", "TRIM(", "REVERSE(") + string(d) + ")";
      case 4 -> pick("LEFT(", "RIGHT(") + string(d) + "," + pick("0", "1", "2") + ")";
      case 5 -> "SUBSTRING(" + string(d) + "," + pick("1", "2") + "," + pick("1", "2") + ")";
      case 6 -> "REPLACE(" + string(d) + "," + pick(STRING_LITERALS) + "," + string(d) + ")";
      case 7 -> "COALESCE(" + string(d) + "," + string(d) + ")";
      case 8 -> "IF(" + bool(d) + "," + string(d) + "," + string(d) + ")";
      case 9 -> "CASE WHEN " + bool(d) + " THEN " + string(d) + " ELSE " + string(d) + " END";
      case 10 -> "NULLIF(" + string(d) + "," + string(d) + ")";
      case 11 -> "NVL2(" + any(d) + "," + string(d) + "," + string(d) + ")";
      case 12 -> "CAST(" + integer(d) + " AS STRING)";
      case 13 -> "CAST(" + number(d) + " AS STRING)";
      case 14 -> "TO_CHAR(" + date(d) + ",'YYYY-MM-DD')";
      case 15 -> pick("GREATEST(", "LEAST(") + string(d) + "," + string(d) + ")";
      default ->
          "CASE " + string(d) + " WHEN 'a' THEN " + string(d) + " ELSE " + string(d) + " END";
    };
  }

  /** Generates a date expression, with dates close to the present that can't overflow. */
  private String date(int depth) {
    if (depth == 0 || random.nextInt(3) == 0) {
      return switch (random.nextInt(3)) {
        case 0 -> "D1";
        case 1 -> "CAST(NULL AS DATE)";
        default -> pick(DATE_LITERALS);
      };
    }
    int d = depth - 1;
    return switch (random.nextInt(9)) {
      case 0, 1 -> pick("ADD_DAYS(", "ADD_MONTHS(") + date(d) + "," + integer(d) + ")";
      case 2 -> pick("FIRST_DAY(", "LAST_DAY(") + date(d) + ")";
      case 3 -> "COALESCE(" + date(d) + "," + date(d) + ")";
      case 4 -> "IF(" + bool(d) + "," + date(d) + "," + date(d) + ")";
      case 5 -> "CASE WHEN " + bool(d) + " THEN " + date(d) + " ELSE " + date(d) + " END";
      case 6 -> pick("GREATEST(", "LEAST(") + date(d) + "," + date(d) + ")";
      case 7 -> "NULLIF(" + date(d) + "," + date(d) + ")";
      default -> "NVL2(" + any(d) + "," + date(d) + "," + date(d) + ")";
    };
  }

  @SafeVarargs
  private <T> T pick(T... values) {
    return values[random.nextInt(values.length)];
  }
}
