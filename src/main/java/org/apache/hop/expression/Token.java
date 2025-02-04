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

public class Token {

  /** Enumerates the possible types of {@link Token}. */
  public enum Id {
    /** The bitwise AND operator "&". */
    BITWISE_AND("&"),
    /** The bitwise NOT operator "~". */
    BITWISE_NOT("~"),
    /** The bitwise OR operator "|". */
    BITWISE_OR("|"),
    /** The bitwise exclusive OR operator "^". */
    BITWISE_XOR("^"),
    /** Case when operator */
    CASE,
    /** Concat operator <code>||<code>. */
    CONCAT("||"),
    /** Used by function CAST(value AS type) */
    AS,
    /** Used by operator AT TIME ZONE */
    AT,
    /** Used by function CAST(value AS type FORMAT '9999') */
    FORMAT,
    /** Cast operator <code>::<code>. */
    CAST("::"),
    /** Comment */
    COMMENT,
    /** DOT */
    DOT("."),
    /** Comma separator */
    COMMA(","),
    /** Left parenthesis */
    LPARENT("("),
    /** Right parenthesis */
    RPARENT(")"),
    /** Left bracket */
    LBRACKET("]"),
    /** Right bracket */
    RBRACKET("["),
    /** Literal decimal number. */
    LITERAL_NUMERIC_DECIMAL,
    /** Literal hexadecimal number 0x1234567890ABCDEF */
    LITERAL_NUMERIC_HEXA,
    /** Literal bit number 0b1101010101 */
    LITERAL_NUMERIC_BINARY,
    /** Literal octal number 0o1234567 */
    LITERAL_NUMERIC_OCTAL,
    /** Literal binary X'1ABC3F' */
    LITERAL_BINARY,
    /** Literal string. */
    LITERAL_STRING,
    /** The "DATE" word for literal date or data type. */
    DATE,
    /** The "BINARY" word for literal binary. */
    BINARY,
    /** The "JSON" word for literal JSON. */
    JSON,
    /** The "INET" word for literal INET. */
    INET,
    /** The "INTERVAL" word for literal INTERVAL '...' YEAR TO MONTH. */
    INTERVAL,
    /** The BETWEEN ASYMMETRIC operator */
    ASYMMETRIC,
    /** The BETWEEN SYMMETRIC operator */
    SYMMETRIC,
    /** The "TIME" word for literal time. */
    TIME,
    /** The "TIMESTAMP" word for literal timestamp. */
    TIMESTAMP,
    /** The AT TIME ZONE */
    ZONE,
    /** Identifier */
    IDENTIFIER,
    /** Function */
    FUNCTION,
    /** Extract(part FROM date_time) */
    FROM,
    /** The arithmetic division operator, "/". */
    DIVIDE("/"),
    /** The arithmetic multiplication operator, "*". */
    MULTIPLY("*"),
    /** ESCAPE word for LIKE operator */
    ESCAPE,
    /** The arithmetic remainder operator, "MOD" (and "%" in some dialects). */
    MODULUS("%"),
    /**
     * The arithmetic unary plus (positive) operator "+" or the arithmetic addition operator "+".
     */
    PLUS("+"),
    /**
     * The arithmetic unary minus (negative) operator "-" or the arithmetic subtract operator "-".
     */
    MINUS("-"),
    /** The "IN" operator. */
    IN,
    /** The "BETWEEN" operator. */
    BETWEEN,
    /** The less-than operator "&lt;". */
    LT("<"),
    /** The greater-than operator "&gt;". */
    GT(">"),
    /** The less-than-or-equal operator "&lt;=". */
    LTE("<="),
    /** The greater-than-or-equal operator "&gt;=". */
    GTE(">="),
    /** The equals operator "=". */
    EQUAL("="),
    /** The not-equals operator "&lt;&gt;" or "&#33;=". */
    NOT_EQUAL("<>"),
    /** The logical "OR" operator. */
    OR,
    /** The logical "XOR" operator. */
    XOR,
    /** The logical "AND" operator or keyword for BETWEEN value1 "AND" value2 . */
    AND,
    /** The "LIKE" operator. */
    LIKE,
    /** The "ILIKE" operator. */
    ILIKE,
    /** The logical "NOT" operator. */
    NOT,
    /** The literal value "NULL". */
    NULL,
    /** For IGNORE NULLS or RESPECT NULLS */
    NULLS,
    /** For IGNORE NULLS or RESPECT NULLS */
    IGNORE,
    /** For IGNORE NULLS or RESPECT NULLS */
    RESPECT,
    /** For JSON_VALUE(... RETURNING) */
    RETURNING,
    /** The "IS" operator. */
    IS,
    /** Used by function "JSON_OBJECT" */
    KEY,
    /** Used by function "JSON_OBJECT" */
    VALUE,
    /** The literal value "TRUE". */
    TRUE,
    /** The literal value "FALSE". */
    FALSE,
    ELSE,
    THEN,
    END,
    WHEN,
    TRY,
    /** The "SIMILAR TO" operator or INTERVAL '...' YEAR TO MONTH */
    TO,
    /** The "SIMILAR TO" operator. */
    SIMILAR,
    /** Used by functions: COUNT([DISTINCT] numeric), LISTAGG( [DISTINCT] string [, delimiter]) */
    DISTINCT,
    /** Data type element DATE, NUMBER, BOOLEAN,... */
    LITERAL_DATATYPE,
    /** Time unit element DAY, MONTH, QUARTER, MINUTE, ... */
    LITERAL_TIMEUNIT;

    private final String label;

    Id() {
      this.label = name();
    }

    Id(final String label) {
      this.label = label;
    }

    @Override
    public String toString() {
      return label;
    }
  }

  private final Id id;
  private final int start;
  private final int end;
  private final String text;

  protected Token(Id id, int start) {
    this(id, start, start + 1, id.label);
  }

  protected Token(Id id, int start, int end, String text) {
    this.id = id;
    this.start = start;
    this.end = end;
    this.text = text;
  }

  public boolean is(final Id id) {
    return this.id == id;
  }

  public Id id() {
    return id;
  }

  /** Returns the start index of the token in the original source. */
  public int start() {
    return start;
  }

  /** Returns the end index of the token in the original source. */
  public int end() {
    return end;
  }

  /** Returns the length of the token. */
  public int length() {
    return end - start;
  }

  /** Returns the token value. */
  public String text() {
    return text;
  }

  @Override
  public String toString() {
    return (text != null) ? text : id.toString();
  }
}
