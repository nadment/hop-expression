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


    /** Variable "${var}" if scanner is used without variable environment substitution. */
    VARIABLE,

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

    /**
     * Concat operator <code>||<code>
     */
    CONCAT("||"),

    CAST, AS,
    /** The AT TIME ZONE */
    AT,
    /** CAST(numeric AS datatype FORMAT '9999') */
    FORMAT,
    /**
     * Cast operator <code>::<code>
     */
    CAST_OP("::"),

    /** Comment */
    COMMENT,

    /** Comma separator */
    COMMA(","),

    /** Left parenthesis */
    LPARENTHESIS("("),

    /** Right parenthesis */
    RPARENTHESIS(")"),

    /** Literal number. */
    LITERAL_NUMBER,

    /** Literal hex binary 0x1234567890ABCDEF */
    LITERAL_BINARY_HEX,

    /** Literal bit binary 0b1101010101 */
    LITERAL_BINARY_BIT,

    /** Literal string. */
    LITERAL_STRING,

    /** The "DATE" word for literal date. */
    DATE,

    /** The "TIME" word for literal time. */
    TIME,

    /** The "TIMESTAMP" word for literal timesamp. */
    TIMESTAMP,

    /** The AT TIME ZONE */
    ZONE,

    /** Identifier */
    IDENTIFIER,

    /** Function */
    FUNCTION,

    FROM,

    /** The arithmetic division operator, "/". */
    DIVIDE("/"),

    /** The arithmetic multiplication operator, "*". */
    MULTIPLY("*"),

    /** ESCAPE word for like operator */
    ESCAPE,

    // /**
    // * TOOD: remove or implement: The arithmetic power operator, "**".
    // */
    // POWER("**"),

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
    LESS_THAN("<"),

    /** The greater-than operator "&gt;". */
    GREATER_THAN(">"),

    /** The less-than-or-equal operator "&lt;=". */
    LESS_THAN_OR_EQUAL("<="),

    /** The greater-than-or-equal operator "&gt;=". */
    GREATER_THAN_OR_EQUAL(">="),

    /** The equals operator "=". */
    EQUAL("="),

    /**
     * Compares whether two expressions are equal.
     *
     * <p>
     * The function is NULL-safe, meaning it treats NULLs as known values for comparing equality.
     * Note that this is different from the EQUAL comparison operator (=), which treats NULLs as
     * unknown values.
     */
    EQUAL_NULL,

    /** The not-equals operator, "&#33;=". */
    NOT_EQUAL("!="),

    /** The not-equals operator "&lt;&gt;". */
    LESS_THAN_OR_GREATER_THAN("<>"),

    /** The logical "OR" operator. */
    OR,

    /** The logical "XOR" operator. */
    XOR,

    /** The logical "AND" operator or keyword for BEETWEN value1 "AND" value2 . */
    AND,

    /** The "LIKE" operator. */
    LIKE,

    /** The "ILIKE" operator. */
    ILIKE,

    /** The logical "NOT" operator. */
    NOT,

    /** The literal value "NULL". */
    NULL,

    /** The "IS" operator. */
    IS,

    /** The literal value "TRUE". */
    TRUE,

    /** The literal value "FALSE". */
    FALSE,

    ELSE, THEN, END, WHEN,

    HOUR, MINUTE, SECOND, DATATYPE, DATEPART;

    private final String source;

    Id() {
      this.source = name();
    }

    Id(final String source) {
      this.source = source;
    }

    @Override
    public String toString() {
      if (this.source.equals(this.name()))
        return source;
      return this.name() + '(' + source + ')';
    }
  }

  private final Id id;
  private final int start;
  private final int end;
  private final String text;

  protected Token(Id id, int start) {
    this(id, start, start + 1, id.source);
  }

  protected Token(Id id, int start, int end, String text) {
    this.id = id;
    this.start = start;
    this.end = end;
    this.text = text;
  }

  public boolean is(Id id) {
    return this.id == id;
  }

  public Id id() {
    return id;
  }

  /**
   * Returns the start index of the token in the original source.
   */
  public int start() {
    return start;
  }

  /**
   * Returns the end index of the token in the original source.
   */
  public int end() {
    return end;
  }

  /**
   * Returns the length of the token.
   */
  public int length() {
    return end - start;
  }

  /** Returns the token value. */
  public String text() {
    return text;
  }

  @Override
  public String toString() {
    return id.name() + "(" + text + ")";
  }
}
