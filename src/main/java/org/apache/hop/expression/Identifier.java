/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.expression;

import static java.util.Objects.requireNonNull;

import java.io.StringWriter;
import java.util.Objects;
import lombok.Getter;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.type.Types;
import org.apache.hop.expression.type.UnknownType;
import org.apache.hop.expression.util.Characters;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Expression representing a named column in an input row. */
@NullMarked
public class Identifier implements IExpression {

  /** The characters that end an unquoted identifier, as in {@link ExpressionLexer}. */
  private static final String IDENTIFIER_DELIMITERS = "()/*%,^&><=~+-.!|$:[]";

  /** The position in the expression source */
  @Getter private final int position;

  /** The name of the identifier */
  @Getter private final String name;

  /** The data type of the identifier when resolved or UNKNOWN if unresolved */
  @Getter protected Type type;

  /** The index of the identifier in IRowMeta */
  @Getter protected int ordinal;

  public Identifier(String name) {
    this(0, name);
  }

  public Identifier(int position, String name) {
    this.position = position;
    this.name = requireNonNull(name, "name");
    this.type = UnknownType.UNKNOWN;
    this.ordinal = -1;
  }

  /**
   * If the name contains space or a special character, is a reserved word or is a function name
   * must be quoted.
   *
   * @param name the name to quote
   */
  public static String quoteIfNeeded(final String name) {

    if (hasSpecialCharacter(name)
        || Keyword.is(name)
        || TypeName.of(name) != null
        || TimeUnit.of(name) != null
        || FunctionRegistry.isFunction(name)) {
      return '\"' + name + '\"';
    }

    return name;
  }

  /**
   * Checks if a name cannot be read back as a single identifier without quotes: it is empty, starts
   * like a number or a string, or contains a white space or a character that ends an identifier in
   * {@link ExpressionLexer}, for example {@code A-B} would be read as a subtraction.
   *
   * @param name the name to check
   * @return true if the name must be quoted
   */
  private static boolean hasSpecialCharacter(final String name) {
    if (name.isEmpty()) return true;

    char first = name.charAt(0);
    if (Characters.isDigit(first) || first == '\'') return true;

    for (int i = 0; i < name.length(); i++) {
      char c = name.charAt(i);
      if (Characters.isSpace(c) || IDENTIFIER_DELIMITERS.indexOf(c) >= 0) return true;
    }
    return false;
  }

  @Override
  public Kind getKind() {
    return Kind.IDENTIFIER;
  }

  @Override
  public int getCost() {
    return 2;
  }

  /**
   * Validate an identifier in the context.
   *
   * <ul>
   *   <li>Resolve index in IRowMeta
   *   <li>Determine a data type of value in a row.
   * </ul>
   */
  @Override
  public void validate(final IExpressionContext context) throws ExpressionException {

    // An identifier is valid only if the context is IRowExpressionContext.
    if (context instanceof IRowExpressionContext rowContext) {
      IRowMeta rowMeta = rowContext.getRowMeta();
      this.ordinal = rowMeta.indexOfValue(name);
      if (ordinal >= 0) {
        this.type = Types.createType(rowMeta.getValueMeta(ordinal));
        return;
      }
    }

    throw new ParseExpressionException(position, ErrorCode.UNRESOLVED_IDENTIFIER, name);
  }

  @Override
  public <E> E accept(IExpressionVisitor<E> visitor) {
    return visitor.visitIdentifier(this);
  }

  @Override
  public void unparse(StringWriter writer, int leftPrec, int rightPrec) {
    // If the identifier name contains space or a special character, or is a reserved word or a
    // function name
    if (hasSpecialCharacter(name)
        || Keyword.is(name)
        || FunctionRegistry.isFunction(name)
        || TypeName.of(name) != null
        || TimeUnit.of(name) != null) {
      writer.append('\"');
      writer.append(name);
      writer.append('\"');
    } else {
      writer.append(name);
    }
  }

  @Override
  public int hashCode() {
    return Objects.hash(name);
  }

  @Override
  public boolean equals(@Nullable Object other) {
    // Compare the exact class to stay symmetric with subclasses overriding equals (see Field)
    if (other == null || getClass() != other.getClass()) {
      return false;
    }
    Identifier identifier = (Identifier) other;
    return this.name.equals(identifier.name)
        && this.type.equals(identifier.type)
        && this.ordinal == identifier.ordinal;
  }

  @Override
  public String toString() {
    StringWriter writer = new StringWriter();
    unparse(writer, 0, 0);
    return writer.toString();
  }
}
