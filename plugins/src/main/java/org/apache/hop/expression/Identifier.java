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
import static org.apache.hop.expression.Expressions.createDataType;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.type.UnknownType;
import java.io.StringWriter;
import java.util.Objects;

/**
 * Expression representing a named column in an input row.
 */
public class Identifier implements IExpression {
  protected final String name;
  protected Type type;
  protected int ordinal;

  public Identifier(final String name, final Type type) {
    this(name, type, -1);
  }

  protected Identifier(final String name, final Type type, int ordinal) {
    this.name = requireNonNull(name, "name");
    this.type = requireNonNull(type, "data type");
    this.ordinal = ordinal;
  }

  public Identifier(final String name) {
    this(name, UnknownType.UNKNOWN);
  }

  @Override
  public Kind getKind() {
    return Kind.IDENTIFIER;
  }

  /**
   * Return the name of the identifier.
   * 
   * @return
   */
  public String getName() {
    return name;
  }

  @Override
  public Type getType() {
    return type;
  }

  @Override
  public int getCost() {
    return 2;
  }

  /**
   * Return the index in IRowMeta when resolved or -1 if unresolved identifier.
   * 
   * @return
   */
  public int getIndex() {
    return ordinal;
  }

  /**
   * Validate a identifier in the context.
   * 
   * <ul>
   * <li>Resolve index in IRowMeta</li>
   * <li>Determine data type of a value in row.</li>
   * </ul>
   */
  @Override
  public void validate(final IExpressionContext context) throws ExpressionException {
    IRowExpressionContext rowContext = getRowContext(context);
    IRowMeta rowMeta = rowContext.getRowMeta();

    this.ordinal = rowMeta.indexOfValue(name);
    if (ordinal < 0) {
      throw new ExpressionException(ExpressionError.UNRESOLVED_IDENTIFIER, this);
    }

    this.type = createDataType(rowMeta.getValueMeta(ordinal));
  }

  /**
   * Compile a identifier.
   * 
   * <ul>
   * <li>Resolve index in IRowMeta</li>
   * <li>Determine data type of a value in row.</li>
   * </ul>
   */
  @Override
  public IExpression compile(final IExpressionContext context) throws ExpressionException {
    IRowExpressionContext rowContext = getRowContext(context);
    IRowMeta rowMeta = rowContext.getRowMeta();

    int index = rowMeta.indexOfValue(name);
    if (index < 0) {
      throw new ExpressionException(ExpressionError.UNRESOLVED_IDENTIFIER, this);
    }

    return new Field(rowContext, name, createDataType(rowMeta.getValueMeta(index)), index);
  }

  @Override
  public <E> E accept(IExpressionContext context, IExpressionVisitor<E> visitor) {
    return visitor.apply(context, this);
  }

  @Override
  public void unparse(StringWriter writer) {
    // If identifier name contains space or is a reserved word or a function name
    if (name.indexOf(' ') >= 0 || ExpressionParser.isReservedWord(name)
        || FunctionRegistry.isFunction(name) || TypeName.of(name) != null
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
    return Objects.hash(name, type, ordinal);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o)
      return true;
    if (o == null)
      return false;
    if (getClass() != o.getClass())
      return false;

    Identifier other = (Identifier) o;
    return this.name.equals(other.name) && this.type.equals(other.type)
        && this.ordinal == other.ordinal;
  }

  protected IRowExpressionContext getRowContext(IExpressionContext context)
      throws ExpressionException {
    if (context instanceof IRowExpressionContext) {
      return (IRowExpressionContext) context;
    }
    throw new ExpressionException(ExpressionError.CONTEXT_ERROR);
  }

  @Override
  public Identifier asIdentifier() {
    return this;
  }

  @Override
  public String toString() {
    return this.name;
  }
}
