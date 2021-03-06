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

import java.io.StringWriter;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.stream.Stream;

/** Immutable list of expression. */
public class ExpressionList implements IExpression, Iterable<IExpression> {

  /**
   * Iterator implementation used to efficiently expose contents of an ExpressionList as read-only
   * iterator.
   */
  public class Iterator implements java.util.Iterator<IExpression> {

    private int index;

    public Iterator() {
      index = 0;
    }

    @Override
    public boolean hasNext() {
      return index < values.length;
    }

    @Override
    public IExpression next() {
      if (index >= values.length) {
        throw new NoSuchElementException();
      }
      return values[index++];
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException();
    }
  }

  /** An immutable, empty ExpressionList. */
  public static final ExpressionList EMPTY = new ExpressionList() {};

  private final IExpression[] values;

  public ExpressionList(IExpression... expressions) {
    this.values = expressions;
  }

  public ExpressionList(List<IExpression> expressions) {
    this.values = expressions.toArray(new IExpression[0]);
  }

  public ExpressionList(Set<IExpression> expressions) {
    this.values = expressions.toArray(new IExpression[0]);
  }

  public Kind getKind() {
    return Kind.LIST;
  }

  @Override
  public int getCost() {
    int cost = 1;
    for (IExpression e : values) {
      cost += e.getCost();
    }
    return cost;
  }

  public IExpression get(int index) {
    return values[index];
  }

  public boolean isEmpty() {
    return values.length == 0;
  }

  public int size() {
    return values.length;
  }

  public IExpression[] toArray() {
    return values;
  }

  @Override
  public Object eval(IExpressionContext context) throws ExpressionException {
    throw new ExpressionException("ExpressionException.ExpressionListNotEvaluable");
  }

  @Override
  public String toString() {
    StringWriter writer = new StringWriter();
    write(writer, 0, 0);
    return writer.toString();
  }

  public void write(StringWriter writer, int leftPrec, int rightPrec) {

    writer.append('(');
    boolean first = true;
    for (IExpression expression : values) {
      if (first)
        first = false;
      else {
        writer.append(',');
      }
      expression.write(writer, 2, 3);
    }
    writer.append(')');
  }

  Stream<IExpression> stream() {
    return Stream.of(values);
  }

  @Override
  public java.util.Iterator<IExpression> iterator() {
    return new Iterator();
  }
}
