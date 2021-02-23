/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression;

import java.io.StringWriter;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/** Immutable list of expression. */
public class ExpressionList implements IExpression, Iterable<IExpression> {

  /**
   * Iterator implementation used to efficiently expose contents of an ExpressionList as read-only
   * iterator.
   */
  public class ExpressionIterator implements Iterator<IExpression> {

    private int index;

    public ExpressionIterator() {
      index = 0;
    }

    @Override
    public boolean hasNext() {
      return index < list.length;
    }

    @Override
    public IExpression next() {
      if (index >= list.length) {
        throw new NoSuchElementException();
      }
      return list[index++];
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException();
    }
  }

  /** An immutable, empty ExpressionList. */
  public static final ExpressionList EMPTY = new ExpressionList() {};

  private final IExpression[] list;

  public ExpressionList() {
    super();
    this.list = new IExpression[0];
  }

  public ExpressionList(IExpression... expressions) {
    this.list = expressions;
  }

  public ExpressionList(List<IExpression> expressions) {
    this.list = expressions.toArray(new IExpression[0]);
  }

  public Kind getKind() {
    return Kind.LIST;
  }

  @Override
  public int getCost() {
    int cost = 1;
    for (IExpression e : list) {
      cost += e.getCost();
    }
    return cost;
  }

  public IExpression get(int index) {
    return list[index];
  }

  public boolean isEmpty() {
    return list.length == 0;
  }

  @Override
  public boolean isConstant() {
    for (IExpression e : list) {
      if (!e.isConstant()) {
        return false;
      }
    }
    return true;
  }

  public int size() {
    return list.length;
  }

  public IExpression[] toArray() {
    return list;
  }

  @Override
  public Value eval(IExpressionContext context) throws ExpressionException {
    throw new ExpressionException("ExpressionException.ExpressionListNotEvaluable");
  }

  public void write(StringWriter writer, int leftPrec, int rightPrec) {

    writer.append('(');
    boolean first = true;
    for (IExpression expression : list) {
      if (first) first = false;
      else {
        writer.append(',');
      }
      expression.write(writer, 2, 3);
    }
    // if (parenthese)
    writer.append(')');
  }

  @Override
  public Iterator<IExpression> iterator() {
    return new ExpressionIterator();
  }

  @Override
  public boolean isNull() {   
    return false;
  }

  @Override
  public IExpression optimize(IExpressionContext context) throws ExpressionException {
    return this;
  }
}
