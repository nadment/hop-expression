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

import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.UnknownType;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.stream.Stream;

/**
 * A tuple is a immutable ordered list of expressions.
 */
public final class Tuple implements IExpression, Iterable<IExpression> {

  /**
   * Iterator implementation used to efficiently expose contents of an Tuple as read-only
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

  private final IExpression[] values;

  public Tuple(IExpression... expressions) {
    this.values = expressions;
  }

  public Tuple(Collection<IExpression> expressions) {
    this.values = expressions.toArray(new IExpression[0]);
  }

  @Override
  public Kind getKind() {
    return Kind.TUPLE;
  }

  @Override
  public Type getType() {
    // Returns the first known data type of values.
    for (IExpression expression : values) {
      Type type = expression.getType();
      if (type != UnknownType.UNKNOWN)
        return type;
    }

    return UnknownType.UNKNOWN;
  }

  @Override
  public int getCost() {
    int cost = 1;
    for (IExpression expression : values) {
      cost += expression.getCost();
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
  public <E> E accept(IExpressionContext context, IExpressionVisitor<E> visitor) {
    return visitor.apply(context, this);
  }

  @Override
  public boolean equals(Object other) {
    if (other instanceof Tuple) {
      Tuple o = (Tuple) other;
      return Arrays.equals(values, o.values);
    }
    return false;
  }

  @Override
  public int hashCode() {
    return Arrays.hashCode(values);
  }

  @Override
  public String toString() {
    StringWriter writer = new StringWriter();
    unparse(writer);
    return writer.toString();
  }

  public void unparse(StringWriter writer) {
    writer.append('(');
    boolean first = true;
    for (IExpression expression : values) {
      if (first)
        first = false;
      else {
        writer.append(',');
      }
      expression.unparse(writer);
    }
    writer.append(')');
  }
  
  @Override
  public void validate(final IExpressionContext context) throws ExpressionException {
    // Validate all elements
    for (IExpression expression : this) {
      expression.validate(context);
    }
  }
  
  @Override
  public IExpression compile(final IExpressionContext context) throws ExpressionException {
    
    // Compile all elements
    List<IExpression> elements = new ArrayList<>(size());
    for (IExpression expression : this) {
      elements.add(expression.compile(context));
    }
    return new Tuple(elements);
  }
  
  public Stream<IExpression> stream() {
    return Stream.of(values);
  }

  @Override
  public java.util.Iterator<IExpression> iterator() {
    return new Iterator();
  }

  @Override
  public Tuple asTuple() {
    return this;
  }
  
  @Override
  public boolean isConstant() {
    for (IExpression expression : values) {
      if (!expression.isConstant()) {
        return false;
      }
    }
    
    return true;
  }

}
