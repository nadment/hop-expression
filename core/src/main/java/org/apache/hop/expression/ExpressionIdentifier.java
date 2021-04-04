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

import org.apache.hop.expression.value.Value;
import java.io.StringWriter;

public class ExpressionIdentifier implements IExpression {

  private final String name;

  public ExpressionIdentifier(final String name) {
    super();

    this.name = name;
  }

  @Override
  public Kind getKind() {
    return Kind.IDENTIFIER;
  }

  public String getName() {
    return name;
  }

  @Override
  public int getCost() {
    return 2;
  }

  @Override
  public Value eval(IExpressionContext context) throws ExpressionException {
    return context.resolve(this.name);
  }

  @Override
  public void write(StringWriter writer, int leftPrec, int rightPrec) {
    // If identifier name contains space or is a reserved word or a function name
    if (name.indexOf(' ') >= 0 || ExpressionScanner.isReservedWord(name) || DataType.exist(name) || DatePart.exist(name) || ExpressionScanner.isFunctionName(name) ) {
      writer.append('[');
      writer.append(this.name);
      writer.append(']');
    } else
      writer.append(this.name);
  }


  
  @Override
  public String toString() {
    return this.name;
  }

  @Override
  public boolean isConstant() {
    return false;
  }

  @Override
  public IExpression optimize(IExpressionContext context) throws ExpressionException {
    return this;
  }
}
