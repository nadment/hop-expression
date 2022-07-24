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

/** A <code>Function</code> is a type of operator which has conventional function-call syntax. */

public abstract class Function extends Operator {

  /**
   * Creates an function.
   *
   * Note that some function has specific syntax CAST, CONCAT, COUNT, EXTRACT, POSITION.
   * 
   * @param id The unique identifier of the function
   * @param name The name of function
   */
  public Function(String id, String name, boolean isDeterministic, String category, String documentationUrl) {
    super(id, name, 10, true, isDeterministic, category, documentationUrl);
  }
 
  public Function(String id, String name, int precedence, boolean isLeftAssociative, boolean isDeterministic, String category, String documentationUrl) {
    super(id, name, precedence, isLeftAssociative, isDeterministic, category, documentationUrl);
  }
  
  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append(this.getName());
    writer.append('(');
    boolean first = true;
    for (IExpression operand : operands) {
      if (!first)
        writer.append(',');
      else
        first = false;
      operand.unparse(writer);
    }
    writer.append(')');
  }
  
  /**
   * Returns whether this function is an aggregate function.
   */
  public boolean isAggregator() {
    return false;
  }
}