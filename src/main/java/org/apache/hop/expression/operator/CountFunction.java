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
package org.apache.hop.expression.operator;

import java.io.StringWriter;
import org.apache.hop.expression.AggregateFunction;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IAggregateProcessor;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.jspecify.annotations.NullMarked;

@FunctionPlugin
@NullMarked
public class CountFunction extends AggregateFunction {
  public static final CountFunction COUNT_ALL = new CountFunction(Count.ALL);
  public static final CountFunction COUNT_DISTINCT = new CountFunction(Count.DISTINCT);
  public static final CountFunction COUNT_ROW = new CountFunction(Count.ROW);
  private final Count count;

  /**
   * Default constructor to register function but not used. The parser detects the different count
   * modes.
   */
  public CountFunction() {
    this(Count.ROW);
  }

  public CountFunction(Count count) {
    super("COUNT", ReturnTypes.INTEGER_NOT_NULL, OperandTypes.ANY, "/docs/count.html");
    this.count = count;
  }

  @Override
  public IAggregateProcessor createProcessor(IExpressionContext context, IExpression[] operands) {
    return switch (count) {
      case ALL -> new CountAllProcessor();
      case DISTINCT -> new CountDistinctProcessor();
      case ROW -> new CountRowProcessor();
    };
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append(this.getName());
    writer.append('(');
    switch (count) {
      case ALL:
        writer.append("ALL ");
        operands[0].unparse(writer, 0, 0);
        break;
      case DISTINCT:
        writer.append("DISTINCT ");
        operands[0].unparse(writer, 0, 0);
        break;
      case ROW:
        writer.append('*');
        break;
    }
    writer.append(')');
  }

  public enum Count {
    ALL,
    DISTINCT,
    ROW
  }
}
