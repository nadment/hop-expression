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

import org.apache.hop.expression.AggregateFunction;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.IExpressionProcessor;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

@FunctionPlugin
public class ListAggFunction extends AggregateFunction {
  public static final ListAggFunction LISTAGG_ALL = new ListAggFunction(ListAgg.ALL);
  public static final ListAggFunction LISTAGG_DISTINCT = new ListAggFunction(ListAgg.DISTINCT);

  public enum ListAgg {
    ALL,
    DISTINCT
  }

  private final ListAgg option;

  /**
   * Default constructor to register function but not used. The different options are detected by
   * parser.
   */
  public ListAggFunction() {
    this(ListAgg.ALL);
  }

  public ListAggFunction(ListAgg option) {
    super(
        "LISTAGG",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING.or(OperandTypes.STRING_STRING),
        "/docs/listagg.html");
    this.option = option;
  }

  @Override
  public IExpressionProcessor createProcessor(IExpressionContext context, IExpression[] operands)
      throws ExpressionException {

    String delimiter = ",";
    if (operands.length == 2) {
      delimiter = operands[1].getValue(String.class);
    }

    if (option == ListAgg.DISTINCT) {
      return new ListAggDistinctProcessor(delimiter);
    }
    return new ListAggProcessor(delimiter);
  }
}
