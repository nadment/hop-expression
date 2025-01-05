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

import org.apache.hop.expression.type.TypeName;

public final class ExpressionFactory {

  private ExpressionFactory() {
    // Utility class
  }

  public static IExpression create(IExpressionContext context, String source)
      throws ExpressionException {

    // Syntax analysis
    ExpressionParser parser = new ExpressionParser(context.resolve(source));
    IExpression expression = parser.parse();

    // Semantic analysis
    expression.validate(context);

    // Compile expression
    ExpressionCompiler compiler = new ExpressionCompiler(context);
    expression = compiler.compile(expression);

    // Return type Unknown is not expected here
    if (!expression.isNull() && expression.getType().is(TypeName.UNKNOWN)) {
      throw new ExpressionParseException(0, ErrorCode.RETURN_TYPE_UNKNOWN);
    }

    return expression;
  }
}
