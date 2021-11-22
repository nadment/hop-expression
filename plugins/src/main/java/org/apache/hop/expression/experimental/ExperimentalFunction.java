/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression.experimental;

import static org.apache.hop.expression.Operator.coerceToString;
import org.apache.commons.lang.StringUtils;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.ScalarFunction;

public class ExperimentalFunction {

  private ExperimentalFunction() {
  }

  /** The function compute Levenshtein distance. */
  @ScalarFunction(name = "LEVENSHTEIN", category = "i18n::Operator.Category.String", minArgs = 2, maxArgs = 2)
  public static Object levenshtein(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;
    
    return Long.valueOf(StringUtils.getLevenshteinDistance(coerceToString(v0), coerceToString(v1)));
  }
}
