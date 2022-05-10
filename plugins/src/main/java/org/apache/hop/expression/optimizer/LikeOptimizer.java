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
package org.apache.hop.expression.optimizer;

import org.apache.hop.expression.Call;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.util.Coerse;
import java.util.regex.Pattern;

/**
 * Simplifies LIKE expressions that do not need full regular expressions to evaluate the
 * condition. For example, when the expression is just checking to see if a string starts with a
 * given pattern.
 */
public class LikeOptimizer extends Optimizer {
  Pattern startsWith = Pattern.compile("^([^_%]+)%$");
  Pattern endsWith = Pattern.compile("^%([^_%]+)$");
  Pattern contains = Pattern.compile("^%([^_%]+)%$");
  Pattern equalTo = Pattern.compile("^[^_%]*$");

  @Override
  public IExpression apply(final IExpressionContext context, final Call call) {
    try {
      if (call.is(Operators.LIKE)) {

        // Optimize NULL LIKE FIELD to NULL
        IExpression v0 = call.getOperand(0);
        if (v0 == Literal.NULL)
          return Literal.NULL;

        IExpression v1 = call.getOperand(1);
        if (v1.is(Kind.LITERAL)) {
          String pattern = Coerse.toString(v1.eval(context));

          // Optimize FIELD LIKE NULL to NULL
          if (pattern == null)
            return Literal.NULL;

          if (call.getOperandCount() == 3) {
            String escape = Coerse.toString(call.getOperand(2).eval(context));
            if (escape == null)
              return Literal.NULL;

            // For now don't optimize if special escape char
            return call;
          }

          // Optimize the common case of FIELD LIKE '%foo%' to CONTAINS(FIELD,'foo')
          // Try contains before starts and ends
          if (contains.matcher(pattern).find()) {
            String search = pattern.replace("%", "");
            return new Call(FunctionRegistry.getFunction("CONTAINS"), v0,
                Literal.of(search));
          }

          // Optimize the common case of FIELD LIKE 'foo%' to STARTSWITH(FIELD,'foo')
          if (startsWith.matcher(pattern).find()) {
            String search = pattern.replace("%", "");
            return new Call(FunctionRegistry.getFunction("STARTSWITH"), v0,
                Literal.of(search));
          }

          // Optimize the common case of FIELD LIKE '%foo' to ENDSWITH(FIELD,'foo')
          if (endsWith.matcher(pattern).find()) {
            String search = pattern.replace("%", "");
            return new Call(FunctionRegistry.getFunction("ENDSWITH"), v0,
                Literal.of(search));
          }

          // Optimize FIELD LIKE 'Hello' to FIELD='Hello'
          if (equalTo.matcher(pattern).find()) {
            String search = pattern.replace("%", "");
            return new Call(Operators.EQUAL, v0, Literal.of(search));
          }
        }
      }

      return call;
    } catch (Exception e) {
      return call;
    }    
  }
}
