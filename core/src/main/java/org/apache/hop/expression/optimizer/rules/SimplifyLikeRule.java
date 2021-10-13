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
package org.apache.hop.expression.optimizer.rules;

import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCall;
import org.apache.hop.expression.OperatorRegistry;
import org.apache.hop.expression.optimizer.Optimizer.Rule;
import java.util.regex.Pattern;

/**
 * Simplifies LIKE expressions that do not need full regular expressions to evaluate the
 * condition. For example, when the expression is just checking to see if a string starts with a
 * given pattern.
 */
public class SimplifyLikeRule implements Rule {
  Pattern startsWith = Pattern.compile("^([^_%]+)%$");
  Pattern endsWith = Pattern.compile("^%([^_%]+)$");
  Pattern contains = Pattern.compile("^%([^_%]+)%$");
  Pattern equalTo = Pattern.compile("^[^_%]*$");

  @Override
  public IExpression apply(IExpressionContext context, OperatorCall call) {
    if ( call.is(Operator.LIKE)) {

      // Optimize NULL LIKE FIELD to NULL
      IExpression v0 = call.getOperand(0);
      if (v0 == Literal.UNKNOWN)
        return Literal.UNKNOWN;

      IExpression v1 = call.getOperand(1);
      if (v1.getKind() == Kind.LITERAL) {
        String pattern = Operator.coerceToString(v1.eval(context));

        // Optimize FIELD LIKE NULL to NULL
        if (pattern == null)
          return Literal.UNKNOWN;

        if (call.getOperandCount() == 3) {
          String escape = Operator.coerceToString(call.getOperand(2).eval(context));
          if (escape == null)
            return Literal.UNKNOWN;

          // For now don't optimize if special escape char
          return call;
        }

        // Optimize the common case of FIELD LIKE '%foo%' to CONTAINS(FIELD,'foo')
        // Try contains before starts and ends
        if (contains.matcher(pattern).find()) {
          String search = pattern.replace("%", "");
          return new OperatorCall(OperatorRegistry.getInstance().getFunction("CONTAINS"), v0,
              Literal.of(search));
        }

        // Optimize the common case of FIELD LIKE 'foo%' to STARTSWITH(FIELD,'foo')
        if (startsWith.matcher(pattern).find()) {
          String search = pattern.replace("%", "");
          return new OperatorCall(OperatorRegistry.getInstance().getFunction("STARTSWITH"), v0,
              Literal.of(search));
        }

        // Optimize the common case of FIELD LIKE '%foo' to ENDSWITH(FIELD,'foo')
        if (endsWith.matcher(pattern).find()) {
          String search = pattern.replace("%", "");
          return new OperatorCall(OperatorRegistry.getInstance().getFunction("ENDSWITH"), v0,
              Literal.of(search));
        }

        // Optimize FIELD LIKE 'Hello' to FIELD='Hello'
        if (equalTo.matcher(pattern).find()) {
          String search = pattern.replace("%", "");
          return new OperatorCall(Operator.EQUAL, v0, Literal.of(search));
        }
      }
    }

    return call;
  }
}
