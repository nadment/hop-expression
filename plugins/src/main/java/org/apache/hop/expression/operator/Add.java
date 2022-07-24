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
package org.apache.hop.expression.operator;

import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.IReturnTypeInference;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.ReturnTypes;
import org.apache.hop.expression.util.Coerse;
import java.io.StringWriter;
import java.math.BigDecimal;

/**
 * Arithmetic addition operator.
 * <br><strong>Syntax:</strong> <code>x + y</code>
 */
public class Add extends Operator {

  protected static final double SECONDS_BY_DAY = 24D * 60 * 60;

  public Add() {
    super( "ADD", "+", 100, true, true, "i18n::Operator.Category.Mathematical", "/docs/add.html");
  }
  
  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    Object left = operands[0].eval(context);
    if (left == null)
      return null;
    Object right = operands[1].eval(context);
    if (right == null)
      return null;
    
// Removed because it's hard to optimize    
//    if (left instanceof ZonedDateTime) {
//      // Computes fraction of day
//      long seconds = (long) (Coerse.toNumber(right) * SECONDS_BY_DAY);
//      return Coerse.toDate(left).plusSeconds(seconds);
//    }
    if (left instanceof BigDecimal || right instanceof BigDecimal) {
      return Coerse.toBigNumber(left).add(Coerse.toBigNumber(right));
    }
    if (left instanceof Double || right instanceof Double) {
      return Coerse.toNumber(left) + Coerse.toNumber(right);
    }
    if (left instanceof Long || right instanceof Long) {
      return Coerse.toInteger(left) + Coerse.toInteger(right);
    }

    return Coerse.toBigNumber(left).add(Coerse.toBigNumber(right));
  }
  
  @Override
  public boolean isSymmetrical() {
    return true;
  }
  
  @Override
  public IReturnTypeInference getReturnTypeInference() {
    return ReturnTypes.LEAST_RESTRICTIVE;
  }
  
  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append('+');
    operands[1].unparse(writer);
  }
}
