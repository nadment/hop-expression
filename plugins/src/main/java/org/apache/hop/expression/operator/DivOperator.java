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

import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.math.MathContext;

/**
 * Arithmetic division operator.
 * <br>
 * <strong>Syntax:</strong> <code>x / y</code>
 */
public class DivOperator extends Operator {

  public DivOperator() {
    super("DIV", "/", 50, true, true, ReturnTypes.BIGNUMBER, OperandTypes.NUMERIC_NUMERIC,
        OperatorCategory.MATHEMATICAL, "/docs/div.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands) throws Exception {
    BigDecimal value = operands[0].getValue(context, BigDecimal.class);
    if (value == null)
      return null;
    BigDecimal divisor = operands[1].getValue(context, BigDecimal.class);
    if (divisor == null)
      return null;

    // Prevent a division by zero ..
    if (divisor.signum() == 0)
      throw new ArithmeticException(ExpressionError.DIVISION_BY_ZERO.message());
    
    return value.divide(divisor, MathContext.DECIMAL128);
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append('/');
    operands[1].unparse(writer);
  }
}
