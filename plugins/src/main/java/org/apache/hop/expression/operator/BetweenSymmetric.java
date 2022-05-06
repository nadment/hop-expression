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
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.util.Coerse;
import java.io.StringWriter;

/**
 * <code>BETWEEN SYMMETRIC</code> operator. 
 * The order of lower bound and upper bound is irrelevant.
 *  
 */
public class BetweenSymmetric extends Operator {

  public BetweenSymmetric() {
    super("BETWEEN", 120, true, true, "i18n::Operator.Category.Comparison", "/docs/between.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    Object start = operands[1].eval(context);
    Object end = operands[2].eval(context);

    if (value == null || start == null || end == null) {
      return null;
    }

    // If lower bound is greater than upper bound 
    if ( Coerse.compare(start, end) >= 0 ) {
      return Coerse.compare(value, end) >= 0 && Coerse.compare(value, start) <= 0;
    }
    
    return Coerse.compare(value, start) >= 0 && Coerse.compare(value, end) <= 0;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" BETWEEN SYMMETRIC ");
    operands[1].unparse(writer);
    writer.append(" AND ");
    operands[2].unparse(writer);
  }
}
