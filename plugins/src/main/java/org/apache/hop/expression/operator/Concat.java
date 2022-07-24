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
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.IReturnTypeInference;
import org.apache.hop.expression.ReturnTypes;
import org.apache.hop.expression.ScalarFunction;
import org.apache.hop.expression.util.Coerse;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringWriter;


/** 
 * String concatenation operator '<code>||</code>'
 */
public class Concat extends Function {

  public Concat() {
    super("CONCAT", "||", 110, true, true, "i18n::Operator.Category.String", "/docs/concat.html");
  }

  @ScalarFunction(id = "CONCAT", minArgs = 2, maxArgs = Integer.MAX_VALUE,
      category = "i18n::Operator.Category.String", documentationUrl="/docs/concat.html")
  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {

    Object firstNotNull = null;
    Object[] values = new Object[operands.length];
    int i=0;
    for (IExpression operand : operands) {
      Object value = operand.eval(context);
      if (firstNotNull==null && value != null)  firstNotNull=value;
      values[i++] = value;
    }
    
    if (firstNotNull == null )
      return null;
    
    // Concat Binary
    if (firstNotNull instanceof byte[]) {
      // int lenth = 0;
      try (ByteArrayOutputStream output = new ByteArrayOutputStream()) {
        for (Object value : values) {
          if (value != null) {
            output.write((byte[]) value);
          }
        }
        return output.toByteArray();
      } catch (IOException e) {
         throw new ExpressionException(ExpressionError.INTERNAL_ERROR, e.getMessage());
      }
    }

    // Concat String
    StringBuilder builder = new StringBuilder();
    for (IExpression operand : operands) {
      Object value = operand.eval(context);
      if (value != null)
        builder.append(Coerse.toString(value));
    }

    return builder.toString();
  }

  @Override
  public IReturnTypeInference getReturnTypeInference() {
    return ReturnTypes.FIRST_KNOWN;
  }
  
  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    boolean concatFirst = true;
    for (IExpression operand : operands) {
      if (concatFirst)
        concatFirst = false;
      else
        writer.append("||");
      operand.unparse(writer);
    }
  }
}
