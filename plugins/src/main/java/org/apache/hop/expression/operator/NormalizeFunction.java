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
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;
import java.text.Normalizer;
import java.text.Normalizer.Form;

/**
 * The function returns a string as a normalized string.
 */
@FunctionPlugin(id = "NORMALIZE")
public class NormalizeFunction extends Function {

 
  public NormalizeFunction() {
    super("NORMALIZE", true, ReturnTypes.STRING, OperandTypes.STRING_OPTIONAL_STRING, "i18n::Operator.Category.String", "/docs/normalize.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].getValue(context);
    if (v0 == null)
      return null;

    String str = Coerse.toString(v0);

    Form form = Form.NFD;
    
    if ( operands.length==2 ) {
      try {
        form = Form.valueOf(Coerse.toString(operands[1].getValue(context)));
      } catch (Exception e) {
        throw new ExpressionException(ExpressionError.ILLEGAL_ARGUMENT, this.getName());
      }
    }
    
    return Normalizer.normalize(str, form);
  }
}