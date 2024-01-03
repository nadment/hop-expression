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

import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;
import java.io.StringWriter;
import java.math.BigDecimal;

/**
 * Arithmetic modulus operator.
 * <br>
 * <strong>Syntax:</strong> <code>x % y</code>
 */
@FunctionPlugin
public class ModFunction extends Function {

  public ModFunction() {
    super("MOD", ReturnTypes.MOD_OPERATOR, OperandTypes.NUMERIC_NUMERIC, OperatorCategory.MATHEMATICAL,
        "/docs/mod.html");
  }

  public ModFunction(String name) {
    super("MOD", name, 50, true, ReturnTypes.NUMBER_NULLABLE, OperandTypes.NUMERIC_NUMERIC,
        OperatorCategory.MATHEMATICAL, "/docs/mod.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    BigDecimal value = operands[0].getValue(BigDecimal.class);
    if (value == null)
      return null;
    BigDecimal divisor = operands[1].getValue(BigDecimal.class);
    if (divisor == null)
      return null;

    // Prevent a division by zero ..
    if (divisor.signum() == 0)
      throw new ArithmeticException(ErrorCode.DIVISION_BY_ZERO.message());

    return value.remainder(divisor);
  }


  @Override
  public Call castType(Call call) {
    Types.arithmeticCoercion(call);    
    return super.castType(call);
  }
  
  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append('%');
    operands[1].unparse(writer);
  }
}
