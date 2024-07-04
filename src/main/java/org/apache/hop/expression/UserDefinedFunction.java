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
package org.apache.hop.expression;

import java.util.List;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Types;
import org.apache.hop.expression.type.UserDefinedFunctionOperandTypeChecker;

public class UserDefinedFunction extends Function {

  private final UserDefinedFunctionMeta meta;

  public UserDefinedFunction(UserDefinedFunctionMeta meta) {
    super(
        meta.getName(),
        ReturnTypes.ANY,
        new UserDefinedFunctionOperandTypeChecker(meta),
        OperatorCategory.UDF,
        "/docs/udf.html");
    this.meta = meta;
  }

  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {
    try {
      // Create a context for arguments
      IRowExpressionContext ctx =
          new RowExpressionContext(
              new Variables(), createRowMetaFromArguments(meta.getArguments()));

      // Parse function source
      ExpressionParser parser = new ExpressionParser(getSource());
      IExpression expression = parser.parse();
      expression.validate(ctx);

      // Replace function arguments with real operands
      expression = expression.accept(new UserDefinedFunctionResolver(call.getOperands()));
      expression.validate(context);

      // Compile
      ExpressionCompiler compiler = new ExpressionCompiler(context);
      return compiler.compile(expression);
    } catch (Exception e) {
      throw new ExpressionException(ErrorCode.UDF_COMPILATION_ERROR, getName());
    }
  }

  public String getSource() {
    return meta.getSource();
  }

  public List<FunctionArgument> getArguments() {
    return meta.getArguments();
  }

  public static IRowMeta createRowMetaFromArguments(List<FunctionArgument> arguments) {

    // Convert arguments to row meta
    IRowMeta rowMeta = new RowMeta();
    for (FunctionArgument argument : arguments) {
      IValueMeta vm = Types.createValueMeta(argument.getName(), argument.getType());
      rowMeta.addValueMeta(vm);
    }

    return rowMeta;
  }
}
