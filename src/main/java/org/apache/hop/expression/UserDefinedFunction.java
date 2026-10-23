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
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.type.Types;
import org.apache.hop.expression.type.UserDefinedFunctionOperandTypeChecker;
import org.jspecify.annotations.NullMarked;

/**
 * A scalar function defined by the user as an expression over named arguments.
 *
 * <p>The function has no evaluation of its own: when a call is compiled, the source expression is
 * parsed, its arguments are replaced by the operands of the call and the result is inlined in the
 * calling expression.
 *
 * @see UserDefinedFunctionMeta
 */
@NullMarked
public class UserDefinedFunction extends Function {

  private final UserDefinedFunctionMeta meta;

  /**
   * Creates a function from its metadata.
   *
   * @param meta the metadata that defines the name, the arguments and the source of the function
   */
  public UserDefinedFunction(UserDefinedFunctionMeta meta) {
    super(
        meta.getName(),
        ReturnTypes.ANY,
        new UserDefinedFunctionOperandTypeChecker(meta),
        OperatorCategory.UDF,
        "/docs/hop-user-manual/expression/udf.html");
    this.meta = meta;
  }

  /**
   * Creates the row metadata used to resolve the arguments in the source of a function, with one
   * field per argument.
   *
   * @param arguments the arguments of the function
   * @return the row metadata
   */
  public static IRowMeta createRowMetaFromArguments(List<FunctionArgument> arguments) {

    // Convert arguments to row meta
    IRowMeta rowMeta = new RowMeta();
    for (FunctionArgument argument : arguments) {
      // An argument being edited may not have a type yet
      TypeName type = argument.getType() == null ? TypeName.UNKNOWN : argument.getType();
      IValueMeta vm = Types.createValueMeta(argument.getName(), type);
      rowMeta.addValueMeta(vm);
    }

    return rowMeta;
  }

  /**
   * Replaces the call by the source expression of the function, in which the arguments are
   * substituted by the operands of the call.
   *
   * @throws ExpressionException with {@link ErrorCode#UDF_COMPILATION_ERROR} if the source of the
   *     function is invalid or not applicable to the operands
   */
  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {
    try {
      // Create a context for arguments
      IRowExpressionContext ctx =
          new RowExpressionContext(
              new Variables(), createRowMetaFromArguments(meta.getArguments()));

      // Parse function source
      ExpressionLexer lexer = new ExpressionLexer(getSource());
      ExpressionParser parser = new ExpressionParser(lexer);
      IExpression expression = parser.parse();
      expression.validate(ctx);

      // Replace function arguments with real operands
      expression = expression.accept(new UserDefinedFunctionResolver(call.getOperands()));
      expression.validate(context);

      // Compile
      ExpressionCompiler compiler = new ExpressionCompiler(context);
      return compiler.compile(expression);
    } catch (Exception e) {
      throw new ExpressionException(ErrorCode.UDF_COMPILATION_ERROR, getName(), e.getMessage());
    }
  }

  /**
   * Returns the source expression of the function.
   *
   * @return the source expression
   */
  public String getSource() {
    return meta.getSource();
  }

  /**
   * Returns the arguments of the function.
   *
   * @return the arguments, in declaration order
   */
  public List<FunctionArgument> getArguments() {
    return meta.getArguments();
  }
}
