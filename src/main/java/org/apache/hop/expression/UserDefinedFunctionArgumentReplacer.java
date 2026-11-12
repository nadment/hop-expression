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

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import org.jspecify.annotations.NullMarked;

/**
 * Visitor that replaces, in the source expression of a user-defined function, each argument by the
 * operand of the call at the same position.
 *
 * <p>The source expression must have been validated against the arguments of the function, so that
 * each identifier knows its position.
 *
 * <p>A lambda parameter of the source with the name of an identifier of the operands is renamed, so
 * an operand placed in the lambda keeps its meaning. With the function {@code ADD_ALL(v0)} defined
 * as {@code TRANSFORM([1,2],x->x+v0)}, the call {@code ADD_ALL(x)} in a lambda {@code x->...} is
 * replaced by {@code TRANSFORM([1,2],x_1->x_1+x)}.
 */
@NullMarked
final class UserDefinedFunctionArgumentReplacer implements IExpressionVisitor<IExpression> {

  private final IExpression[] operands;

  /**
   * The names of the identifiers of the operands, a lambda parameter must not hide them. The name
   * of an identifier is not case-sensitive.
   */
  private final Set<String> operandNames = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);

  /** The names of the identifiers of the operands and of the source, not free for a renaming. */
  private final Set<String> usedNames = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);

  /** The renamed lambda parameters of the source, by identity. */
  private final Map<LambdaParameter, LambdaParameter> renamedParameters = new IdentityHashMap<>();

  /**
   * Creates a replacer for a call.
   *
   * @param source the source expression of the function
   * @param operands the operands of the call, in the order of the arguments of the function
   */
  private UserDefinedFunctionArgumentReplacer(IExpression source, IExpression[] operands) {
    this.operands = operands;
    for (IExpression operand : operands) {
      collectNames(operand, operandNames);
    }
    usedNames.addAll(operandNames);
    collectNames(source, usedNames);
  }

  /**
   * Replaces the arguments of the source expression of a function by the operands of a call.
   *
   * @param source the source expression of the function, validated against its arguments
   * @param operands the operands of the call, in the order of the arguments of the function
   * @return the expression that replaces the call
   */
  static IExpression replace(IExpression source, IExpression[] operands) {
    return source.accept(new UserDefinedFunctionArgumentReplacer(source, operands));
  }

  /** Collects the names of the fields and of the lambda parameters of an expression. */
  private static void collectNames(IExpression expression, Set<String> names) {
    if (expression instanceof Identifier identifier) {
      names.add(identifier.getName());
    } else if (expression instanceof Call call) {
      for (IExpression operand : call.getOperands()) {
        collectNames(operand, names);
      }
    } else if (expression instanceof Array array) {
      for (IExpression element : array) {
        collectNames(element, names);
      }
    } else if (expression instanceof Lambda lambda) {
      for (LambdaParameter parameter : lambda.getParameters()) {
        names.add(parameter.getName());
      }
      collectNames(lambda.getExpression(), names);
    }
  }

  /** Returns a name that is not used, the name followed by a number like {@code x_1}. */
  private String freeName(String name) {
    int index = 1;
    String freeName = name + '_' + index;
    while (!usedNames.add(freeName)) {
      index++;
      freeName = name + '_' + index;
    }
    return freeName;
  }

  @Override
  public IExpression visitIdentifier(final Identifier identifier) {
    // The parameter of a lambda is not an argument of the function
    if (identifier instanceof LambdaParameter parameter) {
      return renamedParameters.getOrDefault(parameter, parameter);
    }
    return operands[identifier.getOrdinal()];
  }

  @Override
  public IExpression visitCall(final Call call) {
    List<IExpression> expressions = new ArrayList<>(call.getOperandCount());
    for (IExpression expression : call.getOperands()) {
      expression = expression.accept(this);
      expressions.add(expression);
    }

    Call resolved = new Call(call.getOperator(), expressions);
    resolved.inferReturnType();

    return resolved;
  }

  @Override
  public IExpression visitArray(final Array array) {
    List<IExpression> expressions = new ArrayList<>(array.size());
    for (IExpression expression : array) {
      expression = expression.accept(this);
      expressions.add(expression);
    }
    return new Array(expressions);
  }

  @Override
  public IExpression visitLiteral(final Literal literal) {
    return literal;
  }

  /**
   * {@inheritDoc}
   *
   * <p>A parameter with the name of an identifier of the operands is renamed, as well as its
   * references in the expression of the lambda.
   */
  @Override
  public IExpression visitLambda(final Lambda lambda) {
    List<LambdaParameter> parameters = new ArrayList<>(lambda.getParameterCount());
    for (LambdaParameter parameter : lambda.getParameters()) {
      if (operandNames.contains(parameter.getName())) {
        LambdaParameter renamed =
            new LambdaParameter(parameter.getPosition(), freeName(parameter.getName()));
        renamedParameters.put(parameter, renamed);
        parameter = renamed;
      }
      parameters.add(parameter);
    }
    return new Lambda(lambda.getPosition(), parameters, lambda.getExpression().accept(this));
  }
}
