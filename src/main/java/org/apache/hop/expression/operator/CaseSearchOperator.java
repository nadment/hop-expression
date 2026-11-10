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

import static org.apache.hop.expression.type.Types.coerceOperandType;
import static org.apache.hop.expression.type.Types.getLeastRestrictive;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeFamily;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** A search case operator describing the <code>CASE</code> operator. */
@NullMarked
public class CaseSearchOperator extends Operator {

  public static final CaseSearchOperator INSTANCE = new CaseSearchOperator();

  public CaseSearchOperator() {
    super(
        "CASE",
        120,
        Associativity.LEFT,
        ReturnTypes.CASE_OPERATOR,
        null,
        OperatorCategory.CONDITIONAL,
        "/docs/hop-user-manual/expression/case.html");
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    int index = 0;
    Array whenTerm = (Array) operands[1];
    Array thenTerm = (Array) operands[2];
    IExpression elseTerm = operands[3];

    for (IExpression whenOperand : whenTerm) {
      Boolean predicate = whenOperand.getValue(Boolean.class);
      if (predicate != null && predicate) {
        return thenTerm.get(index).getValue();
      }
      index++;
    }

    return elseTerm.getValue();
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Array whenTerm = (Array) call.getOperand(1);
    Array thenTerm = (Array) call.getOperand(2);
    IExpression elseTerm = call.getOperand(3);

    // Flatten search case
    //
    // When a searched CASE expression nests another CASE expression in its ELSE clause, that can
    // be flattened into the top level CASE expression.
    if (elseTerm.isOperator(CaseSearchOperator.INSTANCE)) {
      List<IExpression> whenOperands = new ArrayList<>();
      whenTerm.forEach(whenOperands::add);
      array(call(elseTerm).getOperand(1)).forEach(whenOperands::add);

      List<IExpression> thenOperands = new ArrayList<>();
      thenTerm.forEach(thenOperands::add);
      array(call(elseTerm).getOperand(2)).forEach(thenOperands::add);

      return new Call(
          CaseSearchOperator.INSTANCE,
          Literal.NULL,
          new Array(whenOperands),
          new Array(thenOperands),
          call(elseTerm).getOperand(3));
    }

    // A nested CASE expression with one condition is already turned into IF, the operands being
    // compiled first:
    // CASE WHEN a THEN x ELSE IF(b,y,z) END → CASE WHEN a THEN x WHEN b THEN y ELSE z END
    if (elseTerm.isOperator(IfFunction.INSTANCE) && call(elseTerm).getOperandCount() == 3) {
      List<IExpression> whenOperands = new ArrayList<>();
      whenTerm.forEach(whenOperands::add);
      whenOperands.add(call(elseTerm).getOperand(0));

      List<IExpression> thenOperands = new ArrayList<>();
      thenTerm.forEach(thenOperands::add);
      thenOperands.add(call(elseTerm).getOperand(1));

      return new Call(
          CaseSearchOperator.INSTANCE,
          Literal.NULL,
          new Array(whenOperands),
          new Array(thenOperands),
          call(elseTerm).getOperand(2));
    }

    // Remove the clauses that never match, and the clauses after a clause that always matches
    List<IExpression> whenOperands = new ArrayList<>();
    List<IExpression> thenOperands = new ArrayList<>();
    IExpression elseOperand = elseTerm;
    boolean changed = false;
    for (int i = 0; i < whenTerm.size(); i++) {
      IExpression whenOperand = whenTerm.get(i);
      if (whenOperand instanceof Literal) {
        Boolean value = whenOperand.getValue(Boolean.class);
        // CASE WHEN a THEN x WHEN FALSE THEN y ELSE z END → CASE WHEN a THEN x ELSE z END
        // A NULL condition is not TRUE and never matches either
        if (value == null || !value) {
          changed = true;
          continue;
        }
        // CASE WHEN a THEN x WHEN TRUE THEN y WHEN b THEN z END → CASE WHEN a THEN x ELSE y END
        elseOperand = thenTerm.get(i);
        changed = true;
        break;
      }
      // CASE WHEN a THEN x WHEN a THEN y ELSE z END → CASE WHEN a THEN x ELSE z END
      // A condition already tested never matches, if it always evaluates to the same value
      if (whenOperands.stream().anyMatch(operand -> operand.isSameExpression(whenOperand))) {
        changed = true;
        continue;
      }
      whenOperands.add(whenOperand);
      thenOperands.add(thenTerm.get(i));
    }

    // CASE WHEN a THEN x WHEN b THEN y ELSE y END → CASE WHEN a THEN x ELSE y END
    while (!thenOperands.isEmpty() && thenOperands.getLast().equals(elseOperand)) {
      whenOperands.removeLast();
      thenOperands.removeLast();
      changed = true;
    }

    // The removed results don't change the data type, CASE WHEN TRUE THEN 1 ELSE 2.5 END is a
    // number
    Type type = ReturnTypes.CASE_OPERATOR.inferReturnType(call);
    if (whenOperands.isEmpty()) {
      return castResult(elseOperand, type);
    }

    if (changed) {
      return new Call(
          CaseSearchOperator.INSTANCE,
          Literal.NULL,
          new Array(whenOperands),
          new Array(thenOperands.stream().map(result -> castResult(result, type)).toList()),
          castResult(elseOperand, type));
    }

    // Search CASE expressions with one condition are turned into IF, which is then turned into
    // IFNULL, NULLIF or NVL2 if possible
    //
    if (whenTerm.size() == 1) {
      IExpression whenTerm0 = whenTerm.get(0);

      // CASE WHEN x THEN y ELSE z END → IF(x,y,z)
      return new Call(IfFunction.INSTANCE, whenTerm0, thenTerm.get(0), elseTerm);
    }

    return call;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append("CASE");

    Array whenTerms = (Array) operands[1];
    Array thenTerms = (Array) operands[2];

    int index = 0;
    for (IExpression whenOperand : whenTerms) {
      writer.append(" WHEN ");
      if (whenOperand instanceof Array array) {
        array.unparseValues(writer);
      } else whenOperand.unparse(writer, 0, 0);
      writer.append(" THEN ");
      IExpression thenOperand = thenTerms.get(index++);
      thenOperand.unparse(writer, 0, 0);
    }

    IExpression elseOperand = operands[3];
    if (!elseOperand.isNull()) {
      writer.append(" ELSE ");
      elseOperand.unparse(writer, 0, 0);
    }
    writer.append(" END");
  }

  @Override
  protected boolean checkOperandTypes(Call call) {

    // Search case should be a predicate, a number is coerced to a boolean
    for (IExpression operand : (Array) call.getOperand(1)) {
      if (operand.isNull()) continue;
      Type type = operand.getType();
      if (!(Types.isBoolean(type) || Types.isNumeric(type) || type.isFamily(TypeFamily.ANY))) {
        return false;
      }
    }

    return checkResultTypes(call);
  }

  /**
   * Checks that the THEN and ELSE operands of a CASE call are in the same type family, like the IF
   * and COALESCE functions. A NULL operand is ignored.
   *
   * @param call the call of a CASE operator
   * @return true if the result types are compatible
   */
  /**
   * Casts a result of a CASE call to the data type of the call if needed, when a simplification
   * removes the results that gave this data type.
   *
   * @param result the result expression kept by the simplification
   * @param type the data type of the CASE call before the simplification
   * @return the result expression, cast if its data type is lower
   */
  static IExpression castResult(IExpression result, Type type) {
    if (result.isNull() || !Types.needToCast(result, type)) {
      return result;
    }
    return Types.cast(result, type);
  }

  static boolean checkResultTypes(Call call) {
    Array thenTerm = (Array) call.getOperand(2);
    IExpression elseTerm = call.getOperand(3);

    // Determine common return type
    Type returnType = getLeastRestrictive(getLeastRestrictive(thenTerm), elseTerm.getType());
    if (returnType.isName(TypeName.UNKNOWN)) return false;

    @Nullable TypeFamily family = null;
    List<IExpression> results = new ArrayList<>();
    thenTerm.forEach(results::add);
    results.add(elseTerm);
    for (IExpression result : results) {
      if (result.isNull()) continue;
      // Ignore a value of any type, like ERROR(), it matches every family whatever its position
      if (result.getType().getFamily() == TypeFamily.ANY) continue;
      if (family == null) {
        family = result.getType().getFamily();
      } else if (!result.getType().isFamily(family)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Find common type for all the then operands and else operands, then try to coerce the then/else
   * operands to the type if needed.
   */
  @Override
  public boolean coerceOperandsType(Call call) {
    boolean coerced = false;

    // Coerce THEN and ELSE operands
    coerced |= coerceOperandType(call, call.getType(), 2);
    coerced |= coerceOperandType(call, call.getType(), 3);

    return coerced;
  }
}
