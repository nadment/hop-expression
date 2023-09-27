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

import static java.util.Objects.requireNonNull;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.IOperandCountRange;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.JsonType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.type.UnknownType;
import org.apache.hop.expression.util.NumberFormat;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.Objects;
import com.fasterxml.jackson.databind.JsonNode;

/**
 * An expression formed by a call to an {@link Operator} with zero or more expressions as operands.
 */
public final class Call implements IExpression {

  protected Type type = UnknownType.UNKNOWN;
  // The position of this expression in the source before compilation else 0.  
  protected final int position;
  
  protected final Operator operator;
  protected final IExpression[] operands;

  public Call(Operator operator, IExpression... operands) {
    this(0, operator, operands);
  }

  public Call(Operator operator, Collection<IExpression> operands) {
    this(0, operator, operands);
  }
  
  public Call(int position, Operator operator, IExpression... operands) {
    this.operator = requireNonNull(operator, "operator");
    this.operands = requireNonNull(operands, "operands");
    this.position = position;
  }

  public Call(int position, Operator operator, Collection<IExpression> operands) {
    this.operator = requireNonNull(operator, "operator");
    this.operands = requireNonNull(operands, "operands").toArray(new IExpression[0]);
    this.position = position;
  }

  @Override
  public Kind getKind() {
    return Kind.CALL;
  }

  @Override
  public int getCost() {
    int cost = 3;
    for (IExpression operand : operands) {
      cost += operand.getCost();
    }
    return cost;
  }

  /**
   * Data type is unknown before validation.
   */
  @Override
  public Type getType() {
    return type;
  }

  /**
   * Returns the position of this expression in the source before compilation else 0.
   *
   * @return position
   */
  public int getPosition() {
    return position;
  }
  
  /**
   * Get the operator
   *
   * @return the operator
   */
  public Operator getOperator() {
    return operator;
  }

  /**
   * Get array of operands.
   * An empty array is returned if no operands
   *
   * @return the operands
   */
  public IExpression[] getOperands() {
    return operands;
  }

  public IExpression getOperand(int index) {
    return operands[index];
  }

  /**
   * Returns a count of operands of this expression.
   */
  public int getOperandCount() {
    return operands.length;
  }

  @Override
  public Object getValue() {
    try {
      return operator.eval(operands);
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.OPERATOR_ERROR, operator.getName(),
          e.getMessage());
    }
  }

  @Override
  public <T> T getValue(Class<T> clazz) {
    try {
      Object value = operator.eval(operands);

      if (clazz.isInstance(value)) {
        return clazz.cast(value);
      }

      if (value == null)
        return null;

      switch (type.getName()) {

        case BOOLEAN:
          if (clazz == String.class) {
            return clazz.cast(String.valueOf(value));
          }
          if (clazz == Long.class) {
            return clazz.cast(((boolean) value) ? 1L : 0L);
          }
          if (clazz == BigDecimal.class) {
            return clazz.cast(((boolean) value) ? BigDecimal.ONE : BigDecimal.ZERO);
          }
          break;

        case STRING:
          if (clazz == Boolean.class) {
            return clazz.cast(BooleanType.convert((String) value));
          }
          if (clazz == Long.class) {
            return clazz.cast(IntegerType.convert((String) value));
          }
          if (clazz == BigDecimal.class) {
            return clazz.cast(NumberType.convert((String) value));
          }
          if (clazz == byte[].class) {
            return clazz.cast(BinaryType.convert((String) value));
          }
          if (clazz == JsonNode.class) {
            return clazz.cast(JsonType.convert((String) value));
          }
          break;

        case INTEGER:
          if (clazz == Boolean.class) {
            return clazz.cast(((Long) value) != 0);
          }
          if (clazz == BigDecimal.class) {
            return clazz.cast(BigDecimal.valueOf((Long) value));
          }
          if (clazz == String.class) {
            return clazz.cast(String.valueOf(value));
          }
          break;

        case NUMBER:
          if (clazz == Boolean.class) {
            return clazz.cast(((BigDecimal) value).unscaledValue() != BigInteger.ZERO);
          }
          if (clazz == Long.class) {
            return clazz.cast(((BigDecimal) value).longValue());
          }
          if (clazz == String.class) {
            return clazz.cast(NumberFormat.of("TM").format((BigDecimal) value));
          }
          break;

        case BINARY:
          if (clazz == String.class) {
            return clazz.cast(StringType.convert((byte[]) value));
          }
          break;

        case JSON:
          if (clazz == String.class) {
            return clazz.cast(JsonType.convert((String) value));
          }
          break;

        case ANY:
          // JSon function return type ANY
          if (value instanceof String) {
            if (clazz == Boolean.class) {
              return clazz.cast(BooleanType.convert((String) value));
            }
            if (clazz == Long.class) {
              return clazz.cast(IntegerType.convert((String) value));
            }
            if (clazz == BigDecimal.class) {
              return clazz.cast(NumberType.convert((String) value));
            }
            if (clazz == byte[].class) {
              return clazz.cast(BinaryType.convert((String) value));
            }
            if (clazz == JsonNode.class) {
              return clazz.cast(JsonType.convert((String) value));
            }
          }
          if (value instanceof BigDecimal) {
            if (clazz == Boolean.class) {
              return clazz.cast(((BigDecimal) value).unscaledValue() != BigInteger.ZERO);
            }
            if (clazz == Long.class) {
              return clazz.cast(((BigDecimal) value).longValue());
            }
            if (clazz == String.class) {
              return clazz.cast(NumberFormat.of("TM").format((BigDecimal) value));
            }
          }
          if (value instanceof Boolean) {
            if (clazz == String.class) {
              return clazz.cast(String.valueOf(value));
            }
            if (clazz == Long.class) {
              return clazz.cast(((boolean) value) ? 1L : 0L);
            }
            if (clazz == BigDecimal.class) {
              return clazz.cast(((boolean) value) ? BigDecimal.ONE : BigDecimal.ZERO);
            }
          }

          break;
        case DATE:
        case SYMBOL:
        case UNKNOWN:
        default:
          break;
      }

      throw new ExpressionException(ExpressionError.UNSUPPORTED_COERCION, value,
          TypeName.from(value), TypeName.from(clazz));
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.OPERATOR_ERROR, operator.getName(),
          e.getMessage());
    }
  }

  /**
   * Validate a call.
   * 
   * @param context The context against which the expression will be validated.
   */
  @Override
  public void validate(final IExpressionContext context) throws ExpressionException {

    // Validate all operands
    for (IExpression operand : operands) {
      operand.validate(context);
    }

    // Check the number of operands expected
    IOperandCountRange operandCountRange = operator.getOperandCountRange();
    if (!operandCountRange.isValid(this.getOperandCount())) {
      if (getOperandCount() < operandCountRange.getMin()) {
        throw new ExpressionException(position, ExpressionError.NOT_ENOUGH_ARGUMENT, operator);
      }
      if (getOperandCount() > operandCountRange.getMax()) {
        throw new ExpressionException(position, ExpressionError.TOO_MANY_ARGUMENT, operator);
      }
    }

    // Check operand types expected
    IOperandTypeChecker operandTypeChecker = operator.getOperandTypeChecker();
    if (!operandTypeChecker.checkOperandTypes(this)) {
      throw new ExpressionException(position, ExpressionError.ILLEGAL_ARGUMENT_TYPE, operator);
    }

    // Inference type
    this.type = operator.getReturnTypeInference().getReturnType(this);
  }
  
  /**
   * Compile and optimize the call in the context
   * 
   * @param context The context against which the expression will be compiled. 
   * @return the compiled expression
   */
  public IExpression compile(final IExpressionContext context) throws ExpressionException {

    // Compile all operands
    for (int i = 0; i < operands.length; i++) {
      this.operands[i] = operands[i].compile(context);
    }

    if (isConstant())
      try {
        return evaluate();
      } catch (Exception e) {
        // Ignore error like division by zero "X IN (1,3/0)" and continue
      }

    IExpression expression = operator.compile(context, this);

    if (expression.is(Kind.CALL)) {
      Call call = expression.asCall();

      // If operator is symmetrical reorganize operands
      if (call.getOperator().isSymmetrical()) {
        call = call.reorganizeSymmetrical();
      }

      // Inference return type
      call.inferenceType();

      // If operator is deterministic and all operands are constant try to evaluate
      if (isConstant())
        try {
          return call.evaluate();
        } catch (Exception e) {
          // Ignore error like division by zero "X IN (1,3/0)" and continue
        }

      expression = call;
    }

    return expression;
  }

  public Call inferenceType() {
    this.type = this.operator.getReturnTypeInference().getReturnType(this);
    return this;
  }

  protected Literal evaluate() {

    Object value = getValue();
    inferenceType();

    // Some operator don't known return type like JSON_VALUE.
    if (TypeName.ANY.equals(type.getName())) {
      return Literal.of(value);
    }

    return new Literal(value, type);
  }

  @Override
  public <E> E accept(IExpressionContext context, IExpressionVisitor<E> visitor) {
    return visitor.apply(context, this);
  }

  @Override
  public boolean is(final Operator other) {
    return operator.is(other);
  }

  @Override
  public Call asCall() {
    return this;
  }

  @Override
  public boolean equals(Object other) {
    if (other instanceof Call) {
      Call call = (Call) other;
      return this.type.equals(call.type) && this.operator.equals(call.operator)
          && Arrays.equals(this.operands, call.operands);
    }
    return false;
  }

  @Override
  public int hashCode() {
    return Objects.hash(type, operator, operands);
  }

  @Override
  public void unparse(StringWriter writer) {
    operator.unparse(writer, operands);
  }

  @Override
  public String toString() {
    StringWriter writer = new StringWriter();
    unparse(writer);
    return writer.toString();
  }


  /**
   * Reorganize symmetrical operator
   * 
   * <ul>
   * <li>Move low cost operand to the left</li>
   * <li>Go up an operand if low cost</li>
   * <li>Order identifier by name (only useful for test)</li>
   * </ul>
   */
  protected Call reorganizeSymmetrical() {
    IExpression left = this.getOperand(0);
    IExpression right = this.getOperand(1);

    // Normalize, move low cost operand to the left
    if (left.getCost() > right.getCost()) {
      return new Call(operator, right, left);
    }

    // Normalize, order identifier by name
    if (left.is(Kind.IDENTIFIER) && right.is(Kind.IDENTIFIER)
        && left.asIdentifier().getName().compareTo(right.asIdentifier().getName()) > 0) {
      return new Call(operator, right, left);
    }

    if (right.is(operator)) {
      IExpression subLeft = right.asCall().getOperand(0);
      IExpression subRight = right.asCall().getOperand(1);

      // Go up left operand if low cost
      if (subLeft.getCost() < left.getCost()) {
        return new Call(operator, subLeft, new Call(operator, left, subRight));
      }

      // Order identifier by name
      if (left.is(Kind.IDENTIFIER) && subLeft.is(Kind.IDENTIFIER)
          && left.asIdentifier().getName().compareTo(subLeft.asIdentifier().getName()) > 0) {
        return new Call(operator, subLeft, new Call(operator, left, subRight));
      }
    }

    return this;
  }

  @Override
  public boolean isConstant() {
    // A call is constant if the operator is deterministic and all operands are constant
    if (operator.isDeterministic()) {
      for (IExpression operand : operands) {
        if (!operand.isConstant()) {
          return false;
        }
      }
      return true;
    }
    return false;
  }
}
