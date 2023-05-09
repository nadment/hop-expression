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
import org.apache.hop.expression.type.Converter;
import org.apache.hop.expression.type.DataName;
import org.apache.hop.expression.type.DataType;
import org.apache.hop.expression.type.IOperandCountRange;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.util.NumberFormat;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collection;
import java.util.Objects;
import com.fasterxml.jackson.databind.JsonNode;

/**
 * An expression formed by a call to an {@link Operator} with zero or more expressions as operands.
 */
public final class Call implements IExpression {

  protected DataType type = DataType.UNKNOWN;
  protected final Operator operator;
  protected final IExpression[] operands;

  public Call(Operator operator, IExpression... operands) {
    this.operator = requireNonNull(operator, "operator");
    this.operands = requireNonNull(operands, "operands");
  }

  public Call(Operator operator, Collection<IExpression> operands) {
    this.operator = requireNonNull(operator, "operator");
    this.operands = requireNonNull(operands, "operands").toArray(new IExpression[0]);
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
  public DataType getType() {
    return type;
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
  public Object getValue(IExpressionContext context) throws ExpressionException {
    try {
      return operator.eval(context, operands);
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.OPERATOR_ERROR, operator.getName(),
          e.getMessage());
    }
  }

  @Override
  public <T> T getValue(IExpressionContext context, Class<T> clazz) throws ExpressionException {
    try {
      Object value = operator.eval(context, operands);
      
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
          if (clazz == Double.class) {
            return clazz.cast(((boolean) value) ? 1D : 0D);
          }
          if (clazz == BigDecimal.class) {
            return clazz.cast(((boolean) value) ? BigDecimal.ONE : BigDecimal.ZERO);
          }
          break;

        case STRING:
          if (clazz == Boolean.class) {
            return clazz.cast(Converter.parseBoolean((String) value));
          }
          if (clazz == Long.class) {
            return clazz.cast(Converter.parseInteger((String) value));
          }
          if (clazz == Double.class) {
            return clazz.cast(Converter.parseNumber((String) value));
          }
          if (clazz == BigDecimal.class) {
            return clazz.cast(Converter.parseBigNumber((String) value));
          }
          if (clazz == byte[].class) {
            return clazz.cast(((String) value).getBytes(StandardCharsets.UTF_8));
          }
          if (clazz == JsonNode.class) {
            return clazz.cast(Converter.parseJson((String) value));
          }
          break;

        case INTEGER:
          if (clazz == Boolean.class) {
            return clazz.cast(((Long) value) != 0);
          }
          if (clazz == Double.class) {
            return clazz.cast(Double.valueOf((Long) value));
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
            return clazz.cast(((Double) value) != 0);
          }
          if (clazz == Long.class) {
            return clazz.cast(((Double) value).longValue());
          }
          if (clazz == BigDecimal.class) {
            return clazz.cast(BigDecimal.valueOf((Double) value));
          }
          if (clazz == String.class) {
            return clazz.cast(String.valueOf(value));
          }
          break;

        case BIGNUMBER:
          if (clazz == Boolean.class) {
            return clazz.cast(((BigDecimal) value).unscaledValue() != BigInteger.ZERO);
          }
          if (clazz == Long.class) {
            return clazz.cast(((BigDecimal) value).longValue());
          }
          if (clazz == Double.class) {
            return clazz.cast(((BigDecimal) value).doubleValue());
          }
          if (clazz == String.class) {
            return clazz.cast(NumberFormat.of("TM").format((BigDecimal) value));
          }
          break;

        case BINARY:
          if (clazz == String.class) {
            return clazz.cast(new String((byte[]) value, StandardCharsets.UTF_8));
          }
          break;

        case JSON:
          if (clazz == String.class) {
            return clazz.cast(Converter.parseJson((String) value));
          }
          break;

        case ANY:
          // JSon function return type ANY
          if ( value instanceof String ) {
            if (clazz == Boolean.class) {
              return clazz.cast(Converter.parseBoolean((String) value));
            }
            if (clazz == Long.class) {
              return clazz.cast(Converter.parseInteger((String) value));
            }
            if (clazz == Double.class) {
              return clazz.cast(Converter.parseNumber((String) value));
            }
            if (clazz == BigDecimal.class) {
              return clazz.cast(Converter.parseBigNumber((String) value));
            }
            if (clazz == byte[].class) {
              return clazz.cast(((String) value).getBytes(StandardCharsets.UTF_8));
            }
            if (clazz == JsonNode.class) {
              return clazz.cast(Converter.parseJson((String) value));
            }
          }
          if ( value instanceof BigDecimal ) {
            if (clazz == Boolean.class) {
              return clazz.cast(((BigDecimal) value).unscaledValue() != BigInteger.ZERO);
            }
            if (clazz == Long.class) {
              return clazz.cast(((BigDecimal) value).longValue());
            }
            if (clazz == Double.class) {
              return clazz.cast(((BigDecimal) value).doubleValue());
            }
            if (clazz == String.class) {
              return clazz.cast(NumberFormat.of("TM").format((BigDecimal) value));
            }
          }
          if ( value instanceof Boolean ) {
            if (clazz == String.class) {
              return clazz.cast(String.valueOf(value));
            }
            if (clazz == Long.class) {
              return clazz.cast(((boolean) value) ? 1L : 0L);
            }
            if (clazz == Double.class) {
              return clazz.cast(((boolean) value) ? 1D : 0D);
            }
            if (clazz == BigDecimal.class) {
              return clazz.cast(((boolean) value) ? BigDecimal.ONE : BigDecimal.ZERO);
            }
          }
          
          break;
        case DATE:
        case TIMEUNIT:
        case UNKNOWN:
          break;
      }
      
      throw new ExpressionException(ExpressionError.UNSUPPORTED_COERCION, value,
          DataName.of(value), DataName.of(clazz));
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.OPERATOR_ERROR, operator.getName(),
          e.getMessage());
    }
  }

  /**
   * Validate a call.
   * 
   * @param context The context against which the expression will be validated.
   * @param call Call
   */
  public void validate(final IExpressionContext context) throws ExpressionException {

    // Validate all operands
    for (IExpression operand : operands) {
      // Some operand can be null
      if ( operand!=null) {
        operand.validate(context);
      }
    }

    // Check the number of operands expected
    IOperandCountRange operandCountRange = operator.getOperandCountRange();
    if (!operandCountRange.isValid(this.getOperandCount())) {
      if (getOperandCount() < operandCountRange.getMin()) {
        throw new ExpressionException(ExpressionError.NOT_ENOUGH_ARGUMENT.message(operator));
      }
      if (getOperandCount() > operandCountRange.getMax()) {
        throw new ExpressionException(ExpressionError.TOO_MANY_ARGUMENT.message(operator));
      }
    }

    // Check operand types expected
    IOperandTypeChecker operandTypeChecker = operator.getOperandTypeChecker();
    if (!operandTypeChecker.checkOperandTypes(this)) {
      throw new ExpressionException(ExpressionError.ILLEGAL_ARGUMENT_TYPE.message(operator));
    }
    
    inferenceType(context);
  }
  
  public IExpression compile(final IExpressionContext context) throws ExpressionException {
    
    // Compile all operands
    for (int i=0; i<operands.length;i++ ) {
      IExpression operand = operands[i];
      
      // Some operand can be null
      if ( operand!=null) {
        this.operands[i] = operand.compile(context);
      }
    }
    
    // If operator is deterministic and all operands are constant
    if (isConstant()) {
      try {
        Object value = getValue(context);

        inferenceType(context);

        // Some operator don't known return type like JSON_VALUE.
        if (DataName.ANY.equals(type.getName())) {
          type = DataType.of(value);
        }

        return new Literal(value, type);
      } catch (Exception e) {
        // Ignore and continue
      }
    }
    
    
    Call call = this;
    
    // If operator is symmetrical reorganize operands
    if (operator.isSymmetrical()) {
      call = reorganizeSymmetrical();
    }
    
    IExpression expression = call.getOperator().compile(context, call);
    
    // Inference return type
    if ( expression.is(Kind.CALL)) {
      call = (Call) expression;
      call.inferenceType(context);
    }
    
    return expression;
  }
  
  public void inferenceType(final IExpressionContext context) {
    this.type = this.operator.getReturnTypeInference().getReturnType(context, this);
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

    // Move low cost operand to the left
    if (left.getCost() > right.getCost()) {
      return new Call(operator, right, left);
    }

    // Order identifier by name
    if (left.is(Kind.IDENTIFIER) && right.is(Kind.IDENTIFIER)) {
      if (((Identifier) left).getName().compareTo(((Identifier) right).getName()) > 0) {
        return new Call(operator, right, left);
      }
    }

    if (right.is(operator)) {
      Call subCall = (Call) right;
      IExpression subLeft = subCall.getOperand(0);
      IExpression subRight = subCall.getOperand(1);

      // Go up left operand if low cost
      if (subLeft.getCost() < left.getCost()) {
        return new Call(operator, subLeft, new Call(operator, left, subRight));
      }

      // Order identifier by name
      if (left.is(Kind.IDENTIFIER) && subLeft.is(Kind.IDENTIFIER)) {
        if (((Identifier) left).getName().compareTo(((Identifier) subLeft).getName()) > 0) {
          return new Call(operator, subLeft, new Call(operator, left, subRight));
        }
      }
    }

    return this;
  }

  @Override
  public boolean isConstant() {
    // A call is constant if the operator is deterministic and all operands are constant
    if (operator.isDeterministic()) {
      for (IExpression operand : operands) {
        if (operand == null) {
          continue;
        }
        if (!operand.isConstant()) {
          return false;
        }
      }      
      return true;
    }
    return false;
  }
}
