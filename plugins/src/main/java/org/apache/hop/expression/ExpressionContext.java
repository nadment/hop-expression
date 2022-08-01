/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.expression;

import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.Variable;
import org.apache.hop.core.variables.Variables;
import java.security.SecureRandom;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class ExpressionContext extends Variables implements IExpressionContext {

  protected static final Class<?> PKG = IExpression.class; // for i18n purposes
  
  /**
   * This parameter prevents ambiguous dates when importing or converting data with the YY date format.
   */
  @Variable(value = "1970", description = "Control Two-digit year format YY, when set to 1980, values of 79 and 80 parsed as 2079 and 1980 respectively")
  public static final String EXPRESSION_TWO_DIGIT_YEAR_START = "EXPRESSION_TWO_DIGIT_YEAR_START";

  /**
   * The date format used for conversions between dates and strings.
   */
  @Variable(value = "YYYY-MM-DD", description = "The default date format used by expression for conversions between dates and strings")
  public static final String EXPRESSION_DATE_FORMAT = "EXPRESSION_DATE_FORMAT";

  /**
   * The timestamp format used for conversions between timestamps and strings
   */
  @Variable(value = "YYYY-MM-DD H24:MI:SS", description = "The default timestamp format used by expression for conversions between timestamps and strings")
  public static final String EXPRESSION_TIMESTAMP_FORMAT = "EXPRESSION_TIMESTAMP_FORMAT";

  /**
   * Defines the first day of a week.
   * The value follows the ISO-8601 standard, from 1 (Monday) to 7 (Sunday).
   */
  public static final String EXPRESSION_FIRST_DAY_OF_WEEK = "EXPRESSION_FIRST_DAY_OF_WEEK";

  /**
   * The {@code Map} field stores the attributes.
   */
  private Map<String,Object> attributes;
  private IRowMeta rowMeta;
  private Object[] row;
  
  public ExpressionContext(IVariables variables, IRowMeta rowMeta) {
    this(variables);

    this.rowMeta = Objects.requireNonNull(rowMeta);
  }

  public ExpressionContext(IVariables variables) {
    super();
        
    // Initialize variables
    this.initializeFrom(Objects.requireNonNull(variables));
    
    // Initialize attributes
    this.attributes = new HashMap<>();
    
    // Set cached attributes
    ZonedDateTime now = ZonedDateTime.now();
    this.setAttribute(Attribute.CURRENT_TIMEZONE, ZoneId.systemDefault().getId());
    this.setAttribute(Attribute.CURRENT_TIMESTAMP, now);
    this.setAttribute(Attribute.CURRENT_DATE, now.truncatedTo(ChronoUnit.DAYS));
    this.setAttribute(Attribute.RANDOM, new SecureRandom());
  }

  protected void setAttribute(Attribute attribute, Object value) {
    attributes.put(attribute.id, value);
  }
  
  protected void setAttribute(String id, Object value) {
    attributes.put(id, value);
  }
      
  @Override
  public Object getAttribute(String id) {
    return attributes.get(id);
  }
  
  @Override
  public IRowMeta getRowMeta() {
    return rowMeta;
  }
  
  @Override
  public Object[] getRow() {
    return this.row;
  }
  
  @Override
  public void setRow(Object[] row) {
    this.row = row;
  }
}

