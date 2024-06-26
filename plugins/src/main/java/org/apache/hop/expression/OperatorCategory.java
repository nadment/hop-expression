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

/** A collection of operator categories. */
public final class OperatorCategory {
  /** Private constructor since this is a utility class. */
  private OperatorCategory() {}

  public static final String ARRAY = "i18n::Expression.Operator.Category.Array";
  public static final String STRING = "i18n::Expression.Operator.Category.String";
  public static final String JSON = "i18n::Expression.Operator.Category.Json";
  public static final String BITWISE = "i18n::Expression.Operator.Category.Bitwise";
  public static final String COMPARISON = "i18n::Expression.Operator.Category.Comparison";
  public static final String CONDITIONAL = "i18n::Expression.Operator.Category.Conditional";
  public static final String CONVERSION = "i18n::Expression.Operator.Category.Conversion";
  public static final String CRYPTOGRAPHIC = "i18n::Expression.Operator.Category.Cryptographic";
  public static final String DATE = "i18n::Expression.Operator.Category.Date";
  public static final String LOGICAL = "i18n::Expression.Operator.Category.Logical";
  public static final String MATHEMATICAL = "i18n::Expression.Operator.Category.Mathematical";
  public static final String TRIGONOMETRY = "i18n::Expression.Operator.Category.Trigonometry";
  public static final String SPECIAL = "i18n::Expression.Operator.Category.Special";
  public static final String AGGREGATION = "i18n::Expression.Operator.Category.Aggregation";
  public static final String UDF = "i18n::Expression.Operator.Category.Udf";
}
