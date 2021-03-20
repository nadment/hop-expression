/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression.util;

public class ExpressionConst {
  
  private ExpressionConst() {
    // utility class
  }
    
  /** TODO: Control Two-digit year, when set to 1980, values of 79 and 80 parsed as 2079 and 1980 respectively. */  
  public static final String TWO_DIGIT_CENTURY_START = "TWO_DIGIT_CENTURY_START";
  
  public static final String WEEK_START = "WEEK_START";
}
