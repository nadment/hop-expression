/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.hop.expression;

import static org.junit.jupiter.api.Assertions.assertEquals;

import lombok.Getter;
import org.apache.hop.expression.type.Type;

public class Evaluator {
  // ANSI escape code constants for text colors
  protected static final String ANSI_RESET = "\u001B[0m";
  protected static final String ANSI_BLACK = "\u001B[30m";
  protected static final String ANSI_RED = "\u001B[31m";
  protected static final String ANSI_GREEN = "\u001B[32m";
  protected static final String ANSI_YELLOW = "\u001B[33m";
  protected static final String ANSI_BLUE = "\u001B[34m";
  protected static final String ANSI_PURPLE = "\u001B[35m";
  protected static final String ANSI_CYAN = "\u001B[36m";
  protected static final String ANSI_WHITE = "\u001B[37m";

  @Getter private final IExpression expression;
  @Getter private final String source;

  public Evaluator(IExpressionContext context, String source) {

    try {
      this.expression = ExpressionFactory.create(context, source);
      System.out.println(
          ANSI_WHITE
              + source
              + ANSI_YELLOW
              + " → "
              + ANSI_GREEN
              + expression
              + ANSI_YELLOW
              + " → "
              + ANSI_BLUE
              + expression.getType()
              + ANSI_PURPLE
              + " (cost "
              + expression.getCost()
              + ")"
              + ANSI_RESET);
    } catch (Exception ex) {
      System.out.println(
          ANSI_WHITE + source + ANSI_YELLOW + " → " + ANSI_RED + ex.getMessage() + ANSI_RESET);
      throw ex;
    }
    this.source = source;
  }

  public Object eval() throws Exception {
    try {
      return expression.getValue();
    } catch (Exception ex) {
      System.out.println(
          ANSI_WHITE + source + ANSI_YELLOW + " → " + ANSI_RED + ex.getMessage() + ANSI_RESET);
      throw ex;
    }
  }

  public <T> T eval(Class<T> clazz) throws Exception {
    try {
      return expression.getValue(clazz);
    } catch (Exception ex) {
      System.err.println(
          ANSI_WHITE + source + ANSI_YELLOW + " → " + ANSI_RED + ex.getMessage() + ANSI_RESET);
      throw ex;
    }
  }

  public void returnType(Type expectedType) {
    assertEquals(expectedType, expression.getType());
  }
}
