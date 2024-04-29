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
package org.apache.hop.ui.expression;

import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.MultiLineRule;
import org.eclipse.jface.text.rules.RuleBasedPartitionScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;

public class ExpressionPartitionScanner extends RuleBasedPartitionScanner {
  public static final String COMMENT = "__exp_comment";
  public static final String STRING = "__exp_string";
  public static final String OTHER = "__exp_other";

  public ExpressionPartitionScanner() {
    IToken comment = new Token(COMMENT);
    IToken string = new Token(STRING);

    setPredicateRules(
        new IPredicateRule[] {
          // Add partition rule for single comment
          new EndOfLineRule("//", comment),
          // Add partition rule for single comment
          new EndOfLineRule("--", comment),
          // Add partition rule for comment
          new MultiLineRule("/*", "*/", comment),
          // Add partition rule for strings
          new SingleLineRule("'", "'", string, (char) 0, true)
        });
  }
}
