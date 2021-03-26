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
package org.apache.hop.ui.expression;

import java.util.Objects;

import org.apache.hop.expression.Operator;
import org.eclipse.jface.fieldassist.IContentProposal;

public class ExpressionProposal implements IContentProposal {

  enum Type {
    Field,
    Operator,
    Function,
    Variable
  }

  private Type type;
  private String content;
  private String label;
  private Object data;

  /**
   * Create a expression content proposal.
   *
   * @param type the type representing the content. Should not be <code>null</code>.
   * @param content the String representing the content. Should not be <code>null</code>.
   * @param label the String representing the label. Should not be <code>null</code>.
   * @param description the String representing the description, or <code>null</code> if there
   *     should be no description.
   */
  public ExpressionProposal(Type type, String content, String label, Object data) {
    this.type = type;
    this.content = Objects.requireNonNull(content);
    this.label = label;
    this.data = data;
  }

  @Override
  public String getContent() {
    return content;
  }

  @Override
  public int getCursorPosition() {
    return content.length();
  }

  public Object getData() {
    return this.data;
  }

  @Override
  public String getLabel() {
    return this.label;
  }

  @Override
  public String getDescription() {
    if (data instanceof String) return (String) data;
    if (data instanceof Operator) return ((Operator) data).getDescription();
    return null;
  }

  public Type getType() {
    return type;
  }

  @Override
  public String toString() {
    return type.name() + ':' + label + '(' + content + ')';
  }
}
