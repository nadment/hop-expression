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
package org.apache.hop.workflow.actions.setvariable;

import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.metadata.api.IEnumHasCode;
import org.apache.hop.metadata.api.IEnumHasCodeAndDescription;

public enum SetVariableScope implements IEnumHasCodeAndDescription {

  /** Java Virual Machine */
  JVM(BaseMessages.getString(SetVariableAction.PKG, "SetVariableAction.VariableScope.JVM")),
  /** Set in current workflow */
  CURRENT_WORKFLOW(BaseMessages.getString(SetVariableAction.PKG,
      "SetVariableAction.VariableScope.CurrentWorkflow")),
  /** Set in parent workflow */
  PARENT_WORKFLOW(BaseMessages.getString(SetVariableAction.PKG,
      "SetVariableAction.VariableScope.ParentWorkflow")),
  /** Set in root workflow */
  ROOT_WORKFLOW(BaseMessages.getString(SetVariableAction.PKG,
      "SetVariableAction.VariableScope.RootWorkflow"));

  private final String description;

  SetVariableScope(String description) {
    this.description = description;
  }

  public static String[] getDescriptions() {
    return IEnumHasCodeAndDescription.getDescriptions(SetVariableScope.class);
  }

  public static SetVariableScope lookupDescription(final String description) {
    return IEnumHasCodeAndDescription.lookupDescription(SetVariableScope.class, description, JVM);
  }

  public static SetVariableScope lookupCode(final String code) {
    return IEnumHasCode.lookupCode(SetVariableScope.class, code, JVM);
  }

  @Override
  public String getCode() {
    return name();
  }

  @Override
  public String getDescription() {
    return description;
  }
}
