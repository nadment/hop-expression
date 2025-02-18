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

import java.util.ArrayList;
import java.util.List;
import lombok.Getter;
import lombok.Setter;
import org.apache.hop.core.CheckResult;
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.Result;
import org.apache.hop.core.annotations.Action;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.exception.HopWorkflowException;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionFactory;
import org.apache.hop.expression.IExpression;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.metadata.api.HopMetadataProperty;
import org.apache.hop.metadata.api.IHopMetadataProvider;
import org.apache.hop.workflow.WorkflowMeta;
import org.apache.hop.workflow.action.ActionBase;
import org.apache.hop.workflow.action.IAction;
import org.apache.hop.workflow.engine.IWorkflowEngine;

/** This defines a 'Set variables' action. */
@Setter
@Getter
@Action(
    id = "SET_VARIABLE",
    name = "i18n::SetVariableAction.Name",
    description = "i18n::SetVariableAction.Description",
    image = "set-variable.svg",
    categoryDescription = "i18n:org.apache.hop.workflow:ActionCategory.Category.Utility",
    keywords = "i18n::SetVariableAction.Keywords",
    documentationUrl = "/workflow/actions/setvariable.html")
public class SetVariableAction extends ActionBase implements IAction {
  static final Class<?> PKG = SetVariableAction.class;

  @HopMetadataProperty(groupKey = "variables", key = "variable")
  private List<SetVariableDefinition> variableDefinitions;

  public SetVariableAction(String name) {
    super(name, "");
    variableDefinitions = new ArrayList<>();
  }

  public SetVariableAction() {
    this("");
  }

  public SetVariableAction(SetVariableAction other) {
    super(other.getName(), other.getDescription(), other.getPluginId());
    this.variableDefinitions = other.getVariableDefinitions();
  }

  @Override
  public Result execute(Result result, int nr) throws HopException {
    result.setResult(true);
    result.setNrErrors(0);
    try {

      // if parentWorkflow exists - clear/reset all entrySetVariables before applying the actual
      // ones
      if (parentWorkflow != null) {
        for (String key : getEntryTransformSetVariablesMap().keySet()) {
          String parameterValue = parentWorkflow.getParameterValue(key);
          // if variable is not a namedParameter then it is a EntryTransformSetVariable - reset
          // value to ""
          if (parameterValue == null) {
            parentWorkflow.setVariable(key, "");
            setVariable(key, "");
          } else {
            // if it is a parameter, then get the initial saved value of parent - saved in
            // entryTransformSetVariables Map
            parentWorkflow.setVariable(key, getEntryTransformSetVariable(key));
            setVariable(key, getEntryTransformSetVariable(key));
          }
        }
      }

      for (SetVariableDefinition definition : variableDefinitions) {
        String name = definition.getName();

        try {
          IExpression expression =
              ExpressionFactory.create(
                  new ExpressionContext(this), resolve(definition.getExpression()));
          String value = expression.getValue(String.class);

          // OK, where do we set this value...
          switch (definition.getScope()) {
            case JVM:
              if (value != null) {
                System.setProperty(name, value);
              } else {
                System.clearProperty(name);
              }
              setVariable(name, value);
              IWorkflowEngine<WorkflowMeta> parentWorkflowTraverse = parentWorkflow;
              while (parentWorkflowTraverse != null) {
                parentWorkflowTraverse.setVariable(name, value);
                parentWorkflowTraverse = parentWorkflowTraverse.getParentWorkflow();
              }
              break;

            case ROOT_WORKFLOW:
              // set variable in this action
              setVariable(name, value);
              IWorkflowEngine<WorkflowMeta> rootWorkflow = parentWorkflow;
              while (rootWorkflow != null) {
                rootWorkflow.setVariable(name, value);
                rootWorkflow = rootWorkflow.getParentWorkflow();
              }
              break;

            case CURRENT_WORKFLOW:
              setVariable(name, value);

              if (parentWorkflow != null) {
                String parameterValue = parentWorkflow.getParameterValue(name);
                // if not a parameter, set the value
                if (parameterValue == null) {
                  setEntryTransformSetVariable(name, value);
                } else {
                  // if parameter, save the initial parameter value for use in reset/clear variables
                  // in future calls
                  if (!parameterValue.equals(value)
                      && !entryTransformSetVariablesMap.containsKey(name)) {
                    setEntryTransformSetVariable(name, parameterValue);
                  }
                }
                parentWorkflow.setVariable(name, value);

              } else {
                throw new HopWorkflowException(
                    BaseMessages.getString(
                        PKG, "SetVariableAction.Error.UnableSetVariableCurrentWorkflow", name));
              }
              break;

            case PARENT_WORKFLOW:
              setVariable(name, value);

              if (parentWorkflow != null) {
                parentWorkflow.setVariable(name, value);
                IWorkflowEngine<WorkflowMeta> gpWorkflow = parentWorkflow.getParentWorkflow();
                if (gpWorkflow != null) {
                  gpWorkflow.setVariable(name, value);
                } else {
                  throw new HopWorkflowException(
                      BaseMessages.getString(
                          PKG, "SetVariableAction.Error.UnableSetVariableParentWorkflow", name));
                }
              } else {
                throw new HopWorkflowException(
                    BaseMessages.getString(
                        PKG, "SetVariableAction.Error.UnableSetVariableCurrentWorkflow", name));
              }
              break;

            default:
              break;
          }

          // ok we can process this line
          if (isDetailed()) {
            logDetailed(
                BaseMessages.getString(
                    PKG, "SetVariableAction.Log.SetVariableToValue", name, value));
          }
        } catch (Exception e) {
          result.setResult(false);
          result.increaseErrors(1);
          logError(
              BaseMessages.getString(
                  PKG,
                  "SetVariableAction.Error.UnableSetVariableWithExpression",
                  name,
                  definition.getExpression(),
                  e.getMessage()));
        }
      }
    } catch (Exception e) {
      result.setResult(false);
      result.setNrErrors(1);
      logError(BaseMessages.getString(PKG, "SetVariableAction.UnexpectedError", e.getMessage()));
      return result;
    }

    return result;
  }

  @Override
  public boolean isEvaluation() {
    return true;
  }

  @Override
  public void check(
      List<ICheckResult> remarks,
      WorkflowMeta workflowMeta,
      IVariables variables,
      IHopMetadataProvider metadataProvider) {

    // Check variable definition
    ExpressionContext context = new ExpressionContext(variables);
    for (SetVariableDefinition definition : this.variableDefinitions) {
      if (Utils.isEmpty(definition.getName())) {
        remarks.add(
            new CheckResult(
                ICheckResult.TYPE_RESULT_ERROR,
                BaseMessages.getString(
                    PKG,
                    "SetVariableAction.CheckResult.VariableNameIsMissing",
                    definition.getExpression()),
                this));
      } else if (Utils.isEmpty(definition.getExpression())) {
        remarks.add(
            new CheckResult(
                ICheckResult.TYPE_RESULT_ERROR,
                BaseMessages.getString(
                    PKG, "SetVariableAction.CheckResult.ExpressionIsMissing", definition.getName()),
                this));
      } else
        try {
          ExpressionFactory.create(context, variables.resolve(definition.getExpression()));
        } catch (Exception e) {
          remarks.add(
              new CheckResult(
                  ICheckResult.TYPE_RESULT_ERROR,
                  BaseMessages.getString(
                      PKG,
                      "SetVariableAction.CheckResult.ExpressionError",
                      definition.getName(),
                      e.getMessage()),
                  this));
        }
    }
  }
}
