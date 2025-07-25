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

package org.apache.hop.workflow.actions.loop;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.apache.hop.base.AbstractMeta;
import org.apache.hop.core.CheckResult;
import org.apache.hop.core.Const;
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.Result;
import org.apache.hop.core.annotations.Action;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.file.IHasFilename;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionFactory;
import org.apache.hop.expression.IExpression;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.metadata.api.HopMetadataProperty;
import org.apache.hop.metadata.api.IHopMetadataProvider;
import org.apache.hop.resource.IResourceExport;
import org.apache.hop.resource.IResourceNaming;
import org.apache.hop.resource.ResourceDefinition;
import org.apache.hop.workflow.WorkflowMeta;
import org.apache.hop.workflow.action.ActionBase;
import org.apache.hop.workflow.action.IAction;
import org.apache.hop.workflow.engine.IWorkflowEngine;
import org.apache.hop.workflow.engine.WorkflowEngineFactory;

@Setter
@Getter
@Action(
    id = "LOOP",
    name = "i18n::LoopAction.Name",
    description = "i18n::LoopAction.Description",
    categoryDescription = "i18n:org.apache.hop.workflow:ActionCategory.Category.General",
    keywords = "i18n::LoopAction.Keywords",
    image = "loop.svg",
    documentationUrl = "/workflow/actions/loop.html")
public class LoopAction extends ActionBase implements IAction {
  static final Class<?> PKG = LoopAction.class;

  /** The workflow to execute */
  @HopMetadataProperty(key = "filename")
  private String filename;

  /** The run configuration to use to execute workflow */
  @HopMetadataProperty(key = "run_configuration")
  private String runConfigurationName;

  /** The condition expression to evaluate */
  @HopMetadataProperty(key = "condition")
  private String condition;

  /** The parameters */
  @HopMetadataProperty(groupKey = "parameters", key = "parameter")
  private List<Parameter> parameters;

  private static class ExecutionContext {
    public Result result;
    public IVariables variables;

    public ExecutionContext(Result result, IVariables variables) {
      this.result = result;
      this.variables = variables;
    }
  }

  public LoopAction(String name, String description) {
    super(name, description);
    parameters = new ArrayList<>();
  }

  public LoopAction() {
    this("", "");
  }

  @Override
  public Result execute(Result prevResult, int numberOfRows) throws HopException {

    // Load the workflow meta to execute
    //
    String realFilename = resolve(filename);
    if (StringUtils.isEmpty(realFilename)) {
      throw new HopException("Please specify a workflow to execute");
    }

    this.logBasic("Execute workflow '" + realFilename + "'");

    WorkflowMeta workflowMeta = loadWorkflow(realFilename, getMetadataProvider(), this);

    ExecutionContext execution = new ExecutionContext(null, getVariablesAndParameters());

    // If the condition is false at the beginning of the loop, don't execute at all!
    //
    boolean repeat = isConditionTrue(execution.variables);
    while (repeat && !parentWorkflow.isStopped()) {

      execution = executeWorkflow(workflowMeta, numberOfRows, execution);
      Result result = execution.result;
      if (!result.getResult() || result.getNrErrors() > 0 || result.isStopped()) {
        logError(
            "Workflow execution has encountered an error or has been stopped. This ends the loop.");

        // On error or a false result, stop the loop
        //
        prevResult.setResult(false);

        repeat = false;
      } else {
        // Loop while the condition is true
        //
        repeat = isConditionTrue(execution.variables);
      }
    }

    // Add last execution results
    //
    if (execution.result != null) {
      prevResult.add(execution.result);
    }

    return prevResult;
  }

  private ExecutionContext executeWorkflow(
      WorkflowMeta workflowMeta, int numberOfRows, ExecutionContext executionContext)
      throws HopException {

    IWorkflowEngine<WorkflowMeta> workflow =
        WorkflowEngineFactory.createWorkflowEngine(
            this, resolve(runConfigurationName), getMetadataProvider(), workflowMeta, this);
    workflow.setParentWorkflow(getParentWorkflow());
    workflow.setParentVariables(this);
    workflow.initializeFrom(executionContext.variables);

    // The internal variables need to be reset to be able to use them properly.
    workflow.getWorkflowMeta().setInternalHopVariables(workflow);

    // workflow.setVariables(getVariablesMap(workflow, previousResult));
    workflow.setLogLevel(getLogLevel());

    if (parentWorkflow.isInteractive()) {
      workflow.setInteractive(true);
      workflow.getActionListeners().addAll(parentWorkflow.getActionListeners());
    }

    // Link the workflow with the sub-workflow
    parentWorkflow.getWorkflowTracker().addWorkflowTracker(workflow.getWorkflowTracker());
    // Link both ways!
    workflow.getWorkflowTracker().setParentWorkflowTracker(parentWorkflow.getWorkflowTracker());

    Result result = workflow.startExecution();
    return new ExecutionContext(result, workflow);
  }

  public IVariables getVariablesAndParameters() {
    Variables variablesAndParameters = new Variables();
    variablesAndParameters.initializeFrom(getVariables());
    for (Parameter parameter : getParameters()) {
      variablesAndParameters.setVariable(parameter.getName(), parameter.getValue());
    }
    return variablesAndParameters;
  }

  private boolean isConditionTrue(IVariables variables) {
    String source = variables.resolve(condition);
    IExpression expression = ExpressionFactory.create(new ExpressionContext(variables), source);

    boolean result = expression.getValue(Boolean.class);

    this.logBasic("Evaluate loop condition '" + source + "' is " + result);

    return result;
  }

  @Override
  public String[] getReferencedObjectDescriptions() {
    String referenceDescription;
    if (StringUtils.isEmpty(filename)) {
      referenceDescription = "";
    } else {
      referenceDescription = "Workflow";
    }

    return new String[] {referenceDescription};
  }

  @Override
  public boolean[] isReferencedObjectEnabled() {
    return new boolean[] {StringUtils.isNotEmpty(filename)};
  }

  @Override
  public IHasFilename loadReferencedObject(
      int index, IHopMetadataProvider metadataProvider, IVariables variables) throws HopException {
    String realFilename = variables.resolve(filename);
    return loadWorkflow(realFilename, metadataProvider, variables);
  }

  @Override
  public String exportResources(
      IVariables variables,
      Map<String, ResourceDefinition> definitions,
      IResourceNaming namingInterface,
      IHopMetadataProvider metadataProvider)
      throws HopException {

    copyFrom(variables);
    String realFileName = resolve(filename);
    AbstractMeta meta;

    meta = loadWorkflow(realFileName, metadataProvider, this);

    String proposedNewFilename =
        ((IResourceExport) meta)
            .exportResources(variables, definitions, namingInterface, metadataProvider);
    String newFilename =
        "${" + Const.INTERNAL_VARIABLE_ENTRY_CURRENT_FOLDER + "}/" + proposedNewFilename;
    meta.setFilename(newFilename);
    filename = newFilename;
    return proposedNewFilename;
  }

  private WorkflowMeta loadWorkflow(
      String realFilename, IHopMetadataProvider metadataProvider, IVariables variables)
      throws HopException {
    return new WorkflowMeta(variables, realFilename, metadataProvider);
  }

  @Override
  public boolean isEvaluation() {
    return true;
  }

  @Override
  public boolean isUnconditional() {
    return false;
  }

  @Override
  public void check(
      List<ICheckResult> remarks,
      WorkflowMeta workflowMeta,
      IVariables variables,
      IHopMetadataProvider metadataProvider) {

    // Check variable definition
    ExpressionContext context = new ExpressionContext(variables);
    if (Utils.isEmpty(condition)) {
      remarks.add(
          new CheckResult(
              ICheckResult.TYPE_RESULT_ERROR,
              BaseMessages.getString(PKG, "LoopAction.CheckResult.LoopConditionIsMissing"),
              this));
    } else
      try {
        ExpressionFactory.create(context, variables.resolve(condition));
      } catch (Exception e) {
        remarks.add(
            new CheckResult(
                ICheckResult.TYPE_RESULT_ERROR,
                BaseMessages.getString(
                    PKG, "LoopAction.CheckResult.LoopConditionExpressionError", e.getMessage()),
                this));
      }
  }
}
