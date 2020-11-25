package org.apache.hop.expression.jsr223;

import javax.script.CompiledScript;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptException;

import org.apache.hop.expression.Expression;

public class ExpressionCompiled extends CompiledScript {

  private final ExpressionEngine engine;
  private final Expression expression;

  public ExpressionCompiled(ExpressionEngine engine, Expression expression) {
    this.engine = engine;
    this.expression = expression;
  }

  @Override
  public Object eval(ScriptContext context) throws ScriptException {
    return null;
  }

  @Override
  public ScriptEngine getEngine() {
    return engine;
  }
}
