package org.apache.hop.expression.jsr223;

import java.io.IOException;
import java.io.Reader;

import javax.script.AbstractScriptEngine;
import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptException;

import org.apache.hop.expression.Expression;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.expression.Value;

public class ExpressionEngine extends AbstractScriptEngine implements ScriptEngine, Compilable {

  private volatile ExpressionEngineFactory factory;

  @Override
  public ScriptEngineFactory getFactory() {
    if (this.factory == null) {
      synchronized (this) {
        if (this.factory == null) {
          this.factory = new ExpressionEngineFactory();
        }
      }
    }

    return this.factory;
  }

  @Override
  public Bindings createBindings() {
    return new ExpressionBindings();
  }

  @Override
  public Object eval(String script, ScriptContext context) throws ScriptException {
    try {
      Expression expression = ExpressionParser.parse(script);
      Value result = expression.eval((ExpressionContext) context);

      return result;
    } catch (Exception e) {
      throw new ScriptException(e);
    }
  }

  @Override
  public Object eval(Reader reader, ScriptContext context) throws ScriptException {
    return this.eval(readFully(reader), context);
  }

  private static String readFully(Reader reader) throws ScriptException {
    char[] arr = new char[8192];
    StringBuilder buf = new StringBuilder();

    int numChars;
    try {
      while ((numChars = reader.read(arr, 0, arr.length)) > 0) {
        buf.append(arr, 0, numChars);
      }
    } catch (IOException var5) {
      throw new ScriptException(var5);
    }

    return buf.toString();
  }

  @Override
  public CompiledScript compile(String script) throws ScriptException {
    Expression expression = ExpressionParser.parse(script);
    return new ExpressionCompiled(this, expression);
  }

  @Override
  public CompiledScript compile(Reader reader) throws ScriptException {
    return this.compile(readFully(reader));
  }
}
