package org.apache.hop.expression;

import java.time.Instant;
import java.time.ZoneId;
import java.util.Locale;
import java.util.Random;

import javax.script.SimpleScriptContext;

import org.apache.hop.i18n.BaseMessages;

public class ExpressionContext extends SimpleScriptContext implements IExpressionContext {

  protected static final Class<?> PKG = Expression.class; // for i18n purposes

  private ZoneId zone;
  private Locale locale;
  private Random random;
  private Instant currentDate;

  public ExpressionContext() {
    super();

    this.locale = Locale.ROOT;
    this.zone = ZoneId.of("UTC");
    this.random = new Random();
    this.currentDate = Instant.now();
  }

  @Override
  public Value resolve(String name) throws ExpressionException {
    throw new ExpressionException(BaseMessages.getString(PKG, "Expression.OptimizerError", name));
  }

  public Locale getLocale() {
    return locale;
  }

  public void setLocale(Locale locale) {
    this.locale = locale;
  }

  public ZoneId getZone() {
    return zone;
  }

  public void setZone(ZoneId zone) {
    this.zone = zone;
  }

  public Random getRandom() {
    return random;
  }

  public Instant getCurrentDate() {
    return currentDate;
  }
}
