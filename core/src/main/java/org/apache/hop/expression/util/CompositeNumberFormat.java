package org.apache.hop.expression.util;

import org.apache.hop.expression.IExpression;
import org.apache.hop.i18n.BaseMessages;
import java.math.BigDecimal;
import java.text.ParseException;
import java.util.Locale;

public class CompositeNumberFormat implements IFormat<BigDecimal> {
  protected static final Class<?> PKG = IExpression.class; // for i18n purposes
  
  private final String pattern;
  private final NumberFormat[] formats;

  public CompositeNumberFormat(String pattern, NumberFormat[] formats) {
    this.pattern = pattern;
    this.formats = formats;
  }

  @Override
  public String format(BigDecimal value, Locale locale) {
    return formats[0].format(value, locale);
  }

  @Override
  public BigDecimal parse(String text, Locale locale) throws ParseException {
    for (IFormat<BigDecimal> format : formats) {
      try {
        return format.parse(text, locale);
      } catch (Exception e) {
        continue;
      }
    }
    
    throw new ParseException(BaseMessages.getString(PKG, "Expression.UnparsableNumber", text, pattern), 0);
  }

}
