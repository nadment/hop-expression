package org.apache.hop.expression.util;

import org.apache.hop.expression.IExpression;
import org.apache.hop.i18n.BaseMessages;
import java.text.ParseException;
import java.time.Instant;
import java.util.Locale;

public class CompositeDateFormat implements IFormat<Instant> {
  protected static final Class<?> PKG = IExpression.class; // for i18n purposes
  
  private final String pattern;
  private final DateFormat[] formats;

  public CompositeDateFormat(String pattern, DateFormat[] formats) {
    this.pattern = pattern;
    this.formats = formats;
  }

  @Override
  public String format(Instant value, Locale locale) {
    return null; //formats[0].format(value, locale);
  }

  @Override
  public Instant parse(String text, Locale locale) throws ParseException {
//    for (DateTimeFormat format : formats) {
//      try {
//        return format.parse(text, locale);
//      } catch (Exception e) {
//        continue;
//      }
//    }
    
    throw new ParseException(BaseMessages.getString(PKG, "Expression.UnparsableDate", text, pattern), 0);
  }

}
