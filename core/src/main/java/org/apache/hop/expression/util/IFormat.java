package org.apache.hop.expression.util;

import java.text.ParseException;
import java.text.ParsePosition;
import java.util.Locale;

public interface IFormat<V> {
  
  public String format(V value, Locale locale);

  public V parse(String value, Locale locale) throws ParseException;
}
