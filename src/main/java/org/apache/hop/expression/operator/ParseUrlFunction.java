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
package org.apache.hop.expression.operator;

import com.jayway.jsonpath.internal.Utils;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/** Returns the specified part of the URL or the value for the specified QUERY key. */
@FunctionPlugin
public class ParseUrlFunction extends Function {

  private static final Map<String, Pattern> cache = new ConcurrentHashMap<>();

  public ParseUrlFunction() {
    super(
        "PARSE_URL",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING_STRING.or(OperandTypes.STRING_STRING_STRING),
        OperatorCategory.STRING,
        "/docs/parse_url.html");
  }

  static Pattern keyToPattern(final String keyToExtract) {
    return Pattern.compile("(&|^)" + keyToExtract + "=([^&]*)");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String url = operands[0].getValue(String.class);
    if (url == null) return null;

    String part = operands[1].getValue(String.class);
    if (part == null) return null;

    if (operands.length == 3) {
      String key = operands[2].getValue(String.class);
      if (key == null) {
        return null;
      }

      return parseUrl(url, part, key);
    }

    return parseUrl(url, part);
  }

  public String parseUrl(String url, String partStr) {
    UrlPart part;

    URI uri;
    try {
      uri = new URI(url);
    } catch (URISyntaxException e) {
      return null;
    }

    try {
      part = UrlPart.valueOf(partStr);
    } catch (IllegalArgumentException e) {
      return null;
    }

    switch (part) {
      case HOST:
        return uri.getHost();
      case PORT:
        return (uri.getPort() < 0) ? null : String.valueOf(uri.getPort());
      case PATH:
        String path = uri.getRawPath();
        return (Utils.isEmpty(path)) ? null : path;
      case QUERY:
        return uri.getRawQuery();
      case REF:
        return uri.getRawFragment();
      case PROTOCOL:
        return uri.getScheme();
      case FILE:
        if (uri.getRawQuery() != null) {
          return uri.getRawPath() + "?" + uri.getRawQuery();
        } else {
          return uri.getRawPath();
        }
      case AUTHORITY:
        return uri.getRawAuthority();
      case USERINFO:
        return uri.getRawUserInfo();
      default:
        return null;
    }
  }

  public String parseUrl(String url, String part, String keyToExtract) {
    if (part.equals("QUERY")) {
      String query = parseUrl(url, part);
      if (query != null) {
        Pattern pattern = cache.computeIfAbsent(keyToExtract, ParseUrlFunction::keyToPattern);
        Matcher matcher = pattern.matcher(query);
        return matcher.find() ? matcher.group(2) : null;
      }
    }
    return null;
  }

  public enum UrlPart {
    HOST,
    PORT,
    PATH,
    QUERY,
    REF,
    PROTOCOL,
    FILE,
    AUTHORITY,
    USERINFO
  }
}
