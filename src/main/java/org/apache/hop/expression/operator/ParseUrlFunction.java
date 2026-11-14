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
import java.util.Locale;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.ParseExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Returns the specified part of the URL or the value for the specified QUERY key. */
@FunctionPlugin
@NullMarked
public class ParseUrlFunction extends Function {

  public ParseUrlFunction() {
    super(
        "PARSE_URL",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING_STRING.or(OperandTypes.STRING_STRING_STRING),
        OperatorCategory.STRING,
        "/docs/hop-user-manual/expression/parse_url.html");
  }

  /**
   * {@inheritDoc}
   *
   * <p>A constant part that is not a part of a URL is rejected when the expression is validated,
   * with the position of the call. A part that is not a constant returns NULL when a row is
   * evaluated.
   */
  @Override
  protected void validateOperands(IExpressionContext context, Call call)
      throws ExpressionException {
    super.validateOperands(context, call);
    if (call.getOperand(1) instanceof Literal literal
        && literal.getValue() instanceof String part) {
      try {
        UrlPart.valueOf(part.toUpperCase(Locale.ROOT));
      } catch (IllegalArgumentException e) {
        throw new ParseExpressionException(call.getPosition(), ErrorCode.INVALID_ARGUMENT, part);
      }
    }
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
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

  public @Nullable String parseUrl(String url, String partStr) {
    UrlPart part;

    URI uri;
    try {
      uri = new URI(url);
    } catch (URISyntaxException e) {
      return null;
    }

    try {
      // The part is not case-sensitive
      part = UrlPart.valueOf(partStr.toUpperCase(Locale.ROOT));
    } catch (IllegalArgumentException e) {
      return null;
    }

    // The authority of a host with an underscore, like my_host:8080, is not parsed by the URI
    String authority = uri.getRawAuthority();
    boolean unparsedAuthority = uri.getHost() == null && authority != null;

    switch (part) {
      case HOST:
        return unparsedAuthority ? parseAuthority(authority, part) : uri.getHost();
      case PORT:
        if (unparsedAuthority) {
          return parseAuthority(authority, part);
        }
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
        return authority;
      case USERINFO:
        return unparsedAuthority ? parseAuthority(authority, part) : uri.getRawUserInfo();
      default:
        return null;
    }
  }

  /**
   * Returns the user information, the host or the port of an authority {@code
   * [userinfo@]host[:port]} that the URI does not parse, like a host with an underscore.
   *
   * @param authority the authority of the URL
   * @param part the part to return, {@code USERINFO}, {@code HOST} or {@code PORT}
   * @return the part, or {@code null} if the authority does not have it or if its port is not a
   *     number
   */
  private static @Nullable String parseAuthority(String authority, UrlPart part) {
    int at = authority.lastIndexOf('@');
    if (part == UrlPart.USERINFO) {
      return (at < 0) ? null : authority.substring(0, at);
    }

    String hostAndPort = authority.substring(at + 1);
    int colon = hostAndPort.lastIndexOf(':');
    String host = (colon < 0) ? hostAndPort : hostAndPort.substring(0, colon);
    String port = (colon < 0) ? "" : hostAndPort.substring(colon + 1);
    if (host.isEmpty() || !port.chars().allMatch(ch -> ch >= '0' && ch <= '9')) {
      return null;
    }
    if (part == UrlPart.HOST) {
      return host;
    }
    return port.isEmpty() ? null : port;
  }

  public @Nullable String parseUrl(String url, String part, String keyToExtract) {
    if (part.equalsIgnoreCase("QUERY")) {
      String query = parseUrl(url, part);
      if (query != null) {
        return extractValue(query, keyToExtract);
      }
    }
    return null;
  }

  /**
   * Finds the value of the first {@code key=value} parameter of a query.
   *
   * @param query the query, with the parameters separated by {@code &}
   * @param key the key to find
   * @return the value, or {@code null} if the key is not found
   */
  private static @Nullable String extractValue(String query, String key) {
    int start = 0;
    while (start < query.length()) {
      int equal = start + key.length();
      if (equal < query.length() && query.charAt(equal) == '=' && query.startsWith(key, start)) {
        int end = query.indexOf('&', equal + 1);
        return query.substring(equal + 1, end < 0 ? query.length() : end);
      }
      int next = query.indexOf('&', start);
      if (next < 0) {
        break;
      }
      start = next + 1;
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
