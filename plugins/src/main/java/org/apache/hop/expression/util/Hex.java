/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package org.apache.hop.expression.util;

public class Hex {

  /**
   * Used to build output as hex.
   */
  private static final char[] DIGITS_LOWER =
      {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};
  /**
   * Used to build output as hex.
   */
  private static final char[] DIGITS_UPPER =
      {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};

  private Hex() {
    // Utility class
  }

  /**
   * Converts a hexadecimal character to an integer.
   *
   * @param ch A character to convert to an integer digit
   * @param index The index of the character in the source
   * @return An integer
   * @throws IllegalArgumentException Thrown if ch is an illegal hex character
   */
  protected static int toDigit(final char ch, final int index) {
    final int digit = Character.digit(ch, 16);
    if (digit == -1) {
      throw new IllegalArgumentException(
          "Illegal hexadecimal character " + ch + " at index " + index);
    }
    return digit;
  }


  /**
   * Converts a String representing hexadecimal values into an array of bytes of those same values.
   * The returned array will be half the length of the passed String, as it takes two characters to
   * represent any given
   * byte. An exception is thrown if the passed String has an odd number of elements.
   *
   * @param data A String containing hexadecimal digits
   * @return A byte array containing binary data decoded from the supplied char array.
   * @throws IllegalArgumentException Thrown if an odd number of characters or illegal characters
   *         are supplied
   */
  public static byte[] decode(final String data) {
    return decode(data.toCharArray());
  }


  /**
   * Converts an array of characters representing hexadecimal values into an array of bytes of those
   * same values. The
   * returned array will be half the length of the passed array, as it takes two characters to
   * represent any given
   * byte. An exception is thrown if the passed char array has an odd number of elements.
   *
   * @param data An array of characters containing hexadecimal digits
   * @throws IllegalArgumentException Thrown if an odd number of characters or illegal characters
   *         are supplied
   */
  public static byte[] decode(final char[] data) {
    final int length = data.length;

    if ((length & 0x01) != 0) {
      throw new IllegalArgumentException("Odd number of characters.");
    }

    final int outLen = length >> 1;
    final byte[] out = new byte[outLen];

    // two characters form the hex value.
    for (int i = 0, j = 0; j < length; i++) {
      int f = toDigit(data[j], j) << 4;
      j++;
      f = f | toDigit(data[j], j);
      j++;
      out[i] = (byte) (f & 0xFF);
    }

    return out;
  }

  /**
   * Converts an array of bytes into an array of characters representing the hexadecimal values of
   * each byte in order.
   * The returned array will be double the length of the passed array, as it takes two characters to
   * represent any
   * given byte.
   *
   * @param data a byte[] to convert to hex characters
   * @return A char[] containing lower-case hexadecimal characters
   */
  public static char[] encode(final byte[] data) {
    return encode(data, true);
  }

  /**
   * Converts an array of bytes into an array of characters representing the hexadecimal values of
   * each byte in order.
   * The returned array will be double the length of the passed array, as it takes two characters to
   * represent any
   * given byte.
   *
   * @param data a byte[] to convert to Hex characters
   * @param toLowerCase {@code true} converts to lowercase, {@code false} to uppercase
   * @return A char[] containing hexadecimal characters in the selected case
   * @since 1.4
   */
  public static char[] encode(final byte[] data, final boolean toLowerCase) {
    return encode(data, toLowerCase ? DIGITS_LOWER : DIGITS_UPPER);
  }

  /**
   * Converts an array of bytes into an array of characters representing the hexadecimal values of
   * each byte in order.
   *
   * @param data a byte[] to convert to hex characters
   * @param toDigits the output alphabet (must contain at least 16 chars)
   */
  private static char[] encode(final byte[] data, final char[] toDigits) {

    final int length = data.length;
    final char[] out = new char[length << 1];

    // two characters form the hex value.
    for (int i = 0, j = 0; i < length; i++) {
      out[j++] = toDigits[(0xF0 & data[i]) >>> 4];
      out[j++] = toDigits[0x0F & data[i]];
    }

    return out;
  }
}
