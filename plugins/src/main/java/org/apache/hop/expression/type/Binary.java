package org.apache.hop.expression.type;

import java.io.Serializable;
import java.util.Arrays;


/**
 * An immutable sequence of bytes.
 */
public class Binary  implements Serializable, Comparable<Binary> {
  
  private static final long serialVersionUID = 1L;

  /** A singleton empty {@code ByteString}. */
  public static final Binary EMPTY = Binary.of();
  
  final byte[] bytes;
  transient int hashCode; // Lazily computed; 0 if unknown.

  /**
   * Returns a new byte string containing a clone of the bytes of {@code data}.
   */
  public static Binary of(byte... data) {
    if (data == null) throw new IllegalArgumentException("data == null");
    return new Binary(data.clone());
  }
  
  Binary(byte[] data) {
    this.bytes = data; // Trusted internal constructor doesn't clone data.
  }
  
  /**
   * Returns the number of bytes in this ByteString.
   */
  public int size() {
    return bytes.length;
  }

  /**
   * Returns true if the bytes of this in {@code [offset..offset+byteCount)} equal the bytes of
   * {@code other} in {@code [otherOffset..otherOffset+byteCount)}. Returns false if either range is
   * out of bounds.
   */
  public boolean rangeEquals(int offset, Binary other, int otherOffset, int byteCount) {
    return other.rangeEquals(otherOffset, this.bytes, offset, byteCount);
  }
  
  /**
   * Returns true if the bytes of this in {@code [offset..offset+byteCount)} equal the bytes of
   * {@code other} in {@code [otherOffset..otherOffset+byteCount)}. Returns false if either range is
   * out of bounds.
   */
  public boolean rangeEquals(int offset, byte[] other, int otherOffset, int byteCount) {
    return offset <= bytes.length - byteCount
        && otherOffset <= other.length - byteCount
        && arrayRangeEquals(bytes, offset, other, otherOffset, byteCount);
  }
  
  private static boolean arrayRangeEquals(
      byte[] a, int aOffset, byte[] b, int bOffset, int byteCount) {
    for (int i = 0; i < byteCount; i++) {
      if (a[i + aOffset] != b[i + bOffset]) return false;
    }
    return true;
  }
  
  @Override public boolean equals(Object o) {
    if (o == this) return true;
    return o instanceof Binary
        && ((Binary) o).size() == bytes.length
        && ((Binary) o).rangeEquals(0, bytes, 0, bytes.length);
  }

  @Override public int hashCode() {
    int result = hashCode;
    return result != 0 ? result : (hashCode = Arrays.hashCode(bytes));
  }

  @Override public int compareTo(Binary byteString) {
    int sizeA = size();
    int sizeB = byteString.size();
    for (int i = 0, size = Math.min(sizeA, sizeB); i < size; i++) {
      int byteA =  bytes[i] & 0xff;
      int byteB = byteString.bytes[i] & 0xff;
      if (byteA == byteB) continue;
      return byteA < byteB ? -1 : 1;
    }
    if (sizeA == sizeB) return 0;
    return sizeA < sizeB ? -1 : 1;
  }
  
  /**
   * Returns a byte string that is a substring of this byte string, beginning at the specified
   * index until the end of this string. Returns this byte string if {@code beginIndex} is 0.
   */
  public Binary substring(int beginIndex) {
    return substring(beginIndex, bytes.length);
  }

  /**
   * Returns a byte array containing a copy of the bytes in this {@code ByteString}.
   */
  public byte[] toByteArray() {
    return bytes.clone();
  }

  
  /**
   * Returns a byte string that is a substring of this byte string, beginning at the specified
   * {@code beginIndex} and ends at the specified {@code endIndex}. Returns this byte string if
   * {@code beginIndex} is 0 and {@code endIndex} is the length of this byte string.
   */
  public Binary substring(int beginIndex, int endIndex) {
    if (beginIndex < 0) throw new IllegalArgumentException("beginIndex < 0");
    if (endIndex > bytes.length) {
      throw new IllegalArgumentException("endIndex > length(" + bytes.length + ")");
    }

    int subLen = endIndex - beginIndex;
    if (subLen < 0) throw new IllegalArgumentException("endIndex < beginIndex");

    if ((beginIndex == 0) && (endIndex == bytes.length)) {
      return this;
    }

    byte[] copy = new byte[subLen];
    System.arraycopy(bytes, beginIndex, copy, 0, subLen);
    return new Binary(copy);
  }
  
  /**
   * Returns a byte string equal to this byte string, but with the bytes 'A'
   * through 'Z' replaced with the corresponding byte in 'a' through 'z'.
   * Returns this byte string if it contains no bytes in 'A' through 'Z'.
   */
  public Binary toAsciiLowercase() {
    // Search for an uppercase character. If we don't find one, return this.
    for (int i = 0; i < bytes.length; i++) {
      byte c = bytes[i];
      if (c < 'A' || c > 'Z') continue;

      // If we reach this point, this string is not not lowercase. Create and
      // return a new byte string.
      byte[] lowercase = bytes.clone();
      lowercase[i++] = (byte) (c - ('A' - 'a'));
      for (; i < lowercase.length; i++) {
        c = lowercase[i];
        if (c < 'A' || c > 'Z') continue;
        lowercase[i] = (byte) (c - ('A' - 'a'));
      }
      return new Binary(lowercase);
    }
    return this;
  }

  /**
   * Returns a byte string equal to this byte string, but with the bytes 'a'
   * through 'z' replaced with the corresponding byte in 'A' through 'Z'.
   * Returns this byte string if it contains no bytes in 'a' through 'z'.
   */
  public Binary toAsciiUppercase() {
    // Search for an lowercase character. If we don't find one, return this.
    for (int i = 0; i < bytes.length; i++) {
      byte c = bytes[i];
      if (c < 'a' || c > 'z') continue;

      // If we reach this point, this string is not not uppercase. Create and
      // return a new byte string.
      byte[] lowercase = bytes.clone();
      lowercase[i++] = (byte) (c - ('a' - 'A'));
      for (; i < lowercase.length; i++) {
        c = lowercase[i];
        if (c < 'a' || c > 'z') continue;
        lowercase[i] = (byte) (c - ('a' - 'A'));
      }
      return new Binary(lowercase);
    }
    return this;
  }

}
