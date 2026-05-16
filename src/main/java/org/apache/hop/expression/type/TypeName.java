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
package org.apache.hop.expression.type;

import com.fasterxml.jackson.databind.JsonNode;
import java.math.BigDecimal;
import java.net.InetAddress;
import java.time.ZonedDateTime;
import java.util.Set;
import lombok.Getter;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Interval;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Enumeration of the data type identifier which can be used to construct an expression.
 *
 * <p>The order of enum declaration is important to be usable with <code>compareTo</code> method.
 *
 * <p>If values need to be converted to match the other operands data type, the value with the lower
 * order is converted to the value with the higher order.
 */
@NullMarked
public enum TypeName {

    /**
     * The null value. It has its own special type.
     */
    UNKNOWN(TypeFamily.UNKNOWN, false, false, -1, -1, -1, -1, Void.class),

    ANY(TypeFamily.ANY, false, false, -1, -1, -1, -1, Object.class),

    /**
     * Unlimited length text
     */
    STRING(TypeFamily.STRING, true, false, 16_777_216, 1, 0, 0, String.class),

    /**
     * Boolean (true or false)
     */
    BOOLEAN(TypeFamily.BOOLEAN, false, false, 1, 0, 0, 0, Boolean.class),

    /**
     * Signed integer (64-bit)
     */
    INTEGER(TypeFamily.NUMERIC, true, false, 19, 1, 0, 0, Long.class),

    /**
     * Unlimited precision number
     */
    NUMBER(TypeFamily.NUMERIC, true, true, 38, 1, 37, 0, BigDecimal.class),

    /**
     * A interval type TODO: add precision for nanoseconds
     */
    INTERVAL(TypeFamily.INTERVAL, false, false, -1, -1, -1, -1, Interval.class),

    /**
     * Date-time value with nanosecond precision and time zone TODO: add precision for nanoseconds
     */
    DATE(TypeFamily.TEMPORAL, false, false, -1, -1, -1, -1, ZonedDateTime.class),

    /**
     * A Json type
     */
    JSON(TypeFamily.JSON, false, false, -1, -1, -1, -1, JsonNode.class),

    /**
     * A INET type
     */
    INET(TypeFamily.NETWORK, false, false, -1, -1, -1, -1, InetAddress.class),

    /**
     * A binary type can be images, sounds, videos, and other types of binary data
     */
    BINARY(TypeFamily.BINARY, true, false, 16_777_216, 1, 0, 0, byte[].class),

    /**
     * An Array type
     */
    ARRAY(TypeFamily.ARRAY, false, false, -1, -1, 0, 0, Array.class),

    ENUM(TypeFamily.ENUM, false, false, -1, -1, -1, -1, Enum.class);

    public static final Set<String> ALL_NAMES =
            Set.of(
                    "Binary", "Boolean", "Date", "Integer", "Number", "Json", "String", "Interval", "Inet");

    /**
     * If the precision parameter is supported.
     */
    private final boolean supportsPrecision;

    /**
     * If the scale parameter is supported.
     */
    private final boolean supportsScale;

    /**
     * The minimum supported precision (or length) allowed for this type, or -1 if precision/length are
     * not applicable for this type.
     */
    @Getter
    private final int minPrecision;

    /**
     * The maximum supported precision (or length) allowed for this type, or -1 if precision/length are
     * not applicable for this type.
     */
    @Getter
    private final int maxPrecision;

    /**
     * The lowest possible scale.
     */
    @Getter
    private final int minScale;

    /**
     * The highest possible scale.
     */
    @Getter
    private final int maxScale;

    /**
     * The {@link TypeFamily} containing this {@link TypeName}.
     */
    @Getter
    private final TypeFamily family;

    @Getter
    private final Class<?> javaClass;

    TypeName(
            TypeFamily family,
            boolean supportsPrecision,
            boolean supportsScale,
            int maxPrecision,
            int minPrecision,
            int maxScale,
            int minScale,
            Class<?> javaClass) {
        this.family = family;
        this.supportsPrecision = supportsPrecision;
        this.supportsScale = supportsScale;
        this.maxPrecision = maxPrecision;
        this.minPrecision = minPrecision;
        this.maxScale = maxScale;
        this.minScale = minScale;
        this.javaClass = javaClass;
    }

    /**
     * Returns a {@link TypeName} with a given name (ignore case).
     *
     * @param name The name of the data name
     * @return data name, or null if not valid
     */
    public static @Nullable TypeName of(final @Nullable String name) {
        for (TypeName type : TypeName.values()) {
            if (type.name().equalsIgnoreCase(name)) {
                return type;
            }
        }
        return null;
    }

    /**
     * Search a data type identifier from a java class.
     *
     * @return The {@link TypeName} or 'UNKNOWN' if not found
     */
    public static TypeName fromClass(final @Nullable Class<?> clazz) {
        if (clazz == null) return UNKNOWN;

        for (TypeName id : values()) {

            // Ignore ANY
            if (id.equals(ANY)) continue;

            if (id.getJavaClass().isAssignableFrom(clazz)) {
                return id;
            }
        }
        return UNKNOWN;
    }

    /**
     * Search a data type identifier from a value.
     *
     * @return The type id or 'UNKNOWN' if not found
     */
    public static TypeName fromValue(final @Nullable Object value) {
        return switch (value) {
            case null -> UNKNOWN;
            case Integer i -> INTEGER;
            case Double v -> NUMBER;
            default -> fromClass(value.getClass());
        };
    }

    /**
     * Returns whether {@link TypeName} are in same type family. The ANY {@link TypeName} is in the
     * same family as any other {@link TypeName} type.
     */
    public boolean isFamily(TypeFamily other) {
        return this.family == TypeFamily.ANY || this.family == other;
    }

    /**
     * Returns whether this {@link TypeName} support explicit cast to the specified {@link TypeName}.
     */
    public boolean isCastable(final @Nullable TypeName name) {
        if (name == null) return false;
        if (name == this) return true;

        return switch (this) {
            case BOOLEAN -> name.is(INTEGER, NUMBER, BINARY, STRING);
            case STRING -> name.is(BOOLEAN, INTEGER, NUMBER, DATE, BINARY, JSON, INET);
            case DATE -> name.is(INTEGER, NUMBER, STRING);
            case INTEGER -> name.is(NUMBER, BOOLEAN, BINARY, STRING, DATE);
            case NUMBER -> name.is(INTEGER, BOOLEAN, BINARY, STRING, DATE);
            case BINARY, JSON, INET -> name.is(STRING);
            case UNKNOWN, ANY -> true;
            default -> false;
        };
    }

    /**
     * Returns whether this {@link TypeName} support implicit coercion to the specified {@link
     * TypeName}. Implicit coercion is generally only possible when the cast cannot fail.
     */
    public boolean isCoercible(final @Nullable TypeName name) {
        if (name == null) return false;
        if (ANY == this || name == ANY || this.equals(name)) return true;
        return switch (this) {
            case BOOLEAN -> name.is(INTEGER, NUMBER, STRING);
            case INTEGER -> name.is(NUMBER, BOOLEAN, STRING);
            // TODO: NUMBER to INTEGER can overflow, not sure it's a good choice to coerce
            case NUMBER -> name.is(INTEGER, BOOLEAN, STRING);
            case STRING -> name.is(BINARY);
            case DATE, BINARY, JSON, INTERVAL, INET -> name.is(STRING);
            case UNKNOWN -> true;
            default -> false;
        };
    }

    public boolean supportsPrecision() {
        return this.supportsPrecision;
    }

    public boolean supportsScale() {
        return this.supportsScale;
    }

    public int getDefaultScale() {
        return switch (this) {
            case NUMBER -> 9;
            case BOOLEAN -> 0;
            default -> -1;
        };
    }

    /**
     * Returns whether types are in the same type.
     */
    public boolean is(final TypeName... names) {
        for (TypeName name : names) {
            if (ANY == this || name == ANY || this.equals(name)) return true;
        }
        return false;
    }
}
