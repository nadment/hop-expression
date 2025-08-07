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
package org.apache.hop.expression;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoField;
import java.time.temporal.IsoFields;
import java.util.Set;
import org.apache.commons.collections4.SetUtils;

/**
 * Enumeration of time units. <br>
 * A time unit can be used with functions such as {@code EXTRACT}, {@code FIRST_DAY}...
 */
public enum TimeUnit {
  /** The epoch. The number of seconds since 1970-01-01 00:00:00.00 */
  EPOCH {

    @Override
    public long extract(final LocalDate date) {
      return date.atStartOfDay().toEpochSecond(ZoneOffset.UTC);
    }

    @Override
    public long extract(final LocalDateTime datetime) {
      return datetime.toEpochSecond(ZoneOffset.UTC);
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.toEpochSecond();
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.toEpochSecond();
    }
  },

  /** The Gregorian millennium. The year 2000 is in the 2nd millennium, the year 2001 in the 3rd. */
  MILLENNIUM("MILLENNIUMS") {

    @Override
    public long extract(final LocalDate date) {
      return millennium(date.getYear());
    }

    @Override
    public long extract(final LocalDateTime datetime) {
      return millennium(datetime.getYear());
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return millennium(datetime.getYear());
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return millennium(datetime.getYear());
    }

    @Override
    public long extract(final Interval interval) {
      return interval.getSign() * interval.getYears() / 1000;
    }
  },

  /** The Gregorian century. The year 2000 is in the 20th century, the year 2001 in the 21st. */
  CENTURY {
    @Override
    public long extract(final LocalDate date) {
      return century(date.getYear());
    }

    @Override
    public long extract(final LocalDateTime datetime) {
      return century(datetime.getYear());
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return century(datetime.getYear());
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return century(datetime.getYear());
    }

    @Override
    public long extract(final Interval interval) {
      return interval.getSign() * interval.getYears() / 100;
    }
  },

  /** The Gregorian decade. The year divided by 10. */
  DECADE("DECADES") {
    @Override
    public long extract(final LocalDate date) {
      return decade(date.getYear());
    }

    @Override
    public long extract(final LocalDateTime datetime) {
      return decade(datetime.getYear());
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return decade(datetime.getYear());
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return decade(datetime.getYear());
    }

    @Override
    public long extract(final Interval interval) {
      return interval.getSign() * interval.getYears() / 10;
    }
  },

  /** The Gregorian years */
  YEAR("YEARS") {
    @Override
    public long extract(final LocalDate date) {
      return date.getYear();
    }

    @Override
    public long extract(final LocalDateTime datetime) {
      return datetime.getYear();
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.getYear();
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.getYear();
    }

    @Override
    public long extract(final Interval interval) {
      return interval.getSign() * interval.getYears();
    }
  },

  /** The years of week ISO. The ISO year starts at the first day (Monday) of week 01 */
  ISOYEAR {
    @Override
    public long extract(final LocalDate date) {
      return date.get(IsoFields.WEEK_BASED_YEAR);
    }

    @Override
    public long extract(final LocalDateTime datetime) {
      return datetime.get(IsoFields.WEEK_BASED_YEAR);
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.get(IsoFields.WEEK_BASED_YEAR);
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.get(IsoFields.WEEK_BASED_YEAR);
    }
  },

  /** The number (1 - 12) of the month */
  MONTH("MONTHS") {
    @Override
    public long extract(final LocalDate date) {
      return date.getMonthValue();
    }

    @Override
    public long extract(final LocalDateTime datetime) {
      return datetime.getMonthValue();
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.getMonthValue();
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.getMonthValue();
    }

    @Override
    public long extract(final Interval interval) {
      return interval.getSign() * interval.getMonths();
    }
  },

  /** The number (1 - 31) of the day */
  DAY("DAYS", "DAYOFMONTH") {
    @Override
    public long extract(final LocalDate date) {
      return date.getDayOfMonth();
    }

    @Override
    public long extract(final LocalDateTime datetime) {
      return datetime.getDayOfMonth();
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.getDayOfMonth();
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.getDayOfMonth();
    }

    @Override
    public long extract(final Interval interval) {
      return interval.getSign() * interval.getDays();
    }
  },

  /** A number (1 = Sunday, 2 = Monday, 7 = Saturday) indicating the day of the week */
  DAYOFWEEK {
    @Override
    public long extract(final LocalDate date) {
      return dow(date.getDayOfWeek().getValue());
    }

    @Override
    public long extract(final LocalDateTime datetime) {
      return dow(datetime.getDayOfWeek().getValue());
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return dow(datetime.getDayOfWeek().getValue());
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return dow(datetime.getDayOfWeek().getValue());
    }
  },

  /**
   * A number (1 = Monday, 7 = Sunday) indicating the day of the week following the ISO 8601
   * standard
   */
  ISODAYOFWEEK {
    @Override
    public long extract(final LocalDate date) {
      return date.getDayOfWeek().getValue();
    }

    @Override
    public long extract(final LocalDateTime datetime) {
      return datetime.getDayOfWeek().getValue();
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.getDayOfWeek().getValue();
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.getDayOfWeek().getValue();
    }
  },

  /** A number (1 - 366) indicating the day of the year */
  DAYOFYEAR {
    @Override
    public long extract(final LocalDate date) {
      return date.getDayOfYear();
    }

    @Override
    public long extract(final LocalDateTime datetime) {
      return datetime.getDayOfYear();
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.getDayOfYear();
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.getDayOfYear();
    }
  },

  /**
   * The number (1 - 54) of the week of the year. Weeks begin with Sunday, and dates prior to the
   * first Sunday of the year are in week 0.
   */
  WEEK("WEEKS", "WEEKOFYEAR") {
    @Override
    public long extract(final LocalDate date) {
      return date.get(ChronoField.ALIGNED_WEEK_OF_YEAR);
    }

    @Override
    public long extract(final LocalDateTime datetime) {
      return datetime.get(ChronoField.ALIGNED_WEEK_OF_YEAR);
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.get(ChronoField.ALIGNED_WEEK_OF_YEAR);
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.get(ChronoField.ALIGNED_WEEK_OF_YEAR);
    }
  },

  /**
   * The number (1 - 53) of the week of the year ISO 8601. The first week of the ISO year is the
   * week that contains January 4.
   */
  ISOWEEK("ISOWEEKOFYEAR") {
    @Override
    public long extract(final LocalDate date) {
      return date.get(IsoFields.WEEK_OF_WEEK_BASED_YEAR);
    }

    @Override
    public long extract(final LocalDateTime datetime) {
      return datetime.get(IsoFields.WEEK_OF_WEEK_BASED_YEAR);
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.get(IsoFields.WEEK_OF_WEEK_BASED_YEAR);
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.get(IsoFields.WEEK_OF_WEEK_BASED_YEAR);
    }
  },

  /** Week from the beginning of the month (0-5) */
  WEEKOFMONTH {
    @Override
    public long extract(final LocalDate date) {
      return date.get(ChronoField.ALIGNED_WEEK_OF_MONTH);
    }

    @Override
    public long extract(final LocalDateTime datetime) {
      return datetime.get(ChronoField.ALIGNED_WEEK_OF_MONTH);
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.get(ChronoField.ALIGNED_WEEK_OF_MONTH);
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.get(ChronoField.ALIGNED_WEEK_OF_MONTH);
    }
  },

  /** Quarter. Jan-Mar = 1, Apr-Jun = 2, Jul-Sep = 3, Oct-Dec = 4. */
  QUARTER("QUARTERS") {
    @Override
    public long extract(final LocalDate date) {
      return date.get(IsoFields.QUARTER_OF_YEAR);
    }

    @Override
    public long extract(final LocalDateTime datetime) {
      return datetime.get(IsoFields.QUARTER_OF_YEAR);
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.get(IsoFields.QUARTER_OF_YEAR);
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.get(IsoFields.QUARTER_OF_YEAR);
    }
  },

  /** Hour (0-23). */
  HOUR("HOURS") {
    @Override
    public long extract(final LocalDateTime datetime) {
      return datetime.getHour();
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.getHour();
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.getHour();
    }

    @Override
    public long extract(final Interval interval) {
      return interval.getSign() * interval.getHours();
    }
  },

  /** Minute (0-59). */
  MINUTE("MINUTES") {
    @Override
    public long extract(final LocalDateTime datetime) {
      return datetime.getMinute();
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.getMinute();
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.getMinute();
    }

    @Override
    public long extract(final Interval interval) {
      return interval.getSign() * interval.getMinutes();
    }
  },

  /** Second (0-59). */
  SECOND("SECONDS") {
    @Override
    public long extract(final LocalDateTime datetime) {
      return datetime.getSecond();
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.getSecond();
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.getSecond();
    }

    @Override
    public long extract(final Interval interval) {
      return interval.getSign() * interval.getSeconds();
    }
  },

  /** Millisecond. */
  MILLISECOND("MILLISECONDS") {
    @Override
    public long extract(final LocalDateTime datetime) {
      return datetime.get(ChronoField.MILLI_OF_SECOND);
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.get(ChronoField.MILLI_OF_SECOND);
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.get(ChronoField.MILLI_OF_SECOND);
    }

    @Override
    public long extract(final Interval interval) {
      return interval.getSign() * interval.getMilliseconds();
    }
  },

  /** Microsecond. */
  MICROSECOND("MICROSECONDS") {
    @Override
    public long extract(final LocalDateTime datetime) {
      return datetime.get(ChronoField.MICRO_OF_SECOND);
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.get(ChronoField.MICRO_OF_SECOND);
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.get(ChronoField.MICRO_OF_SECOND);
    }

    @Override
    public long extract(final Interval interval) {
      return interval.getSign() * interval.getMicroseconds();
    }
  },

  /** The nanosecond. */
  NANOSECOND("NANOSECONDS") {
    @Override
    public long extract(final LocalDateTime datetime) {
      return datetime.getNano();
    }

    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.getNano();
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.getNano();
    }

    @Override
    public long extract(final Interval interval) {
      return interval.getSign() * interval.getNanoseconds();
    }
  },

  /** Time zone offset's hour part. */
  TIMEZONE_HOUR {
    @Override
    public long extract(final OffsetDateTime datetime) {
      return datetime.getOffset().getTotalSeconds() / (60 * 60);
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return datetime.getOffset().getTotalSeconds() / (60 * 60);
    }
  },

  /** Time zone offset's minute part. */
  TIMEZONE_MINUTE {
    @Override
    public long extract(final OffsetDateTime datetime) {
      return (datetime.getOffset().getTotalSeconds() / 60) % 60;
    }

    @Override
    public long extract(final ZonedDateTime datetime) {
      return (datetime.getOffset().getTotalSeconds() / 60) % 60;
    }
  };

  private final String[] alias;
  private final Set<String> names;

  TimeUnit() {
    this.alias = new String[0];
    this.names = Set.of(name());
  }

  TimeUnit(final String... alias) {
    this.alias = alias;
    this.names = SetUtils.union(Set.of(name()), Set.of(alias));
  }

  /**
   * Returns a {@link TimeUnit} with a given name or alias (ignore case).
   *
   * @param name The name of the time unit
   * @return Time unit, or null if not valid
   */
  public static TimeUnit of(final String name) {
    for (TimeUnit unit : TimeUnit.values()) {
      if (unit.name().equalsIgnoreCase(name)) {
        return unit;
      }

      for (String alias : unit.alias) {
        if (alias.equalsIgnoreCase(name)) {
          return unit;
        }
      }
    }

    return null;
  }

  protected static int millennium(int year) {
    return year > 0 ? (year + 999) / 1000 : year / 1000;
  }

  protected static int century(int year) {
    return year > 0 ? (year + 99) / 100 : year / 100;
  }

  protected static int decade(int year) {
    return year >= 0 ? year / 10 : (year - 9) / 10;
  }

  protected static int dow(int dow) {
    dow++;
    if (dow == 8) dow = 1;
    return dow;
  }

  public long extract(LocalDate date) {
    throw new ExpressionException(ErrorCode.INVALID_ARGUMENT, this);
  }

  public long extract(LocalDateTime datetime) {
    throw new ExpressionException(ErrorCode.INVALID_ARGUMENT, this);
  }

  public long extract(OffsetDateTime datetime) {
    throw new ExpressionException(ErrorCode.INVALID_ARGUMENT, this);
  }

  public long extract(ZonedDateTime datetime) {
    throw new ExpressionException(ErrorCode.INVALID_ARGUMENT, this);
  }

  public long extract(Interval interval) {
    throw new ExpressionException(ErrorCode.INVALID_ARGUMENT, this);
  }

  /** Returns a list of name and alias. */
  public Set<String> names() {
    return names;
  }
}
