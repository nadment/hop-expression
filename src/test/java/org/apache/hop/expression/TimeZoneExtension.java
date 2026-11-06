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

import java.util.TimeZone;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtensionContext;

/**
 * Runs all the tests in the Europe/Paris time zone, ahead of UTC and with daylight saving time, so
 * that a date shifted by the time zone is detected. In UTC, such a shift is invisible.
 *
 * <p>The extension is registered for all the test classes by the service loader, see {@code
 * junit-platform.properties}. The time zone is set before each test, because the Hop parent POM
 * runs the tests in UTC and {@code RestoreHopEnvironmentExtension} sets UTC before all the tests of
 * a class.
 */
public class TimeZoneExtension implements BeforeEachCallback {

  /** The time zone of the tests. */
  public static final String TIME_ZONE = "Europe/Paris";

  @Override
  public void beforeEach(ExtensionContext context) {
    TimeZone.setDefault(TimeZone.getTimeZone(TIME_ZONE));
  }
}
