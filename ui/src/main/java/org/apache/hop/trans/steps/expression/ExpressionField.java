/*******************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 ******************************************************************************/

package org.apache.hop.trans.steps.expression;

import org.apache.commons.lang.StringUtils;
import org.apache.hop.core.injection.Injection;
import org.apache.hop.core.row.value.ValueMetaFactory;

/**
 * Contains the properties of the fields to set with expression.
 *
 * @author Nicolas ADMENT
 */
public class ExpressionField implements Cloneable {

	public ExpressionField() {
		super();
	}

	/** The target field name */

	private String name;
	private String expression;
	private int type;

	@Injection(name = "LENGTH", group = "FIELDS")
	private int length = -1;
	@Injection(name = "PRECISION", group = "FIELDS")
	private int precision = -1;
	@Injection(name = "FORMAT", group = "FIELDS")
	private String format;

	@Override
	public Object clone() {
		ExpressionField clone;
		try {
			clone = (ExpressionField) super.clone();

		} catch (CloneNotSupportedException e) {
			return null;
		}
		return clone;
	}

	public String getName() {
		return name;
	}

	@Injection(name = "NAME", group = "FIELDS")
	public void setName(final String name) {
		this.name = StringUtils.stripToNull(name);
	}

	public String getExpression() {
		return expression;
	}

	@Injection(name = "EXPRESSION", group = "FIELDS")
	public void setExpression(final String expression) {
		this.expression = StringUtils.stripToNull(expression);
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	private String getTypeDesc() {
		return ValueMetaFactory.getValueMetaName(type);
	}

	@Injection(name = "TYPE", group = "FIELDS")
	public void setType(final String name) {
		this.type = ValueMetaFactory.getIdForValueMeta(name);
	}

	public int getLength() {
		return length;
	}

	public void setLength(int length) {
		this.length = length;
	}

	public String getFormat() {
		return format;
	}

	public void setFormat(String format) {
		this.format = format;
	}

	public int getPrecision() {
		return precision;
	}

	public void setPrecision(int precision) {
		this.precision = precision;
	}

	@Override
	public String toString() {
		return name + ":" + getTypeDesc() + "(" + length + "," + precision + ")";
	}

}
