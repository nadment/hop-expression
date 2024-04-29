/*
 * ! ******************************************************************************
 *
 * Hop : The Hop Orchestration Platform
 *
 * http://www.project-hop.org
 *
 *******************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 ******************************************************************************/

package org.apache.hop.pipeline.transforms.clonerowexpression;

import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IRowExpressionContext;
import org.apache.hop.pipeline.transform.BaseTransformData;
import org.apache.hop.pipeline.transform.ITransformData;

public class CloneRowData extends BaseTransformData implements ITransformData {

  protected IRowExpressionContext context;
  protected IExpression numberOfClones;
  protected IRowMeta outputRowMeta;
  protected boolean addInfosToRow;
  protected int NrPrevFields;

  public CloneRowData() {
    super();
    addInfosToRow = false;
    NrPrevFields = 0;
  }
}
