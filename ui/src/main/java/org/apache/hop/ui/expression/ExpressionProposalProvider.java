package org.apache.hop.ui.expression;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.apache.hop.core.Const;
import org.apache.hop.core.row.RowMetaInterface;
import org.apache.hop.core.row.ValueMetaInterface;
import org.apache.hop.core.variables.VariableSpace;
import org.apache.hop.expression.Function;
import org.apache.hop.ui.expression.ExpressionProposal.Type;
import org.eclipse.jface.fieldassist.IContentProposal;
import org.eclipse.jface.fieldassist.IContentProposalProvider;

public class ExpressionProposalProvider implements IContentProposalProvider {

	private Set<ExpressionProposal> elements = new HashSet<>();

	private VariableSpace space;

	private RowMetaInterface rowMeta;

	public ExpressionProposalProvider() {

	}

	public void setRowMeta(final RowMetaInterface rowMeta) {
		this.rowMeta = rowMeta;
	}

	public void setVariables(final VariableSpace space) {
		this.space = space;
	}

	@Override
	public IContentProposal[] getProposals(String contents, int position) {

		System.out.println(position + ":>" + contents);

		// Find significant characters entered by the user to restrict the number of
		// proposals
		String qualifier = contents.substring(0, position);

		int start = position;
		while (start > 0) {
			char ch = contents.charAt(start-1);
			if (!Character.isAlphabetic(ch) && ch!='.' )
				break;
			start--;
		}

		if ( start>0 && contents.charAt(start-1)=='{' )  start--;
		if ( start>0 && contents.charAt(start-1)=='$' )  start--;

		qualifier = qualifier.substring(start, position);

		int end = position;
		while (end < contents.length() && !Character.isWhitespace(contents.charAt(end)) && contents.charAt(end) != '}')
			end++;

		ArrayList<IContentProposal> list = new ArrayList<IContentProposal>();

		if ( qualifier.length()>0 &&  qualifier.charAt(0) == '$')
			this.buildVariableProposals(list, qualifier, position - start);
		//else if (start > 2 && contents.charAt(start - 1) == '{')
//			this.buildVariableProposals(list, qualifier, position - start - 1);

		else {
			this.buildFieldProposals(list, qualifier, position - start);
			this.buildFunctionProposals(list, qualifier, position - start);
		}

		System.out.println('\r');
		return list.toArray(new IContentProposal[list.size()]);
	}

	protected void buildVariableProposals(List<IContentProposal> list, String qualifier, int position) {

		System.out.println("list variables [" + qualifier + "] " + position);

		if (space != null) {
			
			String name = qualifier;
			if ( name.startsWith("${") ) name = name.substring(2); 
			else if ( name.startsWith("$") ) name = name.substring(1); 
			 
			
			for (String variable : space.listVariables()) {
				// Add to proposal if variable name start with the qualifier
				
				if (variable.length() >= name.length()
						&& variable.substring(0, name.length()).equalsIgnoreCase(name)) {
					boolean isDeprecated = Arrays.asList(Const.DEPRECATED_VARIABLES).contains(variable);

					String content = "${" + variable + '}';
					String description = (isDeprecated) ? Const.getDeprecatedPrefix() : null;
					Type type = (isDeprecated) ? Type.VariableDeprecated : Type.Variable;

					list.add(new ExpressionProposal(type, content.substring(position), variable, description));
				}
			}
		}
	}

	protected void buildFunctionProposals(List<IContentProposal> list, String qualifier, int position) {

		System.out.println("list functions [" + qualifier + "] " + position);

		for (Function function : Function.getFunctions()) {
			elements.add(new ExpressionProposal(Type.Function, function.getName(), function.getSyntax(),
					function.getDescription()));

			String name = function.getName();

			// Add to proposal if function name start with the qualifier
			if (name.length() >= qualifier.length()
					&& name.substring(0, qualifier.length()).equalsIgnoreCase(qualifier)) {

				list.add(new ExpressionProposal(Type.Function, name.substring(position), function.getSyntax(),
						function));
			}
		}
	}

	protected void buildFieldProposals(List<IContentProposal> list, String qualifier, int position) {

		System.out.println("list field [" + qualifier + "] " + position);

		if (rowMeta != null) {
			for (int i = 0; i < rowMeta.size(); i++) {
				ValueMetaInterface valueMeta = rowMeta.getValueMeta(i);

				String name = valueMeta.getName();

				// Add to proposal if field name start with the qualifier
				if (name.length() >= qualifier.length()
						&& name.substring(0, qualifier.length()).equalsIgnoreCase(qualifier)) {

					StringBuilder description = new StringBuilder();
					description.append("Type: ");
					description.append(valueMeta.getTypeDesc());
					description.append("\nStep origin: ");
					description.append(valueMeta.getOrigin());
					description.append("\nComment: ");
					description.append(StringUtils.defaultString(valueMeta.getComments()));

					ExpressionProposal.Type type = ExpressionProposal.Type.FieldString;
					switch (valueMeta.getType()) {
					case ValueMetaInterface.TYPE_BOOLEAN:
						type = ExpressionProposal.Type.FieldBoolean;
						break;
					case ValueMetaInterface.TYPE_BINARY:
						type = ExpressionProposal.Type.FieldBinary;
						break;
					case ValueMetaInterface.TYPE_DATE:
					case ValueMetaInterface.TYPE_TIMESTAMP:
						type = ExpressionProposal.Type.FieldDate;
						break;
					case ValueMetaInterface.TYPE_NUMBER:
					case ValueMetaInterface.TYPE_BIGNUMBER:
						type = ExpressionProposal.Type.FieldNumber;
						break;
					case ValueMetaInterface.TYPE_STRING:
						type = ExpressionProposal.Type.FieldString;
						break;
					default:
					}

					list.add(new ExpressionProposal(type, name.substring(position), name, description.toString()));
				}
			}
		}
	}
}
