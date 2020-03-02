package org.apache.hop.ui.expression;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.apache.hop.core.Const;
import org.apache.hop.core.row.RowMetaInterface;
import org.apache.hop.core.row.ValueMetaInterface;
import org.apache.hop.core.variables.VariableSpace;
import org.apache.hop.expression.Function;
import org.apache.hop.ui.expression.ExpressionProposal.Type;
import org.eclipse.jface.fieldassist.IContentProposal;
import org.eclipse.jface.fieldassist.IContentProposalProvider;

import com.google.common.base.Strings;

public class ExpressionProposalProvider implements IContentProposalProvider {

	private static IContentProposal[] EMPTY_PROPOSAL = new IContentProposal[0];
	
	private Set<ExpressionProposal> elements = new HashSet<>();

	public ExpressionProposalProvider() {

		for (String keyword : ExpressionEditor.KEYWORDS) {
			elements.add(new ExpressionProposal(Type.Operator, keyword, keyword, null));
		}

		for (Function function : Function.getFunctions()) {
			elements.add(new ExpressionProposal(Type.Function, function.getName(), function.getSyntax(),
					function.getDescription()));
		}
	}

	public void init(final RowMetaInterface rowMeta) {
		for (int i = 0; i < rowMeta.size(); i++) {
			ValueMetaInterface valueMeta = rowMeta.getValueMeta(i);

			StringBuffer description = new StringBuffer();
			description.append("Type: ");
			description.append(valueMeta.getTypeDesc());
			description.append("\nStep origin: ");
			description.append(valueMeta.getOrigin());
			description.append("\nComment: ");
			description.append(Strings.nullToEmpty(valueMeta.getComments()));

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

			elements.add(
					new ExpressionProposal(type, valueMeta.getName(), valueMeta.getName(), description.toString()));
		}
	}

	public void init(final VariableSpace space) {

		for (String variable : space.listVariables()) {
			boolean isDeprecated = Arrays.asList(Const.DEPRECATED_VARIABLES).contains(variable);

			String content = "${" + variable + '}';
			String description = (isDeprecated) ? Const.getDeprecatedPrefix() : null;

			elements.add(new ExpressionProposal(Type.Variable, content, variable, description));
		}

	}

	@Override
	public IContentProposal[] getProposals(String contents, int position) {

		System.out.println(position+">"+contents);
		
		// Find significant characters entered by the user to restrict the number of proposals
		String qualifier = contents.substring(0, position);
		int start = position;
		
		while (start > 0) {
			char ch = contents.charAt(--start);
			if ( !Character.isAlphabetic(ch) )
//			if (Character.isWhitespace(ch) || ch == '(' || ch == ')' || ch == ',')
				break;
		}
		if (start > 0 && start < position) {
			qualifier = qualifier.substring(start + 1, position);
			start = position - start - 1;
		}
		if (qualifier.length() == 0) {
			return EMPTY_PROPOSAL;			
		}

		System.out.println("["+qualifier+"]");
		
//		int start = cursorPosition;
//		while ( start>0 && !Character.isWhitespace(contents.charAt(start-1)) && contents.charAt(start-1)!='(' ) start--;
//		int end = cursorPosition;
//		while ( end<contents.length() && !Character.isWhitespace(contents.charAt(end)) && contents.charAt(end)!=')' ) end++;

//		String word = contents.substring(0, cursorPosition);
//		int start = word.lastIndexOf(" ", cursorPosition);
//		if (start > 0) {
//			word = word.substring(start + 1, cursorPosition);
//		}

//		int end = cursorPosition;
//		while ( end<contents.length() && !Character.isWhitespace(contents.charAt(end)) && contents.charAt(end)!=')' ) end++;

		ArrayList<IContentProposal> list = new ArrayList<IContentProposal>();
		for (ExpressionProposal proposal : elements) {
			String content = proposal.getContent();

			// Only allow the contents that start with the qualifier
			if (content.length() >= qualifier.length()
					&& content.substring(0, qualifier.length()).equalsIgnoreCase(qualifier)) {
				ExpressionProposal p = new ExpressionProposal(proposal.getType(), content.substring(start),
						proposal.getLabel(), proposal.getDescription());
				
				System.out.println("\t>>>>"+proposal);
				list.add(p);
			}
		}
		
		System.out.println('\r');
		return list.toArray(new IContentProposal[list.size()]);
	}
}
