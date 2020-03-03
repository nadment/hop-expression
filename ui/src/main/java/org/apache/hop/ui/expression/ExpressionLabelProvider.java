package org.apache.hop.ui.expression;

import org.apache.hop.core.row.ValueMetaInterface;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.Operator;
import org.apache.hop.ui.core.gui.GUIResource;
import org.apache.hop.ui.util.ImageUtil;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;

public class ExpressionLabelProvider implements ILabelProvider {

	private Image imgOperator;
	private Image imgBoolean;
	private Image imgString;
	private Image imgDate;
	private Image imgNumber;
	private Image imgBinary;
	private Image imgInet;
	private Image imgFunction;
	private Image imgVariable;
	private Image imgVariableDeprecated;

	public ExpressionLabelProvider() {
		super();

		imgOperator = GUIResource.getInstance().getImage("ui/images/inSmall.png"); //$NON-NLS-1$
		imgBoolean = ImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "Boolean.png"); //$NON-NLS-1$
		imgString = ImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "String.png"); //$NON-NLS-1$
		imgNumber = ImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "Number.png"); //$NON-NLS-1$
		imgDate = ImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "Date.png"); //$NON-NLS-1$
		imgBinary = ImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "Binary.png"); //$NON-NLS-1$
		imgInet = ImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "Inet.png"); //$NON-NLS-1$
		imgFunction = ImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "Function.png"); //$NON-NLS-1$
		imgVariable = ImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "Variable.png"); //$NON-NLS-1$
		imgVariableDeprecated = ImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "VariableDeprecated.png"); //$NON-NLS-1$
	}

	@Override
	public void addListener(ILabelProviderListener var1) {
	}

	@Override
	public void dispose() {
		if (imgBoolean != null)
			imgBoolean.dispose();
		if (imgString != null)
			imgString.dispose();
		if (imgNumber != null)
			imgNumber.dispose();
		if (imgDate != null)
			imgDate.dispose();
		if (imgBinary != null)
			imgBinary.dispose();
		if (imgInet != null)
			imgInet.dispose();
		if (imgFunction != null)
			imgFunction.dispose();
		if (imgVariable != null)
			imgVariable.dispose();
		if (imgVariableDeprecated != null)
			imgVariableDeprecated.dispose();
	}

	@Override
	public boolean isLabelProperty(Object var1, String var2) {
		return false;
	}

	@Override
	public void removeListener(ILabelProviderListener var1) {
	}

	@Override
	public Image getImage(Object element) {

		if (element instanceof String) {
			return this.imgVariable;
		}

		if (element instanceof Function) {
			return this.imgFunction;
		}

		if (element instanceof ValueMetaInterface) {
			ValueMetaInterface valueMeta = (ValueMetaInterface) element;
			switch (valueMeta.getType()) {
			case ValueMetaInterface.TYPE_BOOLEAN:
				return this.imgBoolean;
			case ValueMetaInterface.TYPE_DATE:
			case ValueMetaInterface.TYPE_TIMESTAMP:
				return this.imgDate;
			case ValueMetaInterface.TYPE_INTEGER:
			case ValueMetaInterface.TYPE_NUMBER:
			case ValueMetaInterface.TYPE_BIGNUMBER:
				return this.imgNumber;
			case ValueMetaInterface.TYPE_STRING:
				return this.imgString;
			case ValueMetaInterface.TYPE_BINARY:
			case ValueMetaInterface.TYPE_SERIALIZABLE:
				return this.imgBinary;
			case ValueMetaInterface.TYPE_INET:
				return this.imgInet;
			default:
				return this.imgOperator;
			}
		}

		if (element instanceof ExpressionProposal) {
			ExpressionProposal proposal = (ExpressionProposal) element;

			switch (proposal.getType()) {
			case Operator:
				return imgOperator;
			case Function:
				return imgFunction;
			case FieldBoolean:
				return imgBoolean;
			case FieldString:
				return imgString;
			case FieldNumber:
				return imgNumber;
			case FieldDate:
				return imgDate;
			case FieldBinary:
				return imgBinary;
			case Variable:
				return imgVariable;
			case VariableDeprecated:
				return imgVariableDeprecated;
			default:
				break;
			}
		}

		return null;
	}

	@Override
	public String getText(Object element) {

		if (element instanceof Operator) {
			return ((Function) element).getName();
		}

		if (element instanceof ExpressionProposal) {
			ExpressionProposal proposal = (ExpressionProposal) element;
			return proposal.getLabel();
		}
		return ""; //$NON-NLS-1$
	}
}