package org.apache.hop.ui.expression;

import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.Operator;
import org.apache.hop.ui.core.gui.GuiResource;
import org.apache.hop.ui.util.ImageUtil;
import org.apache.hop.ui.util.SwtSvgImageUtil;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IToolTipProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;

public class ExpressionLabelProvider implements ILabelProvider, IToolTipProvider {

	private Image imgOperator;
	private Image imgBoolean;
	private Image imgString;
	private Image imgDate;
	private Image imgTimestamp;
	private Image imgNumber;
	private Image imgBinary;
	private Image imgInet;
	private Image imgFunction;
	private Image imgVariable;
	private Image imgVariableDeprecated;

	public ExpressionLabelProvider() {
		super();

		imgOperator = GuiResource.getInstance().getImageInject();
		imgString = SwtSvgImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "string.svg", 16, 16); //$NON-NLS-1$
		imgBoolean = SwtSvgImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "boolean.svg", 16, 16); //$NON-NLS-1$
		imgNumber = SwtSvgImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "number.svg", 16, 16); //$NON-NLS-1$
		imgDate = SwtSvgImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "date.svg", 16, 16); //$NON-NLS-1$
		imgTimestamp = SwtSvgImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "timestamp.svg", 16, 16); //$NON-NLS-1$
		imgBinary = SwtSvgImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "binary.svg", 16, 16); //$NON-NLS-1$
		imgInet = SwtSvgImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "inet.svg", 16, 16); //$NON-NLS-1$
		imgFunction = SwtSvgImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "function.svg", 16, 16); //$NON-NLS-1$
		imgVariable = ImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "variable.png"); //$NON-NLS-1$
		imgVariableDeprecated = ImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "variableDeprecated.png"); //$NON-NLS-1$
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
		if (imgTimestamp != null)
			imgTimestamp.dispose();
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

		if (element instanceof IValueMeta) {
			IValueMeta valueMeta = (IValueMeta) element;
			switch (valueMeta.getType()) {
			case IValueMeta.TYPE_BOOLEAN:
				return this.imgBoolean;
			case IValueMeta.TYPE_DATE:
				return this.imgDate;
			case IValueMeta.TYPE_TIMESTAMP:
				return this.imgTimestamp;				
			case IValueMeta.TYPE_INTEGER:
			case IValueMeta.TYPE_NUMBER:
			case IValueMeta.TYPE_BIGNUMBER:
				return this.imgNumber;
			case IValueMeta.TYPE_STRING:
				return this.imgString;
			case IValueMeta.TYPE_BINARY:
			case IValueMeta.TYPE_SERIALIZABLE:
				return this.imgBinary;
			case IValueMeta.TYPE_INET:
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
		return String.valueOf(element);
	}

	@Override
	public String getToolTipText(Object element) {
		if (element instanceof Operator) {
			Operator operator = (Operator) element;

			return Operator.getHtmlDocumentation(operator.getKind());
		}

		return null;
	}
}