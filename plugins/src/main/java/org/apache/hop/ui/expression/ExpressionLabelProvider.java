package org.apache.hop.ui.expression;

import java.util.List;

import org.apache.hop.core.exception.HopPluginException;
import org.apache.hop.core.plugins.IPlugin;
import org.apache.hop.core.plugins.PluginRegistry;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.value.ValueMetaPluginType;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.Operator;
import org.apache.hop.ui.util.ImageUtil;
import org.apache.hop.ui.util.SwtSvgImageUtil;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IToolTipProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;

public class ExpressionLabelProvider implements ILabelProvider, IToolTipProvider {

	private Image imgFunction;
	private Image imgVariable;
	private Image imgVariableDeprecated;
	private Image[] imgValueMeta;
	
	public ExpressionLabelProvider() {
		super();

		imgFunction = SwtSvgImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "function.svg", 16, 16); //$NON-NLS-1$
		imgVariable = ImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "variable.png"); //$NON-NLS-1$
		imgVariableDeprecated = ImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), "variableDeprecated.png"); //$NON-NLS-1$

		imgValueMeta = new Image[20];
		
	    PluginRegistry registry = PluginRegistry.getInstance();
	    List<IPlugin> plugins = registry.getPlugins( ValueMetaPluginType.class );
	    for ( IPlugin plugin : plugins ) {     
	    	 try {
				IValueMeta meta = (IValueMeta) registry.loadClass( plugin );
  
				String file = plugin.getImageFile();
				Image image = SwtSvgImageUtil.getImage(Display.getCurrent(), getClass().getClassLoader(), file, 16, 16);
				imgValueMeta[meta.getType()]= image;
			} catch (HopPluginException e) {
				// Ignore
			}
	    }
	}

	@Override
	public void addListener(ILabelProviderListener var1) {
	}

	@Override
	public void dispose() {
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

		if (element instanceof ExpressionProposal) {
			ExpressionProposal proposal = (ExpressionProposal) element;

			switch (proposal.getType()) {
			case Function:
			case Field:
				element = proposal.getData();
				break;
			case Variable:
				return imgVariable;
			case VariableDeprecated:
				return imgVariableDeprecated;
			default:
				break;
			}
		}	
		
		if (element instanceof Function) {
			return this.imgFunction;
		}
		
		if (element instanceof IValueMeta) {
			IValueMeta valueMeta = (IValueMeta) element;					
			return imgValueMeta[valueMeta.getType()];
		}

		return null;
	}

	@Override
	public String getText(Object element) {
		if (element instanceof Operator) {
			return ((Operator) element).getName();
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