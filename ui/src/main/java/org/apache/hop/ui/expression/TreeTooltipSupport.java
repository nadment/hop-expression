package org.apache.hop.ui.expression;

import org.apache.hop.core.util.Utils;
import org.apache.hop.expression.Operator;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.window.DefaultToolTip;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

public class TreeTooltipSupport extends DefaultToolTip {

	private ILabelProvider labelProvider;

	public TreeTooltipSupport(Tree control, int style) {
		super(control, style, true);
	}

	/**
	 * Creates the content are of the the tooltip. By default this creates a CLabel
	 * to display text. To customize the text Subclasses may override the following
	 * methods
	 * <ul>
	 * <li>{@link #getStyle(Event)}</li>
	 * <li>{@link #getBackgroundColor(Event)}</li>
	 * <li>{@link #getForegroundColor(Event)}</li>
	 * <li>{@link #getFont(Event)}</li>
	 * <li>{@link #getImage(Event)}</li>
	 * <li>{@link #getText(Event)}</li>
	 * <li>{@link #getBackgroundImage(Event)}</li>
	 * </ul>
	 *
	 * @param event  the event that triggered the activation of the tooltip
	 * @param parent the parent of the content area
	 * @return the content area created
	 */
	@Override
	protected Composite createToolTipContentArea(Event event, Composite parent) {
		Image image = getImage(event);
		Image bgImage = getBackgroundImage(event);
		String text = getText(event);
		Color fgColor = getForegroundColor(event);
		Color bgColor = getBackgroundColor(event);
		Font font = getFont(event);

		Composite comp = new Composite(parent, SWT.NONE);
		GridLayoutFactory.fillDefaults().applyTo(comp);

		Composite topArea = new Composite(comp, SWT.NONE);
		if (bgColor != null)
			topArea.setBackground(bgColor);
		if (bgImage != null)
			topArea.setBackgroundImage(bgImage);
		GridDataFactory.fillDefaults().grab(true, true).applyTo(topArea);
		GridLayoutFactory.fillDefaults().spacing(1, 1).extendedMargins(2, 2, 2, 2).applyTo(topArea);

		Label l = null;
		if (image != null) {
			l = new Label(topArea, SWT.NONE);
			l.setImage(image);
			if (bgColor != null)
				l.setBackground(bgColor);
			if (fgColor != null)
				l.setForeground(fgColor);
		}

		if (text != null && !text.isEmpty()) {
			l = new Label(topArea, SWT.NONE);
			l.setText(text);
			if (font != null)
				l.setFont(font);
			if (bgColor != null)
				l.setBackground(bgColor);
			if (fgColor != null)
				l.setForeground(fgColor);
		}

		return comp;
	}

	@Override
	protected final String getText(Event event) {
		Tree tree = (Tree) event.widget;
		TreeItem item = tree.getItem(new Point(event.x, event.y));

		if (item == null)
			return "";

		if (item.getData() instanceof Operator) {
			Operator operator = (Operator) item.getData();

			StringBuilder s = new StringBuilder();
			s.append(operator.getDescription());
			s.append("\n\nSyntax:\t");
			s.append(operator.getSyntax());
			if (!Utils.isEmpty(operator.getConstraints())) {
				s.append("\n\nConstraints:\t");
				s.append(operator.getConstraints());
			}
			if (!Utils.isEmpty(operator.getReturns())) {
				s.append("\n\nReturns:\t");
				s.append(operator.getReturns());
			}
			return s.toString();
		}

		return null;
	}

	@Override
	protected final Image getImage(Event event) {
		Tree tree = (Tree) event.widget;
		TreeItem item = tree.getItem(new Point(event.x, event.y));
		return this.labelProvider.getImage(item);
	}

	@Override
	protected boolean shouldCreateToolTip(Event event) {
		return super.shouldCreateToolTip(event) && (getText(event) != null || getImage(event) != null);
	}

	public void setLabelProvider(ILabelProvider labelProvider) {
		this.labelProvider = labelProvider;
	}
}
