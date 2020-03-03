package org.apache.hop.ui.expression;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.apache.hop.core.Const;
import org.apache.hop.core.Props;
import org.apache.hop.core.row.RowMetaInterface;
import org.apache.hop.core.row.ValueMetaInterface;
import org.apache.hop.core.variables.VariableSpace;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.Operator;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.ui.core.FormDataBuilder;
import org.apache.hop.ui.core.PropsUI;
import org.apache.hop.ui.core.gui.GUIResource;
import org.apache.hop.ui.core.widget.StyledTextComp;
import org.eclipse.jface.bindings.keys.KeyStroke;
import org.eclipse.jface.fieldassist.ContentProposalAdapter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.custom.VerifyKeyListener;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceAdapter;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

public class ExpressionEditor extends SashForm {
	private static final Class<?> PKG = ExpressionEditor.class;

	private ExpressionLabelProvider labelProvider;
	private ExpressionProposalProvider contentProposalProvider;
	private VariableSpace variables;
	private RowMetaInterface rowMeta;
	private StyledTextComp txtEditor;
	private Tree tree;
	private TreeItem treeItemField;
	private TreeItem treeItemVariable;

	public ExpressionEditor(Composite parent, int style) {
		super(parent, style + SWT.HORIZONTAL);

		this.labelProvider = new ExpressionLabelProvider();

		this.createTree(this);
		this.createEditor(this);

		this.setWeights(new int[] { 25, 75 });
	}

	protected void createEditor(final Composite parent) {
		txtEditor = new StyledTextComp(variables, parent,
				SWT.MULTI | SWT.LEFT | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER, "", false);

		txtEditor.setLayoutData(new FormDataBuilder().top().fullWidth().bottom().result());
		txtEditor.addLineStyleListener(new ExpressionSyntaxHighlight());
		//txtEditor.addLineStyleListener(new LineNumber(txtEditor.getStyledText()));
		// wEditor.getStyledText().setMargins(30, 5, 3, 5);

		PropsUI.getInstance().setLook(txtEditor, Props.WIDGET_STYLE_FIXED);

		PropsUI.getInstance().setLook(this);

		// See PDI-1284 in chinese window, Ctrl-SPACE is reserved by system for input
		// chinese character. use Ctrl-ALT-SPACE instead.
		int modifierKeys = SWT.CTRL;
		if (System.getProperty("user.language").equals("zh")) {
			modifierKeys = SWT.CTRL | SWT.ALT;
		}
		KeyStroke keyStroke = KeyStroke.getInstance(modifierKeys, SWT.SPACE);

		contentProposalProvider = new ExpressionProposalProvider();
		// contentProposalProvider.init(variables);
		// contentProposalProvider.init(rowMeta);

		StyledText styledText = txtEditor.getStyledText();

		ContentProposalAdapter contentProposalAdapter = new ContentProposalAdapter(styledText,
				new StyledTextContentAdapter(), contentProposalProvider, keyStroke, new char[] { '(','$' });
		contentProposalAdapter.setProposalAcceptanceStyle(ContentProposalAdapter.PROPOSAL_INSERT);
		contentProposalAdapter.setLabelProvider(new ExpressionLabelProvider());
		contentProposalAdapter.setPropagateKeys(true);
		contentProposalAdapter.setAutoActivationDelay(10);
		contentProposalAdapter.setPopupSize(new Point(300, 200));

		// Avoid Enter key to be inserted when selected content proposal
		styledText.addVerifyKeyListener(new VerifyKeyListener() {
			public void verifyKey(VerifyEvent event) {
				if ('\r' == event.keyCode && contentProposalAdapter.isProposalPopupOpen()) {
					event.doit = false;
				}
			}
		});
	}

	protected void createTree(final Composite parent) {

		// Tree
		tree = new Tree(parent, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
		tree.setLayoutData(new FormDataBuilder().top().fullWidth().bottom().result());
		PropsUI.getInstance().setLook(tree);

		// Create the drag source on the tree
		DragSource ds = new DragSource(tree, DND.DROP_MOVE);
		ds.setTransfer(new Transfer[] { TextTransfer.getInstance() });
		ds.addDragListener(new DragSourceAdapter() {

			public void dragStart(DragSourceEvent event) {
				TreeItem item = tree.getSelection()[0];
				
				if (item != null && item.getData()!= null) {
					event.doit = true;
				}

				event.doit = false;
			}

			public void dragSetData(DragSourceEvent event) {
				// Set the data to be the first selected item's text
				event.data = tree.getSelection()[0].getText();
			}
		});

		Image imgFolder = GUIResource.getInstance().getImageBol();

		treeItemField = new TreeItem(tree, SWT.NULL);
		treeItemField.setImage(imgFolder);
		treeItemField.setText(BaseMessages.getString(PKG, "Expression.Fields.Label"));

		Map<String, TreeItem> items = new HashMap<>();

		TreeItem functionItem = new TreeItem(tree, SWT.NULL);
		functionItem.setImage(GUIResource.getInstance().getImageBol());
		functionItem.setText(BaseMessages.getString(PKG, "Expression.Functions.Label"));

		TreeItem item = new TreeItem(functionItem, SWT.NULL);
		item.setImage(imgFolder);
		item.setText(BaseMessages.getString(PKG, "Expression.Functions.Category.Bitwise.Label"));
		items.put("Bitwise", item);

		item = new TreeItem(functionItem, SWT.NULL);
		item.setImage(imgFolder);
		item.setText(BaseMessages.getString(PKG, "Expression.Functions.Category.Numeric.Label"));
		items.put("Numeric", item);

		item = new TreeItem(functionItem, SWT.NULL);
		item.setImage(imgFolder);
		item.setText(BaseMessages.getString(PKG, "Expression.Functions.Category.String.Label"));
		items.put("String", item);

		item = new TreeItem(functionItem, SWT.NULL);
		item.setImage(imgFolder);
		item.setText(BaseMessages.getString(PKG, "Expression.Functions.Category.Date.Label"));
		items.put("Date", item);

		item = new TreeItem(functionItem, SWT.NULL);
		item.setImage(imgFolder);
		item.setText(BaseMessages.getString(PKG, "Expression.Functions.Category.Other.Label"));
		items.put("Other", item);

		TreeItem operatorItem = new TreeItem(tree, SWT.NULL);
		operatorItem.setImage(GUIResource.getInstance().getImageBol());
		operatorItem.setText(BaseMessages.getString(PKG, "Expression.Operators.Label"));

		item = new TreeItem(operatorItem, SWT.NULL);
		item.setImage(GUIResource.getInstance().getImageBol());
		item.setText(BaseMessages.getString(PKG, "Expression.Operators.Category.Arithmetic.Label"));
		items.put("Arithmetic", item);

		item = new TreeItem(operatorItem, SWT.NULL);
		item.setImage(GUIResource.getInstance().getImageBol());
		item.setText(BaseMessages.getString(PKG, "Expression.Operators.Category.Comparison.Label"));
		items.put("Comparison", item);

		item = new TreeItem(operatorItem, SWT.NULL);
		item.setImage(GUIResource.getInstance().getImageBol());
		item.setText(BaseMessages.getString(PKG, "Expression.Operators.Category.Logical.Label"));
		items.put("Logical", item);

		item = new TreeItem(operatorItem, SWT.NULL);
		item.setImage(GUIResource.getInstance().getImageBol());
		item.setText(BaseMessages.getString(PKG, "Expression.Operators.Category.Special.Label"));
		items.put("Special", item);

		for (Function function : Function.getFunctions()) {
			TreeItem parentItem = items.get(function.getCategory());

			if (parentItem == null)
				item = new TreeItem(tree, SWT.NULL);
			else
				item = new TreeItem(parentItem, SWT.NULL);

			item.setImage(labelProvider.getImage(function));
			item.setText(function.getName());
			item.setData(function);
		}

		for (Operator operator : Operator.getOperators()) {

			TreeItem parentItem = items.get(operator.getCategory());

			if (parentItem == null)
				item = new TreeItem(tree, SWT.NULL);
			else
				item = new TreeItem(parentItem, SWT.NULL);

			item.setText(operator.getName());
			item.setData(operator);
		}

		treeItemVariable = new TreeItem(tree, SWT.NULL);
		treeItemVariable.setImage(imgFolder);
		treeItemVariable.setText(BaseMessages.getString(PKG, "Expression.Variables.Label"));

		// Tooltip for syntax and help
		TreeTooltipSupport toolTip = new TreeTooltipSupport(tree, SWT.NONE);
		toolTip.setLabelProvider(new ExpressionLabelProvider());
		toolTip.activate();
	}

	public void setText(String text) {
		txtEditor.setText(text);
	}

	public String getText() {
		return txtEditor.getText();
	}

	@Override
	public void dispose() {
		this.labelProvider.dispose();
		super.dispose();
	}

	public VariableSpace getVariables() {
		return variables;
	}

	public void setVariables(VariableSpace variables) {
		this.variables = variables;

		if (variables != null) {
			this.contentProposalProvider.setVariables(variables);

			String[] names = this.variables.listVariables();
			Arrays.sort(names);

			this.treeItemVariable.removeAll();
			for (String name : names) {
				boolean isDeprecated = Arrays.asList(Const.DEPRECATED_VARIABLES).contains(name);

				String content = "${" + name + '}';
				// String description = (isDeprecated) ? Const.getDeprecatedPrefix() : null;

				TreeItem item = new TreeItem(treeItemVariable, SWT.NULL);
				item.setImage(labelProvider.getImage(name));
				item.setText(name);
				item.setGrayed(isDeprecated);
				item.setData(content);
			}
		}
	}

	public RowMetaInterface getRowMeta() {
		return rowMeta;
	}

	public void setRowMeta(final RowMetaInterface rowMeta) {
		this.rowMeta = rowMeta;

		if (rowMeta != null) {
			this.contentProposalProvider.setRowMeta(rowMeta);

			Display.getDefault().asyncExec(() -> {
				treeItemField.removeAll();

				for (int i = 0; i < rowMeta.size(); i++) {
					ValueMetaInterface valueMeta = rowMeta.getValueMeta(i);

					TreeItem item = new TreeItem(treeItemField, SWT.NULL);
					item.setImage(labelProvider.getImage(valueMeta));
					item.setText(valueMeta.getName());
					item.setData(valueMeta);
				}
			});
		}
	}

}
