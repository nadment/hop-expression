package org.apache.hop.ui.expression;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.hop.core.Const;
import org.apache.hop.core.Props;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Scanner;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.ui.core.FormDataBuilder;
import org.apache.hop.ui.core.PropsUi;
import org.apache.hop.ui.core.gui.GuiResource;
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
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

public class ExpressionEditor extends SashForm {
	private static final Class<?> PKG = ExpressionEditor.class;

	private ExpressionLabelProvider labelProvider;
	private ExpressionProposalProvider contentProposalProvider;
	private IVariables variables;
	private IRowMeta rowMeta;
	private StyledTextComp textEditor;
	private Tree tree;
	private TreeItem treeItemField;
	private TreeItem treeItemVariable;
	private boolean isUseField;

	public ExpressionEditor(Composite parent, int style, boolean isUseField) {
		super(parent, style + SWT.HORIZONTAL);

		this.isUseField = isUseField;
		this.labelProvider = new ExpressionLabelProvider();

		this.createTree(this);
		this.createEditor(this);

		this.setWeights(new int[] { 25, 75 });
	}

	protected void createEditor(final Composite parent) {
		textEditor = new StyledTextComp(variables, parent,
				SWT.MULTI | SWT.LEFT | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER, "", false);

		textEditor.setLayoutData(new FormDataBuilder().top().fullWidth().bottom().result());
		textEditor.addLineStyleListener(new ExpressionSyntaxHighlighter());

		// txtEditor.addLineStyleListener(new LineNumber(txtEditor.getStyledText()));
		// wEditor.getStyledText().setMargins(30, 5, 3, 5);

		PropsUi.getInstance().setLook(textEditor, Props.WIDGET_STYLE_FIXED);
		PropsUi.getInstance().setLook(this);

		// See PDI-1284 in chinese window, Ctrl-SPACE is reserved by system for input
		// chinese character. use Ctrl-ALT-SPACE instead.
		int modifierKeys = SWT.CTRL;
		if (System.getProperty("user.language").equals("zh")) {
			modifierKeys = SWT.CTRL | SWT.ALT;
		}
		KeyStroke keyStroke = KeyStroke.getInstance(modifierKeys, SWT.SPACE);

		contentProposalProvider = new ExpressionProposalProvider();

		StyledText styledText = textEditor.getStyledText();

		ContentProposalAdapter contentProposalAdapter = new ContentProposalAdapter(styledText,
				new StyledTextContentAdapter(), contentProposalProvider, keyStroke, new char[] { '(', '$' });
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
		PropsUi.getInstance().setLook(tree);

		// Create the drag source on the tree
		DragSource ds = new DragSource(tree, DND.DROP_MOVE);
		ds.setTransfer(new Transfer[] { TextTransfer.getInstance() });
		ds.addDragListener(new DragSourceAdapter() {

			public void dragStart(DragSourceEvent event) {
				TreeItem item = tree.getSelection()[0];
				
				if (item != null && item.getData()!= null) {
					event.doit = true;
				}
				else event.doit = false;
			}

			public void dragSetData(DragSourceEvent event) {
				// Set the data to be the first selected item's text
				event.data = labelProvider.getText(tree.getSelection()[0].getData());
			}
		});

		if ( isUseField ) { 
		treeItemField = new TreeItem(tree, SWT.NULL);
		treeItemField.setImage(GuiResource.getInstance().getImageBol());
		treeItemField.setText(BaseMessages.getString(PKG, "Expression.Fields.Label"));
		}
		
		TreeItem treeItemOperator = new TreeItem(tree, SWT.NULL);
		treeItemOperator.setImage(GuiResource.getInstance().getImageBol());
		treeItemOperator.setText(BaseMessages.getString(PKG, "Expression.Operators.Label"));

		// Create operator category
		Map<String, TreeItem> items = new HashMap<>();
		for (OperatorCategory category: OperatorCategory.values() ) {
			
			if ( category==OperatorCategory.None ) continue;
			
			TreeItem item = new TreeItem(treeItemOperator, SWT.NULL);
			item.setImage(GuiResource.getInstance().getImageBol());
			item.setText(BaseMessages.getString(PKG, "Expression.Operators.Category."+category.name()+".Label"));
			items.put(category.name(), item);
		}

		
		List<Operator> primaryOperators = new ArrayList<>();
		HashMap<Kind,String> mapDisplay = new HashMap<>();
		
		// Primary operator
		for ( Operator o:Operator.getOperators()) {						
			if ( !o.isAlias() ) {
				primaryOperators.add(o);
				mapDisplay.put(o.getKind(), o.getName());
			}
		}

		// Alias operator
		for ( Operator o:Operator.getOperators()) {
			if ( o.isAlias() ) {				
				String alias = mapDisplay.get(o.getKind());	
				mapDisplay.replace(o.getKind(), String.join(", ", alias, o.getName()));
			}
		}
			
		// Create item operator
		for (Operator operator : primaryOperators) {

			TreeItem parentItem = items.get(operator.getKind().category());

			TreeItem item;		
			if (parentItem == null)
				item = new TreeItem(tree, SWT.NULL);
			else
				item = new TreeItem(parentItem, SWT.NULL);
			item.setImage(labelProvider.getImage(operator));			
			item.setText(mapDisplay.get(operator.getKind()));
			item.setData(operator);
		}

		treeItemVariable = new TreeItem(tree, SWT.NULL);
		treeItemVariable.setImage(GuiResource.getInstance().getImageBol());
		treeItemVariable.setText(BaseMessages.getString(PKG, "Expression.Variables.Label"));

		// Tooltip for syntax and help
		HtmlToolTip toolTip = new HtmlToolTip(tree, labelProvider);
		toolTip.activate();
	}

	public void setText(String text) {
		textEditor.setText(text);
	}

	public String getText() {
		return textEditor.getText();
	}

	@Override
	public void dispose() {
		this.labelProvider.dispose();
		super.dispose();
	}

	public IVariables getVariables() {
		return variables;
	}

	public void setVariables(IVariables variables) {
		this.variables = variables;

		if (variables != null) {
			this.contentProposalProvider.setVariables(variables);

			String[] names = this.variables.listVariables();
			Arrays.sort(names);

			this.treeItemVariable.removeAll();
			for (String name : names) {
				boolean isDeprecated = Arrays.asList(Const.DEPRECATED_VARIABLES).contains(name);

				String data = "${" + name + '}';

				TreeItem item = new TreeItem(treeItemVariable, SWT.NULL);
				item.setImage(labelProvider.getImage(name));
				item.setText(name);
				item.setGrayed(isDeprecated);
				item.setData(data);
			}
		}
	}

	public IRowMeta getRowMeta() {
		return rowMeta;
	}

	public void setRowMeta(final IRowMeta rowMeta) {
		this.rowMeta = rowMeta;

		if (rowMeta != null && 	isUseField  ) {			
			
			this.contentProposalProvider.setRowMeta(rowMeta);

			Display.getDefault().asyncExec(() -> {
				treeItemField.removeAll();

				for (int i = 0; i < rowMeta.size(); i++) {
					IValueMeta valueMeta = rowMeta.getValueMeta(i);

					// Escape field name matching reserved words or function
					String name = valueMeta.getName();
					if (Scanner.getReservedWords().contains(name.toUpperCase())
							|| Function.getFunction(name) != null) {
						name = '[' + name + ']';
					}

					TreeItem item = new TreeItem(treeItemField, SWT.NULL);
					item.setImage(labelProvider.getImage(valueMeta));
					item.setText(valueMeta.getName());
					item.setData(name);
				}
			});
		}
	}

}
