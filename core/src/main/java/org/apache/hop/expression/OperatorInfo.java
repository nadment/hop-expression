package org.apache.hop.expression;

import org.apache.hop.core.xml.XMLHandler;
import org.w3c.dom.Node;

/**
 * This class contains information about a built-in function.
 */
/* protected */ class OperatorInfo {
	public static final String EXPRESSION_TAG = "expression";
	public static final String OPERATOR_TAG = "operator";
	public static final String CATEGORY_TAG = "category";
	public static final String NAME_TAG = "name";
	public static final String DESCRIPTION_TAG = "description";
	public static final String SYNTAX_TAG = "syntax";
	public static final String RETURNS_TAG = "returns";
	/**
	 * The name of the function.
	 */
	private String name;
	private String description;
	private String category;
	private String syntax;
	private String returns;
	private String constraints;
	private String semantics;

	/**
	 * @param name             The name of the function.
	 * @param category
	 * @param description
	 * @param syntax
	 * @param returns
	 * @param constraints
	 * @param semantics
	 * @param functionExamples
	 */
	public OperatorInfo(String name, String category, String description, String syntax, String returns,
			String constraints, String semantics) {
		this.category = category;
		this.name = name;
		this.description = description;
		this.syntax = syntax;
		this.returns = returns;
		this.constraints = constraints;
		this.semantics = semantics;
		// this.functionExamples = functionExamples;
	}

	public OperatorInfo(Node node) {
		this.category = XMLHandler.getTagValue(node, CATEGORY_TAG);
		this.name = XMLHandler.getTagValue(node, NAME_TAG);
		this.description = XMLHandler.getTagValue(node, DESCRIPTION_TAG);
		this.syntax = XMLHandler.getTagValue(node, SYNTAX_TAG);
		this.returns = XMLHandler.getTagValue(node, RETURNS_TAG);
		this.constraints = XMLHandler.getTagValue(node, "constraints");
		this.semantics = XMLHandler.getTagValue(node, "semantics");

		// this.functionExamples = new ArrayList<FunctionExample>();

		// Node examplesNode = XMLHandler.getSubNode(node, "examples");
		// int nrExamples = XMLHandler.countNodes(examplesNode, XML_TAG);
		// for (int i = 0; i < nrExamples; i++) {
		// Node exampleNode = XMLHandler.getSubNodeByNr(examplesNode, XML_TAG, i);
		// this.functionExamples.add( new FunctionExample( exampleNode ) );
		// }
	}

	/**
	 * @return the category
	 */
	public String getCategory() {
		return category;
	}

	/**
	 * @param category the category to set
	 */
	public void setCategory(String category) {
		this.category = category;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the description
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * @param description the description to set
	 */
	public void setDescription(String description) {
		this.description = description;
	}

	/**
	 * @return the syntax
	 */
	public String getSyntax() {
		return syntax;
	}

	/**
	 * @param syntax the syntax to set
	 */
	public void setSyntax(String syntax) {
		this.syntax = syntax;
	}

	/**
	 * @return the returns
	 */
	public String getReturns() {
		return returns;
	}

	/**
	 * @param returns the returns to set
	 */
	public void setReturns(String returns) {
		this.returns = returns;
	}

	/**
	 * @return the constraints
	 */
	public String getConstraints() {
		return constraints;
	}

	/**
	 * @param constraints the constraints to set
	 */
	public void setConstraints(String constraints) {
		this.constraints = constraints;
	}

	/**
	 * @return the semantics
	 */
	public String getSemantics() {
		return semantics;
	}

	/**
	 * @param semantics the semantics to set
	 */
	public void setSemantics(String semantics) {
		this.semantics = semantics;
	}
}
