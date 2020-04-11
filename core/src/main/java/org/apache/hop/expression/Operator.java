package org.apache.hop.expression;

import java.io.InputStream;
import java.io.StringWriter;
import java.text.MessageFormat;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

import org.apache.hop.core.Const;
import org.apache.hop.core.xml.XmlHandler;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

//TODO: implement REGEXP, RLIKE operator
//TODO: implement CONTAINING
//TODO: implement STARTING [WITH]
//TODO: implement ENDING [WITH]
//TODO: implement the square brackets with a character range (MS SQL): like '[A-C]%'
/**
 * Operators have the precedence levels.
 * An operator on higher levels is evaluated before an operator on a lower level
 * 
 * 	 
 *   1			Functions
 *	 2  		Cast
 *   3	Right	+ (Positive), - (Negative)	
 * 	 4			~ (Bitwise NOT)
 *   5	Left	* (Multiplication), / (Division), % (Modulus)
 *   6			<<, >>  
 *   7 			& (Bitwise AND)      
 *   8  		^ (Bitwise Exclusive OR)      
 *   9  		| (Bitwise OR)
 *  10	Left	+ (Addition), - (Subtraction)
 *  11  		|| (Concatenation)
 *  12	-		BETWEEN, IN, LIKE
 *  13	-		=, >, <, >=, <=, <>, !=, !>, !< (Comparison operators)
 *  14  -		IS
 *  15	Right	NOT
 *  16	Left	AND
 *  17  Left	XOR
 * 	18  Left	OR	
 * 
 * @author Nicolas ADMENT
 *
 */
public class Operator implements Comparable<Operator> {

	public enum Category {
		Arithmetic,
		Bitwise,
		Comparison,
		Conditional,
		Conversion,
		Cryptographic,
		Date,
		Logical,
		Mathematical,
		String,
		Other
	}
	
	private static final String JAVA_REGEX_SPECIALS = "[]()|^-+*?{}$\\.";

	protected final Kind kind;

	/**
	 * The precedence with which this operator binds to the expression to the left.
	 * This is less than the right precedence if the operator is left-associative.
	 */
	private final int leftPrecedence;

	/**
	 * The precedence with which this operator binds to the expression to the right.
	 * This is more than the left precedence if the operator is left-associative.
	 */
	private final int rightPrecedence;

	private String category;
	private String description = "";
	private String syntax = "";
	private String returns = "";
	private String constraints = "";

	private static final ConcurrentHashMap<String, OperatorInfo> infos = new ConcurrentHashMap<>();

	static {
		try (InputStream is = Expression.class.getResourceAsStream("expression.xml")) {
			Document document = XmlHandler.loadXmlFile(is);
			Node rootNode = XmlHandler.getSubNode(document, OperatorInfo.EXPRESSION_TAG);
			int count = XmlHandler.countNodes(rootNode, OperatorInfo.OPERATOR_TAG);
			for (int i = 0; i < count; i++) {
				Node node = XmlHandler.getSubNodeByNr(rootNode, OperatorInfo.OPERATOR_TAG, i);
				OperatorInfo info = new OperatorInfo(node);
				infos.put(info.getName(), info);

				//System.out.println(info.getName());
			}
		} catch (Exception e) {

		}
	}



	
	/**
	 * Creates an operator specifying left and right precedence.
	 * 
	 * @param kind            Kind of operator
	 * @param leftPrecedence  Left precedence
	 * @param rightPrecedence Right precedence
	 */
	protected Operator(Kind kind,  String name, Category category, int leftPrecedence, int rightPrecedence) {
		super();
		this.kind = kind;
		this.name = name;
		this.leftPrecedence = leftPrecedence;
		this.rightPrecedence = rightPrecedence;
		this.category = category.name();

		OperatorInfo info = infos.get(kind.name());
		if (info != null) {
			this.syntax = MessageFormat.format(Const.NVL(info.getSyntax(), ""), name);
			//this.category = Const.NVL(info.getCategory(), "");
			this.description = info.getDescription();
			this.constraints = info.getConstraints();
			this.returns = info.getReturns();
		}
	}

	/**
	 * Creates an operator specifying left/right associativity.
	 * 
	 * @param kind              Kind of operator
	 * @param leftPrec          Precedence
	 * @param leftAssociativity operators on the left evaluate before ones of the
	 *                          right
	 */
	protected Operator(Kind kind, Category category, int precedence, boolean leftAssociativity) {
		this(kind, kind.toString(), category,  leftPrec(precedence, leftAssociativity), rightPrec(precedence, leftAssociativity));
	}

	protected static int leftPrec(int precedence, boolean leftAssociativity) {
		assert (precedence % 2) == 0;
		if (leftAssociativity) {			
			++precedence;
		}
		return precedence;
	}

	protected static int rightPrec(int precedence, boolean leftAssociativity) {
		assert (precedence % 2) == 0;
		if (!leftAssociativity) {
			++precedence;
		}
		return precedence;
	}

	/**
	 * The name of the operator/function. Ex. "OVERLAY" or "TRIM"
	 */
	private final String name;

	/**
	 * The name of the operator/function. Ex. "=" or "TRIM"
	 */

	public String getName() {
		return name;
	}

	public int getLeftPrecedence() {
		return leftPrecedence;
	}

	public int getRightPrecedence() {
		return rightPrecedence;
	}

	public final String getSyntax() {
		return syntax;
	}

	public Value eval(IExpressionContext context, IExpression... args) throws ExpressionException {
		switch (kind) {
				
		case BITWISE_AND_OPERATOR: {
			Value left = args[0].eval(context);
			if (left.isNull())
				return left;
			Value right = args[1].eval(context);
			if (right.isNull())
				return right;

			return Value.of(left.toInteger() & right.toInteger());
		}

		case BITWISE_NOT_OPERATOR: {
			Value value = args[0].eval(context);
			if (value.isNull())
				return value;

			return Value.of(~value.toInteger());
		}

		case BITWISE_OR_OPERATOR: {
			Value left = args[0].eval(context);
			if (left.isNull())
				return left;
			Value right = args[1].eval(context);
			if (right.isNull())
				return right;

			return Value.of(left.toInteger() | right.toInteger());
		}

		case BITWISE_XOR_OPERATOR: {
			Value left = args[0].eval(context);
			if (left.isNull())
				return left;
			Value right = args[1].eval(context);
			if (right.isNull())
				return right;

			return Value.of(left.toInteger() ^ right.toInteger());
		}
			
		case BETWEEN_OPERATOR: {
			Value operand = args[0].eval(context);
			Value start = args[1].eval(context);
			Value end = args[2].eval(context);

			if (operand.isNull() || start.isNull() || end.isNull()) {
				return Value.NULL;
			}

			return Value.of(operand.compareTo(start) >= 0 && operand.compareTo(end) <= 0);
		}

		case CASE_WHEN_OPERATOR: {
			int index = 0;
			IExpression switchExpression = args[0];
			ExpressionList whenList = (ExpressionList) args[1];
			ExpressionList thenList = (ExpressionList) args[2];
			IExpression elseExpression = args[3];

			if (switchExpression == null) {
				for (Expression whenOperand : whenList) {
					Value condition = whenOperand.eval(context);
					if (condition.toBoolean() == true) {
						return thenList.get(index).eval(context);
					}
					index++;
				}
			} else {
				Value condition = switchExpression.eval(context);
				for (Expression whenOperand : whenList) {
					Value value = whenOperand.eval(context);
					if (condition.compareTo(value) == 0) {
						return thenList.get(index).eval(context);
					}
					index++;
				}
			}

			return elseExpression.eval(context);
		}
		case CAST_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);

			if (left.isNull())
				return Value.NULL;
			if (right.isNull())
				return right;

			Type targetType = Type.valueOf((int) right.toInteger());

			return left.convertTo(targetType);
		}

		case CONCAT_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);

			if (right.isNull())
				return left;
			if (left.isNull())
				return right;

			return Value.of(left.toString().concat(right.toString()));
		}

		case LIKE_OPERATOR: {
			Value subject = args[0].eval(context);
			Value pattern = args[1].eval(context);

			if (subject.isNull() || pattern.isNull()) {
				return Value.FALSE;
			}

			String escape = null;
			if (args.length == 3) {
				Value escapeValue = args[2].eval(context);
				escape = escapeValue.toString();
			}

			final String regex = sqlToRegexLike(pattern.toString(), escape);

			return Value.of(Pattern.matches(regex, subject.toString()));

		}

		case MINUS_OPERATOR: {
			Value value = args[0].eval(context);
			return value.negate();
		}

		case LOGICAL_NOT_OPERATOR: {
			Value operand = args[0].eval(context);

			if (operand.isNull()) {
				return Value.NULL;
			}

			return Value.of(!operand.toBoolean());
		}

		case LOGICAL_AND_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);
			if (left.isNull() || right.isNull()) {
				return Value.NULL;
			}
			return Value.of(left.toBoolean() && right.toBoolean());
		}

		case LOGICAL_OR_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);
			if (left.isNull() && right.isNull()) {
				return Value.NULL;
			}

			return Value.of(left.toBoolean() || right.toBoolean());
		}

		case LOGICAL_XOR_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);
			if (left.isNull() || right.isNull()) {
				return Value.NULL;
			}
			return Value.of((left.toBoolean() || right.toBoolean()) && !(left.toBoolean() && right.toBoolean()));
		}

		case ADD_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);

			if (left.isNull() || right.isNull()) {
				return Value.NULL;
			}
			return left.add(right);
		}

		case SUBTRACT_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);

			if (left.isNull() || right.isNull()) {
				return Value.NULL;
			}
			return left.subtract(right);
		}

		case MULTIPLY_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);

			if (left.isNull() || right.isNull()) {
				return Value.NULL;
			}
			return left.multiply(right);
		}

		case DIVIDE_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);

			if (left.isNull() || right.isNull()) {
				return Value.NULL;
			}
			// prevent a division by zero ..
			if (right.signum() == 0)
				throw createDivisionByZeroError();

			return left.divide(right);
		}

		case MOD: // Same implementation for operator and function
		case MODULUS_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);

			if (left.isNull() || right.isNull()) {
				return Value.NULL;
			}
			// prevent a division by zero ..
			if (right.signum() == 0)
				throw createDivisionByZeroError();

			return left.remainder(right);
		}

		case POWER: // Same implementation for operator and function
		//case POWER_OPERATOR: 
		{
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);

			if (left.isNull() || right.isNull()) {
				return Value.NULL;
			}
			if (right.signum() < 0)
				throw new ArithmeticException("Cannot power negative " + right);
			if (right.signum() == 0)
				return Value.ONE;

			return left.power(right);
		}

		case EQUAL_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);

			// Treats NULLs as unknown values
			if (left.isNull() || right.isNull()) {
				return Value.NULL;
			}

			return Value.of(left.compareTo(right) == 0);
		}

		case LESS_THAN_OR_GREATER_THEN:
		case NOT_EQUAL_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);

			if (left.isNull() && right.isNull()) {
				return Value.FALSE;
			}
			if (left.isNull() || right.isNull()) {
				return Value.TRUE;
			}

			return Value.of(left.compareTo(right) != 0);
		}

		case LESS_THAN_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);

			if (left.isNull() || right.isNull()) {
				return Value.FALSE;
			}

			return Value.of(left.compareTo(right) < 0);
		}

		case LESS_THAN_OR_EQUAL_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);

			if (left.isNull() || right.isNull()) {
				return Value.FALSE;
			}

			return Value.of(left.compareTo(right) <= 0);
		}

		case GREATER_THAN_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);

			if (left.isNull() || right.isNull()) {
				return Value.FALSE;
			}

			return Value.of(left.compareTo(right) > 0);
		}

		case GREATER_THAN_OR_EQUAL_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);

			if (left.isNull() || right.isNull()) {
				return Value.FALSE;
			}

			return Value.of(left.compareTo(right) >= 0);
		}

		case IS_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);

			return Value.of(left.equals(right));
		}

		case IN_OPERATOR: {
			Value left = args[0].eval(context);
			if (left.isNull()) {
				return Value.FALSE;
			}

			ExpressionList list = (ExpressionList) args[1];
			for (Expression expression : list) {
				Value value = expression.eval(context);
				if (left.compareTo(value) == 0) {
					return Value.TRUE;
				}
			}

			return Value.FALSE;
		}

		case CONTAINS:
		case CONTAINS_OPERATOR: {
			Value left = args[0].eval(context);
			Value right = args[1].eval(context);

			if (left.isNull() && right.isNull())
				return Value.TRUE;

			if (left.isNull() || right.isNull())
				return Value.FALSE;

			if (left.toString().contains(right.toString()))
				return Value.TRUE;

			return Value.FALSE;
		}

		default:
			throw createInternalError(kind.name());
		}
	}

	public Expression optimize(IExpressionContext context, Expression... operands) throws ExpressionException {
		switch (kind) {

		case MINUS_OPERATOR:
		case LOGICAL_NOT_OPERATOR: {
			Expression operand = operands[0].optimize(context);

			if (operand.isConstant()) {
				return eval(context, operand);
			}
			else if ( operand.is(this.kind) ) {
				// Eliminate double NOT or MINUS
				ExpressionCall call = (ExpressionCall) operand;
				return call.getOperands()[0];
			}

			return new ExpressionCall(this, operand);
		}

		case LOGICAL_AND_OPERATOR: // Binary operator
		case CONCAT_OPERATOR:
		case CONTAINS_OPERATOR:
		case ADD_OPERATOR:
		case SUBTRACT_OPERATOR:
		case MULTIPLY_OPERATOR:
		case DIVIDE_OPERATOR:
		case MODULUS_OPERATOR:
		//case POWER_OPERATOR:
		case LOGICAL_OR_OPERATOR:
		case LOGICAL_XOR_OPERATOR:
		case EQUAL_OPERATOR:
		case NOT_EQUAL_OPERATOR:
		case LIKE_OPERATOR:
		case LESS_THAN_OPERATOR:
		case LESS_THAN_OR_EQUAL_OPERATOR:
		case GREATER_THAN_OPERATOR:
		case GREATER_THAN_OR_EQUAL_OPERATOR:
		case IS_OPERATOR:
		case IN_OPERATOR: {
			Expression left = operands[0].optimize(context);
			Expression right = operands[1].optimize(context);

			if (left.isConstant() && right.isConstant()) {
				return eval(context, left, right);
			}

			return new ExpressionCall(this, left, right);
		}

		case BETWEEN_OPERATOR: {
			Expression operand = operands[0].optimize(context);
			Expression start = operands[1].optimize(context);
			Expression end = operands[2].optimize(context);

			if (operand.isConstant() && start.isConstant() && end.isConstant()) {
				return eval(context, operand, start, end);
			}

			return new ExpressionCall(this, operand, start, end);
		}

		default:
			System.out.println("Not optimised " + kind);
			return new ExpressionCall(this, operands);
		}

	}

	/**
	 * Writes a expression representation of a call to this operator to a writer,
	 * including parentheses if the operators on either side are of greater
	 * precedence.
	 */
	public void unparse(StringWriter writer, ExpressionCall call, int leftPrec, int rightPrec) {
		switch (kind) {

		case BETWEEN_OPERATOR: {
			Expression[] operands = call.getOperands();
			operands[0].unparse(writer, leftPrec, rightPrec);
			writer.append(' ');
			writer.append("BETWEEN");
			writer.append(' ');
			operands[1].unparse(writer, leftPrec, rightPrec);
			writer.append(" AND ");
			operands[2].unparse(writer, leftPrec, rightPrec);
			break;
		}

		case CASE_WHEN_OPERATOR: {
			Expression[] operands = call.getOperands();
			
			Expression switchExpression = operands[0];
			ExpressionList whenList = (ExpressionList) operands[1];
			ExpressionList thenList = (ExpressionList) operands[2];
			Expression elseExpression = operands[3];

			writer.append("CASE ");

			// Form switch expression
			if (switchExpression != null) {
				switchExpression.unparse(writer, 0, 0);
			}

			int index = 0;
			for (Expression whenOperand : whenList) {
				writer.append("WHEN ");
				whenOperand.unparse(writer, 0, 0);
				writer.append(" THEN ");
				Expression thenOperand = thenList.get(index++);
				thenOperand.unparse(writer, 0, 0);
			}
			if (elseExpression != null) {
				elseExpression.unparse(writer, leftPrec, rightPrec);
			}
			writer.append("END");
			break;
		}

		case CAST_OPERATOR: {
			Expression[] operands = call.getOperands();
			writer.append(this.getName());
			writer.append('(');
			operands[0].unparse(writer, leftPrec, rightPrec);
			writer.append(" AS ");
			operands[1].unparse(writer, leftPrec, rightPrec);
			writer.append(')');
			break;
		}

		case CONCAT_OPERATOR: {
			Expression[] operands = call.getOperands();
			operands[0].unparse(writer, leftPrec, rightPrec);
			writer.append(this.getName());
			operands[1].unparse(writer, leftPrec, rightPrec);
			break;
		}

		case LIKE_OPERATOR: {
			Expression[] operands = call.getOperands();
			operands[0].unparse(writer, leftPrec, rightPrec);
			writer.append(' ');
			writer.append(this.getName());
			writer.append(' ');
			operands[1].unparse(writer, leftPrec, rightPrec);
			if (call.getOperandCount() == 3) {
				writer.append(" ESCAPE ");
				operands[2].unparse(writer, leftPrec, rightPrec);
			}
			break;
		}

		case MINUS_OPERATOR:  {
			Expression[] operands = call.getOperands();
			writer.append(this.getName());
			operands[0].unparse(writer, leftPrec, rightPrec);
			break;
		}

		case LOGICAL_NOT_OPERATOR: { 
			Expression[] operands = call.getOperands();
			writer.append(this.getName());
			writer.append(' ');
			operands[0].unparse(writer, leftPrec, rightPrec);
			break;
		}

		case LOGICAL_AND_OPERATOR:
		case LOGICAL_OR_OPERATOR:
		case LOGICAL_XOR_OPERATOR:
		case EQUAL_OPERATOR:
		case NOT_EQUAL_OPERATOR:
		case LESS_THAN_OR_GREATER_THEN:
		case LESS_THAN_OPERATOR:
		case LESS_THAN_OR_EQUAL_OPERATOR:
		case GREATER_THAN_OPERATOR:
		case GREATER_THAN_OR_EQUAL_OPERATOR:
		case IS_OPERATOR:
		case IN_OPERATOR: {
			Expression[] operands = call.getOperands();
			operands[0].unparse(writer, leftPrec, rightPrec);
			writer.append(' ');
			writer.append(this.getName());
			writer.append(' ');
			operands[1].unparse(writer, leftPrec, rightPrec);
			break;
		}

		case ADD_OPERATOR:
		case SUBTRACT_OPERATOR:
		case MULTIPLY_OPERATOR:
		case DIVIDE_OPERATOR:
		case MODULUS_OPERATOR: {
		//case POWER_OPERATOR:
			Expression[] operands = call.getOperands();
			operands[0].unparse(writer, leftPrec, rightPrec);
			writer.append(this.getName());
			operands[1].unparse(writer, leftPrec, rightPrec);
			break;
		}
		default:
			throw createInternalError(kind.name());
		}
	}

	/**
	 * Check if the number of arguments is correct.
	 *
	 * @param len the number of arguments set
	 * @throws ExpressionException if the number of arguments is incorrect
	 */
	public void checkNumberOfArguments(int len) throws ExpressionException {

	}

	// -------------------------------------------------------------
	// BITWISE OPERATORS
	// -------------------------------------------------------------
	
	/**
	 * Bitwise AND operator "&".
	 */
	public static final Operator BITWISE_AND = new Operator(Kind.BITWISE_AND_OPERATOR, Category.Bitwise, 70, true);

	/**
	 * Bitwise OR operator "|".
	 */
	public static final Operator BITWISE_OR = new Operator(Kind.BITWISE_OR_OPERATOR, Category.Bitwise, 90, true);

	/**
	 * Bitwise NOT operator "~".
	 */
	public static final Operator BITWISE_NOT = new Operator(Kind.BITWISE_NOT_OPERATOR, Category.Bitwise, 40, true);

	/**
	 * Bitwise XOR operator "^".
	 */
	public static final Operator BITWISE_XOR = new Operator(Kind.BITWISE_XOR_OPERATOR, Category.Bitwise, 80, true);

	
	
	// -------------------------------------------------------------
	// LOGICAL OPERATORS
	// -------------------------------------------------------------

	/**
	 * Logical negation <code>NOT</code> operator
	 * 
	 * <p>
	 * Syntax of the operator:
	 * <ul>
	 * <li><code>field [NOT] TRUE</code></li>
	 * <li><code>field [NOT] IN (list of values)</code></li>
	 * <li><code>field [NOT] BETWEEN start AND end</code></li>
	 * </ul>
	 * </p>
	 */
	public static final Operator LOGICAL_NOT = new Operator(Kind.LOGICAL_NOT_OPERATOR, Category.Logical, 150, false);
	/**
	 * Logical disjunction <code>OR</code> operator.
	 */
	public static final Operator LOGICAL_OR = new Operator(Kind.LOGICAL_OR_OPERATOR, Category.Logical, 180, true);
	/**
	 * Logical conjunction <code>AND</code> operator.
	 */
	public static final Operator LOGICAL_AND = new Operator(Kind.LOGICAL_AND_OPERATOR, Category.Logical, 160, true);
	/**
	 * Logical <code>XOR</code> operator.
	 */
	public static final Operator LOGICAL_XOR = new Operator(Kind.LOGICAL_XOR_OPERATOR, Category.Logical, 170, true);

	/**
	 * An operator describing the <code>IS</code> operator.
	 *
	 * <p>
	 * Syntax of the operator:
	 * <ul>
	 * <li><code>field IS TRUE</code></li>
	 * <li><code>field IS FALSE</code></li>
	 * <li><code>field IS NULL</code></li>
	 * </ul>
	 */
	public static final Operator IS = new Operator(Kind.IS_OPERATOR, Category.Logical, 140, true);

	/**
	 * Logical <code>IN</code> operator tests for a value's membership in a list of
	 * values. The IN operator is a shorthand for multiple OR conditions.
	 *
	 * <p>
	 * Syntax of the operator:
	 * <ul>
	 * <li><code>field [NOT] IN list of values</code></li>
	 * </ul>
	 *
	 * <p>
	 * <b>NOTE</b> If the <code>NOT</code> clause is present the
	 * {@link org.apache.hop.core.ExpressionParser parser} will generate a
	 * equivalent to <code>NOT (field IN list of values ...)</code>
	 */
	public static final Operator IN = new Operator(Kind.IN_OPERATOR, Category.Logical, 120, true);
	/**
	 * An operator describing the <code>LIKE</code> operator.
	 *
	 * <p>
	 * Syntax of the operator:
	 *
	 * <ul>
	 * <li><code>field [NOT] LIKE pattern</code></li>
	 * </ul>
	 *
	 * <p>
	 * <b>NOTE</b> If the <code>NOT</code> clause is present the
	 * {@link org.kettle.core.database.ExpressionParser parser} will generate a
	 * equivalent to <code>NOT (field LIKE pattern ...)</code>
	 * 
	 * TODO: implement LIKE <pattern> ESCAPE <char>
	 */
	public static final Operator LIKE = new Operator(Kind.LIKE_OPERATOR, Category.Comparison, 120, true);

	public static final Operator BETWEEN = new Operator(Kind.BETWEEN_OPERATOR, Category.Comparison, 120, true);

	
	// -------------------------------------------------------------
	// COMPARISON OPERATORS
	// -------------------------------------------------------------

	/**
	 * Comparison equals operator '<code>=</code>'.
	 */
	public static final Operator EQUALS = new Operator(Kind.EQUAL_OPERATOR, Category.Comparison, 130, true);
	/**
	 * Comparison not equals operator '<code><></code>'.
	 */
	public static final Operator NOT_EQUALS = new Operator(Kind.NOT_EQUAL_OPERATOR, Category.Comparison, 130, true);
	/**
	 * Comparison not equals operator '<code>!=</code>'.
	 */
	public static final Operator LESS_THAN_OR_GREATER_THAN = new Operator(Kind.LESS_THAN_OR_GREATER_THEN, Category.Comparison, 130, true);
	/**
	 * Comparison less-than operator '<code>&lt;</code>'.
	 */
	public static final Operator LESS_THAN = new Operator(Kind.LESS_THAN_OPERATOR, Category.Comparison, 130, true);
	/**
	 * Comparison less-than-or-equal operator '<code>&lt;=</code>'.
	 */
	public static final Operator LESS_THAN_OR_EQUAL = new Operator(Kind.LESS_THAN_OR_EQUAL_OPERATOR, Category.Comparison, 130, true);
	/**
	 * Comparison greater-than operator '<code>&gt;</code>'.
	 */
	public static final Operator GREATER_THAN = new Operator(Kind.GREATER_THAN_OPERATOR, Category.Comparison, 130, true);
	/**
	 * Comparison greater-than-or-equal operator '<code>&gt;=</code>'.
	 */
	public static final Operator GREATER_THAN_OR_EQUAL = new Operator(Kind.GREATER_THAN_OR_EQUAL_OPERATOR, Category.Comparison, 130, true);
	/**
	 * Comparison contains operator '<code>=~</code>'.
	 */
	public static final Operator CONTAINS = new Operator(Kind.CONTAINS_OPERATOR, Category.Comparison, 130, true);



	// -------------------------------------------------------------
	// ARITHMETIC OPERATORS
	// -------------------------------------------------------------

	/**
	 * Arithmetic unary negate operator '<code>-</code>'.
	 */
	public static final Operator NEGATE = new Operator(Kind.MINUS_OPERATOR, Category.Arithmetic, 30, true);

//	/**
//	 * Arithmetic power operator '<code>**</code>'.
//	 */
//	public static final Operator POWER = new Operator(Kind.POWER_OPERATOR, Category.Arithmetic, 70, true);

	/**
	 * Arithmetic multiplication operator '<code>*</code>'.
	 */
	public static final Operator MULTIPLY = new Operator(Kind.MULTIPLY_OPERATOR, Category.Arithmetic, 50, true);

	/**
	 * Arithmetic division operator '<code>/</code>'.
	 */
	public static final Operator DIVIDE = new Operator(Kind.DIVIDE_OPERATOR, Category.Arithmetic, 50, true);

	/**
	 * Arithmetic modulus operator '<code>%</code>'.
	 */
	public static final Operator MODULUS = new Operator(Kind.MODULUS_OPERATOR, Category.Arithmetic, 50, true);

	/**
	 * Arithmetic addition operator '<code>+</code>'.
	 */
	public static final Operator ADD = new Operator(Kind.ADD_OPERATOR, Category.Arithmetic, 100, true);

	/**
	 * Arithmetic subtraction operator '<code>-</code>'.
	 */
	public static final Operator SUBTRACT = new Operator(Kind.SUBTRACT_OPERATOR, Category.Arithmetic, 100, true);

	// -------------------------------------------------------------
	// SPECIAL OPERATORS
	// -------------------------------------------------------------

	/**
	 * Casting operator '<code>CAST(value AS dataType)</code>'.
	 */
	public static final Operator CAST = new Operator(Kind.CAST_OPERATOR, Category.Conversion,20, true);

	/**
	 * An operator describing the <code>CASE</code> operator.
	 */
	public static final Operator CASE = new Operator(Kind.CASE_WHEN_OPERATOR, Category.Conditional, 120, true);

	/**
	 * String concatenation operator '<code>||</code>'.
	 */
	public static final Operator CONCAT = new Operator(Kind.CONCAT_OPERATOR, Category.Other, 110, true);

	

	public String getCategory() {
		return category;
	}

	public String getDescription() {
		return description;
	}

	public String getReturns() {
		return returns;
	}

	public String getConstraints() {
		return constraints;
	}

	protected final ExpressionException createArgumentOutOfRangeError(Object arg) {
		return new ExpressionException("Argument '{0}' is out of range", arg);
	}

	protected final ExpressionException createDivisionByZeroError() {
		return new ExpressionException("Division by 0 for operator " + this);
	}

	protected final ExpressionException createInvalidNumberOfArgumentsError() {
		return new ExpressionException("ExpressionException.InvalidNumberOfArguments", this.getSyntax());
	}

	protected final ExpressionException createInternalError(final String error) {
		return new ExpressionException("ExpressionException.InternalError", error);
	}

	/**
	 * Translates a SQL LIKE pattern to Java regex pattern, with optional escape
	 * string.
	 */
	private String sqlToRegexLike(String sqlPattern, CharSequence escapeStr) {
		final char escapeChar;
		if (escapeStr != null) {
			if (escapeStr.length() != 1) {
				throw createInvalidEscapeCharacter(escapeStr.toString());
			}
			escapeChar = escapeStr.charAt(0);
		} else {
			escapeChar = 0;
		}
		return sqlToRegexLike(sqlPattern, escapeChar);
	}

	/**
	 * Translates a SQL LIKE pattern to Java regex pattern.
	 */
	private String sqlToRegexLike(String sqlPattern, char escapeChar) {
		int i;
		final int len = sqlPattern.length();
		final StringBuilder javaPattern = new StringBuilder(len + len);
		for (i = 0; i < len; i++) {
			char c = sqlPattern.charAt(i);
			if (JAVA_REGEX_SPECIALS.indexOf(c) >= 0) {
				javaPattern.append('\\');
			}

			if (c == escapeChar) {
				if (i == (sqlPattern.length() - 1)) {
					throw createInvalidEscapeSequence(sqlPattern, i);
				}
				char nextChar = sqlPattern.charAt(i + 1);
				if ((nextChar == '_') || (nextChar == '%') || (nextChar == escapeChar)) {
					javaPattern.append(nextChar);
					i++;
				} else {
					throw createInvalidEscapeSequence(sqlPattern, i);
				}
			} else if (c == '_') {
				javaPattern.append('.');
			} else if (c == '%') {
				javaPattern.append("(?s:.*)");
			} else {
				javaPattern.append(c);
			}
		}
		return javaPattern.toString();
	}

	private ExpressionException createInvalidEscapeCharacter(String s) {
		return new ExpressionException("Invalid escape character '" + s + "'");
	}

	private ExpressionException createInvalidEscapeSequence(String s, int i) {
		return new ExpressionException("Invalid escape sequence '" + s + "', " + i);
	}

	@Override
	public int compareTo(Operator o) {
		return this.kind.compareTo(o.kind);
	}
}
