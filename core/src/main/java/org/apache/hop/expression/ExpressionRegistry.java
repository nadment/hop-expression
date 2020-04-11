package org.apache.hop.expression;

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Set;
import java.util.TreeSet;

import org.apache.hop.expression.Operator.Category;

public class ExpressionRegistry {

	// TODO: Java 9 use unmodifiable Set.of()
	private static final Set<String> KEYWORDS = Collections
			.unmodifiableSet(new TreeSet<>(Arrays.asList("%", "+", "-", "*", "^", "/", "=", "<>", "!=", "<", "<=", ">",
					">=", "(", ")", "[","]","||", "AS", "AND", "BETWEEN", "CASE", "CAST", "DATE", "ELSE", "END", "ESCAPE",
					"FALSE", "IN", "IS", "LIKE", "NOT", "NULL", "OR", "THEN", "TRUE", "WHEN", "XOR")));

	/**
	 * Set of operators.
	 */
	/* TODO: Java 9 use unmodifiable Set.of(...) */
	private static final Set<Operator> OPERATORS = Collections
			.unmodifiableSet(new TreeSet<>(Arrays.asList(Operator.ADD, Operator.CAST, Operator.SUBTRACT,
					Operator.MULTIPLY, Operator.DIVIDE, Operator.BITWISE_AND, Operator.BITWISE_OR,
					// Operator.BITWISE_NOT,
					Operator.BITWISE_XOR, Operator.MODULUS, Operator.EQUALS, Operator.GREATER_THAN,
					Operator.GREATER_THAN_OR_EQUAL, Operator.LESS_THAN, Operator.LESS_THAN_OR_EQUAL,
					Operator.LESS_THAN_OR_GREATER_THAN, Operator.NOT_EQUALS, Operator.LOGICAL_AND, Operator.BETWEEN,
					Operator.CASE, Operator.CONCAT, Operator.IN, Operator.IS, Operator.LIKE, Operator.LOGICAL_NOT,
					Operator.LOGICAL_OR, Operator.LOGICAL_XOR)));

	public static Set<Operator> getOperators() {
		return OPERATORS;
	}

	/**
	 * Set of functions or alias.
	 */
	private static final TreeSet<Function> FUNCTIONS = new TreeSet<>(Comparator.comparing(Function::getName));

	/**
	 * Set of functions or alias by name.
	 */
	private static final HashMap<String, Function> FUNCTIONS_BY_NAME = new HashMap<>(256);

	// -------------------------------------------------------------
	// FUNCTIONS
	// -------------------------------------------------------------
	static {
		addFunction(Kind.ABS, Category.Mathematical);
		addFunction(Kind.ADD_MONTHS, Category.Date);
		addFunction(Kind.ACOS, Category.Mathematical);
		addFunction(Kind.ASCII, Category.String);
		addFunction(Kind.ASIN, Category.Mathematical);
		addFunction(Kind.ATAN, Category.Mathematical);
		addFunction(Kind.ATAN2, Category.Mathematical);
		// addFunction(Kind.BIT_LENGTH);
		// TODO: addFunction(Kind.BITGET, Category.Bitwise);
		// addFunction(Kind.BITAND, Category.Bitwise);
		// addFunction(Kind.BITNOT, Category.Bitwise);
		// addFunction(Kind.BITOR, Category.Bitwise);
		// addFunction(Kind.BITXOR, Category.Bitwise);
		addFunction(Kind.CBRT, Category.Mathematical);
		addFunction(Kind.CEIL, Category.Mathematical);
		addFunction(Kind.CHR, Category.String);
		addFunction(Kind.COALESCE, Category.Conditional);
		addFunction(Kind.CONCAT, Category.String);
		addFunction(Kind.COS, Category.Mathematical);
		addFunction(Kind.COSH, Category.Mathematical);
		addFunction(Kind.COT, Category.Mathematical);
		addFunction(Kind.CONTAINS, Category.Comparison);
		addFunctionNotDeterministic(Kind.CURRENT_DATE, Category.Date); // Alias "NOW", "CURRENT_DATE", "CURDATE",
																		// "SYSDATE", "TODAY");
		addFunction(Kind.DAY_NAME, Category.Date); // "DAYNAME"
		addFunction(Kind.DAY_OF_MONTH, Category.Date, "DAY", "DAY_OF_MONTH"); // "DAYOFMONTH"
		addFunction(Kind.DAY_OF_WEEK, Category.Date, "DAY_OF_WEEK"); // "DAYOFWEEK"
		addFunction(Kind.DAY_OF_YEAR, Category.Date, "DAY_OF_YEAR"); // "DAYOFYEAR"
		addFunction(Kind.DECODE, Category.Conditional);
		addFunction(Kind.DEGREES, Category.Mathematical);
		addFunction(Kind.EQUAL_NULL, Category.Comparison);
		addFunction(Kind.ENDSWITH, Category.Comparison);
		addFunction(Kind.EXP, Category.Mathematical);
		addFunction(Kind.FLOOR, Category.Mathematical);
		addFunction(Kind.GREATEST, Category.Conditional);
		addFunction(Kind.HOUR, Category.Date);
		addFunction(Kind.IF, Category.Conditional);
		addFunction(Kind.IFNULL, Category.Conditional, "IFNULL", "NVL");
		addFunction(Kind.INITCAP, Category.String);
		addFunction(Kind.INSTR, Category.String);
		addFunction(Kind.LAST_DAY, Category.Date);
		addFunction(Kind.LEAST, Category.Conditional);
		addFunction(Kind.LEFT, Category.String);
		addFunction(Kind.LENGTH, Category.String); // "LENGTH", "LEN", "CHAR_LENGTH");
		addFunction(Kind.LN, Category.Mathematical);
		addFunction(Kind.LOG10, Category.Mathematical);
		addFunction(Kind.LOWER, Category.String); // , "LOWER", "LCASE");
		addFunction(Kind.LPAD, Category.String);
		addFunction(Kind.LTRIM, Category.String);
		addFunction(Kind.MD5, Category.Cryptographic);
		addFunction(Kind.MINUTE, Category.Date);
		addFunction(Kind.MOD, Category.Mathematical);
		addFunction(Kind.MONTH, Category.Date);
		addFunction(Kind.MONTH_NAME, Category.Date); // "MONTHNAME"
		addFunction(Kind.NULLIF, Category.Conditional);
		// addFunction(Kind.OCTET_LENGTH);
		addFunction(Kind.PI, Category.Mathematical);
		addFunction(Kind.POWER, Category.Mathematical); // Alias POW
		addFunction(Kind.QUARTER, Category.Date);
		addFunction(Kind.UPPER, Category.String); // , "UPPER", "UCASE");
		addFunction(Kind.RADIANS, Category.Mathematical);
		addFunctionNotDeterministic(Kind.RAND, Category.Mathematical);
		addFunction(Kind.REPEAT, Category.String);
		addFunction(Kind.REPLACE, Category.String);
		addFunction(Kind.REVERSE, Category.String);
		addFunction(Kind.RIGHT, Category.String);
		addFunction(Kind.ROUND, Category.Mathematical);
		addFunction(Kind.RPAD, Category.String);
		addFunction(Kind.RTRIM, Category.String);
		addFunction(Kind.SHA1, Category.Cryptographic);
		addFunction(Kind.SHA256, Category.Cryptographic);
		addFunction(Kind.SHA384, Category.Cryptographic);
		addFunction(Kind.SHA512, Category.Cryptographic);
		addFunction(Kind.SECOND, Category.Date);
		addFunction(Kind.SIGN, Category.Mathematical);
		addFunction(Kind.SIN, Category.Mathematical);
		addFunction(Kind.SINH, Category.Mathematical);
		addFunction(Kind.SOUNDEX, Category.String);
		addFunction(Kind.SPACE, Category.String);
		addFunction(Kind.SQRT, Category.Mathematical);
		addFunction(Kind.STARTSWITH, Category.Comparison);
		addFunction(Kind.STRINGDECODE, Category.String);
		addFunction(Kind.STRINGENCODE, Category.String);
		addFunction(Kind.SUBSTR, Category.String, "SUBSTR", "SUBSTRING");
		addFunction(Kind.TAN, Category.Mathematical);
		addFunction(Kind.TANH, Category.Mathematical);
		addFunction(Kind.TO_BOOLEAN, Category.Conversion);
		addFunction(Kind.TO_CHAR, Category.Conversion);
		// addFunction(Kind.TO_DATE, Category.Conversion);
		// addFunction(Kind.TO_NUMBER, Category.Conversion);
		addFunction(Kind.TRIM, Category.String);
		addFunction(Kind.TRANSLATE, Category.String);
		addFunction(Kind.UNICODE, Category.String);
		addFunction(Kind.URLDECODE, Category.String);
		addFunction(Kind.URLENCODE, Category.String);
		addFunction(Kind.WEEK_OF_YEAR, Category.Date, "WEEK_OF_YEAR", "WEEK");
		addFunction(Kind.YEAR, Category.Date);
	}

	private static void addFunction(Kind kind, Category category) {
		addFunction(kind, category, kind.name());
	}

	private static void addFunction(Kind kind, Category category, String... alias) {
		for (String name : alias) {
			Function function = new Function(kind, name, category, true);
			FUNCTIONS.add(function);
			FUNCTIONS_BY_NAME.put(name, function);
		}
	}

	private static void addFunctionNotDeterministic(Kind kind, Category category) {
		addFunctionNotDeterministic(kind, category, kind.name());
	}

	private static void addFunctionNotDeterministic(Kind kind, Category category, String... alias) {
		for (String name : alias) {
			Function function = new Function(kind, name, category, false);
			FUNCTIONS.add(function);
			FUNCTIONS_BY_NAME.put(name, function);
		}
	}

	public static Set<Function> getFunctions() {
		return FUNCTIONS;
	}

	public static Function getFunction(final String name) {
		if (name == null)
			return null;

		return FUNCTIONS_BY_NAME.get(name.toUpperCase());
	}

//	public static Operator getOperator(String name) {
//		for (Operator operator : OPERATORS) {
//			if ( operator.getName().equalsIgnoreCase(name) )
//				return operator;
//		}
//		
//		return null;
//	}
	
	public static Set<String> getReservedWords() {
		return KEYWORDS;
	}
}
