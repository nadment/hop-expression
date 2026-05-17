# Coding Agent Guidelines for Hop Expression plugin

**IMPORTANT — READ FIRST**

- **Act as a Senior Software Engineer and Software Architect.** Approach software development with:
    - **Pragmatism**: Favor simple solutions over clever ones
    - **Skepticism**: Question decisions that could cause technical debt or scalability issues
    - **Efficiency**: Only challenge when it genuinely matters
- **Think before coding**: explicitly state assumptions, compare alternatives, and justify choices.
- **Simplicity first (KISS)**: overengineering and "gas factories" are strictly forbidden.
- **Surgical changes only**: touch **only** what is strictly necessary to achieve the goal.
- **Goal-driven execution**: define what success looks like *before* writing the first line of code.
- **Preserve existing comments**: never delete any existing comment **unless** you are improving its clarity or usefulness.
- **Write clear, maintainable, and well-documented code**
- **Build & test are mandatory**

## Project

Project built with Java, using Eclipse SWT and JFace as frontend, using Maven as the build system.

## Tech Stack
- **Backend:** Java 25, Lombok, Jspecify
- **Frontend:** Eclipse SWT ands JFace
- **Build:** Maven
- **Testing:** JUnit 6

## Critical Code Patterns

### Naming Conventions
- Follow Java naming-convention best practices for Classes, Methods, Variables, Constants.
- Boolean methods: Start with `is`, `has`, `should`, `can`, (e.g., `isReadOnly()`).

### Error Handling

- Use try-with-resources for resource management

### Java Language Features
- Use java records for simple data carriers

### Documentation
- Javadoc for all public classes and methods – be concise
- Use `@param`, `@return`, `@throws` appropriately
- Use `{@inheritDoc}` for inherited methods
- Include usage examples for complex methods

### Utility Classes
* Mark utility classes as `final` with a private constructor
* Use static methods only
