//! Enhanced Integration Error Tests
//! 
//! This module tests complex error scenarios that involve multiple language features
//! interacting together, edge cases, and error propagation across components.
//! 
//! Unlike individual component error tests, these focus on:
//! - Cross-component error interactions
//! - Complex nested error scenarios  
//! - Real-world error patterns
//! - Error recovery and reporting quality

#[cfg(test)]
mod tests {
    use crate::error::Error;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::semantic::SemanticVisitor;
    use crate::interpreter::InterpreterVisitor;
    use crate::ast::visitor::AstVisitor;

    /// Helper to run full pipeline and capture any error
    fn run_program_expect_error(program: &str) -> Result<Error, String> {
        // Lexing
        let mut lexer = Lexer::new(program);
        let tokens = lexer.lex_all();

        // Parsing  
        let mut parser = Parser::new(tokens);
        let ast = match parser.parse() {
            Ok(ast) => ast,
            Err(e) => return Ok(e),
        };

        // Semantic Analysis
        let mut analyzer = SemanticVisitor::new();
        for stmt in &ast {
            if let Err(e) = analyzer.visit_stmt(stmt) {
                return Ok(e);
            }
        }

        // Interpretation
        let mut interpreter = InterpreterVisitor::new();
        for stmt in &ast {
            if let Err(e) = interpreter.visit_stmt(stmt) {
                return Ok(e);
            }
        }

        Err("Expected error but program succeeded".to_string())
    }

    /// Test cascading errors in complex nested structures
    #[test]
    fn test_nested_structure_errors() {
        // Array of structs with invalid field access in loop
        let program = r#"
            struct Point { x: int, y: int }
            let points = [Point { x: 1, y: 2 }, Point { x: 3, y: 4 }];
            for p in points {
                println(p.z);  // Field 'z' doesn't exist
            }
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        assert!(error.to_string().contains("no field 'z'") || 
                error.to_string().contains("field"));
    }

    /// Test error propagation through method calls
    #[test]
    fn test_method_call_error_propagation() {
        let program = r#"
            struct Calculator { value: int }
            
            impl Calculator {
                fn add(self, x: int) -> int {
                    self.value + x
                }
                
                fn divide(self, x: int) -> int {
                    self.value / x  // Division by zero potential
                }
            }
            
            let calc = Calculator { value: 10 };
            let result = calc.divide(0);  // Should cause runtime error
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        assert!(error.to_string().contains("division by zero") ||
                error.to_string().contains("Division by zero"));
    }

    /// Test compound assignment errors with complex targets
    #[test] 
    fn test_compound_assignment_complex_errors() {
        // Type error in compound assignment with array element
        let program = r#"
            let numbers = [1, 2, 3];
            numbers[0] += "string";  // Type mismatch
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        assert!(error.to_string().contains("type") && 
                (error.to_string().contains("mismatch") || 
                 error.to_string().contains("numeric")));
    }

    /// Test array bounds errors in complex expressions
    #[test]
    fn test_array_bounds_in_expressions() {
        let program = r#"
            let arr = [1, 2, 3];
            let index = 5;
            let result = arr[index] + arr[index - 1] * 2;  // Multiple bounds violations
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        assert!(error.to_string().contains("bounds") || 
                error.to_string().contains("index"));
    }

    /// Test enum pattern matching with invalid variants
    #[test]
    fn test_enum_pattern_matching_errors() {
        let program = r#"
            enum Status { Ok(int), Error(string) }
            
            let status = Status::Ok(42);
            match status {
                Status::Ok(x) => x + 1,
                Status::Invalid(msg) => 0,  // Invalid variant
            }
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        // Could be parse error (Invalid variant) or semantic error
        assert!(error.to_string().contains("Invalid") || 
                error.to_string().contains("variant") ||
                error.to_string().contains("not found"));
    }

    /// Test function call errors with wrong argument types
    #[test]
    fn test_function_argument_type_cascade() {
        let program = r#"
            fn calculate(x: int, y: int) -> int {
                x * y + 10
            }
            
            fn main() -> int {
                let a = "hello";
                let b = 5;
                calculate(a, b)  // Type error: string instead of int
            }
            
            main()
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        assert!(error.to_string().contains("type mismatch") ||
                error.to_string().contains("expected int"));
    }

    /// Test struct instantiation with missing or wrong fields
    #[test]
    fn test_struct_instantiation_errors() {
        let program = r#"
            struct Person {
                name: string,
                age: int
            }
            
            let person = Person {
                name: "Alice",
                height: 175,  // Wrong field name
                age: "thirty"  // Wrong type
            };
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        // Should catch either missing 'age' or unknown 'height' or type mismatch
        assert!(error.to_string().contains("field") ||
                error.to_string().contains("type") ||
                error.to_string().contains("name"));
    }

    /// Test deeply nested expression errors
    #[test]
    fn test_deeply_nested_expression_errors() {
        let program = r#"
            struct Point { x: int, y: int }
            let p = Point { x: 1, y: 2 };
            
            // Deeply nested expression with error at the end
            let result = ((((p.x + p.y) * 2) + 5) / (p.x - p.x)) + p.z; // Division by zero + invalid field
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        // Should catch either division by zero or invalid field access
        assert!(error.to_string().contains("division by zero") ||
                error.to_string().contains("field") ||
                error.to_string().contains("'z'"));
    }

    /// Test control flow with error conditions
    #[test]
    fn test_control_flow_error_scenarios() {
        let program = r#"
            let numbers = [1, 2, 3, 4, 5];
            
            for i in 0..10 {
                if i < 3 {
                    println(numbers[i]);
                } else {
                    println(numbers[i]);  // This will cause bounds error when i >= 5
                }
            }
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        assert!(error.to_string().contains("bounds") ||
                error.to_string().contains("index"));
    }

    /// Test assignment to invalid targets
    #[test]
    fn test_invalid_assignment_targets() {
        let program = r#"
            fn get_value() -> int { 42 }
            
            // Try to assign to function call result
            get_value() = 10;
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        assert!(error.to_string().contains("Invalid assignment") ||
                error.to_string().contains("target") ||
                error.to_string().contains("expression"));
    }

    /// Test recursive function with error conditions
    #[test]
    fn test_recursive_function_errors() {
        let program = r#"
            fn factorial(n: int) -> int {
                if n == 0 {
                    return undefined_variable;  // Error in base case
                } else {
                    return n * factorial(n - 1);
                }
            }
            
            factorial(5)
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        assert!(error.to_string().contains("undefined") ||
                error.to_string().contains("Undefined"));
    }

    /// Test mixed type operations in complex expressions
    #[test]
    fn test_mixed_type_operation_errors() {
        let program = r#"
            let number = 42;
            let text = "hello";
            let flag = true;
            
            // Complex expression mixing incompatible types
            let result = (number + text.length()) * (flag + 1) / number;
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        // Should catch type errors - strings don't have length(), can't add bool + int
        assert!(error.to_string().contains("type") ||
                error.to_string().contains("method") ||
                error.to_string().contains("field") ||
                error.to_string().contains("no field") ||
                error.to_string().contains("not a struct instance"));
    }

    /// Test error handling in array operations
    #[test]
    fn test_array_operation_errors() {
        let program = r#"
            let matrix = [[1, 2], [3, 4]];
            
            // Try to access non-existent nested element
            let value = matrix[1][5] + matrix[2][0];  // Multiple bounds errors
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        assert!(error.to_string().contains("bounds") ||
                error.to_string().contains("index"));
    }

    /// Test match expression with type errors
    #[test]
    fn test_match_expression_type_errors() {
        let program = r#"
            enum Result { Success(int), Failure(string) }
            
            let result = Result::Success(42);
            
            match result {
                Result::Success(x) => x + "string",  // Type error: int + string
                Result::Failure(msg) => msg.length(), // Method error: string has no length()
            }
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        assert!(error.to_string().contains("type") ||
                error.to_string().contains("method") ||
                error.to_string().contains("field"));
    }

    /// Test break/continue in wrong contexts with other errors
    #[test]
    fn test_control_flow_context_errors() {
        let program = r#"
            fn process_data() -> int {
                if true {
                    break;  // break outside loop
                }
                return undefined_var;  // Also an undefined variable
            }
            
            process_data()
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        // Should catch either break/continue error or undefined variable
        assert!(error.to_string().contains("break") ||
                error.to_string().contains("continue") ||
                error.to_string().contains("undefined") ||
                error.to_string().contains("loop"));
    }

    /// Test complex struct and enum combinations with errors
    #[test]
    fn test_complex_data_structure_errors() {
        let program = r#"
            struct Config {
                name: string,
                values: array<int>
            }
            
            enum Settings {
                Basic(Config),
                Advanced(Config, int)
            }
            
            let config = Config {
                name: "test",
                values: [1, 2, 3]
            };
            
            let settings = Settings::Basic(config);
            
            match settings {
                Settings::Basic(cfg) => {
                    // Try to access invalid array index and wrong field
                    cfg.values[10] + cfg.invalid_field
                },
                Settings::Advanced(cfg, level) => {
                    cfg.values[0] + level
                }
            }
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        assert!(error.to_string().contains("bounds") ||
                error.to_string().contains("field") ||
                error.to_string().contains("index"));
    }

    /// Test transpiler error scenarios (if transpiler is available)
    #[test]
    fn test_transpiler_integration_errors() {
        // Test a program with a clear semantic error
        let program = r#"
            fn add_numbers(x: int, y: int) -> int {
                x + y
            }
            
            add_numbers("hello", 42)  // Type error: string instead of int
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        assert!(error.to_string().contains("type mismatch") ||
                error.to_string().contains("expected int") ||
                error.to_string().contains("found string") ||
                error.to_string().contains("type"));
    }

    /// Test edge case with empty structures and operations
    #[test]
    fn test_edge_case_empty_structures() {
        let program = r#"
            let empty_array = [];
            let first_element = empty_array[0];  // Bounds error on empty array
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        assert!(error.to_string().contains("bounds") ||
                error.to_string().contains("index") ||
                error.to_string().contains("empty") ||
                error.to_string().contains("out of bounds"));
    }

    /// Test error reporting quality with complex nested calls
    #[test]
    fn test_error_reporting_nested_calls() {
        let program = r#"
            fn inner(x: int) -> int {
                x / 0  // Division by zero
            }
            
            fn middle(y: int) -> int {
                inner(y) + 5
            }
            
            fn outer() -> int {
                middle(10)
            }
            
            outer()
        "#;
        
        let error = run_program_expect_error(program).unwrap();
        assert!(error.to_string().contains("division by zero") ||
                error.to_string().contains("Division by zero"));
        // Error should be clear even through nested calls
    }
}