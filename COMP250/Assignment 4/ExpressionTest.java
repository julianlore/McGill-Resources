/**********************************************************************************************************************
 *                          Test Method for COMP250 A4 Q1 Winter 2017                                                 *
 *                                          Julian Lore                                                               *
 *                                                                                                                    *
 **********************************************************************************************************************/
/* Put this file in the same directory as ExpressionTree.java and compile and run this
Tests evaluate method from Q1 and uses differentiate & evaluate method to test differentiate
*/
// Import Javascript to use ScriptEngine to evaluate Strings (DO NOT import in your actual assignment)
import javax.script.*;

public class ExpressionTest {
    public static void main(String[] args) throws ScriptException{ //throws for JS eval
        // Testing evaluate
        // Make an array of Strings for each expression
        String[] exprToEval = {"mult(add(2,x),cos(x))", "add(7,4)", "add(7,x)", "add(x,7)", "minus(7,x)",
                                "minus(x,9)", "minus(3,6)", "mult(x,37)", "mult(12,x)", "sin(x)", "cos(x)", "exp(x)",
                                "exp(cos(mult(x,x)))", "mult(sin(x),mult(cos(x),exp(x)))"};
        String[] expressions = {"(2+x)*cos(x)", "7+4", "7+x", "x+7", "7-x", "x-9", "3-6", "x*37",
                                "12*x", "sin(x)", "cos(x)", "euler(x)", "euler(cos(x*x))",
                                "(sin(x)*cos(x)*euler(x))"};
        String[] derivatives = {"cos(x)-(x+2)*sin(x)", "0", "1", "1", "-1", "1", "0", "37", "12", "cos(x)",
                                "-sin(x)", "euler(x)", "-2*x*sin(x*x)*euler(cos(x*x))",
                                "(0.5)*euler(x)*(sin(2*x)+2*cos(2*x))"};
        boolean failed = false; // boolean that will be set to true if one of the tests failed
        System.out.println("Checking evaluate method and evaluate method after using differentiate.");
        // Loop through each expression
        for (int i=0; i<expressions.length; i++){
            ExpressionTree e = new ExpressionTree(exprToEval[i]); // read from exprToEval
            double x=Math.random()*25; // Generate a random x
            // read from legible expression format, replace x with it's value
            String expression = mathify(expressions[i],x);
            double current = e.evaluate(x);
            double answer = evalStr(expression);
            result(expressions[i], current, answer, x);
            if (!approx(current, answer)){ //if not approx, failed
                failed = true;
            }
            // Now we want to differentiate
            String derived = mathify(derivatives[i],x);
            current = e.differentiate().evaluate(x); //Differentiate then evaluate
            answer = evalStr(derived);
            System.out.println("Derived:");
            result(derivatives[i], current, answer, x);
            if (!approx(current, answer)){
                failed = true;
            }
            System.out.println("----------------------------------------------------------------------------------");
        }
        // out of loop now
        if (failed){
            System.out.println("One of the tests failed.");
        }
        else{
            System.out.println("All tests succeeded.");
        }
    }

    // Helper method to check if current is approximately answer
    public static boolean approx(double current, double answer) {
        // Since floating points aren't extremely precise,
        // we'll use bounds and inequalities to check if the answer
        // is in the range
        double lower = answer * 0.999; // Take off 0.1% from answer
        double upper = answer * 1.001; // Add 0.1% to answer
        // If answer is positive, need lower<current<upper
        if (answer > 0) {
            if (current > lower && current < upper) { //if within bounds
                return true;
            }
        }
        // Answer is negative, need upper<current<lower
        else if (answer < 0) {
            if (current > upper && current < lower) {
                return true;
            }
        }
        // Answer is 0, should be 0
        else if (answer == 0){
            if(current == 0){
                return true;
            }
        }
        // return false if we hit here
        return false;
    }

    // Helper method to print out expressions we'll be testing
    // and whether or not evaluate returns a good value
    public static void result(String expression, double current, double answer, double x) {
        // Print 3 things per equation
        // The equation itself
        // The answer
        // Whether ExpressionTree returns a good answer or not
        System.out.print(expression + " = ");
        System.out.print(answer + ": ");
        System.out.print(approx(current, answer) + " with x = " + x + "\n");
    }

    // Method to replace literal math with library methods
    public static String mathify(String expression, double x){
        return expression.replaceAll("x",String.valueOf(x)) // Will read from legible math equation
                .replaceAll("cos", "Math.cos") // and replace x and math operators by literals
                .replaceAll("sin", "Math.sin")
                .replaceAll("euler", "Math.exp");
    }

    // Method to evaluate a String
    public static double evalStr(String expression) throws ScriptException{ //throws for JS evaluation
        // Declare a ScriptEngine to evaluate expressions
        ScriptEngineManager manager = new ScriptEngineManager();
        ScriptEngine engine = manager.getEngineByName("js");
        double answer=0;
        Object temp = engine.eval(expression); // temporary Store result of evaluation to check if double or int
        if (temp instanceof Integer){ //if int, cast to int and assign to double
            answer = (int) temp;
        }
        else if (temp instanceof Double){ //if double ,cast to double and assign to double
            answer = (double) temp;
        }
        return answer;
    }
}
