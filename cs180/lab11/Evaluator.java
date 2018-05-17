import java.util.Stack;

public class Evaluator {

	private Stack<Integer> stack;
	private String[] RPN;

	public Evaluator(String[] RPN) {
		this.RPN = RPN;
		stack = new Stack<Integer>();
	}

	private int evaluate() {
		try {
			for (int i = 0; i < RPN.length; i++) {
				String s = RPN[i];

				switch (s) {
				case "+":
					stack.push(stack.pop() + stack.pop());
					break;
				case "-":
					int first = stack.pop();
					stack.push(stack.pop() - first);
					break;
				case "*":
					stack.push(stack.pop() * stack.pop());
					break;
				case "x":
					stack.push(stack.pop() * stack.pop());
					break;
				case "/": 
					int denom = stack.pop();
					stack.push(stack.pop() / denom);
					break;
				default:
					stack.push(Integer.parseInt(s));
					break;
				}
//				System.out.println(stack.peek());
			}
		} catch (Exception e) {	return 0; }
		return stack.pop();
	}

	public static void main(String[] args) {
		if (args.length == 0) {
			args = new String[] { "5", "1", "2", "+", "4", "*", "+", "3", "-" };
		}
		Evaluator e = new Evaluator(args);
		System.out.println(e.evaluate());
	}
}
