package assignment_0;

/**
 * Write a program Ordered.java that takes three int command-line arguments,
 * x, y, and z. Define a boolean variable whose value is true if the three
 * values are either in strictly ascending order (x < y < z) or in strictly
 * descending order (x > y > z), and false otherwise.
 *
 * Extra credit without using the comparison operators (<, <=, >, and >=)
 */
public class Ordered {

    public static void main(String[] args) {
        int a = Integer.parseInt(args[0]);
        int b = Integer.parseInt(args[1]);
        int c = Integer.parseInt(args[2]);

        boolean asc = lessThan(a, b) && lessThan(b, c);
        boolean desc = greaterThan(a, b) && greaterThan(b, c);
        boolean res = asc || desc;

        System.out.println(res);
    }

    /**
     * True if a is less than b
     * @param a
     * @param b
     * @return result
     */
    private static boolean lessThan(int a, int b) {
        return b - a == Math.abs(b - a);
    }

    /**
     * True if a is greater than b
     * @param a
     * @param b
     * @return result
     */
    private static boolean greaterThan(int a, int b) {
        return a - b == Math.abs(a - b);
    }
}
