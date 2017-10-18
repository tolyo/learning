package chp2;

import java.util.Arrays;
import java.util.List;

/**
 * Given an array of integers greater than zero,
 * find if it is possible to split it in two (without reordering the elements),
 * such that the sum of the two resulting arrays is the same. Print the resulting arrays.
 * e.g. [1,2,3,3,2,1] it can it be split into two arrays [1,2,3] and [3,2,1] where the sum of each is equal.
 */
public class ArrayBrainTeaser {

    public static void main(String[] args) {
        List<Integer> data = Arrays.asList(2,1,2,3,3,2,1,2);
        boolean test = testSplit(data);
        System.out.println(test);
    }

    private static boolean testSplit(List<Integer> data) {
        Integer total = data.stream().reduce(0, (a, b) -> a + b);
        Integer acc = 0;
        for (Integer j : data) {
            acc += j;
            total -= j;
            if (acc.equals(total)) return true;
        }
        return false;
    }
}
