package chp2;

import java.util.Arrays;

/**
 * Bubble sort implementation
 */
public class BubbleSort {

    public static void main(String[] args) {
        int[] a = {2, 4, 7, 3, 9, 1, 5, 6};
        System.out.println(Arrays.toString(a));
        int[] b = bubbleSort(a);
        System.out.println(Arrays.toString(b));

    }

    private static int[] bubbleSort(int[] a) {
        while (true) {
            boolean swap = false;
            for (int x = 0; x < a.length - 1; x++) {
                if (a[x] > a[x + 1]) {
                    int b = a[x];
                    a[x] = a[x + 1];
                    a[x + 1] = b;
                    swap = true;
                }
            }
            if (!swap) break;
        }

        return a;
    }
}
