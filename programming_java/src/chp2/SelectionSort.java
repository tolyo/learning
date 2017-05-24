package chp2;

import java.util.Arrays;

public class SelectionSort {

    public static void main(String[] args) {
        int[] a = {2, 4, 7, 3, 9, 1, 5, 6};
        System.out.println(Arrays.toString(a));
        int[] b = selectionSort(a);
        System.out.println(Arrays.toString(b));
    }

    private static int[] selectionSort(int[] a) {
        for (int x = 0; x < a.length; x++) {
            int smallest = x;
            for (int j = x; j < a.length; j++) {
                if (a[j] < a[smallest]) {
                    smallest = j;
                }
            }
            if (smallest == x) {
                continue;
            } else {
              int b = a[x];
              a[x] = a[smallest];
              a[smallest] = b;
            }
        }
        return a;
    }
}
