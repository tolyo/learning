package chp2;

import java.util.ArrayList;
import java.util.Collections;

public class Converter {

    public static void main(String[] args) {
        //System.out.println(toBinaryByTwoDivision(254));
        System.out.println(toBinaryBySubtractionMethod(247));
    }

    /**
     * Method 1  Repeated-division-by-2 method
     * 1.	 Divide the dividend, that is, the decimal number by two
     * and obtain the quotient and remainder.
     * 2.	 Divide the quotient by two and obtain the new quotient
     * and remainder.
     * 3.	 Repeat step 2 until the quotient is equal to zero (0).
     * 4.	 The first remainder produced is the least significant bit
     * (LSB) in the binary number and the last remainder is
     * the most significant bit (MSB). Accordingly, the binary
     * number is then written (from left to right) with the MSB
     * occurring first (list the remainder values in reverse
     * order). This is the binary equivalent.
     * @return String
     */
    public static String toBinaryByTwoDivision(Integer dividend) {
        ArrayList<CharSequence> binaries = new ArrayList();
        Integer remainder = dividend % 2;
        binaries.add(remainder.toString());
        Integer quotient  = dividend / 2;
        while (quotient != 0) {
            remainder = quotient % 2;
            binaries.add(remainder.toString());
            quotient =  quotient / 2;
        }
        Collections.reverse(binaries);
        String binaryString = String.join("", binaries);
        return binaryString;
    }

    /**
     * Method 2  Power-of-2-subtraction method
     * 1. Let D be the number that has to be converted from
     *    decimal to binary.
     *
     * 2.
     *  (a)	Find the largest power of two that is less than or
     *  equal to D. Let this equal P.
     *  (b)	If |D| ≥ P, subtract P from D, obtain a result which is
     *  a decimal number. Put 1 in the digit position where
     *  the weighting factor is P.
     *  (c)	Otherwise, if |D| < |P|, put 0 in the corresponding
     *  weighting factor column.
     *
     * 3.	 Repeat step 2 with D = remainder decimal number until
     * D = 0, or |D| < |P|.
     */
    public static String toBinaryBySubtractionMethod(Integer D) {
        String binaryString = "";
        int power = largestPowerOfTwo(D);
        // check for presence of powers of 2 in n, from largest to smallest
        while (power > 0) {
            // power is not present in n
            if (D < power) {
                binaryString = binaryString + "0";
            }
            // power is present in n, so subtract power from n
            else {
                binaryString = binaryString + "1";
                D -= power;
            }
            // next smallest power of 2
            power /= 2;
        }
        return binaryString;
    }

    public static Integer largestPowerOfTwo(Integer D) {
        int power = 1;
        while (power <= D/2) {
            power *= 2;
        }
        return power;
    }
}
