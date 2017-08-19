package chp2;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

public class Converter {

    public static void main(String[] args) {
        System.out.println(toBinaryByTwoDivision(254));
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
}
