package chp1;

public class Exercise1_2_1 {

    public static void main(String[] args) {

        double G = 2;
        double mass1 = 2;
        double mass2 = 2;
        double r = 2;
        double force = G * mass1 * mass2 / r * r;
        double force2 = (G * mass1 * mass2) / (r * r);
        System.out.println(force);
        System.out.println(force2);
    }
}
