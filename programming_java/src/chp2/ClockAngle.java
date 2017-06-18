package chp2;

/**
 *
 */
public class ClockAngle {

    public static void main(String[] args) {
        System.out.println(getClockAngle(0,0));
        System.out.println(getClockAngle(1,0));
        System.out.println(getClockAngle(15,0));
        System.out.println(getClockAngle(12,30));
        System.out.println(getClockAngle(3,30));

    }

    private static double getClockAngle(double hour, double minute) {
        if (hour > 24 || hour < 0) throw new IllegalArgumentException("Hour invalid");
        if (minute > 60 || minute < 0) throw new IllegalArgumentException("Hour invalid");
        hour = hour % 12;
        double minuteOffset = minute * 6;
        double hourOffset = (30 * hour) + (minute / 60  * 30);
        return Math.abs(hourOffset - minuteOffset);
    }
}
