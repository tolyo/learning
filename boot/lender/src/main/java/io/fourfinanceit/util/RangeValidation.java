package io.fourfinanceit.util;

import java.time.temporal.ChronoUnit;

public abstract class RangeValidation {

    public static boolean isDateRangeMinValid(DateRange cmd) {
        long days = ChronoUnit.DAYS.between(cmd.getStartDate().toInstant(), cmd.getEndDate().toInstant());
        boolean range = days >= Long.parseLong(SpringEnvironment.get().getProperty("loan.min.period.days"));
        return range;
    }

    public static boolean isDateRangeValid(DateRange cmd) {
        return cmd.getStartDate().before(cmd.getEndDate());
    }
}
