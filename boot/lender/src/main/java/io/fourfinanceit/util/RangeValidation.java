package io.fourfinanceit.util;

import org.springframework.util.Assert;

import java.util.concurrent.TimeUnit;

public abstract class RangeValidation {

    public static boolean isDateRangeMinValid(DateRange cmd) {
        Assert.notNull(cmd, "Date range object must not be null");
        if (cmd.getStartDate() == null) return false;
        if (cmd.getEndDate() == null) return false;

        long days = TimeUnit.HOURS.convert(cmd.getEndDate().getTime() - cmd.getStartDate().getTime(), TimeUnit.MILLISECONDS)/24;
        System.out.println(days);
        boolean range = days >= Long.parseLong(SpringEnvironment.get().getProperty("loan.min.period.days"));
        System.out.println(range);
        return range;
    }

    public static boolean isDateRangeValid(DateRange cmd) {
        Assert.notNull(cmd, "Date range object must not be null");
        if (cmd.getStartDate() == null) return false;
        if (cmd.getEndDate() == null) return false;

        return cmd.getStartDate().before(cmd.getEndDate());
    }
}
