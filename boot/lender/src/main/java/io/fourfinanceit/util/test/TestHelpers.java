package io.fourfinanceit.util.test;


import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

abstract public class TestHelpers {

    public static final String CUSTOMER_NUMBER = "111111";

    public static final Date TODAY = new Date();
    public static final Date TOMORROW = Date.from(LocalDate.now().plusDays(1).atStartOfDay().atZone(ZoneId.systemDefault()).toInstant());
    public static final Date YESTERDAY = Date.from(LocalDate.now().minusDays(1).atStartOfDay().atZone(ZoneId.systemDefault()).toInstant());
    public static final Date WEEK_FROM_NOW = Date.from(LocalDate.now().minusDays(7).atStartOfDay().atZone(ZoneId.systemDefault()).toInstant());
    public static final Date WEEK_AGO = Date.from(LocalDate.now().plusDays(7).atStartOfDay().atZone(ZoneId.systemDefault()).toInstant());

}
