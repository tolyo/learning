package io.fourfinanceit.util.test;


import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

abstract public class TestHelpers {

    public static final String CUSTOMER_NUMBER = "111111";
    public static final BigDecimal LOAN_AMOUNT = new BigDecimal(100.00);

    public static final Date TODAY = new Date();
    public static final Date TOMORROW = Date.from(LocalDate.now().plusDays(1).atTime(23, 59).atZone(ZoneId.systemDefault()).toInstant());
    public static final Date YESTERDAY = Date.from(LocalDate.now().minusDays(1).atTime(23, 59).atZone(ZoneId.systemDefault()).toInstant());
    public static final Date WEEK_FROM_NOW = Date.from(LocalDate.now().plusDays(7).atTime(23, 59).atZone(ZoneId.systemDefault()).toInstant());
    public static final Date TWO_WEEKs_FROM_NOW = Date.from(LocalDate.now().plusDays(14).atTime(23, 59).atZone(ZoneId.systemDefault()).toInstant());
    public static final Date WEEK_AGO = Date.from(LocalDate.now().minusDays(7).atTime(23, 59).atZone(ZoneId.systemDefault()).toInstant());

}
