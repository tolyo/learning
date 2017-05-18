package io.fourfinanceit.validation;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.fourfinanceit.domain.Customer;
import io.fourfinanceit.domain.Loan;
import io.fourfinanceit.domain.LoanApplicationAttempt;
import io.fourfinanceit.repository.CustomerRepository;
import io.fourfinanceit.repository.LoanApplicationAttemptRepository;
import io.fourfinanceit.util.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.Assert;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import static io.fourfinanceit.util.RangeValidation.isDateRangeMinValid;
import static io.fourfinanceit.util.RangeValidation.isDateRangeValid;

/**
 * Class for validating loan applications
 */
public class LoanApplicationCommand implements Serializable, Validator, DateRange {

    private static final Logger log = LoggerFactory.getLogger(LoanApplicationCommand.class);

    // Validated fields
    private String customerNumber;

    @NotNull
    @Min(0)
    private BigDecimal amount;

    /**
     * Accurate regex to check for an IP address, allowing leading zeros
     */
    @NotNull
    @Pattern(regexp= RegexUtils.IP_REGEX, message = "invalid ip")
    private String ip;

    @NotNull
    @JsonSerialize(using=JsonDateSerializer.class)
    @JsonDeserialize(using=JsonDateDeserializer.class)
    private Date startDate;

    @NotNull
    @JsonSerialize(using=JsonDateSerializer.class)
    @JsonDeserialize(using=JsonDateDeserializer.class)
    private Date endDate;

    // Popullated fields
    private Customer customer;

    public String getCustomerNumber() {
        return customerNumber;
    }

    public void setCustomerNumber(String customerNumber) {
        this.customerNumber = customerNumber;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public void setAmount(BigDecimal amount) {
        this.amount = amount;
    }

    @Override
    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    @Override
    public Date getEndDate() {
        return endDate;
    }

    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    public Customer getCustomer() {
        return customer;
    }

    public void setCustomer(Customer customer) {
        this.customer = customer;
    }

    public String getIp() {
        return ip;
    }

    public void setIp(String ip) {
        this.ip = ip;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LoanApplicationCommand)) return false;

        LoanApplicationCommand that = (LoanApplicationCommand) o;

        if (customerNumber != null ? !customerNumber.equals(that.customerNumber) : that.customerNumber != null) return false;
        if (amount != null ? !amount.equals(that.amount) : that.amount != null) return false;
        if (startDate != null ? !startDate.equals(that.startDate) : that.startDate != null) return false;
        return endDate != null ? endDate.equals(that.endDate) : that.endDate == null;

    }

    @Override
    public int hashCode() {
        int result = customerNumber != null ? customerNumber.hashCode() : 0;
        result = 31 * result + (amount != null ? amount.hashCode() : 0);
        result = 31 * result + (startDate != null ? startDate.hashCode() : 0);
        result = 31 * result + (endDate != null ? endDate.hashCode() : 0);
        return result;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return this.getClass().equals(clazz);
    }

    @Override
    public void validate(Object target, Errors errors) {
        Assert.notNull(target, "Target must not be null");
        Assert.notNull(errors, "Errors must not be null");

        // Validate constraints first
        if (errors.hasErrors()) return;

        log.info("Begin custom loan application command");

        LoanApplicationCommand cmd = (LoanApplicationCommand) target;
        log.info("cmd: " + cmd.toString());

        // Validate customer number
        CustomerRepository customerRepository = SpringContext.get().getBean(CustomerRepository.class);
        Customer customer = customerRepository.findByNumber(cmd.getCustomerNumber());
        if (customer == null) {
            errors.rejectValue("customerNumber", "", "invalid number");
            return;
        } else cmd.setCustomer(customer);

        log.info("Customer" + customer.toString());


        // Validate dates
        if (!isDateRangeValid(cmd)) {
            errors.rejectValue("startDate", "", "invalid start date");
            return;
        };

        if (!isDateRangeMinValid(cmd)) {
            errors.rejectValue("endDate", "", "invalid end date");
            return;
        };

        // Validate ip address count
        Integer maxAttemptsLimit = Integer.valueOf(SpringEnvironment.get().getProperty("loan.max.attempts"));
        Integer currentAttemptsCount = getCurrentAttemptsCount(cmd);//

        // Risk analysis for IP attempts
        if (currentAttemptsCount >= maxAttemptsLimit) {
            errors.rejectValue("customer", "", "attempts exceeded");
            return;
        }

        // Risk analysis for amount and time
        if ((cmd.getAmount().compareTo(new BigDecimal(SpringEnvironment.get().getProperty("loan.max.limit")))) == 0 &&
            isRiskTime()) {
            errors.rejectValue("customer", "", "risk limits exceeded");
            return;
        }
    }

    private static Integer getCurrentAttemptsCount(LoanApplicationCommand cmd) {
        Assert.notNull(cmd.getCustomer(), "Customer must not be null");
        LoanApplicationAttemptRepository loanApplicationAttemptReposity =
                SpringContext.get().getBean(LoanApplicationAttemptRepository.class);

        final int RIGA_TIME = Integer.parseInt(SpringEnvironment.get().getProperty("utc.offset"));
        log.info("Customer " + cmd.getCustomer().toString());
        log.info("Time " + Date.from(LocalDateTime.now().withHour(0).withMinute(0).withSecond(0).toInstant(ZoneOffset.ofHours(RIGA_TIME))).toString());

        // reached max applications (e.g. 3) per day from a single IP
        List<LoanApplicationAttempt> loanApplicationAttempts =
                loanApplicationAttemptReposity.findByCustomer(cmd.getCustomer())
                    .stream()
                    .filter(x ->
                            x.getIp().contentEquals(cmd.getIp()) &&
                            x.getCreated().after(Date.from(LocalDateTime.now().withHour(0).withMinute(0).withSecond(0).toInstant(ZoneOffset.ofHours(RIGA_TIME)))))
                    .collect(Collectors.toList());

        log.info("Found " + Integer.toString(loanApplicationAttempts.size()));
        return Integer.valueOf(loanApplicationAttempts.size());
    }

    private static boolean isRiskTime() {
        RiskRange riskRange = SpringContext.get().getBean(RiskRange.class);
        Calendar rightNow = Calendar.getInstance();
        int hour = rightNow.get(Calendar.HOUR_OF_DAY);
        int start = riskRange.getStartHour();
        int end = riskRange.getEndHour();

        boolean riskTime = (start <= hour && hour <= end);
        log.info(Boolean.toString(riskTime));
        return(start <= hour && hour <= end);
    }

    public Loan getLoan() {
        return new Loan(this);
    }

    @Override
    public String toString() {
        return "LoanApplicationCommand{" +
                "customerNumber='" + customerNumber + '\'' +
                ", amount=" + amount +
                ", startDate=" + startDate +
                ", endDate=" + endDate +
                '}';
    }

    public LoanApplicationAttempt getLoanApplicationAttempt() {
        return new LoanApplicationAttempt(this);
    }

    /**
     * Verify the application attempt is sufficient persistence.
     * @return
     */
    public boolean isValidApplicationAttempt() {
        return (this.getCustomer() != null && this.getIp() != null);
    }

}
