package io.fourfinanceit.validation;

import io.fourfinanceit.domain.Customer;
import io.fourfinanceit.repository.CustomerRepository;
import io.fourfinanceit.util.IpAddressHolder;
import io.fourfinanceit.util.SpringContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.Assert;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * Class for validating loan applications
 */
public class LoanApplicationCommand implements Serializable, Validator {

    private final Logger log = LoggerFactory.getLogger(LoanApplicationCommand.class);

    // Validated fields
    private String customerNumber;

    @NotNull
    @Min(0)
    @Max(100)
    private BigDecimal amount;

    @NotNull
    private Date startDate;

    private String ip;

    @NotNull
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

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

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
        log.info("Begin validate loan application command");
        LoanApplicationCommand cmd = (LoanApplicationCommand) target;

        // Validate customer number
        CustomerRepository customerRepository = SpringContext.getApplicationContext().getBean(CustomerRepository.class);
        Customer customer = customerRepository.findByNumber(cmd.getCustomerNumber());
        if (customer == null) errors.rejectValue("customerNumber", "", "invalid number");
        else this.setCustomer(customer);

        // Validate ip address count
        IpAddressHolder ipAddress = SpringContext.getApplicationContext().getBean(IpAddressHolder.class);

        Assert.notNull(ipAddress, "IpAddress must not be null");

//        LoanApplicationAttemptRepository loanApplicationAttemptReposity =
//                SpringContext.getApplicationContext().getBean(LoanApplicationAttemptRepository.class);

//        loanApplicationAttemptReposity
//                .findByCustomer(customer)
//                .stream()
//                .filter(x -> x.getDateCreated().after(Date.from(LocalDateTime.now().with(LocalTime.ofNanoOfDay(0)).toInstant(ZoneOffset.MIN)));


    }

    @Override
    public String toString() {
        return "LoanApplicationCommand{" +
                "customerName='" + customerNumber + '\'' +
                ", amount=" + amount +
                ", startDate=" + startDate +
                ", endDate=" + endDate +
                '}';
    }
}
