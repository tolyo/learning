package io.fourfinanceit.validation;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.fourfinanceit.domain.Loan;
import io.fourfinanceit.domain.LoanExtension;
import io.fourfinanceit.repository.LoanRepository;
import io.fourfinanceit.util.DateRange;
import io.fourfinanceit.util.JsonDateDeserializer;
import io.fourfinanceit.util.JsonDateSerializer;
import io.fourfinanceit.util.SpringContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.Assert;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import javax.validation.constraints.NotNull;
import java.util.Date;

import static io.fourfinanceit.util.RangeValidation.isDateRangeMinValid;
import static io.fourfinanceit.util.RangeValidation.isDateRangeValid;

/**
 * Validator for loan extensions
 */
public class LoanExtensionCommand implements Validator, DateRange {

    private static final Logger log = LoggerFactory.getLogger(LoanExtensionCommand.class);

    @NotNull
    private String number;

    @NotNull
    @JsonSerialize(using=JsonDateSerializer.class)
    @JsonDeserialize(using=JsonDateDeserializer.class)
    private Date endDate;

    Loan loan;

    public String getNumber() {
        return number;
    }

    public void setNumber(String number) {
        this.number = number;
    }

    @Override
    public Date getStartDate() {
        return this.loan.getEndDate();
    }

    @Override
    public Date getEndDate() {
        return endDate;
    }

    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    public Loan getLoan() {
        return loan;
    }

    public void setLoan(Loan loan) {
        this.loan = loan;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return LoanExtensionCommand.class.equals(clazz);
    }

    @Override
    public String toString() {
        return "LoanExtensionCommand{" +
                "number='" + number + '\'' +
                ", startDate=" + this.loan.getEndDate().toString() +
                ", endDate=" + endDate +
                '}';
    }

    @Override
    public void validate(Object target, Errors errors) {
        Assert.notNull(target, "Target must not be null");
        Assert.notNull(errors, "Errors must not be null");

        // Validate constraints first
        if (errors.hasErrors()) return;

        log.info("Begin custom loan extension command");

        LoanExtensionCommand cmd = (LoanExtensionCommand) target;
        log.info("cmd: " + cmd.toString());

        LoanRepository loanRepository = SpringContext.get().getBean(LoanRepository.class);
        Loan loan = loanRepository.findByNumber(cmd.getNumber());

        if (loan == null) {
            errors.rejectValue("number", "", "invalid number");
            return;
        } else cmd.setLoan(loan);

        if (!loan.isActive()) {
            errors.rejectValue("loan", "", "expired loan");
            return;
        }

        // Default extension start date from loan end date
        log.info("cmd: " + cmd.toString());

        // Validate dates
        if (!isDateRangeValid(cmd)) {
            errors.rejectValue("startDate", "", "invalid start date");
            return;
        };

        if (!isDateRangeMinValid(cmd)) {
            errors.rejectValue("endDate", "", "invalid end date");
            return;
        };

    }

    public LoanExtension getLoanExtension() {
        return new LoanExtension(this);
    }
}
