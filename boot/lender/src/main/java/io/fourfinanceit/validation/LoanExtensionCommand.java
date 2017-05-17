package io.fourfinanceit.validation;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.fourfinanceit.domain.Loan;
import io.fourfinanceit.domain.LoanExtension;
import io.fourfinanceit.repository.LoanRepository;
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

/**
 * Validator for loan extensions
 */
public class LoanExtensionCommand implements Validator {

    private static final Logger log = LoggerFactory.getLogger(LoanExtensionCommand.class);

    @NotNull
    private String number;

    @NotNull
    @JsonSerialize(using=JsonDateSerializer.class)
    @JsonDeserialize(using=JsonDateDeserializer.class)
    private Date endDate;

    public String getNumber() {
        return number;
    }

    public void setNumber(String number) {
        this.number = number;
    }

    public Date getEndDate() {
        return endDate;
    }

    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return LoanExtensionCommand.class.equals(clazz);
    }

    @Override
    public void validate(Object target, Errors errors) {
        Assert.notNull(target, "Target must not be null");
        Assert.notNull(errors, "Errors must not be null");

        // Validate constraints first
        if (errors.hasErrors()) return;

        log.info("Begin custom loan extention command");

        LoanExtensionCommand cmd = (LoanExtensionCommand) target;
        log.info("cmd: " + cmd.toString());

        LoanRepository loanRepository = SpringContext.getApplicationContext().getBean(LoanRepository.class);
        Loan loan = loanRepository.findByNumber(cmd.getNumber());

        if (loan == null) {

        }

        if (!loan.isActive()) {

        }

    }

    public LoanExtension getLoanExtension() {
        LoanExtension loanExtension = new LoanExtension();
        return loanExtension;
    }
}
