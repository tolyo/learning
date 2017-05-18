package io.fourfinanceit.validation;

import io.fourfinanceit.domain.Customer;
import io.fourfinanceit.repository.CustomerRepository;
import io.fourfinanceit.repository.LoanApplicationAttemptRepository;
import io.fourfinanceit.util.RiskRange;
import io.fourfinanceit.util.test.TestHelpers;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;

import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Date;

import static io.fourfinanceit.util.test.TestHelpers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.core.IsNull.notNullValue;


@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment= SpringBootTest.WebEnvironment.RANDOM_PORT)
public class LoanApplicationCommandTests {

    LoanApplicationCommand cmd;
    Errors errors;
    Customer customer;

    @Autowired
    RiskRange riskRange;

    @Autowired
    CustomerRepository customerRepository;

    @Autowired
    LoanApplicationAttemptRepository loanApplicationAttemptRepository;

    @Before
    public void setup() {
        cmd = new LoanApplicationCommand();
        errors = new BeanPropertyBindingResult(cmd, "cmd");
        customer = customerRepository.findAll().get(0);
        // Tests may be run at night so we need to make sure they always pass
        riskRange.setStartHour(Calendar.getInstance().get(Calendar.HOUR_OF_DAY) + 1);
        riskRange.setEndHour(Calendar.getInstance().get(Calendar.HOUR_OF_DAY) + 4);
    }

    @Test
    public void testCustomerNumberValidationConstraintsInvalid() {
        // when customer number is not present
        ValidationUtils.invokeValidator(cmd, cmd, errors);

        // then errors is populated
        assertThat(errors.hasErrors(), is(true));
        assertThat(errors.getFieldError("customerNumber"), notNullValue());
    }
    public void testCustomerNumberValidationConstraintsValid() {
        // when customer number is present
        cmd = getValidLoanApplicationCommand(customer);
        cmd.setStartDate(new Date());
        cmd.setEndDate(new Date());
        ValidationUtils.invokeValidator(cmd, cmd, errors);

        // then the error field should be empty
        assertThat(errors.getFieldError("customerNumber"),  nullValue());
    }

    @Test
    public void testDatesValidationConstraintsInvalid() {
        // when give invalid start date
        cmd = getValidLoanApplicationCommand(customer);
        cmd.setStartDate(TOMORROW);
        cmd.setEndDate(YESTERDAY);
        ValidationUtils.invokeValidator(cmd, cmd, errors);

        // then errors is populated
        assertThat(errors.getFieldError("startDate"), notNullValue());

        // when given invalid end date
        cmd = getValidLoanApplicationCommand(customer);
        errors = new BeanPropertyBindingResult(cmd, "cmd");
        cmd.setStartDate(TODAY);
        cmd.setEndDate(TOMORROW);

        ValidationUtils.invokeValidator(cmd, cmd, errors);
        // then errors is populated
        assertThat(errors.getFieldError("endDate"),  notNullValue());
    }

    @Test
    public void testDatesValidationConstraintsValid() {
        // when give valid date
        cmd = getValidLoanApplicationCommand(customer);
        cmd.setStartDate(TODAY);
        cmd.setEndDate(WEEK_FROM_NOW);
        ValidationUtils.invokeValidator(cmd, cmd, errors);

        // then errors is empty
        assertThat(errors.getFieldError("startDate"),  nullValue());
        assertThat(errors.getFieldError("endDate"),  nullValue());
    }

    @Test
    public void testRiskTimeConstraintInvalid() {
        // when give risky time range and assuming current time is risky and a maximum amount
        cmd = getValidLoanApplicationCommand(customer);
        riskRange.setStartHour(Calendar.getInstance().get(Calendar.HOUR_OF_DAY) - 1);
        riskRange.setEndHour(Calendar.getInstance().get(Calendar.HOUR_OF_DAY) + 1);
        cmd.setAmount(new BigDecimal(1000.00));
        ValidationUtils.invokeValidator(cmd, cmd, errors);

        // then errors should be populated
        assertThat(errors.getErrorCount(),  is(1));
        assertThat(errors.getFieldError("customer"),  notNullValue());
    }

    @Test
    public void testIpLimitConstraintInvalid() {
        // multiple attempts to take out a loan have been made
        cmd = getValidLoanApplicationCommand(customer);
        loanApplicationAttemptRepository.save(getValidLoanApplicationCommand(customer).getLoanApplicationAttempt());
        loanApplicationAttemptRepository.save(getValidLoanApplicationCommand(customer).getLoanApplicationAttempt());
        loanApplicationAttemptRepository.save(getValidLoanApplicationCommand(customer).getLoanApplicationAttempt());

        ValidationUtils.invokeValidator(cmd, cmd, errors);

        // then errors should be populated
        assertThat(errors.getErrorCount(),  is(1));
        assertThat(errors.getFieldError("customer"),  notNullValue());
    }

    @Test
    public void testIsValidApplicationAttempts() {
        // given a valid loan application attempt
        cmd = getValidLoanApplicationCommand(customer);
        // expect true
        assertThat(cmd.isValidApplicationAttempt(), is(true));

        // give an invalid loan application attempt
        cmd = new LoanApplicationCommand();
        // expect false
        assertThat(cmd.isValidApplicationAttempt(), is(false));
    }

    public static LoanApplicationCommand getValidLoanApplicationCommand(Customer customer) {
        LoanApplicationCommand cmd = new LoanApplicationCommand();
        cmd.setCustomer(customer);
        cmd.setCustomerNumber(customer.getNumber());
        cmd.setAmount(TestHelpers.LOAN_AMOUNT);
        cmd.setIp("127.0.0.1");
        cmd.setStartDate(TestHelpers.TODAY);
        cmd.setEndDate(TestHelpers.WEEK_FROM_NOW);
        return cmd;
    }
}
