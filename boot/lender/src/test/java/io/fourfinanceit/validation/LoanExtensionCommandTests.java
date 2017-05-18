package io.fourfinanceit.validation;

import io.fourfinanceit.HomeworkApplication;
import io.fourfinanceit.domain.Loan;
import io.fourfinanceit.repository.CustomerRepository;
import io.fourfinanceit.util.test.TestHelpers;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.Errors;


@RunWith(SpringRunner.class)
@ContextConfiguration(classes = HomeworkApplication.class)
public class LoanExtensionCommandTests {

    LoanExtensionCommand cmd;

    Errors errors;

    @Autowired
    CustomerRepository customerRepository;

    @Before
    public void setup() {
        cmd = new LoanExtensionCommand();
        errors = new BeanPropertyBindingResult(cmd, "cmd");
    }

    @Test
    public void testCustomerNumberValidationConstraints() {
//        // when customer number is not present
//        ValidationUtils.invokeValidator(cmd, cmd, errors);
//
//        // then errors is popullated
//        assertThat(errors.hasErrors(), is(true));
//        assertThat(errors.getFieldError("customerNumber"), notNullValue());
//
//        // when customer number is present
//        cmd = new LoanApplicationCommand();
//        String number = customerRepository.findAll().get(0).getNumber();
//        cmd.setCustomerNumber(number);
//        cmd.setStartDate(new Date());
//        cmd.setEndDate(new Date());
//        errors = new BeanPropertyBindingResult(cmd, "cmd");
//        ValidationUtils.invokeValidator(cmd, cmd, errors);
//
//        // then the error field should be empty
//        assertThat(errors.getFieldError("customerNumber"),  nullValue());


    }

    @Test
    public void testIsValidApplicationAttempts() {
//        // given a valid loan application attempt
//        cmd = new LoanApplicationCommand();
//        cmd.setCustomer(new Customer());
//        cmd.setIp("127.0.0.1");
//        // expect true
//        assertThat(cmd.isValidApplicationAttempt(), is(true));
//
//        // give an invalid loan application attempt
//        cmd = new LoanApplicationCommand();
//        // expect false
//        assertThat(cmd.isValidApplicationAttempt(), is(false));
    }

    public static LoanExtensionCommand getValidLoanExtensionCommand(Loan loan) {
        LoanExtensionCommand cmd = new LoanExtensionCommand();
        cmd.setEndDate(TestHelpers.TWO_WEEKs_FROM_NOW);
        cmd.setNumber(loan.getNumber());
        cmd.setLoan(loan);
        return cmd;
    }
}
