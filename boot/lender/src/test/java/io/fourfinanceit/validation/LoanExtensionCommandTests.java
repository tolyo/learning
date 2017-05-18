package io.fourfinanceit.validation;

import io.fourfinanceit.domain.Loan;
import io.fourfinanceit.repository.CustomerRepository;
import io.fourfinanceit.repository.LoanRepository;
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

import static io.fourfinanceit.util.test.TestHelpers.TOMORROW;
import static io.fourfinanceit.util.test.TestHelpers.YESTERDAY;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.core.IsNull.notNullValue;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment= SpringBootTest.WebEnvironment.RANDOM_PORT)
public class LoanExtensionCommandTests {

    LoanExtensionCommand cmd;
    Loan loan;
    Errors errors;

    @Autowired
    CustomerRepository customerRepository;

    @Autowired
    LoanRepository loanRepository;

    @Before
    public void setup() {
        cmd = new LoanExtensionCommand();
        errors = new BeanPropertyBindingResult(cmd, "cmd");
        loan = loanRepository.findAll().get(0);
    }

    @Test
    public void testCustomerNumberValidationConstraints() {
        // when customer number is not present
        ValidationUtils.invokeValidator(cmd, cmd, errors);

        // then errors is popullated
        assertThat(errors.hasErrors(), is(true));
        assertThat(errors.getFieldError("number"), notNullValue());

        // when customer number is present
        cmd = getValidLoanExtensionCommand(loan);
        errors = new BeanPropertyBindingResult(cmd, "cmd");
        ValidationUtils.invokeValidator(cmd, cmd, errors);

        // then the error field should be empty
        assertThat(errors.getFieldError("number"),  nullValue());
    }

    @Test
    public void testInactiveLoanValidationConstraints() {
        // given a expired loan application attempt
        loan.setEndDate(YESTERDAY);
        loan = loanRepository.save(loan);
        cmd = getValidLoanExtensionCommand(loan);
        ValidationUtils.invokeValidator(cmd, cmd, errors);

        // expect errors to be popullated
        assertThat(errors.getFieldError("loan"), notNullValue());
    }

    @Test
    public void testValidLoanExtension() {
        // given a valid loan\
        loan.setEndDate(TOMORROW);
        loan = loanRepository.save(loan);
        cmd = getValidLoanExtensionCommand(loan);
        ValidationUtils.invokeValidator(cmd, cmd, errors);

        // expect errors to be empty
        assertThat(errors.hasErrors(), is(false));
    }

    public static LoanExtensionCommand getValidLoanExtensionCommand(Loan loan) {
        LoanExtensionCommand cmd = new LoanExtensionCommand();
        cmd.setEndDate(TestHelpers.TWO_WEEKS_FROM_NOW);
        cmd.setNumber(loan.getNumber());
        cmd.setLoan(loan);
        return cmd;
    }
}
