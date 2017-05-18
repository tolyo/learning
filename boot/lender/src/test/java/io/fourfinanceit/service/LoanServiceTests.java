package io.fourfinanceit.service;

import io.fourfinanceit.domain.Customer;
import io.fourfinanceit.domain.Loan;
import io.fourfinanceit.domain.LoanExtension;
import io.fourfinanceit.repository.CustomerRepository;
import io.fourfinanceit.repository.LoanRepository;
import io.fourfinanceit.validation.LoanApplicationCommand;
import io.fourfinanceit.validation.LoanApplicationCommandTests;
import io.fourfinanceit.validation.LoanExtensionCommand;
import io.fourfinanceit.validation.LoanExtensionCommandTests;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;

import static io.fourfinanceit.util.test.TestHelpers.*;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringBootTest
public class LoanServiceTests {

    Customer customer;
    Errors errors;
    Loan loan;
    LoanExtension loanExtension;
    LoanApplicationCommand loanApplicationCommand;
    LoanExtensionCommand loanExtensionCommand;

    @Autowired
    LoanService loanService;

    @Autowired
    LoanRepository loanRepository;

    @Autowired
    CustomerRepository customerRepository;

    public void setup() {
        assertThat(this.loanService, notNullValue());
    }

    @Test
    public void testCreateLoan() {
        errors = new BeanPropertyBindingResult(loanApplicationCommand, "cmd");

        // give a valid command object
        customer = customerRepository.findByNumber(CUSTOMER_NUMBER);
        assertThat(customer.getId(), notNullValue());
        loanApplicationCommand = LoanApplicationCommandTests.getValidLoanApplicationCommand(customer);

        ValidationUtils.invokeValidator(loanApplicationCommand, loanApplicationCommand, errors);
        assertThat(errors.hasErrors(), is(false));

        // a loan should be saved
        assertThat(loanService.createLoan(loanApplicationCommand).getId(), notNullValue());
    }

    @Test
    public void testExtendLoan() {
        errors = new BeanPropertyBindingResult(loanExtensionCommand, "cmd");
        // given a valid command object
        loan = loanRepository.findAll().get(0);
        assertThat(loan.getId(), notNullValue());
        assertThat(loan.getEndDate().toInstant(), is(TOMORROW.toInstant()));

        loanExtensionCommand = LoanExtensionCommandTests.getValidLoanExtensionCommand(loan);
        ValidationUtils.invokeValidator(loanExtensionCommand, loanExtensionCommand, errors);
        assertThat(errors.hasErrors(), is(false));
        loanExtension = loanService.extendLoan(loanExtensionCommand);

        // a loan extension should be saved
        assertThat(loanExtension.getId(), notNullValue());
        // and loan should be updated
        assertThat(loanRepository.findAll().get(0).getEndDate().toInstant(), is(TWO_WEEKs_FROM_NOW.toInstant()));
    }


}
