package io.fourfinanceit.repository;

import io.fourfinanceit.HomeworkApplication;
import io.fourfinanceit.domain.Customer;
import io.fourfinanceit.domain.Loan;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.math.BigDecimal;

import static io.fourfinanceit.util.test.TestHelpers.TODAY;
import static io.fourfinanceit.util.test.TestHelpers.TOMORROW;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = HomeworkApplication.class)
public class LoanRepositoryTests {

    Loan loan;

    @Autowired
    LoanRepository loanRepository;

    @Autowired
    CustomerRepository customerRepository;

    @Test
    public void testSaveLoan() {
        // given a customer
        Customer customer = customerRepository.findAll().get(0);
        assertThat(customer, notNullValue());
        // and a valid loan
        loan = new Loan();
        loan.setStartDate(TODAY);
        loan.setEndDate(TOMORROW);
        loan.setAmount(new BigDecimal(100.00));
        loan.setCustomer(customer);

        // loan should be saved
        loan = loanRepository.save(loan);
        assertThat(loan.getId(), notNullValue());
    }
}
