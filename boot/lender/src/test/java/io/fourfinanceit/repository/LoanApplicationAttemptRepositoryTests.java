package io.fourfinanceit.repository;

import io.fourfinanceit.HomeworkApplication;
import io.fourfinanceit.domain.Customer;
import io.fourfinanceit.domain.LoanApplicationAttempt;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = HomeworkApplication.class)
public class LoanApplicationAttemptRepositoryTests {

    LoanApplicationAttempt loanApplicationAttempt;

    @Autowired
    LoanApplicationAttemptRepository loanApplicationAttemptRepository;

    @Autowired
    CustomerRepository customerRepository;

    @Test
    public void testSaveApplicationAttempt() {
        // given a customer
        Customer customer = customerRepository.findAll().get(0);
        assertThat(customer, notNullValue());

        // and a valid application attempt
        loanApplicationAttempt = new LoanApplicationAttempt();
        loanApplicationAttempt.setIp("127.0.0.1");
        loanApplicationAttempt.setCustomer(customer);

        // application attempt should be saved
        loanApplicationAttempt = loanApplicationAttemptRepository.save(loanApplicationAttempt);
        assertThat(loanApplicationAttempt.getId(), notNullValue());
    }

    @Test
    public void testFindByCustomer() {
        // given a customer
        Customer customer = customerRepository.findAll().get(0);
        assertThat(customer, notNullValue());

        // application attempt should be found
        int size = loanApplicationAttemptRepository.findByCustomer(customer).size();
        assertThat(size, not(0));
    }
}
