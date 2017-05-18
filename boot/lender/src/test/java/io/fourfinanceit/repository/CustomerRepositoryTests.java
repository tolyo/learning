package io.fourfinanceit.repository;

import io.fourfinanceit.HomeworkApplication;
import io.fourfinanceit.util.test.TestHelpers;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = HomeworkApplication.class)
public class CustomerRepositoryTests {

    @Autowired
    CustomerRepository customerRepository;

    @Test
    public void testFindByNumber() {
        // expect customer to be found
        assertThat(customerRepository.findByNumber(TestHelpers.CUSTOMER_NUMBER).getId(), notNullValue());
    }
}
