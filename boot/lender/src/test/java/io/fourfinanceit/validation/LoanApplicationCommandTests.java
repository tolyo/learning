package io.fourfinanceit.validation;


import io.fourfinanceit.domain.Customer;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.junit4.SpringRunner;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

@RunWith(SpringRunner.class)
public class LoanApplicationCommandTests {

    LoanApplicationCommand cmd;

    @Test
    public void testIsValidApplicationAttempts() {
        // given a valid loan application attempt
        cmd = new LoanApplicationCommand();
        cmd.setCustomer(new Customer());
        cmd.setIp("127.0.0.1");
        // expect true
        assertThat(cmd.isValidApplicationAttempt(), is(true));

        // give an invalid loan application attempt
        cmd = new LoanApplicationCommand();
        // expect false
        assertThat(cmd.isValidApplicationAttempt(), is(false));
    }
}
