package io.fourfinanceit.domain;


import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.Date;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

@RunWith(SpringRunner.class)
@SpringBootTest
public class LoanTests {

    Loan loan;

    @Test
    public void testIsActive() {
        // give an active loan
        loan = new Loan();
        loan.setEndDate(new Date(new Date().getTime() + (100000)));
        // should be trie
        assertThat(loan.isActive(), is(true));

        // given an inactive loan
        loan.setEndDate(new Date(new Date().getTime() - (100000)));
        // should be false
        assertThat(loan.isActive(), is(false));
    }
}
