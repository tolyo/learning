package io.fourfinanceit.repository;

import io.fourfinanceit.HomeworkApplication;
import io.fourfinanceit.domain.Loan;
import io.fourfinanceit.domain.LoanExtension;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static io.fourfinanceit.util.test.TestHelpers.TODAY;
import static io.fourfinanceit.util.test.TestHelpers.TOMORROW;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = HomeworkApplication.class)
public class LoanExtensionRepositoryTests {

    LoanExtension loanExtension;

    @Autowired
    LoanExtensionRepository loanExtensionRepository;

    @Autowired
    LoanRepository loanRepository;

    @Test
    public void testSaveLoanExtension() {
        // given a loan
        Loan loan = loanRepository.findAll().get(0);
        assertThat(loan, notNullValue());

        // and a valid extension
        loanExtension = new LoanExtension();
        loanExtension.setStartDate(TODAY);
        loanExtension.setEndDate(TOMORROW);
        loanExtension.setLoan(loan);

        // extension should be saved
        loanExtension = loanExtensionRepository.save(loanExtension);
        assertThat(loanExtension.getId(), notNullValue());
    }
}
