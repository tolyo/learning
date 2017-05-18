package io.fourfinanceit.util;

import io.fourfinanceit.HomeworkApplication;
import io.fourfinanceit.validation.LoanApplicationCommand;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.*;
import static io.fourfinanceit.util.RangeValidation.*;
import static io.fourfinanceit.util.test.TestHelpers.*;

@RunWith(SpringRunner.class)
@SpringBootTest
public class RangeValidationTests {

    LoanApplicationCommand cmd;

    @Test
    public void testIsDateRangeMinValid() {
        // given invalid
        cmd = new LoanApplicationCommand();

        assertThat(isDateRangeMinValid(cmd), is(false));

        cmd = new LoanApplicationCommand();
        cmd.setStartDate(TODAY);
        cmd.setEndDate(TOMORROW);

        assertThat(isDateRangeMinValid(cmd), is(false));

        // given valid
        cmd = new LoanApplicationCommand();
        cmd.setStartDate(TODAY);
        cmd.setEndDate(WEEK_FROM_NOW);

        assertThat(isDateRangeMinValid(cmd), is(true));
    }

    @Test
    public void testIsDateRangeValid() {
        // given invalid
        cmd = new LoanApplicationCommand();

        assertThat(isDateRangeValid(cmd), is(false));

        cmd = new LoanApplicationCommand();
        cmd.setStartDate(TOMORROW);
        cmd.setEndDate(TODAY);

        assertThat(isDateRangeValid(cmd), is(false));

        // given valid
        cmd = new LoanApplicationCommand();
        cmd.setStartDate(TODAY);
        cmd.setEndDate(TOMORROW);

        assertThat(isDateRangeValid(cmd), is(true));
    }


}
