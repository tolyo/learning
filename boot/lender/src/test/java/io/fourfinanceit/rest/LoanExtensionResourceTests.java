package io.fourfinanceit.rest;

import com.fasterxml.jackson.databind.node.ObjectNode;
import io.fourfinanceit.domain.Loan;
import io.fourfinanceit.repository.CustomerRepository;
import io.fourfinanceit.repository.LoanRepository;
import io.fourfinanceit.validation.LoanExtensionCommand;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringRunner;

import static io.fourfinanceit.util.test.TestHelpers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.core.IsNull.notNullValue;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class LoanExtensionResourceTests {

    Loan loan;

    @Autowired
    TestRestTemplate restTemplate;

    @Autowired
    LoanRepository loanRepository;

    @Test
    public void testCreateLoanExtension() {
        loan = loanRepository.findAll().get(0);
        loan.setCustomer(null); // <- this is a hack
        assertThat(loan, notNullValue());
        assertThat(loan.getNumber(), notNullValue());

        // given a valid loan application command
        LoanExtensionCommand cmd = new LoanExtensionCommand();
        cmd.setEndDate(TWO_WEEKS_FROM_NOW);
        cmd.setLoan(loan);
        cmd.setNumber(loan.getNumber());

        ResponseEntity<ObjectNode> responseEntity = restTemplate.postForEntity("/loanextensions", cmd, ObjectNode.class);

        // should return created
        assertThat(responseEntity.getStatusCode(), is(HttpStatus.CREATED));
    }

}
