package io.fourfinanceit.rest;

import com.fasterxml.jackson.databind.node.ObjectNode;
import io.fourfinanceit.validation.LoanApplicationCommand;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.*;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.web.context.WebApplicationContext;

import static io.fourfinanceit.util.test.TestHelpers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.core.IsNull.notNullValue;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment= SpringBootTest.WebEnvironment.RANDOM_PORT)
public class LoanResourceTests {

    @Autowired
    TestRestTemplate restTemplate;

    @Autowired
    WebApplicationContext webApplicationContext;

    public void setup() {
        assertThat(this.webApplicationContext, notNullValue());
    }

    @Test
    public void testCreateLoan() {
        // given a valid loan application command
        LoanApplicationCommand cmd = new LoanApplicationCommand();
        cmd.setCustomer(null);
        cmd.setStartDate(TOMORROW);
        cmd.setEndDate(TWO_WEEKS_FROM_NOW);
        cmd.setAmount(LOAN_AMOUNT);
        cmd.setCustomerNumber(CUSTOMER_NUMBER);
        cmd.setIp("127.0.0.1");
        ResponseEntity<ObjectNode> responseEntity =
                restTemplate.postForEntity("/loans", cmd, ObjectNode.class);

        // should return created
        assertThat(responseEntity.getStatusCode(), is(HttpStatus.CREATED));
    }

}
