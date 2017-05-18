package io.fourfinanceit.rest;

import io.fourfinanceit.domain.Customer;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.web.context.WebApplicationContext;

import java.util.Arrays;
import java.util.List;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment= SpringBootTest.WebEnvironment.RANDOM_PORT)
public class CustomerResourceTests {

    @Autowired
    private TestRestTemplate restTemplate;

    @Autowired
    WebApplicationContext webApplicationContext;

    public void setup() {
        assertThat(this.webApplicationContext, notNullValue());
    }

    @Test
    public void testGetCustomers() {
        ResponseEntity<Customer[]> responseEntity = this.restTemplate.getForEntity("/customers", Customer[].class);
        List<Customer> customers = Arrays.asList(responseEntity.getBody());
        assertThat(customers.size(), equalTo(1));
    }
}
