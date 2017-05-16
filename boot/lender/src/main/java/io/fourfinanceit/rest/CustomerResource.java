package io.fourfinanceit.rest;

import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.fourfinanceit.domain.Customer;
import io.fourfinanceit.repository.CustomerRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Optional;

import static io.fourfinanceit.util.ControllerUtils.toArrayNode;

@RestController
@RequestMapping("/customers")
public class CustomerResource {

    private final Logger log = LoggerFactory.getLogger(CustomerResource.class);

    @Autowired
    CustomerRepository customerRepository;

    @GetMapping("")
    public ResponseEntity<ArrayNode> getCustomers() {
        log.info("getCustomers > ");
        List<Customer> customerList = customerRepository.findAll();
        return ResponseEntity.ok().body(toArrayNode(customerList));
    }

    @GetMapping("/{number}")
    public ResponseEntity<ObjectNode> getCustomer(@PathVariable String number) {
        log.info("getCustomer > " + number);
        Customer customer = customerRepository.findByNumber(number);
        return Optional.ofNullable(customer)
                .map(response -> ResponseEntity.ok().body(response.toJson()))
                .orElse(new ResponseEntity<>(HttpStatus.NOT_FOUND));
    }
}
