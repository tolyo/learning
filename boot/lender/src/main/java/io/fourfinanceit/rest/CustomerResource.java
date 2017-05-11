package io.fourfinanceit.rest;

import io.fourfinanceit.domain.Customer;
import io.fourfinanceit.repository.CustomerRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Optional;

@RestController
public class CustomerResource {

    @Autowired
    CustomerRepository customerRepository;

    @RequestMapping("/customers")
    public ResponseEntity<List> getCustomers() {
        List<Customer> customerList = customerRepository.findAll();
        return ResponseEntity.ok().body(customerList);
    }

    @RequestMapping("/customers/{number}")
    public ResponseEntity<Customer> getCustomer(@PathVariable String number) {
        Customer customer = customerRepository.findByNumber(number);
        return Optional.ofNullable(customer)
                .map(response -> ResponseEntity.ok().body(response))
                .orElse(new ResponseEntity<>(HttpStatus.NOT_FOUND));
    }
}
