package io.fourfinanceit.rest;

import io.fourfinanceit.domain.Customer;
import io.fourfinanceit.repository.CustomerRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
public class CustomerResource {

    @Autowired
    CustomerRepository customerRepository;

    @RequestMapping("/customers")
    public List<Customer> getCustomers(){
        return customerRepository.findAll();
    }
}
