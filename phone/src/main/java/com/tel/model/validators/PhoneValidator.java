package com.tel.model.validators;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.tel.model.Phone;
import com.tel.util.DomainFilter;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

public class PhoneValidator implements Validator, DomainFilter {

    private Phone phone;


    @Override
    public boolean supports(Class<?> aClass) {
        return false;
    }

    @Override
    public void validate(Object o, Errors errors) {

    }

    @Override
    public ObjectNode toJsonNode() {
        return phone.toJsonNode();
    }
}
