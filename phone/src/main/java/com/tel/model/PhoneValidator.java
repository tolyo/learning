package com.tel.model;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.tel.service.PhoneService;
import com.tel.util.DomainFilter;
import com.tel.util.SpringContext;
import org.hibernate.validator.constraints.NotBlank;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.Assert;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import javax.validation.constraints.NotNull;
import java.util.Optional;

public class PhoneValidator implements Validator, DomainFilter {

    private static final Logger log = LoggerFactory.getLogger(PhoneValidator.class);

    @NotNull
    @NotBlank
    private String number;

    private String country;

    @Override
    public void validate(Object target, Errors errors) {
        Assert.notNull(target, "Target must not be null");
        Assert.notNull(errors, "Errors must not be null");

        // Validate default constraints first
        if (errors.hasErrors()) return;

        PhoneValidator phoneValidator = (PhoneValidator) target;
        PhoneService phoneService = SpringContext.get().getBean(PhoneService.class);

        Integer number = Integer.parseInt(phoneValidator.getNumber().replaceAll("[^0-9]", ""));
        Optional<String> country = phoneService.get(number);

        if (country.isPresent()) phoneValidator.setCountry(country.get());
        else errors.rejectValue("number", "", "country.invalid");
    }

    @Override
    public ObjectNode toJsonNode() {
        ObjectNode node = new ObjectNode(JsonNodeFactory.instance);
        node.put("number", this.number);
        node.put("country", this.country);
        return node;
    }

    public String getNumber() { return number; }
    public void setNumber(String number) { this.number = number; }
    public String getCountry() { return country; }
    public void setCountry(String country) { this.country = country; }

    @Override
    public boolean supports(Class<?> aClass) { return aClass.equals(PhoneValidator.class); }

    @Override
    public String toString() {
        return "PhoneValidator{" +
                "number='" + number + '\'' +
                '}';
    }
}
