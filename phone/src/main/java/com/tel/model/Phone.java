package com.tel.model;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.tel.util.DomainFilter;
import org.springframework.util.Assert;

public class Phone implements DomainFilter {

    private String number;
    private String country;

    Phone(String number, String country) {
        Assert.notNull(number, "Number required");
        Assert.notNull(country, "Locale required");
        this.number = number;
        this.country = country;
    }

    public String getNumber() { return number; }
    public String getCountry() { return country; }

    @Override
    public ObjectNode toJsonNode() {
        ObjectNode node = new ObjectNode(JsonNodeFactory.instance);
        node.put("number", this.number);
        node.put("country", this.country);
        return node;
    }
}
