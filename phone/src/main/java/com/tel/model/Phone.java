package com.tel.model;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.tel.util.DomainFilter;

import java.util.Locale;

public class Phone implements DomainFilter {

    private String number;
    private Locale locale;

    public String getNumber() { return number; }
    public void setNumber(String number) { this.number = number; }
    public Locale getLocale() { return locale; }
    public void setLocale(Locale locale) { this.locale = locale; }

    @Override
    public ObjectNode toJsonNode() {
        ObjectNode node = new ObjectNode(JsonNodeFactory.instance);
        node.put("number", this.number);
        node.put("country", this.locale.getDisplayCountry());
        return node;
    }
}
