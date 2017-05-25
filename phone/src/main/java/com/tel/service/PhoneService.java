package com.tel.service;

import org.springframework.stereotype.Service;
import org.springframework.util.Assert;

import java.util.HashMap;
import java.util.Map;

/**
 * Naive non-thread safe implementation of a service for storing county codes
 */
@Service
public class PhoneService {

    private static final Map<Integer, String> countryCode = new HashMap<>();

    public void load(Integer s, String country) {
        Assert.notNull(s, "Number required");
        Assert.notNull(country, "Country required");
        countryCode.put(s, country);
    }
}
