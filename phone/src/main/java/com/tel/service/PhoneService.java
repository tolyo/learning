package com.tel.service;

import org.springframework.stereotype.Service;
import org.springframework.util.Assert;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

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

    public Optional<String> get(Integer number) {
        if (number == null) return Optional.empty();
        else {
            if (countryCode.containsKey(number)) return Optional.of(countryCode.get(number));
            else return get(shortenNumber(number));
        }
    }

    private Integer shortenNumber(Integer number) {
        String numberString = number.toString();
        numberString = numberString.substring(0, numberString.length() - 1);
        if (numberString.equals("")) return null;
        return Integer.parseInt(numberString);
    }
}
