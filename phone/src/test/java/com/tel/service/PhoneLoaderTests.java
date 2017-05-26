package com.tel.service;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.math.BigInteger;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.core.IsNull.notNullValue;

@RunWith(SpringRunner.class)
@SpringBootTest
public class PhoneLoaderTests {

    @Autowired
    PhoneLoader phoneLoader;

    @Before
    public void setup() {
        assertThat(phoneLoader, notNullValue());
        assertThat(phoneLoader.phoneCodeUrl, notNullValue());
        assertThat(phoneLoader.phoneService, notNullValue());
    }

    @Test
    public void testCountryLoad() {
        // expect countries to be loaded
        assertThat(phoneLoader.phoneService.get(new BigInteger("371")).get(), is("Latvia"));
    }
}
