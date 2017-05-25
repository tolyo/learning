package com.tel.service;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.IOException;

/**
 * Responsible for loading phone
 */
@Service
public class PhoneLoader implements InitializingBean {

    @Autowired
    PhoneService phoneService;

    @Value("${countryUrl}")
    String countryUrl;

    private void loadPhones() {
        try {
            Document doc = Jsoup.connect(this.countryUrl).get();
            // TODO

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void afterPropertiesSet() throws Exception {
        loadPhones();
    }
}
