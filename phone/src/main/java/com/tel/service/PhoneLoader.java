package com.tel.service;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.Arrays;

/**
 * Responsible for loading phone
 */
@Service
public class PhoneLoader implements InitializingBean {

    @Autowired
    PhoneService phoneService;

    @Value("${phone.code.url}")
    String countryUrl;

    private void loadPhones() {
        try {
            Document doc = Jsoup.connect(this.countryUrl).get();

            // Load alphabetical listing by country or region
            Element table = doc.getElementsByClass("wikitable").get(1);
            loadTable(table);
            // Load locations with no country code

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void loadTable(Element table) {
        try {
            Elements trs = table.select("tr");
            trs.forEach(tr -> {
                Elements tds = tr.select("td");
                tds.select("[title]").forEach(td -> {
                    String code = td.text().replaceAll("\\+|\\s+","");
                    if (code.contains(",")) {
                        Arrays
                        .asList(code.split(","))
                        .forEach(x -> phoneService.load(Integer.parseInt(x), tr.text()));
                    } else {
                        phoneService.load(Integer.parseInt(code), tr.text());
                    }
                });
            });

        } catch (NumberFormatException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void afterPropertiesSet() throws Exception {
        loadPhones();
    }
}
