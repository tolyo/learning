package io.fourfinanceit;

import io.fourfinanceit.util.IpFilter;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import javax.servlet.Filter;

/**
 * Beans should be registered here
 */
@Configuration
public class BeanRegister {

    @Bean
    public FilterRegistrationBean ipFilterRegistration() {
        FilterRegistrationBean registration = new FilterRegistrationBean();
        registration.setFilter(ipFilter());
        registration.addUrlPatterns("/*");
        registration.setName("ipFilter");
        registration.setOrder(1);
        return registration;
    }

    /**
     * Filter for popullating request context with request data
     */
    @Bean
    public Filter ipFilter() {
        IpFilter ipFilter = new IpFilter();
        return ipFilter;
    }
}
