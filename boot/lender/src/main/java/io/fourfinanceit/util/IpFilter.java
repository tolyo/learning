package io.fourfinanceit.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.GenericFilterBean;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import java.io.IOException;

@Component
public class IpFilter extends GenericFilterBean {

    private final Logger log = LoggerFactory.getLogger(IpFilter.class);

    @Autowired
    IpAddressHolder ipAddressHolder;

    @Override
    public void doFilter(ServletRequest request,
                         ServletResponse response,
                         FilterChain chain)
            throws IOException, ServletException {
        log.debug("IpFilter setting address to " + request.getRemoteAddr());
        ipAddressHolder.setIpAddress(request.getRemoteAddr());
        chain.doFilter(request, response);
    }
}
