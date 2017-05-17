package io.fourfinanceit.util;

import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

/**
 * Helper for accessing org.springframework.core.env.Environment from static or instance methods
 */
@Service
public class SpringEnvironment implements ApplicationContextAware {

    private static Environment environment;

    public static Environment get() {
        return environment;
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        environment = applicationContext.getEnvironment();
    }
}