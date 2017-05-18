package io.fourfinanceit;

import io.fourfinanceit.util.RiskRange;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Beans should be registered here
 */
@Configuration
public class BeanRegister {

    @Bean
    RiskRange riskRange() {
        RiskRange riskRange = new RiskRange();
        return riskRange;
    }

}
