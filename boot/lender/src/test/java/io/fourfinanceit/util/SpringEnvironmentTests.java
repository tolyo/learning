package io.fourfinanceit.util;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.core.IsNull.notNullValue;

@RunWith(SpringRunner.class)
@SpringBootTest
public class SpringEnvironmentTests {

    @Test
    public void testEnvironmentInjection() {
        assertThat(SpringEnvironment.getEnvironment(), notNullValue());
    }

    @Test
    public void testLoanFactorSetting() {
        assertThat(SpringEnvironment.getEnvironment().getProperty("loan.factor"), equalTo("1.5"));
    }
}
