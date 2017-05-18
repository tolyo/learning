package io.fourfinanceit.util;


import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.junit4.SpringRunner;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.*;

@RunWith(SpringRunner.class)
public class RegexUtilsTests {

    @Test
    public void testIpRegex() {
        // when give valid ip
        assertThat( "127.1.1.1".matches(RegexUtils.IP_REGEX), is(true));
        assertThat( "127.01.01.01".matches(RegexUtils.IP_REGEX), is(true));

        // when given invalid ip
        assertThat( "fail".matches(RegexUtils.IP_REGEX), is(false));
        assertThat( "99999".matches(RegexUtils.IP_REGEX), is(false));
        assertThat( "999.999.999.999".matches(RegexUtils.IP_REGEX), is(false));
    }
}
