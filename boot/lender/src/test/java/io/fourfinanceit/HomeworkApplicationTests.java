package io.fourfinanceit;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static org.junit.Assert.assertNotEquals;
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = HomeworkApplication.class)
public class HomeworkApplicationTests {

    @Autowired
    ApplicationContext applicationContext;

	@Test
	public void contextLoads() {
        assertNotEquals("applicationContext must not be null", applicationContext, null);
	}

}
