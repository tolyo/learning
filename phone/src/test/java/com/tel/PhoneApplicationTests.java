package com.tel;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.junit4.SpringRunner;

import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

@RunWith(SpringRunner.class)
@SpringBootTest
public class PhoneApplicationTests {

	@Autowired
	ApplicationContext applicationContext;

	@Test
	public void contextLoads() {
		assertThat(applicationContext, notNullValue());
	}
}
