package com.tel.service;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;

import java.io.IOException;
import java.math.BigInteger;
import java.util.Optional;

@Service
public class OtpListener implements ApplicationListener<ContextRefreshedEvent> {

    private static final Logger log = LoggerFactory.getLogger(OtpListener.class);

    @Autowired
    PhoneService phoneService;

    @Async
    @Override
    public void onApplicationEvent(ContextRefreshedEvent contextRefreshedEvent) {
        OtpNode myOtpNode = null;
        try {
            myOtpNode = new OtpNode("server");
            myOtpNode.setCookie("secret");
            OtpMbox myOtpMbox = myOtpNode.createMbox("server");

            while (true) {
                OtpErlangTuple tuple = (OtpErlangTuple) myOtpMbox.receive();
                log.info("Received message");
                OtpErlangPid lastPid = (OtpErlangPid) tuple.elementAt(0);
                OtpErlangAtom dispatch = (OtpErlangAtom) tuple.elementAt(1);
                if (dispatch.toString().equals("getphone")) {
                    final OtpErlangBinary message = (OtpErlangBinary) tuple.elementAt(2);
                    String phone = new String(message.binaryValue());
                    log.info(phone);
                    BigInteger phoneNumber = new BigInteger(phone);
                    Optional<String> s = phoneService.get(phoneNumber);
                    myOtpMbox.send(lastPid, new OtpErlangString(s.orElse("")));
                } else {
                    final OtpErlangBinary message = (OtpErlangBinary) tuple.elementAt(2);
                    // Do more UI stuff
                    log.info(message.toString());
                }
            }
        } catch (IOException | OtpErlangExit | OtpErlangDecodeException e) {
            log.error(e.getMessage(), e);
        }

    }
}
