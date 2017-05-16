package io.fourfinanceit.util;

import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.stereotype.Component;


@Component
@Scope(value="request", proxyMode= ScopedProxyMode.TARGET_CLASS)
public class IpAddressHolder {

    public String ipAddress;

    public String getIpAddress() {
        return ipAddress;
    }

    public void setIpAddress(String ipAddress) {
        this.ipAddress = ipAddress;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        IpAddressHolder ipAddressHolder1 = (IpAddressHolder) o;

        return ipAddress != null ? ipAddress.equals(ipAddressHolder1.ipAddress) : ipAddressHolder1.ipAddress == null;

    }

    @Override
    public int hashCode() {
        return ipAddress != null ? ipAddress.hashCode() : 0;
    }
}
