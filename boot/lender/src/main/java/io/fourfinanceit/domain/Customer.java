package io.fourfinanceit.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import org.hibernate.annotations.CreationTimestamp;

import javax.persistence.*;
import java.util.Date;

/*
 * A generic recipient of services provided by the application.
 */
@Entity
@JsonIgnoreProperties(ignoreUnknown = true)
public class Customer {

    @Id @GeneratedValue(strategy = GenerationType.AUTO) @JsonIgnore
    private Long id;

    private String number;

    @CreationTimestamp @Temporal(TemporalType.DATE) @JsonIgnore
    private Date created = new Date();

    public Customer() {}

    public Customer(String number) {
        this.number = number;
    }

    public String getNumber() {
        return number;
    }

    public void setNumber(String number) {
        this.number = number;
    }

    public Date getCreated() {
        return created;
    }

    public void setCreated(Date created) {
        this.created = created;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Customer)) return false;
        Customer customer = (Customer) o;
        return getId().equals(customer.getId());
    }

    @Override
    public int hashCode() {
        return getId().hashCode();
    }

    @Override
    public String toString() {
        return "Customer{" +
                "id=" + id +
                ", number='" + number + '\'' +
                ", created=" + created +
                '}';
    }
}
