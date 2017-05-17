package io.fourfinanceit.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.fourfinanceit.util.DomainFilter;
import io.fourfinanceit.util.FormatUtils;
import io.fourfinanceit.util.JsonDateSerializer;
import io.fourfinanceit.util.SpringEnvironment;
import io.fourfinanceit.validation.LoanApplicationCommand;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;

/**
 * A loan that can be taken out by a customer
 */
@Entity
public class Loan implements Serializable, DomainFilter {

    private static final SimpleDateFormat dateFormat = new SimpleDateFormat(FormatUtils.DATE_FORMAT);

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @JsonIgnore
    private Long id;

    // Validated fields
    @NotNull
    private String number = getRandomNumber();

    @ManyToOne
    private Customer customer;

    @NotNull
    @JsonSerialize(using=JsonDateSerializer.class)
    private Date startDate = new Date();

   //@NotNull
    @JsonSerialize(using=JsonDateSerializer.class)
    private Date endDate;

   //@NotNull
    @Column(precision=10, scale=2)
    private BigDecimal amount;

   //@NotNull
    @CreationTimestamp
    @Temporal(TemporalType.DATE)
    @JsonIgnore
    private Date created = new Date();

   //@NotNull
    @UpdateTimestamp
    @Temporal(TemporalType.DATE)
    @JsonIgnore
    private Date updated = new Date();

    @OneToMany(mappedBy = "loan")
    @JsonIgnore
    private Set<LoanExtension> loanExtensions = new HashSet<>();

    public Loan() {};

    public Loan(LoanApplicationCommand cmd) {
        this.startDate = cmd.getStartDate();
        this.endDate = cmd.getEndDate();
        this.amount = cmd.getAmount();
        this.customer = cmd.getCustomer();

    };

    public Loan(Date startDate, Date endDate, BigDecimal amount) {
        this.startDate = startDate;
        this.endDate = endDate;
        this.amount = amount;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getNumber() {
        return number;
    }

    public Customer getCustomer() {
        return customer;
    }

    public void setCustomer(Customer customer) {
        this.customer = customer;
    }

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    public Date getEndDate() {
        return endDate;
    }

    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public void setAmount(BigDecimal amount) {
        this.amount = amount;
    }

    public Date getUpdated() {
        return updated;
    }

    public void setUpdated(Date updated) {
        this.updated = updated;
    }

    public Set<LoanExtension> getLoanExtensions() {
        return loanExtensions;
    }

    public void setLoanExtensions(Set<LoanExtension> loanExtensions) {
        this.loanExtensions = loanExtensions;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Loan loan = (Loan) o;

        return id != null ? id.equals(loan.id) : loan.id == null;

    }

    @Override
    public int hashCode() {
        return id != null ? id.hashCode() : 0;
    }

    @Override
    public String toString() {
        return "Loan{" +
                "id=" + id +
                ", customer=" + customer +
                ", startDate=" + startDate +
                ", endDate=" + endDate +
                ", amount=" + amount +
                ", created=" + created +
                ", updated=" + updated +
                ", loanExtensions=" + loanExtensions +
                '}';
    }

    @Override
    public ObjectNode toJson() {
        ObjectNode node = new ObjectNode(JsonNodeFactory.instance);
        node.put("number", number);
        node.put("amount", amount.setScale(2).doubleValue());
        node.put("debt", getCurrentDebt());
        node.put("startDate", dateFormat.format(startDate));
        node.put("endDate", dateFormat.format(endDate));

        ArrayNode extensionsNode = new ArrayNode(JsonNodeFactory.instance);
        loanExtensions.stream().forEach(x -> extensionsNode.add(x.toJson()));
        node.set("extensions", extensionsNode);

        return node;
    }

    public BigDecimal getCurrentDebt() {
        int days = (int)((startDate.getTime() - endDate.getTime())/(1000 * 60 * 60 * 24));
        return amount
                .multiply(new BigDecimal(SpringEnvironment.getEnvironment().getProperty("loan.factor")))
                .multiply(new BigDecimal(days/7))
                .abs();
    }

    public boolean isActive() {
        return this.getEndDate().after(new Date());
    }

    private static String getRandomNumber() {
        return String.format("%09d", new Random().nextInt(999999999));
    }

    public void resetNumber() {
        this.number = getRandomNumber();
    }
}
