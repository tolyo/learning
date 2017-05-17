package io.fourfinanceit.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.fourfinanceit.util.DomainFilter;
import io.fourfinanceit.util.FormatUtils;
import io.fourfinanceit.validation.LoanExtensionCommand;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Objects;

/**
 * Loan extentions modify a loan
 */
@Entity
public class LoanExtension implements DomainFilter {

    private static final SimpleDateFormat dateFormat = new SimpleDateFormat(FormatUtils.DATE_FORMAT);

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;

    @NotNull
    private Date startDate;

    @NotNull
    private Date endDate;

    @NotNull
    @CreationTimestamp
    @Temporal(TemporalType.DATE)
    @JsonIgnore
    private Date created = new Date();

    @NotNull
    @UpdateTimestamp
    @Temporal(TemporalType.DATE)
    @JsonIgnore
    private Date updated = new Date();

    @ManyToOne
    private Loan loan;

    public LoanExtension(){}

    public LoanExtension(LoanExtensionCommand cmd) {
        this.startDate = cmd.getStartDate();
        this.endDate = cmd.getEndDate();
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
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

    public Date getCreated() {
        return created;
    }

    public void setCreated(Date created) {
        this.created = created;
    }

    public Date getUpdated() {
        return updated;
    }

    public void setUpdated(Date updated) {
        this.updated = updated;
    }

    public Loan getLoan() {
        return loan;
    }

    public void setLoan(Loan loan) {
        this.loan = loan;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        LoanExtension loanExtension = (LoanExtension) o;
        if (loanExtension.id == null || id == null) {
            return false;
        }
        return Objects.equals(id, loanExtension.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }

    @Override
    public String toString() {
        return "LoanExtension{" +
                "id=" + id +
                ", startDate='" + startDate + "'" +
                ", endDate='" + endDate + "'" +
                ", create='" + created + "'" +
                ", updated='" + updated + "'" +
                '}';
    }

    @Override
    public ObjectNode toJson() {
        ObjectNode node = new ObjectNode(JsonNodeFactory.instance);
        node.put("startDate", dateFormat.format(startDate));
        node.put("endDate", dateFormat.format(endDate));
        return node;
    }
}
