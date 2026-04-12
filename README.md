# Galaxy General Insurance: Cosmic Quarry Risk & Pricing Solution 🚀

## Table of Contents
- [Project Overview](#project-overview)
- [Executive Summary](#executive-summary)
- [Product Design](#product-design)
  - [Business Interruption](#business-interruption)
  - [Cargo Loss](#cargo-loss)
  - [Equipment Failure](#equipment-failure)
  - [Workers Compensation](#workers-compensation)
- [Summary of Pricing & Capital Modelling](#summary-of-pricing--capital-modelling)
  - [Business Interruption](#business-interruption-1)
  - [Cargo Loss](#cargo-loss-1)
  - [Equipment Failure](#equipment-failure-1)
  - [Workers Compensation](#workers-compensation-1)
- [Risk Assessment](#risk-assessment)
- [Key Assumptions](#key-assumptions)
- [Data Limitations](#data-limitations)
- [Conclusion](#conclusion)
- [Further Resources](#further-resources)

---

## Project Overview



---

## Executive Summary



---

# Product Design

All products are structured as annually renewable policies with upfront premiums and flexible scalability.

## Business Interruption

Galaxy General's Business Interruption coverage for Cosmic Quarry employs a risk transfer structure designed to protect both the insured and insurer from catastrophic tail risks while incentivising strong operational safety practices.

### Design Overview

- Coverage for financial losses incurred from suspended operations due to covered operational and environmental hazards
- Đ30,000 per-occurrence deductible to discourage nuisance claims, with a maximum of 4 claims per policy year
- Risk-adjusted coverage limits allocated by solar system based on production exposure and environmental risk profile
- Excess-of-loss reinsurance protecting against catastrophic losses beyond the primary coverage layer
- Safety-based premium discount program rewarding proactive inspection and operational governance
- Exclusions for non-operational risks (e.g. planned maintenance, market fluctuations, unauthorised hazard zone entry)

### Coverage Structure

| Layer | Limit | Notes |
|---|---|---|
| Primary (Galaxy General) | Up to Đ110M | Allocated across solar systems by risk |
| Reinsurance (XL) | Đ60M xs Đ110M | Covers catastrophic losses up to Đ170M |
| Insured retention | Above Đ170M | Tail risk retained by Cosmic Quarry |

Coverage limits by solar system reflect their distinct risk profiles:

- **Helionis Cluster**: Đ60M limit. High risk: frequent micro-collisions, communication disruptions, and asteroid cluster events (54.5% of production volume)
- **Bayesia System**: Đ25M limit. Lower risk: predictable radiation spike patterns (27.3% of production volume)
- **Oryn Delta**: Đ25M limit. High risk: rapid orbital shear, fluctuating gravitational gradients, low-visibility environment (18.2% of production volume)

### Key Insight

The product balances risk transfer with behavioural incentives. By combining deductibles, claim frequency limits, and a safety discount program, it ensures that:

- Minor, preventable disruptions remain with the insured
- Catastrophic operational losses are effectively transferred through layered coverage
- Strong safety practices are financially rewarded, aligning insurer and insured interests

### Safety-Based Discount Program

Premiums are reduced by **1.5% for each safety inspection above 3 per year**, capped at a **4.5% maximum discount**. This incentivises operational excellence and proactive maintenance, which is particularly valuable given Cosmic Quarry's history of environmental disputes and regulatory scrutiny.

### Scalability

An exposure-based pricing structure allows the product to scale alongside Cosmic Quarry's planned 15–25% operational expansion:

- Coverage limits can be adjusted annually to reflect new production volumes
- System-level allocations remain aligned with underlying exposure as operations grow
- Annual repricing accommodates inflation, environmental condition changes, and evolving risk profiles

---

## Cargo Loss


---

## Equipment Failure

Equipment reliability is critical to Cosmic Quarry’s mining operations, where failures can result in significant repair costs and operational downtime. Exposure to radiation, debris, and gravitational instability further amplifies this risk.

We designed an indemnity-based insurance product that protects against equipment breakdown while maintaining incentives for effective maintenance and risk management.

### Design Overview

- Coverage for repair or replacement costs following mechanical failure  
- Per-claim deductible to reduce minor claims and promote risk sharing  
- Risk-adjusted premiums reflecting equipment type, operating conditions, and environment  
- Exclusions for non-operational risks (e.g. deliberate damage or poor maintenance)

### Key Insight

The product balances risk transfer and behavioural incentives. By combining deductibles with risk-based pricing, it ensures that:
- Minor, preventable losses remain with the insured  
- Severe operational risks are effectively transferred  

### Scalability

An exposure-based pricing structure allows the product to scale seamlessly:
- New equipment can be added without redesign  
- Risk groups ensure pricing remains aligned with underlying exposure  
- Policy parameters are indexed to inflation to maintain long-term relevance  


---

## Workers Compensation


---

# Summary of Pricing & Capital Modelling

A consistent actuarial framework was applied across all products:
- Frequency and severity modelled separately  
- Monte Carlo simulation used to generate aggregate loss distributions  
- Premiums determined using risk-adjusted principles  
- Long-term projections incorporate growth, inflation, and interest rates  

---

## Business Interruption


---

## Cargo Loss



---

## Equipment Failure (Pricing & Capital)

A structured actuarial framework was used to model equipment failure risk, combining frequency and severity models with detailed risk segmentation.

### Modelling Approach

- Frequency modelled using a Poisson GLM  
- Severity modelled using a lognormal GLM  
- Risks segmented across equipment type, age, maintenance, and environment  

This produced a granular pricing structure, improving alignment between premiums and underlying risk.

### Key Results

- Expected losses are stable with relatively low volatility  
- The portfolio remains profitable across both short and long-term projections  
- Tail risk is effectively managed through deductibles and reinsurance  

### Key Insight

The strength of this portfolio lies in granularity and stability:
- Detailed risk grouping reduces pricing error  
- Low volatility leads to consistent financial performance  
- Reinsurance protects against large individual losses  

### Stress Testing

Stress scenarios show that while extreme shocks may lead to occasional losses, the overall pricing structure remains financially robust.

This confirms that the product is well-capitalised and resilient under adverse conditions.



---

## Workers Compensation



---

# Risk Assessment

The primary risks were grouped into three key categories:

### Environmental Hazard Risk
- Debris collisions, radiation exposure, and orbital instability  
- Highest impact and likelihood across all systems  

### Solar Event Risk
- Cross-system radiation storms and communication failures  
- Potential for correlated losses across multiple lines  

### Operational Risk
- Equipment stress, workforce safety, and system failures  

Additional risks include:
- Pricing and model risk  
- Inflation risk  
- Regulatory risk  
- Catastrophic accumulation risk  

The most critical exposure arises from correlated multi-system events, which can generate simultaneous large losses.

---

# Key Assumptions

Key assumptions underlying the modelling include:
- Historical claims data is representative of future risk  
- Operational conditions remain broadly consistent  
- Inflation and interest rates follow forecasted models  
- Exposure grows in line with operational expansion  

These assumptions form the basis for pricing, projections, and capital modelling.

---

# Data Limitations

### Limited Solar System Data
No historical data was available for some operating systems. Baseline data was adjusted using actuarial judgement to reflect differing risk environments.

### Claim Severity Thresholds
Minimum reporting thresholds resulted in missing smaller claims, introducing potential upward bias in severity estimates.

### Mitigation
- Conservative modelling assumptions  
- Deductibles aligned with reporting thresholds  
- Risk adjustments based on qualitative insights  

---

# Conclusion



---

# Further Resources

- 📄 Full Report: *[Insert link to full report]*  
- 💻 Code & Modelling: *[Insert GitHub repo or folders]*  
- 📊 Appendix & Technical Details: *[Insert appendix link]*  

For a deeper understanding of the modelling approach, assumptions, and detailed results, please refer to the full report and supporting materials.

