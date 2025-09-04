# Operational Risk Modeling with Monte Carlo Simulation 

## Project Description  
This project focuses on **operational risk analysis** across seven business lines in banking.  
We analyze the distribution of **frequency**, **event occurrence**, and **loss severity** based on historical data, and then fit probability distributions to model risk and perform **Monte Carlo simulations**.  

The business lines considered include:  
- BussDistr  
- Com_Ban  
- Damage Empl_Pract  
- Execut_Del  
- External_Fr  
- Internal_Fr

## Methodology  
1. **Data Preparation**  
   - Historical gross loss data (year, loss, business line).  
   - Segmentation into 7 business lines.  

2. **Frequency Modeling**  
   - Distribution fitting (Binomial, Poisson, Negative Binomial).  
   - Rule of thumb based on variance-to-mean ratio:  
     - Var > Mean → Binomial distribution  
     - Var = Mean → Poisson distribution  
     - Var < Mean → Negative Binomial distribution  

3. **Severity Modeling**  
   - Fit distributions (log-normal, exponential, gamma) using method of moments (`fitdist`).  

4. **Monte Carlo Simulation**  
   - Simulate annual losses for each business line.  
   - Compute **VaR (99%)** and extreme loss scenarios.

## Results  
- **Highest risk:** *Employment Practices* — strong legal and fraud-related risks.  
- **Lowest risk:** *Execution & Delivery* — operational issues but limited financial impact.  
- Most distributions are **right-skewed**, indicating many small losses and few extreme losses.  
- Banks must hold **significant capital buffers** to mitigate extreme operational losses.  
