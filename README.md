# Exact smooth term-structure estimation (Filipovic and Willems, 2018): Implementation

Filipovic and Willems (2018) develop a non-parametric method to estimate a smooth term-structure (https://arxiv.org/pdf/1606.03899.pdf).

Here, the method is implemented on Swiss libor and swap rates (https://data.snb.ch/de/topics/ziredev#!/cube/zimoma) and compared to bootstrapped spot rates and spot rates creates by the Nelson-Siegel approach. The Nelson-Siegel parameters are obtained from https://data.snb.ch/de/topics/ziredev#!/cube/rendopar.

