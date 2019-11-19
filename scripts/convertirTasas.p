DEFINE VAR P_Nominal AS DECIMAL.
DEFINE VAR P_TasaEfectiva AS DECIMAL.

P_TasaEfectiva = 6 / 100.

P_Nominal = (EXP(P_TasaEfectiva + 1,1 / 2)) - 1.

MESSAGE p_nominal * 100 * 2
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
