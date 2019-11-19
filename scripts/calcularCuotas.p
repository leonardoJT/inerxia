    DEFINE VAR P_Monto AS DECIMAL FORMAT "->>>,>>>,>>>,>>9" INITIAL 250000000.
    DEFINE VAR P_Tasa AS DECIMAL FORMAT ">>9.9999999" INITIAL 0.0093.
    DEFINE VAR P_Plazo AS DECIMAL INITIAL 84.
    DEFINE VAR P_Resultado AS DECIMAL FORMAT "->>>,>>>,>>>,>>9" INITIAL 0.

    P_Resultado = ROUND((P_Monto * ((P_Tasa * EXP(P_Tasa + 1,P_Plazo)) / (EXP(P_Tasa + 1,P_Plazo) - 1))),6).

    MESSAGE p_resultado
        VIEW-AS ALERT-BOX INFO BUTTONS OK.





    P_Monto * (              ( P_Tasa * EXP(P_Tasa + 1,P_Plazo) ) / (EXP(P_Tasa + 1,P_Plazo) - 1)                          ).
