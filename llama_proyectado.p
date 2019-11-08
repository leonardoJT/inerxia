DEFINE VARIABLE xfec AS DATE.
DEFINE VARIABLE xpro AS DECIMAL.
DEFINE VARIABLE xatr AS DECIMAL.
FOR EACH creditos WHERE nit = "40983343" AND num_credito = 82969:
    xfec = Creditos.Fec_Desembolso.
    IF Creditos.Fec_PagAnti NE ? AND Creditos.Fec_PagAnti GT Creditos.Fec_Desembolso THEN
      xfec = Creditos.Fec_PagAnti.

    RUN HSP(xfec,
            creditos.monto,
            creditos.cuota,
            creditos.monto,
            creditos.tasa,
            creditos.plazo,
            creditos.sdo_capital,
            xpro,
            xatr).

END.
