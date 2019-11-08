DEFINE VARIABLE xplazo AS INTEGER.
DEFINE VARIABLE xsaldoC LIKE creditos.sdo_capital.
DEFINE VARIABLE xsaldoL LIKE creditos.sdo_capital.
FOR EACH creditos WHERE sdo_capital GT 0 AND agencia = 2:
    xplazo = creditos.plazo.
    IF per_pago = 1 THEN xplazo = plazo * 7.
    IF per_pago = 3 THEN xplazo = plazo * 2.
    IF per_pago = 4 THEN xplazo = plazo * 30.
    IF xplazo < 430 THEN ASSIGN xsaldoC = xsaldoc + sdo_capital.
    ELSE xsaldoL = xsaldol + sdo_capital.
END.
DISPLAY xsaldoc
        xsaldol.
