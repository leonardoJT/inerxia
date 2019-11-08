DEFINE INPUT  PARAMETER W_Age AS INTEGER.
DEFINE INPUT  PARAMETER W_Age1 AS INTEGER.
DEFINE INPUT  PARAMETER W_Mes AS INTEGER.
DEFINE INPUT  PARAMETER W_Ano AS INTEGER.
DEFINE OUTPUT PARAMETER W_Operador1 AS CHARACTER.
DEFINE OUTPUT PARAMETER W_Operador2 AS CHARACTER.
DEFINE OUTPUT PARAMETER W_Operando  AS CHARACTER.
DEFINE OUTPUT PARAMETER W_Cuenta    LIKE Cuentas.Cuenta.
DEFINE OUTPUT PARAMETER W_Resultado AS CHARACTER.
DEFINE VARIABLE W_TotAct AS DECIMAL INITIAL 0.
DEFINE VARIABLE W_TotPas AS DECIMAL INITIAL 0.
DEFINE VARIABLE W_TotPor AS DECIMAL INITIAL 0.
FOR EACH Cuentas WHERE SUBSTRING(Cuentas.Cuenta,1,4,'CHARACTER') = '1115' AND Cuentas.Tipo = 2:
 RUN SaldosCuenta.p(INPUT Cuentas.Cuenta, INPUT  W_Ano, INPUT W_Age, INPUT W_Age1, INPUT Cuentas.Naturaleza, INPUT W_Mes, OUTPUT W_TotPor).
 W_TotAct = W_TotAct + W_TotPor.
END.
W_TotPor = 0.
FOR EACH Cuentas WHERE SUBSTRING(Cuentas.Cuenta,1,4,'CHARACTER') = '1300' AND Cuentas.Tipo = 2:
 RUN SaldosCuenta.p(INPUT Cuentas.Cuenta, INPUT  W_Ano, INPUT W_Age, INPUT W_Age1, INPUT Cuentas.Naturaleza, INPUT W_Mes, OUTPUT W_TotPor).
 W_TotAct = W_TotAct + W_TotPor.
END.
W_TotPor = 0.
FOR EACH Cuentas WHERE SUBSTRING(Cuentas.Cuenta,1,4,'CHARACTER') = '1400' AND Cuentas.Tipo = 2:
 RUN SaldosCuenta.p(INPUT Cuentas.Cuenta, INPUT  W_Ano, INPUT W_Age, INPUT W_Age1, INPUT Cuentas.Naturaleza, INPUT W_Mes, OUTPUT W_TotPor).
 W_TotAct = W_TotAct + W_TotPor.
END.
W_TotPor = 0.
FOR EACH Cuentas WHERE SUBSTRING(Cuentas.Cuenta,1,4,'CHARACTER') = '2100' AND Cuentas.Tipo = 2:
 RUN SaldosCuenta.p(INPUT Cuentas.Cuenta, INPUT W_Ano, INPUT W_Age,INPUT W_Age1, INPUT Cuentas.Naturaleza, INPUT W_Mes, OUTPUT W_TotPor).
 W_TotPas = W_TotPas + W_TotPor.
END.
IF W_TotPas NE 0 THEN
 W_TotPor = W_TotAct / W_TotPas.
ASSIGN W_Resultado = STRING(W_TotPor,'->>>,>99.99')  
       W_Operador1 = STRING(W_TotAct,'->>>,>>>,>>>,>99.99') 
       W_Operador2 = STRING(W_TotPas,'->>>,>>>,>>>,>99.99') 
       W_Operando = '/'
       W_Cuenta = '1115+1300+1400 / 2100'. 

