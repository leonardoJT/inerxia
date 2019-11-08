DEFINE INPUT PARAMETER W_CtaSal  LIKE Sal_Cuenta.Cuenta.
DEFINE INPUT PARAMETER W_Ano     AS   INTEGER.
DEFINE INPUT PARAMETER W_AgenSal LIKE Sal_Cuenta.Agencia.
DEFINE INPUT PARAMETER W_Agen2   LIKE Sal_Cuenta.Agencia.
DEFINE INPUT PARAMETER W_NatCta LIKE Cuentas.Naturaleza.
DEFINE INPUT PARAMETER W_MesCor  AS INTEGER.
DEFINE OUTPUT PARAMETER W_Salcta  AS DECIMAL.
DEFINE VARIABLE i AS INTEGER.

W_Salcta = 0.
FOR EACH Sal_Cuenta WHERE Sal_Cuenta.Cuenta = W_CtaSal    AND 
                          Sal_Cuenta.Ano = W_Ano          AND 
                          Sal_Cuenta.Agencia GE W_AgenSal AND 
                          Sal_Cuenta.Agencia LE W_Agen2   NO-LOCK:
 ASSIGN W_SalCta = W_SalCta + Sal_Cuenta.Sal_Inicial.
  DO i = 1 TO W_MesCor:
    IF W_NatCta = 'Db' THEN
      ASSIGN W_SalCta = W_SalCta + (Sal_Cuenta.Db[i] - Sal_Cuenta.Cr[i]).
     ELSE
      ASSIGN W_SalCta = W_SalCta + (Sal_Cuenta.Cr[i] - Sal_Cuenta.Db[i]). 
  END.
END.
