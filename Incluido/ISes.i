DEFINE TEMP-TABLE tpuc
    FIELD pucCta LIKE sal_cuenta.cuenta
    FIELD pucNomCta LIKE Cuentas.nombre
    FIELD pucSaldo LIKE Sal_Cuenta.sal_inicial.

DEFINE TEMP-TABLE SesApoCli
    FIELD wnit LIKE clientes.nit.
