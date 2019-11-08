DEFINE VAR enviado AS LOGICAL.
DEFINE VAR vStatus AS CHARACTER.
DEFINE VAR vCliente AS CHARACTER INITIAL "LEONARDO GÁLVEZ OCAMPO".
DEFINE VAR vCorreo AS CHARACTER INITIAL "cajimenez@fodun.com.co".
DEFINE VAR diasMora AS INTEGER.

FIND FIRST clientes WHERE clientes.nit = "14987916" NO-LOCK NO-ERROR.

vCliente = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.
/*vCorreo = clientes.email.*/

diasMora = 85.

RUN emailRecordatorioPago.p(INPUT vCliente,
                            INPUT vCorreo,
                            INPUT diasMora,
                            INPUT "CORTO PLAZO",
                            INPUT 2305,
                            INPUT 1000000,
                            INPUT 25000,
                            INPUT 08/05/2019,
                            INPUT "lgalvez@localhostenlaweb.com",
                            OUTPUT enviado,
                            OUTPUT vStatus).

MESSAGE enviado vStatus
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
