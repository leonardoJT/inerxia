/*DEFINE TEMP-TABLE tt LIKE carteraVencida.*/
/*DEFINE TEMP-TABLE tt LIKE cfg_origRegCredito.*/
/*DEFINE TEMP-TABLE tt LIKE cfg_RegCredito.*/
/*DEFINE TEMP-TABLE tt LIKE cortoLargo.*/
/*DEFINE TEMP-TABLE tt LIKE liqui_int.*/
    
/*FOR EACH carteraVencida WHERE cod_producto = 192:
    CREATE tt.
    BUFFER-COPY carteraVencida TO tt.
    tt.cod_producto = 193.
END.

FOR EACH tt:
    CREATE carteraVencida.
    BUFFER-COPY tt TO carteraVencida.
END.*/

/*FOR EACH cfg_origRegCredito WHERE cod_credito = 192:
    CREATE tt.
    BUFFER-COPY cfg_origRegCredito TO tt.
    tt.cod_credito = 193.
END.

FOR EACH tt:
    CREATE cfg_origRegCredito.
    BUFFER-COPY tt TO cfg_origRegCredito.
END.*/
    

/*FOR EACH cfg_RegCredito WHERE cod_credito = 192:
    CREATE tt.
    BUFFER-COPY cfg_RegCredito TO tt.
    tt.cod_credito = 193.
END.

FOR EACH tt:
    CREATE cfg_RegCredito.
    BUFFER-COPY tt TO cfg_RegCredito.
END.*/


/*FOR EACH cortoLargo WHERE cod_producto = 192:
    CREATE tt.
    BUFFER-COPY cortoLargo TO tt.
    tt.cod_producto = 193.
END.

FOR EACH tt:
    CREATE cortoLargo.
    BUFFER-COPY tt TO cortoLargo.
END.*/


/*FOR EACH liqui_int WHERE cod_producto = 192:
    CREATE tt.
    BUFFER-COPY liqui_int TO tt.
    tt.cod_producto = 193.
END.

FOR EACH tt:
    CREATE liqui_int.
    BUFFER-COPY tt TO liqui_int.
END.*/
/* ----------------------- */

DEFINE TEMP-TABLE tt LIKE cuentas.

FOR EACH cuentas WHERE SUBSTRING(cuentas.cuenta,1,4) = "1442"
                   AND SUBSTRING(cuentas.cuenta,7,2) = "19" NO-LOCK:
    DISPLAY cuentas WITH 1 COL.
    CREATE tt.
    BUFFER-COPY cuentas TO tt.
    tt.cuenta = SUBSTRING(cuentas.cuenta,1,6) + "20".
    tt.nombre = "Crédito Celebración 30 años".

    CREATE tt.
    BUFFER-COPY cuentas TO tt.
    tt.cuenta = SUBSTRING(cuentas.cuenta,1,6) + "21".
    tt.nombre = "Crédito Educativo".
END.

FOR EACH tt:
    CREATE cuentas.
    BUFFER-COPY tt TO cuentas.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
