DISABLE TRIGGERS FOR LOAD OF creditos.

DEFINE VAR cont AS INTEGER.

/*RUN MENU.p.*/

FOR EACH creditos WHERE creditos.cod_credito <> 123
                    AND creditos.estado = 2
                    AND creditos.nit = "1061624121"
                    AND creditos.num_Credito = 23487 BY num_credito:
    /*UPDATE creditos WITH 1 COL.*/
    cont = cont + 1.

    RUN \\192.168.1.100\Desarrollo\Obj\CrearControlPagos_rec.r(INPUT creditos.nit,
                                                               INPUT creditos.num_credito,
                                                               INPUT creditos.tasa).

    IF cont = 23 THEN
        LEAVE.
END.

MESSAGE "Ok" cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
