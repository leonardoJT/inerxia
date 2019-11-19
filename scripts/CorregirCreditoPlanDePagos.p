DISABLE TRIGGERS FOR LOAD OF creditos.

/*RUN MENU.p.*/

FOR EACH creditos WHERE num_credito = 932:
    /*UPDATE creditos WITH 1 COL.*/

    RUN D:\SFG\Desarrollo\Obj\CrearControlPagos_rec.r(INPUT creditos.nit,
                                                      INPUT creditos.num_credito,
                                                      INPUT creditos.tasa).
END.

/*

fec_aprobacion
fec_iniPag
plazo
cuotas pendientes
monto

Luego se cambia el plazo
*/
