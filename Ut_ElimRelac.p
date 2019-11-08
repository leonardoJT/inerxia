OUTPUT TO C:\InfRed\EliminarCodeu.txt.
FOR EACH Creditos WHERE Creditos.estado EQ 3 NO-LOCK:
    FOR EACH relaciones WHERE relaciones.nit EQ Creditos.Nit
                          AND Clase_Producto EQ 2
                          AND Cod_relacion   EQ 11
                          AND Cod_Producto   EQ Creditos.Cod_credito
                          AND integ(Cuenta)  EQ Num_credito
                          AND relaciones.Estado EQ 1:
        FIND FIRST solicitud WHERE solicitud.nit         EQ Creditos.Nit
                               AND solicitud.Cod_credito EQ Creditos.Cod_credito
                               AND solicitud.num_solicitud EQ integ(Cuenta)
                               AND solicitud.estado        EQ 1 NO-LOCK NO-ERROR.
        IF NOT AVAIL(Solicitud) THEN
           /*ASSIGN relaciones.Estado = 2*/
           DISPLAY Creditos.Agencia relaciones.nit Cuenta nit_Relacion.
    END.
END.
OUTPUT CLOSE.
