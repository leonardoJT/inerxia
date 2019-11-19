FOR EACH creditos WHERE creditos.estado = 2 AND creditos.cod_credito <> 123 AND nit > "10247085" NO-LOCK BY creditos.nit:
    FIND FIRST extras WHERE extras.num_solicitud = creditos.num_solicitud NO-LOCK NO-ERROR.
    IF AVAILABLE extras THEN
        DISPLAY creditos.nit creditos.num_credito creditos.sdo_Capital.
END.
