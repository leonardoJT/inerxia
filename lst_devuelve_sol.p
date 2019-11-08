/*FOR EACH solicitud WHERE   nit = "70877725":
    UPDATE solicitud EXCEPT verificacion WITH 1 COL.
END.
FOR EACH mov_instancia WHERE nit = "70877725" AND num_solicitud = 287:
    UPDATE mov_instancia EXCEPT descripcion WITH 1 COL.
END.
    */
FOR EACH creditos WHERE nit = "70877725" AND num_credito = 33920:
   DISPLAY creditos WITH 1 COL.
   DELETE creditos.
END.
