IF Inserted.Cod_Zona EQ 0 OR Inserted.Cod_Zona EQ ? THEN DO:
   MESSAGE "Código Zona no puede ser" SKIP
           "blanco o nulo. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.

IF Inserted.Nombre = "" THEN DO:
   MESSAGE "Nombre Zona no puede ser" SKIP
           "blanco. Verifique."
           VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
   RETURN ERROR.
END.

IF Inserted.Estado EQ 2 THEN DO:
   FIND FIRST Clientes WHERE Clientes.Cod_Zona EQ Inserted.Cod_Zona NO-LOCK NO-ERROR.
   IF AVAILABLE Clientes THEN DO:
      MESSAGE "La Zona Esta Asignada en Clientes" SKIP
              "no se puede Inactivar. Verifique"
              VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
      RETURN ERROR.
   END.  
   
   
   FIND FIRST Agencias WHERE Agencias.Zona EQ Inserted.Cod_Zona NO-LOCK NO-ERROR.
   IF AVAILABLE Agencias THEN DO:
      MESSAGE "La Zona Esta Asignada en Oficinas" SKIP
              "no se puede Inactivar. Verifique"
              VIEW-AS ALERT-BOX ERROR TITLE "ERROR CONFIGURACION".
      RETURN ERROR.
   END.          
END.
