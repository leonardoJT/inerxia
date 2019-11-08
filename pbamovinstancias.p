/*el error de tsol sale cuando hay un registro en mov_instancias
  que no se encuentra en la tabla de creditos con el mismo nit y el
  mismo numero de credito
  sucede que hay mov_instancias que apuntan a un numero de credito
  que si existe pero para otro nit diferente*/
  
  
/*FOR EACH mov_instancias WHERE
         mov_instancias.instancia EQ 100 AND
         mov_instancias.agencia   EQ 1 AND
         mov_instancias.estado    EQ NO:
    FIND creditos WHERE 
         creditos.nit EQ mov_instancias.nit AND
         creditos.num_solicitud EQ mov_instancias.num_solicitud AND
         creditos.estado EQ 2
         NO-ERROR.
    IF NOT AVAILABLE creditos THEN DO:
      DISPLAY  mov_instancias.fec_ingreso mov_instancias.num_solicitu
               mov_instancias.cuenta mov_instancias.nit.
    END.
         
END.*/

/*FOR EACH creditos WHERE creditos.nit EQ "32527722":
    DISPLAY num_credito num_solicitud dias_atraso.
END.*/

FOR EACH creditos WHERE creditos.num_credito EQ 59:
    DISPLAY creditos.nit dias_atraso num_solicitud num_credito.
END.
