   DEFINE VARIABLE W_AsigInst LIKE Cfg_Instancias.Instancia. 
   DEFINE VARIABLE W_ord LIKE Instancias.Orden_Instancia.
   FOR EACH creditos WHERE nit = "42880563" AND agencia EQ 1:
       FOR EACH cfg_Instancias WHERE /*busca la instancia del rango de mora */
                Cfg_Instancias.Agencia        EQ Creditos.Agencia AND
                Cfg_Instancias.Tipo_Instancia EQ 2                      AND
                Cfg_Instancias.Plazo_Minimo   LE Creditos.Dias_Atraso   AND
                Cfg_Instancias.Plazo_Maximo   GE Creditos.Dias_Atraso   AND
                Cfg_Instancias.Monto_Minimo   LE Creditos.Monto AND
                Cfg_Instancias.Monto_Maximo   GE Creditos.Monto AND 
                Cfg_Instancias.Estado         EQ 1 NO-LOCK:
                
               ASSIGN W_AsigInst = Cfg_Instancias.Instancia. 

               FIND FIRST Mov_Instancias WHERE
                 Mov_instancia.Instancia      EQ Cfg_Instancias.Instancia     AND
                 Mov_Instancias.Cuenta        EQ STRING(Creditos.Num_Credito) AND
                 Mov_Instancias.Num_Solicitud EQ Creditos.Num_Solicitud       AND
                 Mov_instancias.Nit           EQ Creditos.Nit                 AND
                 Mov_Instancias.Estado        EQ NO NO-LOCK NO-ERROR.
                  IF AVAILABLE(mov_instancias) THEN DISPLAY "disp".

               /*DISPLAY W_AsigInst.       
               DISPLAY creditos.monto.
               DISPLAY Cfg_Instancias WITH 1 COLUMN.*/
       END.

        FIND FIRST Mov_Instancias WHERE
             Mov_Instancias.Cuenta        EQ STRING(Creditos.Num_Credito) AND
             Mov_Instancias.Num_Solicitud EQ Creditos.Num_Solicitud       AND
             Mov_instancias.Nit           EQ Creditos.Nit                 AND
             Mov_Instancias.Estado        EQ NO NO-ERROR.
        IF AVAILABLE Mov_Instancias THEN DO:
            DISPLAY "asignado".
        END.
        ELSE DO:
          DISPLAY " no asignado".
          FIND FIRST Instancias WHERE Instancias.Tipo_Instancia EQ 2 AND
                               Instancias.Instancia      EQ W_AsigInst AND 
                               Instancias.Tipo_Producto  EQ 2 AND
                               Instancias.Estado         EQ 1 NO-LOCK NO-ERROR.
            IF AVAILABLE Instancias THEN DO:
                W_ord = Instancias.Orden_Instancia.
                /*DISPLAY  instancias WITH 1 column.*/
                DISPLAY Instancias.Tipo_Instancia Instancias.Orden_Instancia  Creditos.Dias_Atraso Creditos.Monto.
                FOR EACH Cfg_Instancias WHERE Cfg_Instancias.Agencia        EQ 1 AND
                                         Cfg_Instancias.Tipo_Instancia EQ Instancias.Tipo_Instancia AND
                                         Cfg_Instancias.Orden          EQ Instancias.Orden_Instancia AND
                                         Cfg_Instancias.Plazo_Minimo   LE Creditos.Dias_Atraso AND
                                         Cfg_Instancias.Plazo_Maximo   GE Creditos.Dias_Atraso AND
                                         Cfg_Instancias.Monto_Minimo   LE Creditos.Monto AND
                                         Cfg_Instancias.Monto_Maximo   GE Creditos.Monto AND 
                                        Cfg_Instancias.Estado         EQ 1:
                            DISPLAY   Cfg_Instancias WITH 1 COLUMN.
                END.
            END.
        END.
 END.            
