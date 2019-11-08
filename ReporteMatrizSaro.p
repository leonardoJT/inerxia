FOR EACH saro WHERE  Saro.Fec_Aprobacion GE  TODAY  AND
                      Saro.Fec_Aprobacion LE  TODAY  NO-LOCK:
     FIND FIRST Pro_Saro WHERE Pro_Saro.Cod_Factor EQ integer(Saro.Cod_evento) NO-LOCK no-error.
     IF AVAILABLE(Pro_saro) THEN
         DISPLAY  Pro_Saro.Clase_ROperativo 
         Pro_Saro.Cod_Controles Pro_Saro.Cod_EveFactor 
         Pro_Saro.Cod_Factor Pro_Saro.Cod_Probabilidad 
         Pro_Saro.Cod_Severidad Pro_Saro.Cod_UbFactor 
         Pro_Saro.Estado Pro_Saro.Fec_Creacion Pro_Saro.Fec_Retiro 
         Pro_Saro.Tipo WITH 1 COL.
     RUN Describir.
END.

PROCEDURE describir:
    FIND FIRST Pro_Saro WHERE Pro_saro.Estado     = 1
                       AND Pro_Saro.Cod_Factor = INTEGER(Saro.Cod_Evento) NO-LOCK NO-ERROR.
    IF AVAILABLE Pro_Saro THEN DO:
        /*Ubicacion del Factor Interno o Externo */
        FIND FIRST varios WHERE Varios.tipo   = 38 
                            AND Varios.Codigo = Pro_Saro.Cod_UbFactor NO-LOCK NO-ERROR.
        IF AVAILABLE varios THEN DO:
          DISPLAY STRING(Varios.Codigo,"999") + " - " + Varios.Descripcion.  
        END.

        /*Factor de Riesgo */
        FIND FIRST varios WHERE Varios.tipo   = 37 
                            AND Varios.Codigo = Pro_Saro.Cod_EveFactor NO-LOCK NO-ERROR.
        IF AVAILABLE varios THEN DO:
          DISPLAY STRING(Varios.Codigo,"999") + " - " + Varios.Descripcion.  
        END.

        /*Clase de Riesgos*/
        FIND FIRST varios WHERE Varios.tipo   = 35
                            AND Varios.Codigo = Pro_Saro.Clase_ROperativo NO-LOCK NO-ERROR.
        IF AVAILABLE varios THEN DO:
          DISPLAY STRING(Varios.Codigo,"999") + " - " + Varios.Descripcion.  
        END.
    END.
END PROCEDURE.

