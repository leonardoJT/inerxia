DEFINE VAR cont AS INTEGER.
DEFINE VAR descuadre AS DECIMAL INITIAL 117429642.

FOR EACH creditos WHERE sdo_Capital > 0
                    AND INT_corriente > 0
                    AND estado = 2 NO-LOCK:
    cont = cont + 1.
END.

MESSAGE descuadre / cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


DEFINE VAR disminuye AS DECIMAL.
DEFINE VAR fraccion AS DECIMAL INITIAL 46637.

disminuye = descuadre.

FOR EACH creditos WHERE creditos.sdo_capital > 0
                    AND creditos.INT_corriente > 0
                    AND creditos.estado = 2:
    IF disminuye >= fraccion THEN DO:
        creditos.INT_corriente = creditos.INT_corriente + fraccion.
        disminuye = disminuye - fraccion.
    END.
    ELSE DO:
        creditos.INT_corriente = creditos.INT_corriente + disminuye.

        MESSAGE disminuye
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
        LEAVE.
    END.
END.
