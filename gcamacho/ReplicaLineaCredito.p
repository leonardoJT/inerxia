
DEFINE BUFFER BPC FOR pro_creditos.
DEFINE BUFFER BCL FOR cortolargo.
DEFINE BUFFER BCV FOR carteraVencida.
DEFINE BUFFER BLI FOR Liqui_int.

FIND FIRST pro_creditos WHERE pro_creditos.cod_credito EQ 113 NO-LOCK NO-ERROR.
IF AVAILABLE pro_creditos THEN DO:
    CREATE BPC.
    BUFFER-COPY pro_creditos EXCEPT cod_credito TO BPC.
    UPDATE BPC.cod_credito = 114.
END.

FOR EACH cortolargo WHERE CortoLargo.Clase_Producto EQ 2 AND
                          CortoLargo.Cod_Producto EQ 113 NO-LOCK:
    CREATE BCL.
    BUFFER-COPY cortolargo EXCEPT cod_producto TO BCL.
    UPDATE BCL.cod_producto = 114.
END.

FOR EACH carteraVencida WHERE cod_prod EQ 113 NO-LOCK:
    CREATE BCV.
    BUFFER-COPY carteraVencida EXCEPT cod_producto TO BCV.
    UPDATE BCV.cod_producto = 114.
END.

FOR EACH Liqui_int WHERE Liqui_int.cod_prod = 113 NO-LOCK:
    CREATE BLI.
    BUFFER-COPY Liqui_int EXCEPT cod_prod TO BLI.
    UPDATE BLI.cod_producto = 114.
END.
