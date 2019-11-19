DEFINE VAR wconta AS INTEGER INIT 0 NO-UNDO.
/* 
FOR EACH Pro_Creditos NO-LOCK :
    DISPLAY Pro_Creditos.Cod_Credito
            Pro_Creditos.Cod_Tasa 
            Pro_Creditos.Nom_Producto
         WITH WIDTH 250 .
END.
*/

FOR EACH Creditos WHERE Creditos.Cta_Contable NE " " AND Creditos.Cod_Credito = 62  NO-LOCK :
    IF Creditos.Sdo_Capital NE 0  THEN DO:
            DISPLAY  Creditos.Cod_Credito
                     Creditos.Cta_Contable.
            wconta = wconta + 1.
    END.
END.

MESSAGE "Contador " + STRING(wconta)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
