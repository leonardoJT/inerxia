DEFINE VAR wcodigo LIKE Creditos.Cod_Credito NO-UNDO.
FOR EACH creditos WHERE Creditos.Estado = 2  NO-LOCK BY Creditos.Cod_Credito :
    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito  EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Pro_Creditos THEN DO:
        MESSAGE "No exixte Configuracion Credito "
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        NEXT. 
    END.
    DISPLAY
        Creditos.Cod_Credito
        Pro_Creditos.Cod_Tasa
        Pro_Creditos.Nom_Producto  FORMAT "XXXXXXXXXXXXXXXXXXXXXXX"
        Creditos.Num_Credito 
        Creditos.Sdo_Capital
        Creditos.Estado VIEW-AS TEXT 
     WITH   WIDTH  200.
END.
