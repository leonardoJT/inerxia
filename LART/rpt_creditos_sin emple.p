DEFINE VAR wtotal LIKE Creditos.Sdo_Capital NO-UNDO INIT 0.
FOR EACH Creditos WHERE (Creditos.Sdo_Capital NE 0 AND Creditos.Cod_Credito NE 62 )  NO-LOCK BY Creditos.Categoria :
    DISPLAY Creditos.Cod_Credito 
            Creditos.Cta_Contable
            Creditos.Categoria 
            Creditos.Sdo_Capital .
    PAUSE 0.
    ASSIGN  wtotal = wtotal + Creditos.Sdo_Capital.   
END.
DISPLAY  wtotal.
MESSAGE wtotal  
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


