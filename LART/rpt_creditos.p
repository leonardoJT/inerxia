DEFINE VAR wtotal LIKE Creditos.Sdo_Capital NO-UNDO INIT 0.
FOR EACH Creditos WHERE Creditos.Sdo_Capital NE 0  NO-LOCK BY Creditos.Categoria :
    DISPLAY Creditos.Categoria 
            Creditos.Sdo_Capital .
    PAUSE 0.
    ASSIGN  wtotal = wtotal + Creditos.Sdo_Capital.   
END.
DISPLAY  wtotal.
MESSAGE wtotal  
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
