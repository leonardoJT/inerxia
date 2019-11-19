FOR EACH Creditos WHERE Creditos.Dias_Atraso GE 500  :
    ASSIGN Creditos.Dias_Atraso = INTE((Creditos.Dias_Atraso) / 12) .     
END.
