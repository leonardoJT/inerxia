DISABLE TRIGGERS FOR LOAD OF Sal_Cuenta.
DEFINE VAR wsaldo LIKE Sal_Cuenta.sal_inicial.
FOR EACH Sal_Cuenta WHERE Sal_Cuenta.agencia EQ 1 AND  Sal_Cuenta.Ano EQ 2011 
                        AND Sal_Cuenta.Cuenta EQ "1655180107"   :
    /* DISPLAY Sal_Cuenta WITH  1 COL. */ 
    DISPLAY Sal_Cuenta.agencia 
            Sal_Cuenta.Ano 
            Sal_Cuenta.Cuenta 
            Sal_Cuenta.cr[4]  NO-LABEL.
    ASSIGN wsaldo =  Sal_Cuenta.cr[4] - 215.26.
    DISPLAY wsaldo.
    /* ASSIGN Sal_Cuenta.cr[4] =  wsaldo.    */ 
    
END.
