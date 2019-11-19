DEFINE VAR wdb AS DECIMAL INIT 0 NO-UNDO.
DEFINE VAR wcr AS DECIMAL INIT 0 NO-UNDO.
DEFINE VAR wdi AS DECIMAL INIT 0 NO-UNDO FORMAT "->>>,>>>>,>>>9.99".
FOR EACH mov_contable WHERE agencia = 1 AND year(Mov_Contable.Fec_Contable) EQ 2011 AND
                            MONTH(Mov_Contable.Fec_Contable) EQ 4 AND DAY(Mov_Contable.Fec_Contable) EQ 1
                             NO-LOCK
                             BY Mov_Contable.Fec_Contable BY Mov_Contable.Comprobante 
                             BY Mov_Contable.Num_Documento
                             :
    DISPLAY Mov_Contable.agencia 
            Mov_Contable.Comprobante
            Mov_Contable.Num_Documento
            Mov_Contable.Fec_Contable 
            Mov_Contable.Cuenta 
            Mov_Contable.Db 
            Mov_Contable.Cr
        WITH WIDTH 400.
    PAUSE 0.
    ASSIGN wdb = wdb + Mov_Contable.Db.
           wcr = wcr + Mov_Contable.Cr.

END.
ASSIGN wdi = wdb -  wcr.
MESSAGE "Diferencia :  "  + STRING(wdi)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
