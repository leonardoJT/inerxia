DEFI VAR VrCons LIKE Mov_Aho.Val_efectivo.
DEFI VAR VrReti LIKE Mov_Aho.Val_efectivo.

OUTPUT TO C:\INFO_utrahuilca\ChequeoCdat.Txt.    

FOR EACH mov_Contab WHERE agencia                   LE 2 AND
                          Fec_Contab                GE DATE(04,01,2005) AND    
                          SUBSTRING(cuenta,1,4)     EQ "2110"                                              
                        NO-LOCK BREAK BY mov_Contab.Nit BY Fec_Contab BY Doc_Refer:
    DISPLAY mov_Contab.agencia LABEL "Ag."
            mov_Contab.Nit     LABEL "Ced/Nit"
            Fec_Contab         LABEL "Fec-Contab"
            Doc_Refer          LABEL "Doc-Refer."      
            Mov_Contable.Enlac LABEL "Enlace"
            Db                 LABEL "Retiros"
            Cr                 LABEL "Consignac." SKIP(0)
        WITH DOWN WIDTH 150 FRAME f1 NO-BOX NO-LABELS USE-TEXT STREAM-IO.
        
    IF LAST-OF(mov_Contab.Nit) THEN DO:
       FOR EACH mov_ahorros WHERE mov_ahorros.Nit EQ mov_Contab.Nit AND
                               Cod_Ahorro      EQ 4              AND
                               fecha           GE DATE(04,01,2005) NO-LOCK BY Fecha BY Cue_aho:
           FIND FIRST Operacion WHERE Operacion.Cod_Oper EQ Mov_aho.Cod_Oper NO-LOCK NO-ERROR. 
           IF Tipo_Operac EQ 1 THEN                                                            
              ASSIGN VrCons = Val_efectivo + val_cheque
                     VrReti = 0.
           ELSE                                                                                
              ASSIGN VrReti = Val_efectivo + val_cheque
                     VrCons = 0.
                                                                                               
           DISPLAY Mov_aho.Agenc LABEL "Ag."                                                   
                   mov_ahorros.Nit LABEL "Ced/Nit"                                             
                   fecha               LABEL "Fec-Movto"                                       
                   mov_ahorros.Cue_aho LABEL "Cta-Titulo"
                   Descrip             LABEL "Descrip-Transac." FORM "X(20)"
                   VrReti              LABEL "Retiro-Cta"                                      
                   VrCons              LABEL "Consig-Cta"
                    SKIP (0)                       
             WITH DOWN WIDTH 150 FRAME f2 NO-BOX NO-LABELS USE-TEXT STREAM-IO.                 
      END.
      DISPLAY SKIP(2).
    END.
END.
OUTPUT CLOSE.
