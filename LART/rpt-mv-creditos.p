OUTPUT TO c:\INFO_fodun\mvcreditos-8258791.txt.
FOR EACH mov_creditos WHERE nit EQ "8258791"  NO-LOCK BY Mov_Creditos.Cod_Credito :
    FIND LAST creditos WHERE (Creditos.Cod_Credito EQ Mov_Creditos.Cod_Credito) AND (Creditos.Num_Credito EQ Mov_Creditos.Num_Credito) NO-ERROR.
    DISPLAY Mov_Creditos.Nit   COLUMN-LABEL "Cedula"
            Mov_Creditos.Fecha COLUMN-LABEL "F.Pago"
            Mov_Creditos.Num_Documento COLUMN-LABEL "Documento"
            Mov_Creditos.Cod_Credito   COLUMN-LABEL "Linea"
            Mov_Creditos.Num_Credito   COLUMN-LABEL "Credito"
            Mov_Creditos.Descrip       COLUMN-LABEL "Concepto"
            Creditos.Fec_Pago          COLUMN-LABEL "F.Ubica"
            Mov_Creditos.Val_Efectivo
        WITH WIDTH  500.     
    
END.

FOR EACH mov_creditos WHERE nit EQ "8543153"  NO-LOCK BY Mov_Creditos.Cod_Credito :
    FIND LAST creditos WHERE (Creditos.Cod_Credito EQ Mov_Creditos.Cod_Credito) AND (Creditos.Num_Credito EQ Mov_Creditos.Num_Credito) NO-ERROR.
    DISPLAY Mov_Creditos.Nit   COLUMN-LABEL "Cedula"
            Mov_Creditos.Fecha COLUMN-LABEL "F.Pago"
            Mov_Creditos.Num_Documento COLUMN-LABEL "Documento"
            Mov_Creditos.Cod_Credito   COLUMN-LABEL "Linea"
            Mov_Creditos.Num_Credito   COLUMN-LABEL "Credito"
            Mov_Creditos.Descrip       COLUMN-LABEL "Concepto"
            Creditos.Fec_Pago          COLUMN-LABEL "F.Ubica"
            Mov_Creditos.Val_Efectivo
        WITH WIDTH  500.     
    
END.

FOR EACH mov_creditos WHERE nit EQ "71672217"  NO-LOCK BY Mov_Creditos.Cod_Credito :
    FIND LAST creditos WHERE (Creditos.Cod_Credito EQ Mov_Creditos.Cod_Credito) AND (Creditos.Num_Credito EQ Mov_Creditos.Num_Credito) NO-ERROR.
    DISPLAY Mov_Creditos.Nit   COLUMN-LABEL "Cedula"
            Mov_Creditos.Fecha COLUMN-LABEL "F.Pago"
            Mov_Creditos.Num_Documento COLUMN-LABEL "Documento"
            Mov_Creditos.Cod_Credito   COLUMN-LABEL "Linea"
            Mov_Creditos.Num_Credito   COLUMN-LABEL "Credito"
            Mov_Creditos.Descrip       COLUMN-LABEL "Concepto"
            Creditos.Fec_Pago          COLUMN-LABEL "F.Ubica"
            Mov_Creditos.Val_Efectivo
        WITH WIDTH  500.     
    
END.


FOR EACH mov_creditos WHERE nit EQ "16618743"  NO-LOCK BY Mov_Creditos.Cod_Credito :
    FIND LAST creditos WHERE (Creditos.Cod_Credito EQ Mov_Creditos.Cod_Credito) AND (Creditos.Num_Credito EQ Mov_Creditos.Num_Credito) NO-ERROR.
    DISPLAY Mov_Creditos.Nit   COLUMN-LABEL "Cedula"
            Mov_Creditos.Fecha COLUMN-LABEL "F.Pago"
            Mov_Creditos.Num_Documento COLUMN-LABEL "Documento"
            Mov_Creditos.Cod_Credito   COLUMN-LABEL "Linea"
            Mov_Creditos.Num_Credito   COLUMN-LABEL "Credito"
            Mov_Creditos.Descrip       COLUMN-LABEL "Concepto"
            Creditos.Fec_Pago          COLUMN-LABEL "F.Ubica"
            Mov_Creditos.Val_Efectivo
        WITH WIDTH  500.     
    
END.

OUTPUT CLOSE.
