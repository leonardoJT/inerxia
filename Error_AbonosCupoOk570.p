DEFINE VAR vdtotal  AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VAR vdiferen AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VAR vnvoval  AS DECIMAL INITIAL 0 NO-UNDO. 
DEFINE VAR vdfecha  AS DATE INITIAL ? NO-UNDO.
DEFINE VAR vlsi     AS LOGICAL INITIAL FALSE NO-UNDO.
DEFI TEMP-TABLE TempMovi
     FIELD AgenCont      LIKE mov_contable.Agencia
     FIELD AgenPro       LIKE Facturacion.Agencia
     FIELD Fec_contable  LIKE mov_contable.fec_contable
     FIELD Comprobante   LIKE mov_contable.Comprobante
     FIELD Nit           LIKE mov_contable.Nit
     FIELD Num_Credito   LIKE mov_contable.doc_referencia
     FIELD cod_credito   LIKE creditos.cod_credito
     FIELD Num_Documento LIKE mov_contable.num_documento
     FIELD Concepto      AS CHARACTER FORMAT "X(15)"
     FIELD Vl_Mora       AS DECIMAL
     FIELD Vl_Interes    AS DECIMAL
     FIELD Rec_Mora      AS DECIMAL
     FIELD Rec_Interes   AS DECIMAL
     FIELD val_recaudo   AS DECIMAL
     FIELD val_contable  AS DECIMAL
     FIELD Diferencia    AS DECIMAL
     FIELD Valor         AS DECIMAL
     INDEX agencia fec_contable Comprobante Nit.

ActFactura:
REPEAT TRANSACTION ON ERROR UNDO ActFactura, LEAVE ActFactura:

    OUTPUT TO "c:\info_juriscoop\ErrorProd19Marzo570.txt".
    PUT "AgenCont;AgenPro;Comprobante;Nit;Num_Credito;Cod_Credito;Fec_Contable;Num_Documento;Concepto;" 
        "Int.Mora;Int.Corriente;Rec_Mora;Rec_Interes;val_recaudo;val_contable;Diferencia;NvoValor" SKIP.
    
    FOR EACH mov_contable WHERE 
        mov_contable.fec_contable GE DATE("08/03/2008") AND  /* GT TODAY - 9 AND*/
        mov_contable.cuenta = "11050501" AND 
        mov_contable.db GT 0  AND
        (mov_contable.nit NE "12193675" AND mov_contable.nit NE "16219330" AND 
         mov_contable.nit NE "86048984")
        NO-LOCK BREAK
        BY mov_contable.agencia BY mov_contable.nit BY mov_contable.Doc_Referencia:
        ASSIGN vdiferen = 0 vnvoval  = 0 vlsi = FALSE.
        FIND FIRST creditos WHERE
            creditos.nit          EQ mov_contable.nit                     AND
            creditos.num_credito  EQ integer(mov_contable.Doc_Referencia) AND
            (creditos.cod_credito EQ 570) /* OR creditos.cod_credito EQ 870)*/
            NO-ERROR.
        IF AVAILABLE (creditos) THEN DO:
            IF FIRST-OF (mov_contable.agencia) OR FIRST-OF (mov_contable.nit) OR FIRST-OF (mov_contable.Doc_Referencia) THEN
                ASSIGN vdtotal = 0
                       vdfecha = mov_contable.fec_contable.
    
            ASSIGN vdtotal = vdtotal + mov_contable.db.
            IF LAST-OF (mov_contable.agencia) OR LAST-OF (mov_contable.nit) OR LAST-OF (mov_contable.Doc_Referencia) THEN DO:
                FIND FIRST facturacion WHERE 
                     facturacion.estado = 1 AND facturacion.val_recaudo GT 0 AND
                     facturacion.nit = mov_contable.nit AND
                     facturacion.num_credito = integer(mov_contable.Doc_Referencia) AND
                     facturacion.val_recaudo NE vdtotal
                     NO-ERROR.
                IF AVAILABLE (facturacion) THEN DO:
                   ASSIGN vdiferen = round(facturacion.val_recaudo - vdtotal,0).
    
                   /* Para valores Negativos*/
                   IF creditos.Int_MorCobrar LT 0 THEN DO:
                      ASSIGN vnvoval  = facturacion.Rec_IntMora - vdiferen.
                      FIND FIRST mov_creditos WHERE 
                           mov_creditos.cpte           EQ mov_contable.comprobante AND
                           mov_creditos.nit            EQ mov_contable.nit AND 
                           mov_creditos.num_credito    EQ INTEGER(mov_contable.Doc_Referencia) AND
                           mov_creditos.num_documento  EQ STRING(mov_contable.num_documento) AND 
                           mov_creditos.Ofi_Fuente     EQ facturacion.agencia  AND
                           mov_creditos.fecha          EQ mov_contable.fec_contable AND
                          (mov_creditos.cod_operacion  EQ 20101002)
                           NO-ERROR.
                      IF AVAILABLE mov_creditos THEN DO:
                          CREATE TempMovi.
                          UPDATE TempMovi.AgenCont      = mov_contable.agencia 
                                 TempMovi.AgenPro       = facturacion.agencia  
                                 TempMovi.Fec_contable  = mov_contable.Fec_contable 
                                 TempMovi.Comprobante   = mov_contable.Comprobante  
                                 TempMovi.Nit           = mov_contable.Nit          
                                 TempMovi.Num_Credito   = mov_contable.Doc_Referencia
                                 TempMovi.cod_credito   = creditos.cod_credito
                                 TempMovi.Num_Documento = mov_contable.Num_Documento
                                 TempMovi.Concepto      = "Mora"
                                 TempMovi.Vl_Mora       = creditos.INT_morcobrar
                                 TempMovi.Vl_Interes    = 0
                                 TempMovi.Rec_Mora      = facturacion.Rec_IntMora
                                 TempMovi.Rec_Interes   = facturacion.Rec_IntCorrientes
                                 TempMovi.val_recaudo   = facturacion.val_recaudo   
                                 TempMovi.val_contable  = vdtotal                   
                                 TempMovi.Diferencia    = vdiferen
                                 TempMovi.Valor         = vnvoval.
    
                          IF mov_creditos.val_efectivo GT 0 THEN
                             UPDATE mov_creditos.val_efectivo = vnvoval.
                          IF mov_creditos.val_cheque GT 0 THEN
                             UPDATE mov_creditos.val_cheque = vnvoval.
                      END.
    
                      UPDATE creditos.Int_MorCobrar = 0.
                      IF creditos.Int_Corrientes LT 0 THEN DO:
                         UPDATE creditos.Int_Corrientes = 0.
                         IF creditos.Sdo_Capital LT 0 THEN
                            UPDATE creditos.Sdo_Capital = 0.
                      END.
                   END.
                   ELSE
                     IF creditos.Int_Corrientes LT 0 THEN DO:
                        ASSIGN vnvoval  = facturacion.Rec_IntCorrientes - vdiferen.
                        FIND FIRST mov_creditos WHERE
                             mov_creditos.cpte           EQ mov_contable.comprobante AND
                             mov_creditos.nit            EQ mov_contable.nit AND
                             mov_creditos.num_credito    EQ INTEGER(mov_contable.Doc_Referencia) AND
                             mov_creditos.num_documento  EQ STRING(mov_contable.num_documento) AND
                             mov_creditos.Ofi_Fuente     EQ facturacion.agencia  AND
                             mov_creditos.fecha          EQ mov_contable.fec_contable AND
                            (mov_creditos.cod_operacion  EQ 20101003)
                             NO-ERROR.
                        IF AVAILABLE mov_creditos THEN DO:
                           CREATE TempMovi.
                           UPDATE TempMovi.AgenCont      = mov_contable.agencia
                                  TempMovi.AgenPro       = facturacion.agencia
                                  TempMovi.Fec_contable  = mov_contable.Fec_contable 
                                  TempMovi.Comprobante   = mov_contable.Comprobante  
                                  TempMovi.Nit           = mov_contable.Nit  
                                  TempMovi.cod_credito   = creditos.cod_credito
                                  TempMovi.Num_Credito   = mov_contable.Doc_Referencia
                                  TempMovi.Num_DoCumento = mov_contable.Num_Documento
                                  TempMovi.Concepto      = "Corrientes"
                                  TempMovi.Vl_Mora       = 0
                                  TempMovi.Vl_Interes    = creditos.INT_corrientes
                                  TempMovi.Rec_Mora      = facturacion.Rec_IntMora
                                  TempMovi.Rec_Interes   = facturacion.Rec_IntCorrientes
                                  TempMovi.val_recaudo   = facturacion.val_recaudo   
                                  TempMovi.val_contable  = vdtotal                   
                                  TempMovi.Diferencia    = vdiferen
                                  TempMovi.Valor         = vnvoval.
    
                           IF mov_creditos.val_efectivo GT 0 THEN
                              UPDATE mov_creditos.val_efectivo = vnvoval.
                           IF mov_creditos.val_cheque GT 0 THEN
                              UPDATE mov_creditos.val_cheque = vnvoval.
                        END.
    
                        UPDATE creditos.Int_Corrientes = 0.
                        IF creditos.Sdo_Capital LT 0 THEN
                           UPDATE creditos.Sdo_Capital = 0.
                     END.
                     ELSE DO:
                       /* Para valores Mayores o Iguales a Cero*/
                       IF creditos.Int_MorCobrar GE 0 THEN DO:
                          ASSIGN vnvoval  = facturacion.Rec_IntMora - vdiferen.
                          IF facturacion.Rec_IntMora GE vnvoval AND vnvoval GT 0 THEN DO:
                             ASSIGN vlsi = TRUE.
                             FIND FIRST mov_creditos WHERE
                                  mov_creditos.cpte           EQ mov_contable.comprobante AND
                                  mov_creditos.nit            EQ mov_contable.nit AND
                                  mov_creditos.num_credito    EQ INTEGER(mov_contable.Doc_Referencia) AND
                                  mov_creditos.num_documento  EQ STRING(mov_contable.num_documento) AND
                                  mov_creditos.Ofi_Fuente     EQ facturacion.agencia  AND
                                  mov_creditos.fecha          EQ mov_contable.fec_contable AND
                                 (mov_creditos.cod_operacion  EQ 20101002)
                                  NO-ERROR.
                             IF AVAILABLE mov_creditos THEN DO:
                                CREATE TempMovi.
                                UPDATE TempMovi.AgenCont      = mov_contable.agencia
                                       TempMovi.AgenPro       = facturacion.agencia
                                       TempMovi.Fec_contable  = mov_contable.Fec_contable 
                                       TempMovi.Comprobante   = mov_contable.Comprobante  
                                       TempMovi.Nit           = mov_contable.Nit  
                                       TempMovi.cod_credito   = creditos.cod_credito
                                       TempMovi.Num_Credito   = mov_contable.Doc_Referencia
                                       TempMovi.Num_DoCumento = mov_contable.Num_Documento
                                       TempMovi.Concepto      = "Mora"
                                       TempMovi.Vl_Mora       = creditos.INT_morcobrar
                                       TempMovi.Vl_Interes    = 0
                                       TempMovi.Rec_Mora      = facturacion.Rec_IntMora
                                       TempMovi.Rec_Interes   = facturacion.Rec_IntCorrientes
                                       TempMovi.val_recaudo   = facturacion.val_recaudo   
                                       TempMovi.val_contable  = vdtotal                   
                                       TempMovi.Diferencia    = vdiferen
                                       TempMovi.Valor         = vnvoval.
        
                             IF mov_creditos.val_efectivo GT 0 THEN
                                UPDATE mov_creditos.val_efectivo = vnvoval.
                             IF mov_creditos.val_cheque GT 0 THEN
                                UPDATE mov_creditos.val_cheque = vnvoval.
                             UPDATE creditos.Int_MorCobrar = creditos.Int_MorCobrar + (facturacion.Rec_IntMora - vnvoval).
                             END.
                          END.
                       END.
                       
                       IF creditos.Int_Corrientes GE 0 AND vlsi EQ FALSE THEN DO:
                          ASSIGN vnvoval  = facturacion.Rec_IntCorrientes - vdiferen.
                          IF facturacion.Rec_IntCorrientes GE vnvoval AND vnvoval GT 0 THEN DO:
                             FIND FIRST mov_creditos WHERE 
                                  mov_creditos.cpte           EQ mov_contable.comprobante AND
                                  mov_creditos.nit            EQ mov_contable.nit AND 
                                  mov_creditos.num_credito    EQ INTEGER(mov_contable.Doc_Referencia) AND
                                  mov_creditos.num_documento  EQ STRING(mov_contable.num_documento) AND 
                                  mov_creditos.Ofi_Fuente     EQ facturacion.agencia  AND
                                  mov_creditos.fecha          EQ mov_contable.fec_contable AND
                                 (mov_creditos.cod_operacion  EQ 20101003)
                                  NO-ERROR.
                             IF AVAILABLE mov_creditos THEN DO:
                                CREATE TempMovi.
                                UPDATE TempMovi.AgenCont      = mov_contable.agencia
                                       TempMovi.AgenPro       = facturacion.agencia
                                       TempMovi.Fec_contable  = mov_contable.Fec_contable 
                                       TempMovi.Comprobante   = mov_contable.Comprobante  
                                       TempMovi.Nit           = mov_contable.Nit  
                                       TempMovi.cod_credito   = creditos.cod_credito
                                       TempMovi.Num_Credito   = mov_contable.Doc_Referencia
                                       TempMovi.Num_DoCumento = mov_contable.Num_Documento
                                       TempMovi.Concepto      = "Corrientes"
                                       TempMovi.Vl_Mora       = 0
                                       TempMovi.Vl_Interes    = creditos.INT_corrientes
                                       TempMovi.Rec_Mora      = facturacion.Rec_IntMora
                                       TempMovi.Rec_Interes   = facturacion.Rec_IntCorrientes
                                       TempMovi.val_recaudo   = facturacion.val_recaudo
                                       TempMovi.val_contable  = vdtotal
                                       TempMovi.Diferencia    = vdiferen
                                       TempMovi.Valor         = vnvoval.
        
                              IF mov_creditos.val_efectivo GT 0 THEN
                                 UPDATE mov_creditos.val_efectivo = vnvoval.
                              IF mov_creditos.val_cheque GT 0 THEN
                                 UPDATE mov_creditos.val_cheque = vnvoval.
                              UPDATE creditos.Int_Corrientes = creditos.Int_Corrientes + (facturacion.Rec_IntCorrientes - vnvoval).
                             END.
                          END.
                       END.
                     END.
                END.
            END.
        END.
    END.
    
    FOR EACH TempMovi BY TempMovi.AgenCont BY TempMovi.Nit:
        PUT TempMovi.AgenCont       ";"
            TempMovi.AgenPro        ";"
            TempMovi.Comprobante    ";"
            TempMovi.Nit            ";"
            TempMovi.Num_Credito    ";"
            TempMovi.cod_credito    ";"
            TempMovi.Fec_Contable   ";"
            TempMovi.Num_Documento  ";"
            TempMovi.Concepto       ";"
            TempMovi.Vl_Mora        ";"
            TempMovi.Vl_Interes     ";"
            TempMovi.Rec_Mora       ";"
            TempMovi.Rec_Interes    ";"
            TempMovi.val_recaudo    ";"
            TempMovi.val_contable   ";"
            TempMovi.Diferencia     ";"
            TempMovi.Valor
            SKIP.
    END.
    
    /* Parte Contable */
    FOR EACH TempMovi:
        IF TempMovi.Concepto = "Mora" THEN DO:
           FOR EACH mov_contable WHERE
                mov_contable.comprobante    EQ TempMovi.Comprobante   AND
                mov_contable.nit            EQ TempMovi.nit           AND 
                mov_contable.Doc_Referencia EQ TempMovi.num_credito   AND
                mov_contable.num_documento  EQ TempMovi.num_documento AND 
                mov_contable.agencia        EQ TempMovi.agencont       AND      
                mov_contable.fec_contable   EQ TempMovi.fec_contable  AND
                mov_contable.cuenta         EQ "1605950518" 
                EXCLUSIVE:

                IF mov_contable.Db GT 0 THEN
                   UPDATE mov_contable.Db = TempMovi.Valor.
                IF mov_contable.Cr GT 0 THEN
                   UPDATE mov_contable.Cr = TempMovi.Valor.
           END.
        END.
        ELSE
          IF TempMovi.Concepto = "Corrientes" THEN DO:
             FOR EACH mov_contable WHERE
                  mov_contable.comprobante    EQ TempMovi.Comprobante   AND
                  mov_contable.nit            EQ TempMovi.nit           AND 
                  mov_contable.Doc_Referencia EQ TempMovi.num_credito   AND
                  mov_contable.num_documento  EQ TempMovi.num_documento AND 
                  mov_contable.agencia        EQ TempMovi.agencont       AND      
                  mov_contable.fec_contable   EQ TempMovi.fec_contable  AND
                  mov_contable.cuenta         EQ "160518"
                  EXCLUSIVE:

                  IF mov_contable.Db GT 0 THEN
                     UPDATE mov_contable.Db = TempMovi.Valor.
                  IF mov_contable.Cr GT 0 THEN
                     UPDATE mov_contable.Cr = TempMovi.Valor.
             END.
          END.
    END.
    LEAVE.
END. /*TRANS*/
MESSAGE "Archivo Generado : " "c:\info_juriscoop\ErrorProd19Marzo570.txt"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
