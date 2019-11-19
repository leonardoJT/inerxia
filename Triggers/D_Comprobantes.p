TRIGGER PROCEDURE FOR DELETE OF Comprobantes.
 FIND FIRST Mov_Contable WHERE Mov_Contable.Comprobante = Comprobantes.Comprobante
                         NO-LOCK NO-ERROR.
 IF AVAILABLE(Mov_Contable) THEN DO:
    MESSAGE "No Puede Borrar Comprobante porque existe en Movimiento Contable"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Borrar Tipo de Comprobante".
            RETURN ERROR.
          END.  
 ELSE DO:
      FIND FIRST Varios WHERE Varios.Comprobante = Comprobantes.Comprobante
                          AND Varios.Estado      = 1
                        NO-LOCK NO-ERROR.
      IF AVAILABLE(Varios) THEN DO:
         MESSAGE "No Puede Borrar Comprobante porque existe en Varios"
              VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Borrar Tipo de Comprobante".
              RETURN ERROR.
            END. 
      ELSE DO:  
           FIND FIRST Operacion WHERE Operacion.Comprobante = Comprobantes.Comprobante
                                  AND Operacion.Estado      = 1 NO-LOCK NO-ERROR.
           IF AVAILABLE(Operacion) THEN DO:
              MESSAGE "No Puede Borrar Comprobante porque existe en Operaciones"
                 VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Borrar Tipo de Comprobante".
                 RETURN ERROR.
                END. 
           ELSE 
           Grabar:
           DO ON ERROR  UNDO, RETRY
              ON ENDKEY UNDO, RETRY:   
              FIND CURRENT Comprobantes EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
              IF AVAILABLE(Comprobantes) THEN DO:
                 IF LOCKED Comprobantes THEN 
                    UNDO Grabar, RETRY Grabar.   
                     MESSAGE "El Comprobante ha sido Borrado"
                     VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Borrar Tipo de Comprobante".
            END.
         END.
      END.          
 END. 
