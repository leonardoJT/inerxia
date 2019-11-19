/* FOR EACH mov_contable WHERE cuenta = "24050501" NO-LOCK: */
/*     DISPLAY mov_contable EXCEPT COMENTARIO  WITH 1 COL . */
/*                                                          */
/* END.                                                     */


FOR EACH mov_contable WHERE cuenta BEGINS  "240510" NO-LOCK:
    DISPLAY
        Mov_Contable.Fec_Contable 
        Mov_contable.cuenta
        /* Mov_Contable.Fec_Grabacion */
        STRING(mov_contable.HORA,"HH:MM AM")
        Mov_Contable.agencia 
        Mov_Contable.Comprobante
        Mov_Contable.Num_Documento 
        Mov_Contable.Usuario
        Mov_Contable.Comentario  FORMAT "xxxxxxxxxxxxxxxxxxxxxxx"
        Mov_Contable.Db 
        Mov_Contable.Cr
        WITH WIDTH 500     .
    
END.
