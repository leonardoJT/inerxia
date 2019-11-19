/* FOR EACH Mov_creditos WHERE  Mov_Creditos.Cod_Credito EQ 22 AND Mov_Creditos.Num_Credito EQ 11003 */
/*          AND  Mov_Creditos.Nit EQ "70076967"  NO-LOCK:                                            */
/*     DISPLAY Mov_Creditos WITH 1 COL.                                                              */
/*                                                                                                   */
/* END.                                                 
                                             
                                             */
                                             
                                             
FOR EACH mov_contable WHERE mov_contable.nit = "70076967" 
                           AND Mov_Contable.Fec_Contable  GE  (TODAY - 5 )  NO-LOCK:
    DISPLAY Mov_Contable.Fec_Contable
            Mov_Contable.Cuenta
            Mov_Contable.Db
            Mov_Contable.Cr   
            Mov_Contable.Comentario FORMAT "XXXXXXXXXXXXXXXXXX"
        WITH WIDTH 255.
END.


FOR EACH Mov_creditos WHERE  Mov_Creditos.Nit EQ "70076967"  and
                            Mov_Creditos.Fecha GE (TODAY - 5)  NO-LOCK:
    DISPLAY Mov_Creditos WITH 1 COL.

END.





