
/* /** Ajusta Valores Negativos*/                                                       */
FOR EACH creditos WHERE
    (cod_credito EQ 570 OR cod_credito EQ 870) AND
    creditos.sdo_capital    LT 0 OR creditos.INT_Corrientes LT 0 OR
    creditos.Int_MoraDifCob LT 0 OR creditos.Int_MorCobrar  LT 0 OR
    creditos.Int_DifCobro   LT 0 OR Creditos.INT_anticipado LT 0 OR
    creditos.polizas        LT 0:
    DISPLAY agencia nit num_credito 
            creditos.sdo_capital
            creditos.INT_Corrientes
            creditos.Int_MoraDifCob
            creditos.Int_MorCobrar
            creditos.Int_DifCobro
            Creditos.INT_anticipado
            creditos.polizas.
                                     
/*     IF creditos.sdo_capital LT 0 THEN      */
/*        UPDATE creditos.sdo_capital = 0.    */
/*     IF creditos.INT_Corrientes LT 0 THEN   */
/*        UPDATE creditos.INT_Corrientes = 0. */
/*     IF creditos.Int_MoraDifCob LT 0 THEN   */
/*        UPDATE creditos.Int_MoraDifCob = 0. */
/*     IF creditos.Int_MorCobrar LT 0 THEN    */
/*        UPDATE creditos.Int_MorCobrar = 0.  */
/*     IF creditos.Int_DifCobro LT 0 THEN     */
/*        UPDATE creditos.Int_DifCobro = 0.   */
/*     IF creditos.INT_anticipado LT 0 THEN   */
/*        UPDATE creditos.INT_anticipado = 0. */
END.
