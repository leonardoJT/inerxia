/*
    calculo de intereses causados para CDAT
*/

DEFINE VARIABLE vdePlazo AS DECIMAL     NO-UNDO.
DEFINE VARIABLE viIntCausado AS INTEGER     NO-UNDO.
DEFINE VARIABLE vdeTasa AS DECIMAL     NO-UNDO.

OUTPUT TO "c:\info_fodun\CDAT-IntCausado.txt".

FOR EACH ahorros WHERE tip_ahorro EQ 3 :
    ASSIGN  vdePlazo = TRUNCATE((Fec_Vencimiento - Fec_UltLiquidacion) / 30, 1)
            vdeTasa = IF cod_ahorro EQ 5 THEN 1.0050 ELSE 1.0060
            viIntCausado = (EXP(vdeTasa,vdePlazo) * ahorros.sdo_disponible) - ahorros.sdo_disponible.

    UPDATE ahorros.Int_Causado = viIntCausado.

/*     DISPLAY                    */
/*         ahorros.nit            */
/*         ahorros.cod_ahorro     */
/*         ahorros.cue_ahorro     */
/*         Fec_Vencimiento        */
/*         Fec_UltLiquidacion     */
/*         ahorros.sdo_disponible */
/*         vdePlazo               */
/*         tasa                   */
/*         viIntCausado           */
/*         WITH WIDTH 250 .       */
END.

OUTPUT CLOSE.
