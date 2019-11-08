DEFINE TEMP-TABLE TTAgencias 
    FIELD Agencia   LIKE Agencias.Agencia
    FIELD nombre    LIKE Agencias.Nombre
    FIELD cnt   AS INTEGER.

DEFINE VARIABLE ipiAgencia AS INTEGER INITIAL 0 NO-UNDO.
DEFINE VARIABLE ipdaFecIni AS DATE INITIAL 09/01/2007 NO-UNDO.
DEFINE VARIABLE ipdaFecFin AS DATE INITIAL 09/15/2007 NO-UNDO.

DEFINE VARIABLE viCntUsu AS INTEGER     NO-UNDO.
    
FOR EACH instancias WHERE tipo_instancia EQ 6 NO-LOCK:
    FOR EACH mov_insSipla WHERE
        (mov_insSipla.instancia EQ instancias.instancia) AND 
        (mov_insSipla.Agencia EQ ipiAgencia OR ipiAgencia EQ 0) AND
        (Mov_InsSipla.Fecha_Transaccion >= ipdaFecIni AND Mov_InsSipla.Fecha_Transaccion <= ipdaFecFin)
        NO-LOCK
        BREAK BY mov_insSipla.Instancia
        BY mov_insSipla.Agencia
        BY Mov_InsSipla.UsuGestiona:

        ASSIGN viCntUsu = viCntUsu + 1.

        IF LAST-OF(Mov_InsSipla.UsuGestiona) THEN DO:
            CREATE TTAgencias.
            BUFFER-COPY Agencias TO TTAgencias.
            UPDATE TTAgencias.cnt = viCntUsu.
/*             DISPLAY                      */
/*                 Mov_InsSipla.Instancia   */
/*                 mov_insSipla.Agencia     */
/*                 Mov_InsSipla.UsuGestiona */
/*                 viCntUsu.                */
            ASSIGN viCntUsu = 0.

        END.

/*         DISPLAY                      */
/*             Mov_InsSipla.Instancia   */
/*             mov_insSipla.Agencia     */
/*             Mov_InsSipla.UsuGestiona */
/*             viCntUsu.                */

    END.

/*     DISPLAY instancias.tipo_instancia instancias.instancia. */
END.


FOR EACH TTAgencias NO-LOCK:
    DISPLAY TTAgencias.
END.
