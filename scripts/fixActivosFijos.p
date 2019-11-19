/*FOR EACH activosFijos WHERE activosFijos.idActivo = "MED0220300007":
    valorActual = 1425314400.
    valorDepreciado = 1118335272.
    
    DISPLAY activosFijos WITH WIDTH 300 1 COL.
END.

    MESSAGE "Fin"

        VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/

DISABLE TRIGGERS FOR LOAD OF mov_contable.

FOR EACH mov_contable WHERE mov_contable.agencia = 2
                          AND mov_contable.fec_contable = TODAY
                          AND mov_contable.comprobante = 10
                          AND mov_contable.num_documento = 39:
    UPDATE mov_contable WITH WIDTH 300 1 COL.

END.
