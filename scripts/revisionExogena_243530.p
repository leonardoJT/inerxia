/* 5002 */

DEFINE BUFFER bfrMovContable FOR mov_contable.

OUTPUT TO d:\leonardo\retefuenteSinReportar_243540.csv.
FOR EACH agencias NO-LOCK:
    FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                            AND year(mov_contable.fec_contable) = 2017
                            AND SUBSTRING(mov_contable.cuenta,1,6) = "243540"
                            AND mov_contable.nit <> "800197268" NO-LOCK:
        FIND FIRST bfrMovContable WHERE bfrMovContable.agencia = mov_contable.agencia
                                    AND bfrMovContable.comprobante = mov_contable.comprobante
                                    AND bfrMovContable.num_documento= mov_contable.num_documento
                                    AND bfrMovContable.nit = mov_contable.nit
                                    AND bfrMovContable.fec_contable = mov_contable.fec_contable
                                    AND (SUBSTRING(bfrMovContable.cuenta,1,8) = "51100201" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51100202" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "52305001" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101102" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "61509501" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51102601" OR

                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "510521" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511011" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511015" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101195" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101251" OR

                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511006" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511007" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511009" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511010" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511018" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511020" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511021" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511022" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511023" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511026" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511095" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101110" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101116" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101118" OR
                                          
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "262505") NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bfrMovContable THEN
            /*DISPLAY mov_contable WITH WIDTH 300 1 COL.*/
            EXPORT DELIMITER ";"
                mov_contable.agencia
                mov_contable.fec_contable
                mov_contable.comprobante
                mov_contable.num_documento
                mov_contable.cuenta
                mov_contable.nit
                mov_contable.comentario
                mov_contable.db
                mov_contable.cr.
    END.
END.
    
MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
