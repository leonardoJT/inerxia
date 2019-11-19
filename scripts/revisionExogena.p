/* 5002 */

DEFINE BUFFER bfrMovContable FOR mov_contable.

OUTPUT TO d:\leonardo\retefuenteSinReportar.csv.
FOR EACH agencias NO-LOCK:
    FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                            AND year(mov_contable.fec_contable) = 2017
                            AND (SUBSTRING(mov_contable.cuenta,1,6) = "243515" OR
                                 SUBSTRING(mov_contable.cuenta,1,6) = "243525")
                                AND mov_contable.nit <> "800197268" NO-LOCK:
        FIND FIRST bfrMovContable WHERE bfrMovContable.agencia = mov_contable.agencia
                                    AND bfrMovContable.comprobante = mov_contable.comprobante
                                    AND bfrMovContable.num_documento= mov_contable.num_documento
                                    AND bfrMovContable.nit = mov_contable.nit
                                    AND bfrMovContable.fec_contable = mov_contable.fec_contable
                                    AND (SUBSTRING(bfrMovContable.cuenta,1,6) = "511001" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51102711" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51102712" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51102713" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51102714" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51102715" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511030" OR
                                         
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "61509512" OR
                                         
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511028" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101154" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101155" OR
                                         
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "52100505" OR
                                         
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511005" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511013" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511014" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511016" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511019" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511024" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511025" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51102701" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51102702" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51102703" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51102704" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51102705" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511031" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511032" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "52100501" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "52100502" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "52100504" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "52100510" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "521020" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101108" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101114" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101136" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101140" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "61509502" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "61509512" OR
                                         
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
                                         
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "262505" OR
                                         
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51102601") NO-LOCK NO-ERROR.
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
