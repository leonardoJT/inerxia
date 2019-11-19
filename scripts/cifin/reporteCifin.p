DEFINE VAR fechaCorte AS DATE INITIAL 09/30/2015.

/* 1. Reportar créditos con saldo >= $1.000 */
FOR EACH rep_creditos WHERE rep_creditos.fecCorte = fechaCorte
                        AND (rep_creditos.estado = 2 OR (rep_creditos.estado = 3 AND MONTH(rep_creditos.fec_canceTotal) = MONTH(fechaCorte) AND YEAR(rep_creditos.fec_canceTotal) = YEAR(fechaCorte))) NO-LOCK:

END.
