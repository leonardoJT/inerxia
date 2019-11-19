FIND FIRST procDia WHERE procDia.cod_proceso = 13
                     AND procDia.agencia = 1
                     AND procDia.fecha_proc = TODAY
                     AND procDia.estado = 2 NO-ERROR.
IF AVAILABLE procDia THEN
    DELETE procDia.

FIND FIRST procDia WHERE procDia.cod_proceso = 13
                     AND procDia.fecha_proc = 07/28/2014 NO-ERROR.
IF AVAILABLE procDia THEN
    DELETE procDia.

FIND FIRST controlMovTDB WHERE controlMovTDB.fecMov EQ 07/28/2014 NO-ERROR.
IF AVAILABLE controlMovTDB THEN
    DELETE controlMovTDB.

