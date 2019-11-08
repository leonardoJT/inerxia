
 DEFINE VARIABLE W_FIni   LIKE Mov_Ahorros.Fecha.
 DEFINE VARIABLE W_FFin   LIKE Mov_Ahorros.Fecha.

 ASSIGN W_FIni = TODAY - 18.
 ASSIGN W_FFin = TODAY.
 
 RUN "C:\Documents and Settings\MARTHAURREGO\Escritorio\Saldos\ExportaAhorrosSubMejorado2.r" (INPUT W_FIni, INPUT W_FFin).

 RUN "C:\Documents and Settings\MARTHAURREGO\Escritorio\Saldos\ExportaAhorros.r".
 RUN "C:\Documents and Settings\MARTHAURREGO\Escritorio\Saldos\ExportaAportes.r".
 RUN "C:\Documents and Settings\MARTHAURREGO\Escritorio\Saldos\ExportaCreditos.r".

MESSAGE "Proceso concluido con éxito, archivos generados en C:\Info_Cooprudea"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
