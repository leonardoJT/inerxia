DEFINE TEMP-TABLE tmpsaldo LIKE sal_cuenta.
INPUT FROM D:\SICOBEL\dllo\migrar\saldoi05.txt.
DISABLE TRIGGERS FOR LOAD OF sal_cuenta.
REPEAT:
   CREATE tmpsaldo.
   IMPORT tmpsaldo.
   
   FIND FIRST sal_cuenta WHERE sal_cuenta.cuenta = tmpsaldo.cuenta AND 
              sal_cuenta.ano EQ 2005 AND sal_cuenta.agencia EQ tmpsaldo.agencia NO-ERROR.
   IF NOT AVAILABLE(sal_cuenta) THEN DO:
       MESSAGE "No existe cuenta " tmpsaldo.cuenta
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       END.
   ELSE
       ASSIGN sal_cuenta.sal_inicial = sal_cuenta.sal_inicial - tmpsaldo.sal_inicial.
END.
/* Si se desea devolver, es solo reemplazar los saldos iniciales con cero
*/
INPUT CLOSE.
MESSAGE "Se han subido los saldos Iniciales Contables" SKIP
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
