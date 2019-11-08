DEFINE TEMP-TABLE tmpsaldo LIKE sal_cuenta.
INPUT FROM D:\SICOBEL\dllo\migrar\diciembre\saldoi04.txt.
DISABLE TRIGGERS FOR LOAD OF sal_cuenta.
FOR EACH sal_cuenta:
    DELETE sal_cuenta.
END.
REPEAT:
   CREATE sal_cuenta.
   IMPORT sal_cuenta.
END.
INPUT CLOSE.
FOR EACH sal_cuenta:
  ASSIGN Ano = 2004.
  FIND FIRST cuentas WHERE cuentas.cuenta = sal_cuenta.cuenta NO-ERROR.
  IF NOT AVAILABLE(cuentas) THEN  DISPLAY sal_cuenta.cuenta sal_inicial.
END.
MESSAGE "Se han subido los saldos Iniciales Contables" SKIP
        ".....Puede proceder a subir los movimientos " SKIP 
        "contables"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
