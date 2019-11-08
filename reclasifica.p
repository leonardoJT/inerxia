/* lee financiera y busca cta homologada  del archivo de reclasifica */
DEFINE VARIABLE tots LIKE creditos.sdo_capital.
DEFINE TEMP-TABLE reclasifica
    FIELD age LIKE sal_cuenta.age
    FIELD cta LIKE sal_cuenta.cuenta
    FIELD cal LIKE creditos.cod_califica
    FIELD nt  LIKE creditos.nit
    FIELD nc  LIKE creditos.num_credito
    FIELD sdoact LIKE sal_cuenta.sal_inicial
    FIELD sdomar  LIKE sal_cuenta.sal_inicial.


INPUT FROM C:\INFo_JURISCOOP\provision_marzo_solidaria\califica.csv.
REPEAT:
    CREATE reclasifica.
    IMPORT DELIMITER ";" reclasifica.
    ASSIGN tots = tots + sdoact.
END.



DEFINE VARIABLE ctahomo LIKE cuentas.cuenta.
OUTPUT TO c:\info_juriscoop\ctacre.txt.
FOR EACH reclasifica:
    FIND FIRST cuentas WHERE cuentas.cuenta = reclasifica.cta NO-ERROR.
    IF AVAILABLE(cuentas) THEN DO:
      /* ctahomo = cuentas.cta_homologa.
       IF SUBSTRING(cuentas.cuenta,1,6) = "259570" THEN DO:
          FIND FIRST cuentas WHERE cuentas.cuenta = ctahomo NO-ERROR.
       END.*/
    END.
    /*ELSE DISP reclasifica.cta.*/
    IF AVAILABLE(cuentas)  THEN DO:
        PUT reclasifica.age ";"
            reclasifica.nt ";"
            reclasifica.nc ";"
            reclasifica.cta ";"
            reclasifica.sdoact SKIP.
    END.
    ELSE PUT reclasifica.age ";"
            reclasifica.nt ";"
            reclasifica.nc ";"
            reclasifica.cta ";"
            reclasifica.sdoact SKIP.
END.
OUTPUT CLOSE.    
/* fin proceso financiera */

/* inicia proceso en consolidada */

MESSAGE "inicia proceso " VIEW-AS ALERT-BOX.
DEFINE TEMP-TABLE calconso
   FIELD cage     LIKE sal_cuenta.agencia
   FIELD nt      LIKE creditos.nit
   FIELD nc      LIKE creditos.num_credito
   FIELD homologa  LIKE cuentas.cuenta
   FIELD sdoact  LIKE sal_cuenta.sal_inicial
   INDEX t1 homologa.

DEFINE TEMP-TABLE tsaldos
    FIELD tage LIKE sal_cuenta.agencia
    FIELD tcta LIKE sal_cuenta.cuenta
    FIELD tsal LIKE sal_cuenta.sal_inicial
    FIELD tdb  LIKE sal_cuenta.sal_inicial
    FIELD tcr  LIKE sal_cuenta.sal_inicial
   INDEX id1 tage tcta.


/* COLOCO SALDOS EN CERO */

DEFINE VARIABLE I AS INTEGER.
DEFINE VARIABLE TSALF AS DECIMAL.
FOR EACH sal_ctacon WHERE SUBSTRING(cuenta,1,2) = "14" and año = 2008:
    IF  SUBSTRING(cuenta,1,4) GT "1480" THEN NEXT.
    FIND FIRST tsaldos WHERE tsaldos.tage = sal_CTACON.agencia AND
                             tsaldos.tcta = SAL_CTACON.CUENTA NO-ERROR.
    IF NOT AVAILABLE(TSALDOS) THEN DO:
        CREATE tSALDOS.
        ASSIGN TAGE = SAL_CTACON.AGENCIA
               TCTA = SAL_CTACON.CUENTA.
    END.
    ASSIGN TSAL = SAL_CTACON.SAL_INICIAL.
    DO I = 1 TO 3 :
         ASSIGN TDB  = TDB  + SAL_CTACON.DB[I]
                TCR  = TCR  + SAL_CTACON.CR[I].
    END.
    TSALF = TSAL + TDB - TCR.
    IF TSALF GT 0 THEN SAL_CTACON.CR[3] = SAL_CTACON.CR[3] + TSALF.
    ELSE SAL_CTACON.DB[3] = SAL_CTACON.DB[3] + (TSALF * -1).
    tdb = 0.
    tcr = 0.

END.
 
/*
OUTPUT TO C:\info_juriscoo\SALCON.TXT.
FOR EACH tSALDOS:
    PUT TAGE ";"
        TCTA ";"
        (TSAL + TDB - TCR) FORMAT "->>>>>>>>>>>>>>>>.99" SKIP.

END.
OUTPUT CLOSE.
  */
/* COLOCO NUEVOS SALDOS */

ASSIGN tots = 0.

INPUT FROM c:\info_juriscoop\ctacre.txt.
REPEAT :
    CREATE calconso.
    IMPORT DELIMITER ";" calconso.
    ASSIGN calconso.homologa = trim(calconso.homologa). 
    tots = tots + calconso.sdoact.
END.


FOR EACH CALCONSO:
    FIND FIRST SAL_CTACON WHERE  SAL_CTACON.CUENTA = CALCONSO.HOMOLOGA AND
                                 sal_ctacon.ano    = 2008              AND 
                                 SAL_CTACON.AGENCIA = CALCONSO.cAGE NO-ERROR.
    IF AVAILABLE(SAL_CTACON) THEN DO:
        IF CALCONSO.SDOACT GT 0 THEN 
              ASSIGN SAL_CTACON.DB[3] = SAL_CTACON.DB[3] + CALCONSO.SDOACT.
        ELSE  ASSIGN SAL_CTACON.CR[3] = SAL_CTACON.CR[3] + (-1 * CALCONSO.SDOACT).
    END.
    ELSE DO:
        CREATE sal_ctacon.
        ASSIGN sal_ctacon.CUENTA =    CALCONSO.Homologa
               sal_ctacon.CEN_COSTO = 999
               sal_ctacon.ANO =       2008
               sal_ctacon.AGENCIA =   CALCONSO.cAGE.
        IF CALCONSO.SDOACT GT 0 THEN 
             ASSIGN SAL_CTACON.DB[3] = SAL_CTACON.DB[3] + CALCONSO.SDOACT.
        ELSE ASSIGN SAL_CTACON.CR[3] = SAL_CTACON.CR[3] + (-1 * CALCONSO.SDOACT).

    END.
END.

DISPLAY tots.

