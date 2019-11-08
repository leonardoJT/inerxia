 DEFINE BUFFER xsal for sal_cuenta.
 FOR EACH xsal WHERE cuenta = "24953001":
    DISP xsal WITH 1 COL.
    ASSIGN sdo = xsal.sal_inicial
           xdb1 = xsal.db[1]
           xdb2 = xsal.db[2]
           cr1  = xsal.cr[1]
           cr2  = xsal.cr[2].
    FIND FIRST sal_cuenta WHERE sal_cuenta.cuenta = "24953001" NO-LOCK NO-ERROR.
    IF AVAILABLE(sal_cuenta)  THEN
        ASSIGN sal_cuenta.sal_inicial = sal_cuenta.sal_inicial + sdo
               sal_cuenta.db[1] xdb1 
               sal_cuenta.db[2] xdb2 
               sal_cuenta.cr[1] cr1 
               sal_cuenta.cr[2] cr2 .
    FIND FIRST sal_cuenta WHERE sal_cuenta.cuenta = "24953001" NO-LOCK NO-ERROR.
    IF AVAILABLE(sal_cuenta) THEN
          DELETE sal_cuenta.
END.
