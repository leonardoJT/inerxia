DEFINE VARIABLE w_ano    AS INTEGER   NO-UNDO INITIAL 2008.
DEFINE VARIABLE w_mes    AS INTEGER   NO-UNDO INITIAL 11.
DEFINE VARIABLE w_ageini AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE w_agefin AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE wtiempo  AS INTEGER   NO-UNDO.
DEFINE VARIABLE xcta     LIKE cuentas.cuenta INITIAL "25957014410524".
DEFINE VARIABLE w_FEC1 AS DATE.
DEFINE VARIABLE w_FEC2 AS DATE.

w_FEC1 = DATE(11,01,2008).
w_FEC2 = DATE(11,30,2008).

MESSAGE 'inicio '
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

 FOR EACH sal_cuenta WHERE 
    SAL_cuenta.agencia EQ 1  AND
    SAL_CUENTA.ano EQ w_ano:
    ASSIGN db[w_mes] = 0
           cr[w_mes] = 0.
 END.

 FOR EACH anexos WHERE
    anexos.agencia EQ 1  AND
    ANEXOS.ano = w_ano:
    ASSIGN anexos.db[w_mes] = 0
           anexos.cr[w_mes] = 0.
 END.


/*DISPLAY "INICIO"
        "Hora : " STRING(TIME,"hh:mm:ss").
ASSIGN wtiempo = TIME.
PAUSE 0 BEFORE-HIDE MESSAGE "". */

DEFINE VARIABLE i AS INTEGER.


FOR EACH agencias :
 DO  i = 1 TO 30 :
     w_FEC1 = DATE(11,i,2008).
 FOR EACH Mov_Contable WHERE
                         Mov_contable.agencia  EQ agencias.agencia AND
                         /*mov_contable.cuenta EQ xcta  AND*/
                         Mov_Contable.Fec_Contable EQ  W_Fec1 NO-LOCK :

 /*FIND cuentas WHERE cuentas.cuenta EQ mov_contable.cuenta NO-LOCK NO-ERROR.*/
 IF (Mov_Contable.Cuenta) <> "" THEN DO:
  REPEAT:
   /* DISPLAY mov_contable.agencia
            mov_contable.cuenta 
            mov_contable.fec_contable WITH 1 DOWN.*/
    FIND Sal_Cuenta WHERE Sal_Cuenta.Agencia    EQ Mov_contable.Agencia      
                      AND Sal_cuenta.Cen_Costos EQ Mov_contable.Cen_Costos
                      AND Sal_Cuenta.Cuenta     EQ Mov_Contable.Cuenta          
                      AND Sal_Cuenta.Ano        EQ w_ano  
         USE-INDEX idppal EXCLUSIVE-LOCK NO-WAIT  NO-ERROR.
    IF NOT AVAILABLE(Sal_Cuenta) THEN DO:
       IF LOCKED(Sal_Cuenta) THEN
          NEXT.
       ELSE DO:
            CREATE Sal_Cuenta.
            ASSIGN Sal_Cuenta.Agencia    = Mov_contable.Agencia
                   Sal_Cuenta.Cuenta     = Mov_contable.Cuenta
                   Sal_Cuenta.Ano        = w_ano
                   Sal_Cuenta.Cen_Costos = Mov_contable.Cen_Costos.
       END.    
    END.
    IF Mov_contable.Db GT 0 THEN
        Sal_Cuenta.Db[w_mes] = Sal_Cuenta.Db[w_mes] + Mov_Contable.Db.
    IF Mov_Contable.Cr GT 0 THEN
        Sal_Cuenta.Cr[w_mes] = Sal_Cuenta.Cr[w_mes] + Mov_contable.Cr.
        
    RELEASE Sal_Cuenta.
    LEAVE.
  END. /* REPEAT */
 END.


 /*ANEXOS*/
 IF (Mov_Contable.Nit) <> "" THEN DO:
  REPEAT:    
    FIND Anexos WHERE Anexos.Agencia      EQ Mov_Contable.Agencia         
                  AND Anexos.Cen_Costos   EQ Mov_Contable.Cen_Costos          
                  AND Anexos.Cuenta       EQ Mov_Contable.Cuenta              
                  AND Anexos.Nit          EQ Mov_Contable.Nit                 
                  AND Anexos.Ano          EQ w_ano
         USE-INDEX idprimanexos EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF NOT AVAILABLE(Anexos) THEN
       IF LOCKED(Anexos) THEN NEXT.
       ELSE DO:
          CREATE Anexos.
          ASSIGN Anexos.Agencia    = Mov_Contable.Agencia
                 Anexos.Nit        = Mov_Contable.Nit
                 Anexos.Cuenta     = Mov_Contable.Cuenta
                 Anexos.Ano        = w_ano
                 Anexos.Cen_Costos = Mov_Contable.Cen_Costos.
       END.    
    ASSIGN Anexos.Base[w_mes] = Anexos.Base[w_mes] + Mov_Contable.Base.
    IF Mov_Contable.Db GT 0 THEN
        Anexos.Db[w_mes] = Anexos.Db[w_mes] + Mov_Contable.Db.
    IF Mov_Contable.Cr GT 0 THEN
       Anexos.Cr[w_mes] = Anexos.Cr[w_mes] + Mov_Contable.Cr.
    
    RELEASE Anexos.
    LEAVE.
   END. /* REPEAT */
   END. /*ANEXOS*/
  END.  /* FOR EACH mov_contable*/
 END. /*do*/
END. /* agencias */
/*
MESSAGE "FIN"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  */
         /*"Hora  :" STRING(TIME,"hh:mm:ss")
        "Tiempo :" STRING(TIME - wtiempo,"hh:mm:ss").*/
 
