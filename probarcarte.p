   DEFI TEMP-TABLE TSdos 
    FIELD Age LIKE Agencias.Agencia
    FIELD Cta LIKE Cuentas.Cuenta
    FIELD Aju LIKE Cuentas.Cuenta
    FIELD Id  AS CHAR FORM "X(2)" 
    FIELD Sdo LIKE Sal_Cuenta.Sal_Inicial INIT 0
    FIELD SiC AS LOG INIT FALSE.
    
        
  FOR EACH Agencias WHERE Agencias.Estado NE 3 AND Agencias.Agencia NE 4 NO-LOCK:
      FOR EACH CarteraVencida NO-LOCK:
          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_AsoAdDB 
                             AND Tsdos.Id  EQ "CC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN
             CREATE TSdos.
          ASSIGN TSdos.Age = Agencias.Agencia
                 Tsdos.Cta = CarteraVencida.Cta_AsoAdDB
                 Tsdos.Id  = "CC".
IF CarteraVencida.Cta_AsoAdDB EQ "1411050107" THEN
   MESSAGE cod_calif CarteraVencida.Cod_Produc
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_NoAAdDB 
                             AND Tsdos.Id  EQ "CC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN           
             CREATE TSdos.                   
          ASSIGN TSdos.Age = Agencias.Agencia
                 Tsdos.Cta = CarteraVencida.Cta_NoAAdDB
                 Tsdos.Id  = "CC".
IF CarteraVencida.Cta_NoAAdDB EQ "1411050107" THEN
   MESSAGE cod_calif CarteraVencida.Cod_Produc
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_AsoNaDB 
                             AND Tsdos.Id  EQ "CC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_AsoNaDB
                 Tsdos.Id  = "CC".
IF CarteraVencida.Cta_AsoNaDB EQ "1411050107" THEN
   MESSAGE cod_calif CarteraVencida.Cod_Produc
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_NoANaDB 
                             AND Tsdos.Id  EQ "CC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_NoANaDB
                 Tsdos.Id  = "CC".
IF CarteraVencida.Cta_NoANaDB EQ "1411050107" THEN
   MESSAGE cod_calif CarteraVencida.Cod_Produc
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.CtaCal_Interes 
                             AND Tsdos.Id  EQ "CI" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.CtaCal_Interes
                 Tsdos.Id  = "CI".   

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia            /*Reclasif.Int-contingente*/
                             AND TSdos.Cta EQ CarteraVencida.Cta_IntContingDb 
                             AND Tsdos.Id  EQ "IC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_IntContingDb
                 TSdos.Aju = CarteraVencida.Cta_IntContingCr
                 Tsdos.Id  = "IC".   

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.CtaCal_Costas 
                             AND Tsdos.Id  EQ "CO" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.CtaCal_Costas
                 Tsdos.Id  = "CO". 

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_AsoPrvAdCr    /*Cta_AsoPrvAdCr*/
                             AND Tsdos.Id  EQ "PC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_AsoPrvAdCr   
                 Tsdos.Id  = "PC". 

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_AsoPrvNaCr   /*Cta_AsoPrvNaCr*/
                             AND Tsdos.Id  EQ "PC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_AsoPrvNaCr              
                 Tsdos.Id  = "PC". 
            
          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_NoaPrvAdCr 
                             AND Tsdos.Id  EQ "PC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_NoaPrvAdCr
                 Tsdos.Id  = "PC". 

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_NoaPrvNaCr 
                             AND Tsdos.Id  EQ "PC" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_NoaPrvNaCr
                 Tsdos.Id  = "PC".   

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_AsoIntAdCr 
                             AND Tsdos.Id  EQ "PI" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_AsoIntAdCr
                 Tsdos.Id  = "PI". 

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_AsoIntNaCr 
                             AND Tsdos.Id  EQ "PI" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_AsoIntNaCr
                 Tsdos.Id  = "PI". 
            
          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_NoaIntAdCr 
                             AND Tsdos.Id  EQ "PI" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_NoaIntAdCr
                 Tsdos.Id  = "PI". 

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_NoaIntNaCr 
                             AND Tsdos.Id  EQ "PI" NO-ERROR.
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_NoaIntNaCr
                 Tsdos.Id  = "PI". 

          FIND FIRST TSdos WHERE Tsdos.Age EQ Agencias.Agencia
                             AND TSdos.Cta EQ CarteraVencida.Cta_CostasCR 
                             AND Tsdos.Id  EQ "PO" NO-ERROR.   /*Costas*/
          IF NOT AVAIL(TSdos) THEN                            
             CREATE TSdos.                                    
          ASSIGN TSdos.Age = Agencias.Agencia                 
                 TSdos.Cta = CarteraVencida.Cta_CostasCR
                 Tsdos.Id  = "PO". 
      END.
  END.

  FOR EACH TSdos BY TSdos.Age BY TSdos.Cta:
      RUN BuscarSdoCta (INPUT TSdos.Age, INPUT TSdos.Cta, OUTPUT TSdos.Sdo).
      IF TSdos.Cta EQ "1411050107" THEN
         MESSAGE TSdos.Sdo
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.

OUTPUT TO C:\infred\SdoCtaerr.Txt.
  FOR EACH TSdos BY TSdos.Age BY TSdos.Cta:
      DISPLAY TSdos.Age TSdos.Cta Sdo.
  END.
OUTPUT CLOSE.

PROCEDURE BuscarSdoCta:
 DEFINE INPUT  PARAMETER CAge LIKE Agencias.Agencia.
 DEFINE INPUT  PARAMETER CCta LIKE Cuentas.Cuenta.
 DEFINE OUTPUT PARAMETER SFin LIKE Sal_Cuenta.Sal_Inicial INIT 0.

 DEFINE VAR I AS INTEGER.

 FIND FIRST Sal_Cuenta WHERE Sal_Cuenta.Cuenta  EQ CCta
                         AND Sal_Cuenta.Ano     EQ 2006        
                         AND Sal_Cuenta.Agencia EQ CAge NO-LOCK NO-ERROR.
 IF AVAILABLE Sal_Cuenta THEN DO:
    FIND Cuentas WHERE Cuentas.Cuenta EQ Sal_Cuenta.Cuenta NO-LOCK NO-ERROR.   
    SFin = Sal_Cuenta.Sal_Inicial.
    DO I = 1 TO 10 BY 1:
       IF Cuentas.Naturaleza EQ "DB" THEN
          SFin  = SFin + Sal_Cuenta.DB[I] - Sal_Cuenta.Cr[I].
       ELSE
          SFin  = SFin - Sal_Cuenta.DB[I] + Sal_Cuenta.Cr[I].
    END.
 END.
END PROCEDURE.


