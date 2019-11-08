DEFI VAR Datos     AS CHARACTER FORMAT "X(80)".


  DEFI TEMP-TABLE Tmp
       FIELD Ced LIKE Creditos.Nit
       FIELD Num LIKE Creditos.Num_Credito
       FIELD FIni AS DATE
       FIELD FDes AS DATE
       FIELD Mont LIKE Creditos.Monto.


  
 
  SESSION:SET-WAIT-STATE("GENERAL").
         
  INPUT FROM C:\InfRed\0001.Txt.
  REPEAT:
    IMPORT UNFORMATTED Datos NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
       MESSAGE "El Archivo tiene Problemas...Revise por favor" VIEW-AS ALERT-BOX.
       LEAVE.
    END.

    MESSAGE SUBSTRING(Datos,1,12)                                                                       
            INTEG(SUBSTRING(Datos,25,2)) INTEG(SUBSTRING(Datos,27,2)) INTEG(SUBSTRING(Datos,21,4))
            INTEG(SUBSTRING(Datos,17,2)) INTEG(SUBSTRING(Datos,19,2)) INTEG(SUBSTRING(Datos,13,4))
            DEC(SUBSTRIN(Datos,29,12))                                                                
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    CREATE Tmp.
    ASSIGN Tmp.Ced  = SUBSTRING(Datos,1,12)
           Tmp.FDes = DATE(INTEG(SUBSTRING(Datos,25,2)),INTEG(SUBSTRING(Datos,27,2)),INTEG(SUBSTRING(Datos,21,4)))           
           Tmp.FIni = DATE(INTEG(SUBSTRING(Datos,17,2)),INTEG(SUBSTRING(Datos,19,2)),INTEG(SUBSTRING(Datos,13,4)))
           Tmp.Mont = DEC(SUBSTRIN(Datos,29,12)).
  END.
