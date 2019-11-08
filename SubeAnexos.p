  DEFINE VAR Datos   AS CHARACTER FORMAT "X(100)".
  DEFI VAR Tvr LIKE Anexos.Sdo_Inicial.
  DEFI VAR Cont AS INTEG FORM "999999".

  DEFI TEMP-TABLE Tmp
       FIELD Ag  LIKE anexos.Agencia INIT 1
       FIELD Cta LIKE Cuentas.Cuenta
       FIELD Ced LIKE Anexos.Nit
       FIELD Vlr LIKE Anexos.Sdo_Inicial.
 
  SESSION:SET-WAIT-STATE("GENERAL").
    
  INPUT FROM C:\migracion\terboya.csv.
  REPEAT:
    IMPORT UNFORMATTED Datos NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
       MESSAGE "El Archivo tiene Problemas...Revise por favor" VIEW-AS ALERT-BOX.
       LEAVE.
    END.

    CREATE Tmp.
    ASSIGN Tmp.Ced = SUBSTRING(Datos,10,9)
           Tmp.Cta = SUBSTRING(Datos,1,8)
           Tmp.Vlr = DEC(SUBSTRING(Datos,58,9))
           Cont = cont + 1
           Tvr  = tvr + Tmp.Vlr.
  END.

  MESSAGE Tmp.Ag 
          Tmp.Cta
          Tmp.Ced
          Tmp.Vlr
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  FIND FIRST Tmp NO-ERROR.
  MESSAGE Tmp.Ag Tmp.Cta Tmp.Ced Tmp.Vlr
      VIEW-AS ALERT-BOX INFO BUTTONS OK.


  SESSION:SET-WAIT-STATE("").

  MESSAGE Tvr Cont VIEW-AS ALERT-BOX.

FOR EACH Tmp WHERE Tmp.Vlr NE 0:
      FIND FIRST Anexos WHERE Anexos.Agencia EQ Tmp.Ag
                          AND Anexos.Cuenta  EQ Tmp.Cta
                          AND Anexos.Nit     EQ Tmp.Ced
                          AND Anexos.Ano     EQ 2006   NO-ERROR.
      IF NOT AVAIL(Anexos) THEN DO:
         CREATE Anexos.
         ASSIGN Anexos.Agencia    = Tmp.Ag
                Anexos.Cen_Costos = 999
                Anexos.Cuenta     = Tmp.Cta
                Anexos.Nit        = Tmp.Ced
                Anexos.Ano        = 2006.
      END.

      ASSIGN Anexos.Sdo_Inicial = Tmp.Vlr.
  END.
   
