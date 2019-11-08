 {Incluido\VARIABLE.I "SHARED"}
 RUN Rutinas.r PERSISTENT SET W_Manija.

DEFINE TEMP-TABLE TCode 
    FIELD TCod LIKE Clientes.Nit
    FIELD TNit LIKE Clientes.Nit
    FIELD TCre LIKE Creditos.Num_Credito.

 DEFINE INPUT PARAMETER WDoc     LIKE Documentos.Nombre.
 DEFINE INPUT PARAMETER WNit     LIKE Clientes.Nit.
 DEFINE INPUT PARAMETER WFirma   AS CHARACTER FORMAT "X(40)".
 DEFINE INPUT PARAMETER WCargo   AS CHARACTER FORMAT "X(40)".
 DEFINE INPUT PARAMETER WCC      AS CHARACTER FORMAT "X(100)".
 
 DEFINE VAR WDocumento  AS CHARACTER FORMAT "X(90)".
 DEFINE VAR WEncabezado AS CHARACTER FORMAT "X(480)".
 DEFINE VAR WCuerpo     AS CHARACTER FORMAT "X(480)".
 DEFINE VAR WPie        AS CHARACTER FORMAT "X(480)".
 DEFINE VAR WMItad      AS INTEGER.
 DEFINE VAR WTipAho     AS CHARACTER FORMAT "X(20)".
 DEFINE VAR WTipGar     AS CHARACTER FORMAT "X(20)".

 DEFINE VAR puntero AS ROWID.

 DEFINE VAR WCar AS CHARACTER FORMAT "X(25)". /*Clientes.Cod_Cargo*/
 DEFINE VAR WEmp AS CHARACTER FORMAT "X(25)". /*Clientes.Cod_Empresa*/
 DEFINE VAR WPro AS CHARACTER FORMAT "X(25)". /*Clientes.Cod_Profesion*/


 DEFINE VAR W_NroDoc           AS   INTEGER INITIAL 0.
 DEFINE VAR W_NomEntidad       AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NitEnti          LIKE Clientes.Nit.
 DEFINE VAR W_ConcatEnti       AS   CHARACTER FORMAT "X(57)".
 DEFINE VAR W_PrimerCom        AS  CHARACTER FORMAT "X(100)" INITIAL "".
 DEFINE VAR W_Comentario2      AS  CHARACTER FORMAT "X(100)" INITIAL "".
 DEFINE VAR W_Comentario       AS  CHARACTER FORMAT "X(100)" INITIAL "".
 DEFINE VAR W_Nomofi           AS  CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NomCli           AS  CHARACTER FORMAT "X(30)".
 DEFINE VAR W_Rpta             AS   LOGICAL. 

 DEFINE VAR W_Tipo    AS CHARACTER FORMAT "X(6)".
 DEFINE VAR Mensaje   AS CHARACTER INITIAL "".  
 DEFINE VAR procname  AS CHARACTER FORMAT "X(132)" INITIAL "temporal.txt".
 DEFINE VAR W_Titulo  AS CHARACTER FORMAT "X(40)".
 DEFINE VAR Listado   AS CHARACTER INITIAL "".
  
  
  FIND Documentos WHERE Documentos.Nombre EQ WDoc NO-LOCK NO-ERROR.
  ASSIGN WEncabezado = Documentos.Encabezado + " " + W_NomCli + " Identificado con el Nit: " + WNit.

  Listado = W_PathSpl + "Doc.Lst".

  DEFINE VAR W_Dispositivo AS CHARACTER INITIAL "P".  
  RUN P-DisPos IN W_Manija (INPUT-OUTPUT Listado, INPUT-OUTPUT W_Dispositivo).
  IF W_Dispositivo = "" THEN
     RETURN.
  OUTPUT TO VALUE(listado) NO-ECHO PAGED PAGE-SIZE 22.
  
  /*VIEW FRAME F-Encabezado.*/
  RUN Carta(INPUT WNit, INPUT "", INPUT 0).
  FOR EACH TCode:
      RUN Carta(INPUT TCode.TNit, INPUT TCode.TCod, INPUT TCode.TCre).
  END.  
  
  OUTPUT CLOSE.
  IF W_Dispositivo = "P" THEN  
    RUN Pantalla IN W_Manija (INPUT listado).
  IF W_Dispositivo = "I" THEN
      RUN adecomm/_osprint.r ( INPUT  ?, INPUT listado,INPUT 2,INPUT  1,INPUT  1,
                               INPUT  99999,OUTPUT W_Rpta).
  ELSE
    IF W_Dispositivo <> "A" THEN
       OS-DELETE VALUE(LISTADO).

PROCEDURE Carta:
    DEFINE INPUT PARAMETER WNit LIKE Clientes.Nit.
    DEFINE INPUT PARAMETER WCod LIKE Clientes.Nit.
    DEFINE INPUT PARAMETER WCre LIKE Creditos.Num_Credito.

    DEFINE VAR WCiu        AS CHARACTER FORMAT "X(30)".
    DEFINE VAR WDocumento  AS CHARACTER FORMAT "X(90)".
    DEFINE VAR WCuerpo     AS CHARACTER FORMAT "X(480)".
    DEFINE VAR WPie        AS CHARACTER FORMAT "X(480)".
    DEFINE VAR WMItad      AS INTEGER.
    DEFINE VAR WTipAho     AS CHARACTER FORMAT "X(20)".
    DEFINE VAR WTipGar     AS CHARACTER FORMAT "X(20)".

    DEFINE VAR puntero AS ROWID.

    DEFINE VAR WCar AS CHARACTER FORMAT "X(25)". /*Clientes.Cod_Cargo*/
    DEFINE VAR WEmp AS CHARACTER FORMAT "X(25)". /*Clientes.Cod_Empresa*/
    DEFINE VAR WPro AS CHARACTER FORMAT "X(25)". /*Clientes.Cod_Profesion*/


    DEFINE VAR W_NroDoc           AS   INTEGER INITIAL 0.
    DEFINE VAR W_NomEntidad       AS   CHARACTER FORMAT "X(30)".
    DEFINE VAR W_NitEnti          LIKE Clientes.Nit.
    DEFINE VAR W_ConcatEnti       AS   CHARACTER FORMAT "X(57)".
    DEFINE VAR W_PrimerCom        AS  CHARACTER FORMAT "X(100)" INITIAL "".
    DEFINE VAR W_Comentario2      AS  CHARACTER FORMAT "X(100)" INITIAL "".
    DEFINE VAR W_Comentario       AS  CHARACTER FORMAT "X(100)" INITIAL "".
    DEFINE VAR W_Nomofi           AS  CHARACTER FORMAT "X(30)".
    DEFINE VAR W_NomCli           AS  CHARACTER FORMAT "X(30)".
    DEFINE VAR W_Rpta             AS   LOGICAL. 

    DEFINE VAR W_Tipo    AS CHARACTER FORMAT "X(6)".
    DEFINE VAR Mensaje   AS CHARACTER INITIAL "".  
    DEFINE VAR procname  AS CHARACTER FORMAT "X(132)" INITIAL "temporal.txt".
    DEFINE VAR W_Titulo  AS CHARACTER FORMAT "X(40)".
    DEFINE VAR Listado   AS CHARACTER INITIAL "".


     IF WCod NE "" THEN
        FIND FIRST Clientes WHERE Clientes.nit EQ WCod NO-LOCK NO-ERROR.
     ELSE
        FIND FIRST Clientes WHERE Clientes.nit EQ WNit NO-LOCK NO-ERROR.

     IF AVAILABLE(Clientes) THEN DO:
        W_Nomcli = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
        IF DIR_Correspondencia THEN DO: /*mandar a la oficina*/
           FIND Ubicacion WHERE Ubicacion.Ubicacion BEGINS 
                SUBSTRING(Clientes.Lugar_comercial,1,5) AND 
                Ubicacion.Tipo EQ "C" NO-LOCK NO-ERROR.
           IF AVAILABLE Ubicacion THEN WCiu = Ubicacion.Nombre.
           ELSE WCiu = "Lugar Comercial no expecificado en clientes".
        END.
        ELSE DO:
           FIND Ubicacion WHERE Ubicacion.Ubicacion BEGINS
                SUBSTRING(Clientes.Lugar_Residencia,1,5) AND
                Ubicacion.Tipo EQ "C" NO-LOCK NO-ERROR.
           IF AVAILABLE Ubicacion THEN WCiu = Ubicacion.Nombre.
           ELSE WCiu = "Lugar Residencia no expecificado en clientes".
        END.
        FIND Varios WHERE Varios.Tipo EQ 1 AND Varios.Codigo EQ Clientes.Cod_Profesion NO-LOCK NO-ERROR.
        IF AVAILABLE Varios THEN WPro = Varios.Descripcion.
        FIND Varios WHERE Varios.Tipo EQ 2 AND Varios.Codigo EQ Clientes.Cod_Cargo NO-LOCK NO-ERROR.
        IF AVAILABLE Varios THEN WCar = Varios.Descripcion.
        puntero = ROWID(Clientes).
        FIND Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
        IF AVAILABLE Empresas THEN DO:
           FIND Clientes WHERE Clientes.Nit EQ Empresas.Nit NO-LOCK NO-ERROR.
           IF AVAILABLE Clientes THEN WEmp = Clientes.Nombre + " " + Clientes.Apellido1  + " " + Clientes.Apellido2.
           FIND Clientes WHERE ROWID(Clientes) EQ Puntero NO-ERROR.
        END.
     END.

     FIND Agencias WHERE Agencias.Agencia = W_Agencia NO-LOCK NO-ERROR.
     IF AVAILABLE(Agencias) THEN DO:
      W_NomOfi = Agencias.Nombre.
      FIND Entidad WHERE Entidad.Entidad EQ Agencia.Entidad NO-LOCK NO-ERROR.
      IF AVAILABLE(Entidad) THEN 
       ASSIGN W_NomEntidad = Entidad.Nombre 
              W_NitEnti    = Entidad.Nit
              w_ConcatEnti = TRIM(W_NomEntidad) + " " + "  Nit: " + w_NitEnti.
     END.

     DISPLAY WDoc AT 1 WITH FRAME FDoc12 NO-LABELS WIDTH 132.

     IF LENGTH(WEncabezado) GT 0 THEN
        DISPLAY WEncabezado VIEW-AS EDITOR SIZE 90 BY 4 WITH FRAME FEnc1 NO-LABELS WIDTH 132.

     IF Documentos.Id_EnviaCodeudor AND WCod NE "" THEN DO:
         DISPLAY 
           W_NomCli SKIP(1)
           "Identificación del Cliente: " WCod SKIP
           "Ciudad                    : " WCiu SKIP
           "Dirección                 : " Clientes.DIR_Residencia SKIP
           "Teléfono                  : " Clientes.Tel_Residencia SKIP
           "Codeudor de la Obligación : " WCre SKIP
           "Cliente Deudor            : " WNit
         WITH FRAME F_EncNom1 WIDTH 132 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO.
     END.
     ELSE DO:
         DISPLAY 
           W_NomCli SKIP(1)
           "Ciudad                    : " WCiu SKIP
           "Identificación del Cliente: " WNit SKIP
           "Dirección                 : " Clientes.DIR_Residencia SKIP
           "Teléfono                  : " Clientes.Tel_Residencia
         WITH FRAME F_EncNom2 WIDTH 132 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO.
     END.

     IF LENGTH(Documentos.Cuerpo) GT 0 THEN
        DISPLAY SKIP(2)
               Documentos.Cuerpo VIEW-AS EDITOR SIZE 90 BY 4 WITH FRAME FCue NO-LABELS WIDTH 132.

     IF Documentos.Id_Ahorros AND NOT Documentos.Id_EnviaCodeudor THEN DO:
        FOR EACH Ahorros WHERE Ahorros.Nit EQ WNit AND
                 Ahorros.Estado EQ 1 NO-LOCK BREAK BY Ahorros.Tip_Ahorro:
           CASE Ahorros.Tip_Ahorro:
             WHEN 1 THEN WTipAho = "A la Vista".
             WHEN 2 THEN WTipAho = "Contractual".
             WHEN 3 THEN WTipAho = "A Termino".
             WHEN 4 THEN WTipAho = "Aportes".
           END CASE.
           DISPLAY  SKIP(1)
                   " INFORMACION DE AHORROS --------------------------------------------------------------" AT 1 SKIP(1)
                   "Tipo de Ahorro  :"       AT 1
                   WTipAho                   AT 20
                   "Número Cuenta   :"       AT 50
                   Ahorros.Cue_Ahorros       AT 70 SKIP
                   "Fecha Apertura  :"       AT 1
                   Ahorros.Fec_Apertura      AT 20
                   "Fec.Ult.Transacc:"       AT 50
                   Ahorros.Fec_UltTransaccion AT 70
                   "Plazo           :"       AT 1
                   Ahorros.Plazo             AT 20
                   "Tasa            :"       AT 50
                   Ahorros.Tasa             AT 70
                   "Monto Inicial   :"       AT 1
                   TRIM(STRING(Ahorros.Monto,">>,>>>,>>>,>>9"))      AT 20 FORMAT "X(25)"
                   "Saldo Disponible:"       AT 50
                   TRIM(STRING(Ahorros.Sdo_Disponible,">>,>>>,>>>,>>9"))  AT 70 FORMAT "X(25)" SKIP(1)
           WITH FRAME FCUE1 WIDTH 132 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE.
        END.
     END.


     IF Documentos.Id_Creditos THEN DO:
        FIND FIRST Creditos WHERE
                   Creditos.Nit    EQ WNit AND
                   Creditos.Estado EQ 2    AND
                   Creditos.Sdo_Capital GT 0 NO-LOCK NO-ERROR.
        IF AVAILABLE Creditos THEN DO:
            DISPLAY SKIP(1)
             "Num.Cred  Fec.Desembolso Pla  Tasa                  MtoInicial                    Saldo"
            WITH FRAME F_EncCred WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

            FOR EACH Creditos WHERE Creditos.Nit EQ WNit AND
                     Creditos.Estado EQ 2 NO-LOCK:
               DISPLAY 
                       Creditos.Num_Credito      
                       Creditos.Fec_Desembolso   
                       Creditos.Plazo            
                       Creditos.Tasa             
                       Creditos.Monto
                       Creditos.Sdo_Capital
               WITH FRAME FCUE2 WIDTH 132 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE.
               IF Documentos.Id_Atrasos THEN
                  RUN Mostrar_Atrasos.
               IF Documentos.Id_GarAdmisible THEN
                  RUN Mostrar_Admisibles.
               IF Documentos.Id_GarPersonal AND WCod EQ "" THEN
                  RUN Mostrar_Codeudores.
            END.
        END.
     END.


     IF Documentos.Id_Cliente THEN DO:
        DISPLAY SKIP(1)
                " INFORMACION DEL CLIENTE --------------------------------------------------------------- " AT 1 SKIP(1)
                "Dirección Residencia    : "     AT 1
                 Clientes.DIR_Residencia         AT 30
                "Telefono  Residencia    : "     AT 1
                 Clientes.Tel_Residencia         AT 30
                "Dirección Comercial     : "     AT 1
                 Clientes.DIR_Comercial          AT 30
                "Telefono  Comercial     : "     AT 1
                 Clientes.Tel_Comercial          AT 30
                "Telefono  Movil         : "     AT 1
                 Clientes.Celular                AT 30
                "Correo Electronico      : "     AT 1
                 Clientes.email                  AT 30
                "Estado Civil            : "     AT 1
                 Clientes.Est_Civil              AT 30
                "Personas a Cargo        : "     AT 1
                 Clientes.Per_Acargo             AT 30
                "Número de Hijos         : "     AT 1
                 Clientes.Num_Hijos              AT 30
                "Nivel Educativo         : "     AT 1 
                 Clientes.Niv_Educativo          AT 30
                "Profesión               : "     AT 1
                 WPro                            AT 30
                "Empresas donde Trabaja  : "     AT 1
                 WEmp                            AT 30
                "Cargo que Desempeña     : "     AT 1
                 WCar                            AT 30
                "Salario                 : "     AT 1
                 Clientes.Salario                AT 30
        WITH FRAME FCUE4 WIDTH 90 NO-LABELS USE-TEXT NO-BOX.
     END.


     IF LENGTH(Documentos.Pie_Pagina) GT 0 THEN
        DISPLAY SKIP(2)
               Documentos.Pie_Pagina VIEW-AS EDITOR SIZE 90 BY 4 WITH FRAME FPie NO-LABELS WIDTH 132.

     IF WFirma NE "" THEN
        DISPLAY SKIP(4)
                "_______________________________________" AT 1 SKIP(1)
               WFirma AT 1 
               WCargo AT 1 SKIP(1)
               "C.C:" AT 1
               WCC    AT 10 WITH FRAME FPie USE-TEXT NO-BOX NO-LABELS WIDTH 132.

     PAGE.   

END PROCEDURE.

PROCEDURE Mostrar_Codeudores:
    DEFINE VAR Wnom AS CHARACTER FORMAT "X(40)".
    FIND FIRST Relaciones WHERE 
             Relaciones.Nit            EQ Creditos.Nit         AND
             INTEG(Relaciones.Cuenta)  EQ Creditos.Num_Credito AND
             Relaciones.Clase_Producto EQ 2                    AND
             Relaciones.Cod_Producto   EQ Creditos.Cod_Credito AND
             Relaciones.Cod_Relacion   EQ 11 NO-LOCK NO-ERROR.
    IF AVAILABLE Relaciones THEN DO:
        DISPLAY SKIP(1)
                "GARANTIAS PERSONALES." SKIP
                "Nit          Nombre                                  Telefono" SKIP
                "-------------------------------------------------------------"
        WITH FRAME F_EncCodeudores WIDTH 90 NO-LABELS USE-TEXT NO-BOX STREAM-IO NO-UNDERLINE.
        FOR EACH Relaciones WHERE 
                 Relaciones.Nit            EQ Creditos.Nit         AND
                 INTEG(Relaciones.Cuenta)  EQ Creditos.Num_Credito AND
                 Relaciones.Clase_Producto EQ 2                    AND
                 Relaciones.Cod_Producto   EQ Creditos.Cod_Credito AND
                 Relaciones.Cod_Relacion   EQ 11 NO-LOCK BREAK BY Relaciones.Nit_Relacion:
            FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
            IF AVAILABLE Clientes THEN DO:
               WNom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
               DISPLAY Clientes.Nit WNom Clientes.Tel_Residencia
               WITH FRAME F_MovCodeudores WIDTH 90 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO.
               CREATE TCode. ASSIGN TCode.TCod = Relaciones.Nit_Relacion
                                    TCode.TNit = Creditos.Nit
                                    TCode.TCre = Creditos.Num_Credito.
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE Mostrar_Admisibles:
    DEFINE VAR WDocumento  AS CHARACTER FORMAT "X(90)".
     DEFINE VAR WEncabezado AS CHARACTER FORMAT "X(480)".
     DEFINE VAR WCuerpo     AS CHARACTER FORMAT "X(480)".
     DEFINE VAR WPie        AS CHARACTER FORMAT "X(480)".
     DEFINE VAR WMItad      AS INTEGER.
     DEFINE VAR WTipAho     AS CHARACTER FORMAT "X(20)".
     DEFINE VAR WTipGar     AS CHARACTER FORMAT "X(12)".
     FIND FIRST Garantias WHERE
                Garantias.Num_Credito EQ Creditos.Num_Credito AND
                Garantias.Estado      EQ 1 NO-LOCK NO-ERROR.
     IF AVAILABLE Garantias THEN DO:
         DISPLAY "GARANTIAS ADMISIBLES." SKIP
                 "----------------------------------------------------------------------------------" SKIP
                 "Tip.Garantia Fec.Matric Identifiac   Nombre.Garantia                Valor.Garantia" SKIP
                 WITH FRAME F_EncGarAdm WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

         FOR EACH Garantias WHERE
                  Garantias.Num_Credito EQ Creditos.Num_Credito AND
                  Garantias.Estado EQ 1 NO-LOCK BREAK BY Garantias.Tip_Credito BY Garantias.Cod_Credito BY Garantias.Num_Credito:
             IF Garantias.Tipo_Garantia EQ 1 THEN WTipGar = "Propiedad".
             IF Garantias.Tipo_Garantia EQ 2 THEN WTipGar = "Vehiculo".
             IF Garantias.Tipo_Garantia EQ 3 THEN WTipGar = "Inversion".
             IF Garantias.Tipo_Garantia EQ 4 THEN WTipGar = "No Admisible".
             DISPLAY WTipGar                  
                     Garantias.Fec_Creacion   
                     Garantias.Identificacion_Bien 
                     Garantias.Nom_Bien FORMAT "X(30)"
                     Garantias.Val_Bien        
               WITH FRAME FCUE5 WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
         END.
     END.

END PROCEDURE.

PROCEDURE Mostrar_Atrasos:
    DEFINE VAR TotMora AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
         FOR EACH Creditos WHERE 
                  Creditos.Nit EQ WNit AND
                  Creditos.Estado EQ 2 AND 
                 (Creditos.Int_MorCobrar GT 0 OR 
                  Creditos.Int_DifCobro  GT 0 OR
                  Creditos.Val_Atraso    GT 0) NO-LOCK BREAK BY Creditos.Nit:
            TotMora = Creditos.Val_Atraso + Creditos.INT_DifCobro + Creditos.INT_MorCobrar.
            DISPLAY SKIP(1)
                    "                                                        Dias de Mora :            "
                    Creditos.Dias_Atraso  FORMAT "9999" SKIP
                    "                                                       Saldo en Mora : " 
                    Creditos.Val_Atraso   FORMAT ">>>,>>>,>>>,>>9"  SKIP
                    "                                                   Int.Dificil.Cobro : " 
                    Creditos.INT_DifCobro FORMAT ">>>,>>>,>>>,>>9"  SKIP
                    "                                                            Int.Mora : " 
                    Creditos.INT_MorCobrar FORMAT ">>>,>>>,>>>,>>9" SKIP
                    "                                                  ------------------------------------" SKIP
                    "                                                               Total : " 
                    TotMora FORMAT ">>>,>>>,>>>,>>9"
            WITH FRAME FAtraso WIDTH 132 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO.
         END.

END PROCEDURE.
