DEFINE VARIABLE Listado     AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vclis1      AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vclis2      AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vclis3      AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcnomgrupo  AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcnomcta    AS CHARACTER FORMAT "X(14)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vccuenta    AS CHARACTER FORMAT "X(14)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vccueanex   AS CHARACTER FORMAT "X(14)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcnatura    AS CHARACTER FORMAT "X(2)"  INITIAL "" NO-UNDO.
DEFINE VARIABLE vdSdoFinal  AS DECIMAL                  INITIAL 0  NO-UNDO.
DEFINE VARIABLE i           AS INTEGER                  INITIAL 1  NO-UNDO.
DEFINE VARIABLE vfcompra    AS DATE FORMAT "99/99/9999" NO-UNDO.
DEFINE VARIABLE vilong      AS INTEGER INITIAL 0 NO-UNDO.

DEFINE TEMP-TABLE TemActivo
    FIELD TAgencia  AS INTEGER 
    FIELD TNit      AS CHARACTER FORMAT "X(12)"
    FIELD TGrupo    AS INTEGER 
    FIELD tNomgru   AS CHARACTER
    FIELD TCuenta   AS CHARACTER
    FIELD tNomcta   AS CHARACTER
    FIELD TCompra   AS DECIMAL
    FIELD TContable AS DECIMAL
    FIELD TDiferen  AS DECIMAL
    INDEX IAgeNitGru TAgencia TNit.

/* {Incluido\VARIABLE.I "SHARED"} */
/* {Incluido\VARCON.I "SHARED"} */
/********************************/
  DEFINE VARIABLE W_Usuario   LIKE usuarios.usuario       INITIAL "339". /* 308 - Contabiliza*/
  DEFINE VARIABLE W_Fecha     AS DATE   INITIAL TODAY.
  DEFINE VARIABLE W_PathSpl   AS CHARACTER FORMAT "X(20)" INITIAL "c:\info_juriscoop\".
  DEFINE VARIABLE W_Agencia   LIKE Agencia.Agencia        INITIAL "024". /*"035".*/

    DEFINE {1} VAR W_Estacion    LIKE Estaciones.Estacion INITIAL "5".
/*     DEFINE {1} VAR W_Usuario     LIKE Usuarios.Usuario. */
    DEFINE {1} VAR W_Clave       LIKE Usuarios.Clave FORMAT "X(16)".
    DEFINE {1} VAR W_Prioridad   LIKE Usuarios.Prioridad INITIAL "".
/*     DEFINE {1} VAR W_Agencia     LIKE Usuarios.Agencia INITIAL 0.  */
    DEFINE {1} VAR W_Ciudad      LIKE Agencia.Ciudad INITIAL 0.
    DEFINE {1} VAR W_Nom_Agencia   AS CHARACTER FORMAT "X(60)".
    DEFINE {1} VAR W_UbiDatos      AS CHAR INITIAL "D".
    /*DEFINE {1} VAR W_Ninv        LIKE Inversion.Nro_inversion.*/
    DEFINE {1} VAR W_Nom_Entidad   AS CHARACTER FORMAT "X(60)".
    DEFINE {1} VAR W_Entidad     LIKE Entidad.Entidad.
    DEFINE {1} VAR W_NitGlobal   LIKE Clientes.Nit INITIAL "".
    DEFINE {1} VAR W_SMLV        LIKE Indicadores.Valor INITIAL 0.
    DEFINE {1} VAR W_Manija        AS HANDLE.
    DEFINE {1} VAR W_ManFin        AS HANDLE.
    DEFINE {1} VAR W_ManTaq        AS HANDLE.
    DEFINE {1} VAR W_Nivel       LIKE Cuentas.Nivel.
    DEFINE {1} VAR W_CtaMay      LIKE Cuentas.Cuenta.
/*     DEFINE {1} VAR W_Fecha         AS DATE FORMAT "99/99/9999" INITIAL TODAY.  */
    DEFINE {1} VAR W_ficina        AS CHARACTER FORMAT "X(40)" VIEW-AS COMBO-BOX INNER-LINES 4 SIZE 40 BY 1.
    DEFINE {1} VAR W_Path        LIKE Entidad.Dir_Programas.
/*     DEFINE {1} VAR W_Pathspl     LIKE Entidad.Dir_Spl.  */
    DEFINE {1} VAR W_Eleccion      AS LOGICAL.
    DEFINE {1} VAR W_CedGral     LIKE Clientes.Nit.
    DEFINE {1} VAR W_CenCosGral  LIKE Cen_Costos.Cen_Costos.
    DEFINE {1} VAR W_Cadena        AS CHARACTER FORMAT "X(9)" INITIAL "SIFINCOOP".
    /*DEFINE     VAR Agencia_Cnt     AS INTEGER FORMAT "999".*/
    DEFINE {1} VAR P-Valida        AS LOGICAL.
    DEFINE {1} VAR W_VCodPcto    LIKE Ahorros.Cod_Ahorro.
    DEFINE {1} VAR W_VCueAhorro  LIKE Ahorros.Cue_Ahorros.
    DEFINE {1} VAR W_Solicitud   LIKE Solicitud.Num_Solicitud.
    DEFINE {1} VAR W_PagareS     LIKE Creditos.Pagare.
    DEFINE {1} VAR P_SdoTot      LIKE Creditos.Sdo_Capital.
    DEFINE {1} VAR W_OfiCierre   LIKE Agencias.Agencia.
  /********************************************************************************** 
   Variables globales contables
**********************************************************************************/
  
  DEFINE {1} VAR W_ManCon AS HANDLE.
  DEFINE {1} VAR W_FecIni AS DATE FORMAT "99/99/9999" INITIAL TODAY.
  DEFINE {1} VAR W_FecFin AS DATE FORMAT "99/99/9999" INITIAL TODAY.
  DEFINE {1} VAR W_Mes    AS INTEGER FORMAT "99".
  DEFINE {1} VAR W_MesFin AS INTEGER FORMAT "99".
  DEFINE {1} VAR W_UsuFin LIKE Usuarios.Usuario.
  DEFINE {1} VAR W_UsuIni LIKE Usuarios.Usuario.
  DEFINE {1} VAR W_CtaIni LIKE Cuentas.Cuenta INITIAL "".
  DEFINE {1} VAR W_CtaFin LIKE Cuentas.Cuenta INITIAL "".
  DEFINE {1} VAR W_ComTra LIKE Comprobantes.Comprobante INITIAL 0.
  DEFINE {1} VAR W_ComIni LIKE Comprobantes.Comprobante INITIAL 0.
  DEFINE {1} VAR W_ComFin LIKE Comprobantes.Comprobante INITIAL 0.  
  DEFINE {1} VAR W_CenTra LIKE Cen_Costos.Cen_Costos    INITIAL 0.
  DEFINE {1} VAR W_OfiIni LIKE Agencias.Agencia         INITIAL 0.
  DEFINE {1} VAR W_OfiFin LIKE Agencias.Agencia         INITIAL 0.
  DEFINE {1} VAR W_CenIni LIKE Cen_Costos.Cen_Costos    INITIAL 0.
  DEFINE {1} VAR W_CenFin LIKE Cen_Costos.Cen_Costos    INITIAL 0.  
  DEFINE {1} VAR W_NitIni LIKE Terceros.Nit INITIAL "".
  DEFINE {1} VAR W_NitFin LIKE Terceros.Nit INITIAL "".
  DEFINE {1} VAR W_BaseInf  AS DECIMAL INITIAL 0 FORMAT "->>>,>>>,>>>,>>9".
  DEFINE {1} VAR W_Ope      AS CHARACTER FORMAT "X(13)"  VIEW-AS COMBO-BOX 
        LIST-ITEMS "Sin Seleccion","Mayor Que","Menor Que","Igual" SIZE 13 BY 4.5.
  DEFINE {1} VAR TotDctoDeb AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotDctoCre AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotGralDeb AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotGralCre AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR VlrDeb     AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR VlrCre     AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotOfCre   AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotOfDeb   AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotCoCre   AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR TotCoDeb   AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
  DEFINE {1} VAR W_SaldoAct AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99" .
  DEFINE {1} VAR W_SaldoAnt AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99" .  
  DEFINE {1} VAR W_PorReex  AS DECIMAL INITIAL 0 FORMAT "-ZZ9.99".
  DEFINE {1} VAR W_Raya1    AS CHAR    INITIAL "" FORMAT "X(130)".
  DEFINE {1} VAR W_Raya2    AS CHAR    INITIAL "" FORMAT "X(130)".
  DEFINE {1} VAR W_NomMes   AS CHAR    INITIAL "" FORMAT "X(20)".
  DEFINE {1} VAR W_NomCta   AS CHAR    INITIAL "".        
  DEFINE {1} VAR W_Opcion   AS CHAR    INITIAL "P".
  DEFINE {1} VAR W_Destino  AS CHAR    INITIAL "".
  DEFINE {1} VAR W_NomOfi LIKE Agencias.Nombre.
  DEFINE {1} VAR W_NomCen LIKE Cen_Costos.Nombre INITIAL "Consolidado".
  DEFINE {1} VAR W_NomCom LIKE Comprobantes.Nombre INITIAL "Consolidado".
  DEFINE {1} VAR W_NomIni LIKE Cuentas.Nombre.    
  DEFINE {1} VAR W_NomFin LIKE Cuentas.Nombre.
  DEFINE {1} VAR W_NoInNit  AS CHAR FORMAT "X(40)" INITIAL "".
  DEFINE {1} VAR W_NoFiNit  AS CHAR FORMAT "X(40)" INITIAL "".
  DEFINE {1} VAR W_Rtipo    AS INTEGER INITIAL 1 LABEL  "Tipo Informe " VIEW-AS RADIO-SET HORIZONTAL
                        RADIO-BUTTONS "Detallado/Nit",1,"Resumido/Cuenta",2 SIZE 30 BY 0.81.
  DEFINE {1} VAR W_Ret    AS INTEGER INITIAL 1 VIEW-AS RADIO-SET HORIZONTAL
                        RADIO-BUTTONS "Iva",1,"Retencion fuente",2,"Pagos a Terceros", 3 SIZE 40 BY 0.71.
  DEFINE {1} VAR W_RForma   AS INTEGER INITIAL 0 LABEL "Formato       " VIEW-AS RADIO-SET HORIZONTAL
                        RADIO-BUTTONS "-/+",0,"(",1,"-/+$",2,"($",3 SIZE 25 BY 0.81.
  DEFINE {1} VAR W_Rpta     AS LOGICAL.
  DEFINE {1} VAR W_Validacion AS INTEGER INITIAL -1.
  DEFINE {1} VAR W_Raya     AS CHAR INITIAL "-" FORMAT "X".
  DEFINE {1} VAR W_OfiTra   AS INTEGER FORMAT ">>9" VIEW-AS FILL-IN.
  DEFINE {1} VAR W_Id_Paag  AS LOGICAL INITIAL FALSE LABEL "Sin Ajuste" VIEW-AS TOGGLE-BOX SIZE 10 BY 1.
  DEFINE {1} VAR W_Id_Defla AS LOGICAL INITIAL FALSE LABEL "Deflactado" VIEW-AS TOGGLE-BOX SIZE 10 BY 1.
  DEFINE {1} VAR W_TitRet  AS CHARACTER FORMAT "X(40)".
/********************/

EMPTY TEMP-TABLE TemActivo.
RUN Proceso.

PROCEDURE Proceso:
TranConta:
REPEAT TRANSACTION ON ERROR UNDO TranConta, LEAVE TranConta:
  /* Activo Fijo */
  FOR EACH act_fijo /*WHERE act_fijo.estado NE 3*/ 
      /*WHERE codigo = "AF005333"*/ NO-LOCK
      BY act_fijo.Agencia BY act_fijo.Codigo:
  
      FIND FIRST pro_activo WHERE pro_activo.grupo EQ act_fijo.grupo NO-ERROR.
      IF AVAILABLE(pro_activo) THEN DO:
         /* Incluir Varios*/
         FIND Varios WHERE Varios.Tipo EQ 7 AND Varios.Codigo EQ Act_Fijo.Grupo NO-LOCK NO-ERROR.
         IF AVAILABLE Varios THEN
            ASSIGN vcnomgrupo = Varios.Descripcion.
         ELSE
            ASSIGN vcnomgrupo = "No Existe".

         FIND FIRST Cuentas WHERE Cuentas.cuenta EQ pro_activo.cta_fuente NO-ERROR.
         IF AVAILABLE (Cuentas) THEN
            ASSIGN vccuenta = Cuentas.cuenta
                   vcnomcta = Cuentas.Nombre.
         ELSE
            ASSIGN vccuenta = ""
                   vcnomcta = "No Existe".
      END.
  
      CREATE TemActivo.
      UPDATE TemActivo.TAgencia  = act_fijo.agencia
             TemActivo.TNit      = act_fijo.codigo
             TemActivo.TGrupo    = act_fijo.grupo
             TemActivo.TNomgru   = vcnomgrupo
             TemActivo.TCuenta   = vccuenta
             TemActivo.TNomcta   = vcnomcta
             TemActivo.TCompra   = act_fijo.val_compra
             TemActivo.TContable = 0
             TemActivo.TDiferen  = TemActivo.TCompra - TemActivo.TContable.
  END.
  RUN SalAnexos.
  ASSIGN listado = W_PathSpl + "ActiContaConcilia-" + STRING(W_Fecha,"99999999") + "-" + STRING(W_Usuario,"999") + ".lst".
  ASSIGN vclis1 = listado.
  RUN BorraCeros.
  RUN ImpTemActivo.
  /********************/
  MESSAGE "Archivo Plano Generado : " vclis1 SKIP
      VIEW-AS ALERT-BOX.
  /*****************/
  LEAVE.
END.  /*Fin Tx*/
END PROCEDURE.

PROCEDURE SalAnexos:
FOR EACH pro_activo /*WHERE pro_activo.grupo = 19*/ NO-LOCK BY pro_activo.grupo:
    /* Incluir Varios*/
    FIND Varios WHERE Varios.Tipo EQ 7 AND Varios.Codigo EQ pro_activo.Grupo NO-LOCK NO-ERROR.
    IF AVAILABLE Varios THEN
       ASSIGN vcnomgrupo = Varios.Descripcion.
    ELSE
       ASSIGN vcnomgrupo = "No Existe".

    FOR EACH Anexos WHERE 
        /* Estaba Anexos.Cuenta EQ pro_activo.cta_fuente*/
         /*anexos.nit = "AF005333" /*"AF009825"*/ AND */
        (SUBSTRING(Anexos.Cuenta,1,4) EQ SUBSTRING(pro_activo.cta_fuente,1,4) OR
        SUBSTRING(Anexos.Cuenta,1,4)  EQ SUBSTRING(pro_activo.CtaDepAcum,1,4)) AND 
        Anexos.Ano EQ YEAR(TODAY)
        NO-LOCK BREAK BY Anexos.agencia BY Anexos.Ni BY Anexos.Cuenta:

/*         ASSIGN vilong = LENGTH(Anexos.cuenta). */
/*         IF (integer(substring(anexos.cuenta,vilong - 1,vilong)) = 5) OR         */
/*            (integer(substring(anexos.cuenta,vilong - 1,vilong)) = 6)  THEN DO:  */

            FIND FIRST Cuentas WHERE Cuentas.cuenta EQ pro_activo.cta_fuente NO-ERROR.
            IF AVAILABLE (Cuentas) THEN
               ASSIGN vccuenta = Cuentas.cuenta
                      vcnomcta = Cuentas.Nombre.
            ELSE
               ASSIGN vccuenta = ""
                      vcnomcta = "No Existe".
/*             IF pro_grupo.grupo = 28 THEN                                                                        */
/*                 MESSAGE anexos.agencia anexos.nit pro_activo.grupo pro_activo.cta_fuente pro_activo.CtaDepAcum  */
/*                     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                          */
            ASSIGN vdSdoFinal  = Anexos.Sdo_Inicial.
            DO i = 1 TO 12 BY 1:  /*month(TODAY)*/
               IF vcnatura EQ "DB" THEN 
                  ASSIGN vdSdoFinal  = vdSdoFinal + Anexos.DB[i] - Anexos.Cr[i].
               ELSE
                  ASSIGN vdSdoFinal  = vdSdoFinal - Anexos.DB[i] + Anexos.Cr[i].
            END. 
            
            IF LAST-OF (Anexos.agencia) OR LAST-OF (Anexos.nit) OR LAST-OF (Anexos.cuenta) THEN DO:
               FIND FIRST TemActivo WHERE
                    TemActivo.TAgencia EQ Anexos.Agencia  AND
                    TemActivo.TNit     EQ Anexos.Nit      AND 
                    TemActivo.TGrupo   EQ pro_activo.grupo NO-ERROR.     /*Nuevo*/
            
               IF AVAILABLE(TemActivo) THEN
                  UPDATE TemActivo.TContable = TemActivo.TContable + vdSdoFinal
                         TemActivo.TDiferen  = TemActivo.TCompra - TemActivo.TContable.
               ELSE 
                  IF vdSdoFinal NE 0 THEN DO:
                     /* Nuevo*/
                     FIND FIRST TemActivo WHERE
                          TemActivo.TAgencia EQ Anexos.Agencia  AND
                          TemActivo.TNit     EQ Anexos.Nit      NO-ERROR.
                     IF NOT AVAILABLE (TemActivo) THEN DO:
                        IF Anexos.Nit = "TE01" THEN
                           MESSAGE "No existe en la temporal"
                               VIEW-AS ALERT-BOX INFO BUTTONS OK.
                        CREATE TemActivo.   
                        UPDATE TemActivo.TAgencia  = Anexos.agencia
                               TemActivo.TNit      = Anexos.nit
                               TemActivo.TGrupo    = pro_activo.grupo
                               TemActivo.TNomgru   = vcnomgrupo
                               TemActivo.TCuenta   = vccuenta
                               TemActivo.TNomcta   = vcnomcta
                               TemActivo.TCompra   = 0
                               TemActivo.TContable = vdSdoFinal
                               TemActivo.TDiferen  = TemActivo.TCompra - TemActivo.TContable.
                      /*Nuevo*/
                     END.
                  END.
/*             END.   */
/*         /*Nuevo*/  */
        END.
    END.
END.
END PROCEDURE.

PROCEDURE BorraCeros:
FOR EACH TemActivo WHERE TemActivo.TDiferen EQ 0:
    DELETE TemActivo.
END.
END PROCEDURE.

PROCEDURE ImpTemActivo:
OUTPUT TO VALUE(listado).
FOR EACH TemActivo BY TemActivo.TAgencia BY TemActivo.TGrupo BY TemActivo.TNit:
    FORM 
        TemActivo.TAgencia  COLUMN-LABEL "Agencia"       FORMAT "999"           
        TemActivo.TGrupo    COLUMN-LABEL "Cod.Grupo"     FORMAT "999"    
        TemActivo.TNomgru   COLUMN-LABEL "Nombre"        FORMAT "X(40)"    
        TemActivo.TNit      COLUMN-LABEL "C�digo/Nit"    FORMAT "X(14)"           
        TemActivo.TCuenta   COLUMN-LABEL "Cta_Activo"    FORMAT "X(14)"
        TemActivo.TNomcta   COLUMN-LABEL "Descripcion"   FORMAT "X(40)"    
        TemActivo.TCompra   COLUMN-LABEL "Vlr.Compra"    FORMAT "->>>,>>>,>>>,>>9.99"
        TemActivo.TContable COLUMN-LABEL "Sdo.Contable/Costo-Ajus"  FORMAT "->>>,>>>,>>>,>>9.99"
        TemActivo.TDiferen  COLUMN-LABEL "Diferencia"    FORMAT "->>>,>>>,>>>,>>9.99"
        WITH FRAME FActFijo DOWN COLUMN 1 WIDTH 220
             NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
    DISPLAY 
         TemActivo.TAgencia 
         TemActivo.TGrupo 
         TemActivo.TNomgru
         TemActivo.TNit     
         TemActivo.TCuenta 
         TemActivo.TNomcta 
         TemActivo.TCompra  
         TemActivo.TContable
         TemActivo.TDiferen 
         WITH FRAME FActFijo.
    DOWN WITH FRAME FActFijo.
END.
OUTPUT CLOSE.
END PROCEDURE.
