DEFINE VARIABLE OKpressed   AS LOGICAL                  INITIAL TRUE.
DEFINE VARIABLE viTotReg    AS INTEGER                  INITIAL 0  NO-UNDO.
DEFINE VARIABLE Archivo     AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcnomage    AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcnomgru    AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcactivo    AS CHARACTER FORMAT "X(60)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcestado    AS CHARACTER FORMAT "X(12)" INITIAL "" NO-UNDO.

DEFINE VARIABLE vcNom       AS CHARACTER                           NO-UNDO.
DEFINE VARIABLE vcespacio   AS CHARACTER FORMAT "X(5)"  INITIAL "  " NO-UNDO.
DEFINE VARIABLE vcMes       AS CHARACTER EXTENT 12      INITIAL 
       ["Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
        "Agosto","Septiembre","Octubre","Noviembre","Diciembre"]   NO-UNDO.
DEFINE VARIABLE Listado     AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vdcompras   AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VARIABLE vdmejoras   AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VARIABLE vdvaldepacu AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VARIABLE vdneto      AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VARIABLE vdcoshisto  AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VARIABLE vdsdodepre  AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VARIABLE vclis       AS CHARACTER FORMAT "X(80)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vclis1      AS CHARACTER FORMAT "X(80)" INITIAL "" NO-UNDO.

DEFINE TEMP-TABLE TemDardeBaja
    FIELD TAgeActu  LIKE act_fijo.agencia
    FIELD TCodigo   AS CHARACTER FORMAT "X(20)" /* LIKE act_fijo.codigo FORMAT "X(20)"*/
    FIELD TGrupo    LIKE act_fijo.grupo
    /*FIELD TCenCosto LIKE act_fijo.cen_costo*/
    FIELD TAclara   AS CHARACTER FORMAT "X(100)"
    INDEX ITras TGrupo TCodigo.

DEFINE TEMP-TABLE TemResultado
    FIELD TAgeActu  LIKE act_fijo.agencia
    FIELD TCodigo   LIKE act_fijo.codigo
    FIELD TCenCosto LIKE act_fijo.cen_costo
    FIELD TEstado   AS INTEGER /*LIKE actfijo.estado*/          /*1-Activo, 2-Retirado, 3-Depreciado*/
    FIELD TNombre   AS CHARACTER FORMAT "X(40)"
    FIELD TECodigo  AS CHARACTER FORMAT "X(15)"
    FIELD TGrupo    LIKE act_fijo.grupo
    FIELD TAclara   AS CHARACTER FORMAT "X(100)"
    INDEX IResul TGrupo TCodigo.

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

EMPTY TEMP-TABLE TemDardeBaja.
RUN Proceso.

PROCEDURE Proceso:
TranConta:
REPEAT TRANSACTION ON ERROR UNDO TranConta, LEAVE TranConta:
  RUN Cargue NO-ERROR. 
  IF OKpressed = FALSE THEN
     RETURN. 
/*   FOR EACH TemDardeBaja:                                           */
/*       DISPLAY TemDardeBaja.Tcodigo VIEW-AS FILL-IN FORMAT "X(20)"  */
/*              TAgencia . /* EXCEPT TAclara WITH 1 COLUMN.*/         */
/*   END.                                                             */
/*   RETURN.                                                          */

  RUN Actualiza.
  ASSIGN listado = W_PathSpl + "BajasActFijos1-" + STRING(W_Fecha,"99999999") + "-" + STRING(TIME) + "-" + STRING(W_Usuario,"999") + ".lst".
  ASSIGN vclis = listado.
  ASSIGN viTotReg = 0.
  RUN ImpTemResultado.

  ASSIGN listado = W_PathSpl + "BajasActFijos2-" + STRING(W_Fecha,"99999999") + "-" + STRING(TIME) + "-" + STRING(W_Usuario,"999") + ".lst".
  ASSIGN vclis1 = listado.
  {Incluido\ImpArch.i "Listado" Tamano}
  MESSAGE "Archivo Plano Generado : " vclis SKIP
          "                                          " vclis1
      VIEW-AS ALERT-BOX.
  /*****************/
  LEAVE.
 END.  /*Fin Tx*/
END PROCEDURE.
/*RUN movcontable.*/

PROCEDURE Cargue:
    SYSTEM-DIALOG GET-FILE Archivo
        TITLE      "Estructura: Códgio-X(15), Grupo-X(5), Nva.Age.-X(3), Nvo.C.Costo-X(3)..."
        FILTERS    "*.csv"   "*.csv"
        MUST-EXIST
        USE-FILENAME
        UPDATE OKpressed.
      
    IF OKpressed = TRUE THEN DO:
        INPUT FROM VALUE(Archivo).
        REPEAT:
             CREATE TemDardeBaja.
             IMPORT DELIMITER ";" TemDardeBaja.
             IF ERROR-STATUS:ERROR THEN DO:
                MESSAGE "El Archivo tiene Problemas...Revise por favor" VIEW-AS ALERT-BOX.
                RETURN ERROR.
             END.
             IF TemDardeBaja.TAgeActu  EQ 0 OR /*TemDardeBaja.TAgencia EQ 0 OR*/
                TemDardeBaja.TCodigo   EQ "" OR TemDardeBaja.TGrupo  EQ 0 /*OR 
                TemDardeBaja.TCenCosto EQ 0*/ THEN
                DELETE TemDardeBaja.
        END.
        INPUT CLOSE.
    END.
END PROCEDURE.

PROCEDURE Actualiza:
FOR EACH TemDardeBaja
    NO-LOCK BREAK BY TAgeActu BY TCodigo:
    FIND FIRST Act_Fijo WHERE 
         Act_Fijo.Agencia   EQ TemDardeBaja.TAgeActu  AND
         Act_Fijo.Grupo     EQ TemDardeBaja.TGrupo    AND
         Act_Fijo.Codigo    EQ TemDardeBaja.TCodigo   NO-ERROR.
    IF AVAILABLE(Act_Fijo) THEN DO:
       UPDATE /*Act_Fijo.Agencia     = TemDardeBaja.TAgencia
              Act_Fijo.Cen_Costos  = TemDardeBaja.TCenCosto*/
              Act_Fijo.Descripcion = TemDardeBaja.TAclara
              Act_Fijo.Fec_debaja  = TODAY
              Act_Fijo.Estado      = 2. /* Retirado*/

       CREATE TemResultado.
       UPDATE TemResultado.TAgeActu  = Act_Fijo.Agencia
              TemResultado.TCodigo   = Act_Fijo.Codigo
              TemResultado.TCenCosto = Act_Fijo.Cen_Costos
              TemResultado.TEstado   = Act_Fijo.Estado
              TemResultado.TNombre   = Act_Fijo.Nombre
              TemResultado.TECodigo  = "Actualizado" 
              TemResultado.TGrupo    = Act_Fijo.Grupo
              TemResultado.TAclara   = Act_Fijo.Descripcion.
    END.
    ELSE DO:
        CREATE TemResultado.
        UPDATE TemResultado.TAgeActu  = TemDardeBaja.TAgeActu  /*TemDardeBaja.TAgencia*/
               TemResultado.TCodigo   = TemDardeBaja.TCodigo
               TemResultado.TCenCosto = 0
               TemResultado.TEstado   = 0
               TemResultado.TNombre   = "No Existe En Activos"
               TemResultado.TECodigo  = "No Existe..." 
               TemResultado.TGrupo    = TemDardeBaja.TGrupo
               TemResultado.TAclara   = TemDardeBaja.TAclara.   
    END.
END.
END PROCEDURE.

PROCEDURE ImpTemResultado:
OUTPUT TO VALUE(listado).
FOR EACH TemResultado BY TemResultado.TAgeActu BY TemResultado.TCodigo:
    ASSIGN viTotReg  = viTotReg + 1.

    ASSIGN vcactivo  = TRIM(TemResultado.TCodigo)   + " - " + TRIM(TemResultado.TNombre).

    CASE TemResultado.TEstado:
    WHEN 1 THEN
        ASSIGN vcestado = "1-Activo".
    WHEN 2 THEN
        ASSIGN vcestado = "2-Retirado".
    WHEN 3 THEN
        ASSIGN vcestado = "3-Depreciado".
    OTHERWISE
        ASSIGN vcestado = "".
    END.

    FORM 
        TemResultado.TAgeActu  COLUMN-LABEL "Agencia"           FORMAT "ZZZ"           
        TemResultado.TGrupo    COLUMN-LABEL "Grupo"             FORMAT "99999"    
        vcactivo               COLUMN-LABEL "Activo De Baja"    FORMAT "X(50)"             
        TemResultado.TCenCosto COLUMN-LABEL "C.Costo"           FORMAT "ZZZZZ" 
        SPACE(4)
        vcestado               COLUMN-LABEL "Estado"            FORMAT "X(14)"
        TemResultado.TAclara   COLUMN-LABEL "Aclaracion"        FORMAT "X(80)" 
        WITH FRAME FTemConta DOWN COLUMN 1 WIDTH 280
             NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
    DISPLAY 
        TemResultado.TAgeActu 
        TemResultado.TGrupo   
        vcactivo
        TemResultado.TCenCosto
        vcestado
        TemResultado.TAclara  
        WITH FRAME FTemConta.
    DOWN WITH FRAME FTemConta.
END.
DISPLAY
   "Total de Activos : " viTotReg
  WITH FRAME FRep3 NO-LABEL.
DOWN WITH FRAME FRep3.
END PROCEDURE.

PROCEDURE ProcesoImprimir:
{Incluido\RepEncabezado.I}
/* ASSIGN W_NmesInf = vcMes[MONTH(W_Fecha)] + " de " + STRING(YEAR(W_Fecha)). */
ASSIGN W_Reporte = "Reporte   : Dar de Bajas Activos Fijos.    Fecha : " + STRING(W_Fecha,"99/99/9999") + "     Hora :" + STRING(TIME,"HH:MM:SS")
       W_EncColumna = "Concepto                    Activo de Baja                      C.Costo    Estado       Aclaracion ".
VIEW FRAME F-Encabezado.
VIEW FRAME f-ftr.
ASSIGN viTotReg = 0.

FOR EACH TemResultado BREAK BY TemResultado.TAgeActu
    BY TemResultado.TGrupo:

    ASSIGN viTotReg  = viTotReg + 1.
    ASSIGN vcactivo  = TRIM(TemResultado.TCodigo)   + " - " + TRIM(TemResultado.TNombre).

    CASE TemResultado.TEstado:
    WHEN 1 THEN
        ASSIGN vcestado = "1-Activo".
    WHEN 2 THEN
        ASSIGN vcestado = "2-Retirado".
    WHEN 3 THEN
        ASSIGN vcestado = "3-Depreciado".
    OTHERWISE
        ASSIGN vcestado = "".
    END.

    FORM
        TemResultado.TAgeActu  COLUMN-LABEL "Agencia"           FORMAT "ZZZ"                  
        TemResultado.TGrupo    COLUMN-LABEL "Grupo"             FORMAT "99999" 
        vcactivo               COLUMN-LABEL "Activo De Baja"    FORMAT "X(50)" 
        TemResultado.TCenCosto COLUMN-LABEL "C.Costo"           FORMAT "ZZZZZ" 
        SPACE(4)                                                               
        vcestado               COLUMN-LABEL "Estado"            FORMAT "X(14)" 
        TemResultado.TAclara   COLUMN-LABEL "Aclaracion"        FORMAT "X(80)" 
        WITH FRAME FRep DOWN COLUMN 1 WIDTH 240
            NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT NO-LABELS STREAM-IO.

    IF FIRST-OF(TemResultado.TAgeActu) THEN DO:
       ASSIGN vcnomage = "".
       FIND Agencias WHERE Agencias.Agencia EQ TemResultado.TAgeActu
       NO-LOCK NO-ERROR.
       IF AVAILABLE (Agencias) THEN
          ASSIGN vcnomage = Agencias.Nombre.
       FORM                                          
          vcNom   FORMAT "X(60)"
         WITH FRAME FAge NO-BOX NO-LABEL WIDTH 60.
       DISPLAY ("Agencia: " + (STRING(Agencias.Agencia,"ZZZ")) + " - "+ CAPS(vcnomage)) @ vcNom  
         WITH FRAME FAge.                                                     
       DOWN WITH FRAME FAge.
    END.

    IF FIRST-OF(TemResultado.TGrupo) THEN DO:
       FIND Varios WHERE Varios.Tipo EQ 7 AND Varios.Codigo EQ TemResultado.TGrupo
       NO-LOCK NO-ERROR.
       IF AVAILABLE Varios THEN 
          ASSIGN vcnomgru = Varios.Descripcion.
       FORM                                          
          vcNom   FORMAT "X(50)"
         WITH FRAME FAge NO-BOX NO-LABEL WIDTH 60.
       DISPLAY ("Grupo: " + (STRING(Varios.Codigo,"99999")) + " - "+ CAPS(vcnomgru)) @ vcNom         
         WITH FRAME FAge.                                                     
       DOWN WITH FRAME FAge.

    END.
    DISPLAY 
        TemResultado.TAgeActu
        TemResultado.TGrupo   
        vcactivo              
        TemResultado.TCenCosto
        vcestado              
        TemResultado.TAclara  
      WITH FRAME FRep.
    DOWN WITH FRAME FRep.

    IF LAST-OF (TemResultado.TAgeActu) OR LAST-OF(TemResultado.TGrupo) THEN DO:
       DISPLAY ("") @ vcNom SKIP
         WITH FRAME FRep.
       DOWN WITH FRAME FRep.
    END.
END.
DISPLAY
   "Total de Activos : " viTotReg
  WITH FRAME FRep1 NO-LABEL.
DOWN WITH FRAME FRep1.
END PROCEDURE.
