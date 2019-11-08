&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}


    {Incluido\variable.i "shared"}


/*     DEFINE {1} VAR W_Estacion    LIKE Estaciones.Estacion.                                                   */
/*     DEFINE {1} VAR W_Usuario     LIKE Usuarios.Usuario.                                                      */
/*     DEFINE {1} VAR W_Clave       LIKE Usuarios.Clave FORMAT "X(16)".                                         */
/*     DEFINE {1} VAR W_Prioridad   LIKE Usuarios.Prioridad INITIAL "".                                         */
/*     DEFINE {1} VAR W_Agencia     LIKE Usuarios.Agencia INITIAL 0.                                            */
/*     DEFINE {1} VAR W_Ciudad      LIKE Agencia.Ciudad INITIAL 0.                                              */
/*     DEFINE {1} VAR W_Nom_Agencia   AS CHARACTER FORMAT "X(60)".                                              */
/*     DEFINE {1} VAR W_UbiDatos      AS CHAR INITIAL "D".                                                      */
/*     /*DEFINE {1} VAR W_Ninv        LIKE Inversion.Nro_inversion.*/                                           */
/*     DEFINE {1} VAR W_Nom_Entidad   AS CHARACTER FORMAT "X(60)".                                              */
/*     DEFINE {1} VAR W_Entidad     LIKE Entidad.Entidad.                                                       */
/*     DEFINE {1} VAR W_NitGlobal   LIKE Clientes.Nit INITIAL "".                                               */
/*     DEFINE {1} VAR W_SMLV        LIKE Indicadores.Valor INITIAL 0.                                           */
/*                                                                                                              */
/*                                                                                                              */
/*     DEFINE {1} VAR W_Manija        AS HANDLE.                                                                */
/*     DEFINE {1} VAR W_ManFin        AS HANDLE.                                                                */
/*     DEFINE {1} VAR W_ManTaq        AS HANDLE.                                                                */
/*     DEFINE {1} VAR W_Nivel       LIKE Cuentas.Nivel.                                                         */
/*     DEFINE {1} VAR W_CtaMay      LIKE Cuentas.Cuenta.                                                        */
/*     DEFINE {1} VAR W_Fecha         AS DATE FORMAT "99/99/9999" INITIAL TODAY.                                */
/*     DEFINE {1} VAR W_ficina        AS CHARACTER FORMAT "X(40)" VIEW-AS COMBO-BOX INNER-LINES 4 SIZE 40 BY 1. */
/*     DEFINE {1} VAR W_Path        LIKE Entidad.Dir_Programas.                                                 */
/*     DEFINE {1} VAR W_Pathspl     LIKE Entidad.Dir_Spl.                                                       */
/*     DEFINE {1} VAR W_Eleccion      AS LOGICAL.                                                               */
/*     DEFINE {1} VAR W_CedGral     LIKE Clientes.Nit.                                                          */
/*     DEFINE {1} VAR W_CenCosGral  LIKE Cen_Costos.Cen_Costos.                                                 */
/*     DEFINE {1} VAR W_Cadena        AS CHARACTER FORMAT "X(9)" INITIAL "SIFINCOOP".                           */
/*     /*DEFINE     VAR Agencia_Cnt     AS INTEGER FORMAT "999".*/                                              */
/*     DEFINE {1} VAR P-Valida        AS LOGICAL.                                                               */
/*     DEFINE {1} VAR W_VCodPcto    LIKE Ahorros.Cod_Ahorro.                                                    */
/*     DEFINE {1} VAR W_VCueAhorro  LIKE Ahorros.Cue_Ahorros.                                                   */
/*                                                                                                              */
/*     DEFINE {1} VAR W_Solicitud   LIKE Solicitud.Num_Solicitud.                                               */
/*     DEFINE {1} VAR W_PagareS     LIKE Creditos.Pagare.                                                       */
/*                                                                                                              */
/*     DEFINE {1} VAR P_SdoTot      LIKE Creditos.Sdo_Capital.                                                  */
/*     DEFINE {1} VAR W_OfiCierre   LIKE Agencias.Agencia.                                                      */

    DEFI VARIABLE TotReg       AS INTEG FORM "9999999"   NO-UNDO.
    DEFI VARIABLE CuoAdel      LIKE Creditos.Cuo_Pagadas NO-UNDO.
    DEFI VARIABLE W_Proyectado LIKE Creditos.Sdo_Proyectado NO-UNDO.
    DEFI VARIABLE W_Tasa       LIKE Creditos.Tasa NO-UNDO.
    DEFI VARIABLE W_PdoLiq     AS INTEG FORM "99" NO-UNDO.
    DEFI VARIABLE W_DiaPdo     AS INTEG FORM "99" NO-UNDO.
    DEFI VARIABLE KSdo         LIKE Creditos.Sdo_Capital INIT 0 NO-UNDO.
    DEFI VARIABLE KInt         LIKE Creditos.Sdo_Capital INIT 0 NO-UNDO.
    DEFI VARIABLE KCap         LIKE Creditos.Sdo_Capital INIT 0 NO-UNDO.
    DEFI VARIABLE KIAcu        LIKE Creditos.Sdo_Capital INIT 0 NO-UNDO.
    DEFI VARIABLE KCAcu        LIKE Creditos.Sdo_Capital INIT 0 NO-UNDO.

    DEFI VARIABLE KCuo         LIKE Creditos.Cuota   NO-UNDO.              
    DEFI VARIABLE NN           AS INTEG FORM "99999" NO-UNDO.
    DEFI VARIABLE CuoFalt      AS INTEG FORM "99999" NO-UNDO.  
    DEFI VARIABLE PdoTrans     AS INTEG FORM "99999" NO-UNDO.
    DEFI VARIABLE W_FecIni     AS DATE NO-UNDO.                      
    DEFI VARIABLE xFecIni      AS DATE NO-UNDO.                      
    DEFI VARIABLE W_FecTra     AS DATE NO-UNDO.
    DEFI VARIABLE W_FecIniCont AS DATE NO-UNDO.
    DEFI VARIABLE W_SiPdoTr    AS LOG INIT FALSE NO-UNDO.
    DEFINE VARIABLE iax AS INTEGER NO-UNDO.
    DEFINE VARIABLE xtas LIKE creditos.tasa NO-UNDO.


    DEFINE VARIABLE Puntero AS ROWID.
    DEFINE VAR W_Age  LIKE Ahorros.Agencia.
    DEFINE VAR W_Pro  LIKE Ahorros.Cod_Ahorro.
    DEFINE VAR W_Nit  LIKE Ahorros.Nit.
    DEFINE VAR vinumcre LIKE creditos.num_credito.
    DEFINE VAR W_Cue  LIKE Ahorros.Cue_Ahorros.
    DEFINE VAR W_vlr  LIKE Ahorros.Sdo_Disponible INITIAL 0.
    DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
    DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
    DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
    DEFINE VARIABLE P_AgeCli   LIKE Clientes.Agencia.
    DEFINE VARIABLE W_Error       AS LOGICAL.
    DEFINE VARIABLE W_Autorizo  LIKE Usuarios.Usuario.
    DEFINE VARIABLE W_NUEVO       AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Clientes

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain Clientes.Nit 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain Clientes.Nit 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain Clientes
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain Clientes
&Scoped-define QUERY-STRING-fMain FOR EACH Clientes SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH Clientes SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-fMain Clientes


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Clientes.Nit 
&Scoped-define ENABLED-TABLES Clientes
&Scoped-define FIRST-ENABLED-TABLE Clientes
&Scoped-Define ENABLED-OBJECTS ECT-309 RECT-274 BUTTON-1 w_Credito ~
Btn_Consulta btn_procesar Btn_Cancelar BtnDone-2 BUTTON-95 
&Scoped-Define DISPLAYED-FIELDS Clientes.Nit 
&Scoped-define DISPLAYED-TABLES Clientes
&Scoped-define FIRST-DISPLAYED-TABLE Clientes
&Scoped-Define DISPLAYED-OBJECTS W_NomTitular w_Credito 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 BUTTON-1 Btn_Consulta Btn_Cancelar BUTTON-95 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dplanpagos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser-2 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone-2 DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "" 
     SIZE 7 BY 1.65 TOOLTIP "Salir"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Cancelar 
     IMAGE-UP FILE "imagenes/borrar.bmp":U
     LABEL "&Cancelar" 
     SIZE 7 BY 1.65 TOOLTIP "Cancelar"
     FONT 4.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 3" 
     SIZE 7 BY 1.65 TOOLTIP "Buscar".

DEFINE BUTTON btn_procesar 
     IMAGE-UP FILE "imagenes/proceso_g.bmp":U
     LABEL "Actualizar" 
     SIZE 7 BY 1.65 TOOLTIP "Actualizar".

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 7 BY 1.65 TOOLTIP "Información".

DEFINE BUTTON BUTTON-95 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 95" 
     SIZE 4 BY 1.12.

DEFINE VARIABLE w_Credito AS INTEGER FORMAT "999999999":U INITIAL 0 
     LABEL "Nro.Credito" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomTitular AS CHARACTER FORMAT "X(50)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 46 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE ECT-309
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 2.96.

DEFINE RECTANGLE RECT-274
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9 BY 8.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      Clientes SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-1 AT ROW 1.54 COL 91.14 WIDGET-ID 58
     Clientes.Nit AT ROW 1.81 COL 11 COLON-ALIGNED WIDGET-ID 64
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     W_NomTitular AT ROW 1.81 COL 39 COLON-ALIGNED WIDGET-ID 70
     w_Credito AT ROW 3.15 COL 11 COLON-ALIGNED WIDGET-ID 68
     Btn_Consulta AT ROW 3.15 COL 91.14 WIDGET-ID 54
     btn_procesar AT ROW 4.77 COL 91.14 WIDGET-ID 56
     Btn_Cancelar AT ROW 6.38 COL 91.14 WIDGET-ID 52
     BtnDone-2 AT ROW 8 COL 91 WIDGET-ID 50
     BUTTON-95 AT ROW 10.15 COL 92 WIDGET-ID 60
     ECT-309 AT ROW 1.27 COL 2 WIDGET-ID 62
     RECT-274 AT ROW 1.27 COL 90.14 WIDGET-ID 66
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.14 BY 10.27
         BGCOLOR 17 FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Plan Pagos - W-Mant_PlanPagos.w"
         HEIGHT             = 10.27
         WIDTH              = 98.14
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME fMain
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Consulta IN FRAME fMain
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME fMain
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-95 IN FRAME fMain
   6                                                                    */
/* SETTINGS FOR FILL-IN W_NomTitular IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       W_NomTitular:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "bdcentral.Clientes"
     _Query            is OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Plan Pagos - W-Mant_PlanPagos.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Plan Pagos - W-Mant_PlanPagos.w */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone-2 wWin
ON CHOOSE OF BtnDone-2 IN FRAME fMain
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar wWin
ON CHOOSE OF Btn_Cancelar IN FRAME fMain /* Cancelar */
DO:
    RUN inicializar_variables NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta wWin
ON CHOOSE OF Btn_Consulta IN FRAME fMain /* Button 3 */
DO:
    RUN C-Clientes.R(INPUT 2, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_procesar wWin
ON CHOOSE OF btn_procesar IN FRAME fMain /* Actualizar */
DO: 
    DEFINE VARIABLE listado AS CHARACTER   NO-UNDO.
    ASSIGN listado = W_Pathspl + "ErrGenreal.Txt".
  OUTPUT TO VALUE(listado).
/*   SESSION:SET-WAIT-STATE("General"). */
  MESSAGE "Inicio Proceso Generar PlanPagos..., Primero se Borran todos los PlanPagos." SKIP
      "Desea continuar?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Genera Plan Pagos" UPDATE choice AS LOGICAL.
  IF NOT choice THEN 
      RETURN.

  FOR EACH Creditos WHERE
    creditos.nit EQ nit AND creditos.num_credito EQ W_Credito
    BY Num_Credito:

    IF Creditos.Sdo_Capital LE 0 THEN NEXT.
    IF Creditos.Sdo_Capital LE 0 
    OR Creditos.Nit         LE "0"      
    OR Creditos.Costas      LT 0 
    OR Creditos.Cuota       LE 0
    OR Creditos.Fec_Desembolso EQ ? OR STRING(Creditos.Fec_Desembolso) LE " "
    OR Creditos.Int_Anticipado LT 0
    OR Creditos.Int_Corrientes LT 0
    OR Creditos.Int_DifCobro   LT 0 
    OR Creditos.Int_MoraDifCob LT 0
    OR Creditos.Int_MorCobrar  LT 0 
    OR Creditos.Monto          LE 0
    OR Creditos.Plazo          LE 0
    OR Creditos.Polizas        LT 0 
    OR (Creditos.Int_Anticipado GT 0 AND (Creditos.INT_Corrientes + Creditos.Int_DifCobro GT 0)) THEN DO: 
        DISPLAY  Creditos.Nit Creditos.Pagare Creditos.Num_credito Cod_credito sdo_capital " Error datos"
         WITH FRAME F1 NO-LABELS NO-BOX.            
       NEXT.   
    END.


    xtas = Tasa.
    FOR EACH  planpagos WHERE planpagos.agencia = creditos.agencia         AND
                              planpagos.nit     = creditos.nit             AND
                              planpagos.cod_credito = creditos.cod_credito AND
                              planpagos.num_credito = creditos.num_credito: 
        DELETE planpagos. 
    END.


    xFecIni = Creditos.Fec_Desembolso.
    IF Creditos.Fec_PagAnti NE ? AND Creditos.Fec_PagAnti GT Creditos.Fec_Desembolso THEN
      xFecIni = Creditos.Fec_PagAnti.

    ASSIGN Per_pago = 4.
          /* Creditos.Fec_Desembolso = Creditos.Fec_Aprobac
           Creditos.Fec_PagAnt     = Creditos.Fec_Aprobac.*/

    IF Creditos.Monto LT Creditos.Sdo_Capital THEN
       Creditos.Monto = Creditos.Sdo_Capital.

    IF Creditos.Sistema NE 1 AND (Creditos.Cuota LT Creditos.Monto) THEN
       Creditos.Sistema = 1.

    ASSIGN W_FecTra   = xfecini
           W_FecIni   = xfecini
           Creditos.Sdo_CapPag     = Creditos.Monto - Creditos.Sdo_Capital
           Creditos.Val_Desembolso = Creditos.Monto
           Creditos.Sdo_Proyectado = Creditos.Monto
           Creditos.Capital_Acum   = 0
           Creditos.Int_LiqAcum    = 0
           Creditos.Sdo_Intpag     = 0
           Creditos.Cuo_Pagadas    = 0
           Creditos.Cuo_Atraso     = 0
           Creditos.Dias_Atraso    = 0
           Creditos.Val_Atraso     = 0
           Creditos.Estado         = 2
           /*Creditos.Fec_Aprobacion = Creditos.Fec_Desembolso*/
           Creditos.For_Interes    = 1
           W_PdoLiq = 12    /*Inicia mensual*/
           W_DiaPdo = 30.

    IF Creditos.Per_Pago EQ 1 THEN
       ASSIGN W_DiaPdo = 7
              W_PdoLiq = 52.
    ELSE IF Creditos.Per_Pago EQ 2 THEN
       ASSIGN W_DiaPdo = 10
              W_PdoLiq = 36.
    ELSE IF Creditos.Per_Pago EQ 3 THEN
       ASSIGN W_DiaPdo = 15
              W_PdoLiq = 24.
  
    ASSIGN Creditos.Fec_Pago = xfecini + W_DiaPdo
           W_Tasa            = xtas / (W_PdoLiq * 100). 

   IF Creditos.Sistema         EQ 2 
    OR Creditos.Plazo           EQ 1
    OR (Creditos.Sdo_Capital GE Creditos.Monto) THEN DO:

       ASSIGN Creditos.Cuo_Pagadas = 0.

       IF Creditos.Sistema EQ 2 AND Creditos.Plazo GT 1 THEN DO
          NN = 1 TO Creditos.Plazo:

          RUN Halla_FecVcto.p (INPUT  W_FecIni,W_DiaPdo,W_FecTra,   
                               OUTPUT W_FecTra).  
          Creditos.Fec_Pago = W_FecTra.
       END.
       ELSE DO: /* Sale */
          RUN Halla_FecVcto.p (INPUT  W_FecIni,W_DiaPdo,W_FecTra,   
                               OUTPUT W_FecTra).  
          Creditos.Fec_Pago = W_FecTra.
       END.            
    END.

    IF Creditos.Sistema EQ 2 OR Creditos.Plazo   EQ 1 THEN DO:
       RUN CrearPlan NO-ERROR.
       ASSIGN PlanPagos.Id_PdoMes         = 1                      
              PlanPagos.Nro_Cuota         = 1                      
              PlanPagos.Fec_Ini           = xfecini
              PlanPagos.Fec_Vcto          = Creditos.Fec_Pago      
              PlanPagos.Fec_ProxPag       = Creditos.Fec_Pago      
              PlanPagos.Cuo_Pagas         = 0
              PlanPagos.Pagos_CapitalAcum = Creditos.Sdo_CapPag
              PlanPagos.Pagos_CapitalPdo  = Creditos.Sdo_CapPag
              PlanPagos.Int_LiqPdo        = Creditos.Int_Corrientes + Creditos.Int_DifCobr.

       IF TODAY GE Creditos.Fec_Pago THEN    /*Ya está vencido totalmente*/
          ASSIGN Creditos.Sdo_Proyectado = 0
                 Creditos.Capital_Acum   = Creditos.Monto
                 Creditos.Int_LiqAcum    = Creditos.Int_Corrientes + Creditos.Int_DifCobro  
                 PlanPagos.Capital_Acum  = Creditos.Monto
                 PlanPagos.Int_LiqAcum   = Creditos.Int_Corrientes + Creditos.Int_DifCobr
                 Creditos.Cuo_Atraso     = 1
                 Creditos.Dias_Atraso    = TODAY - Creditos.Fec_Pago
                 Creditos.Val_Atraso     = Creditos.Sdo_Capital.         
       NEXT.
    END.
    ELSE DO:

        ASSIGN KSdo     = Creditos.Sdo_Capital
               KCuo     = Creditos.Cuota      
               CuoFalt  = Creditos.Plazo
               W_FecTra   = xfecini
               W_FecIni   = xfecini.
    END.
    IF (Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        +
        Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes +
        Creditos.Int_DifCobro   + Creditos.Sdo_Capital    - Creditos.Int_Anticipado) LE Creditos.Cuota THEN 
        ASSIGN Creditos.Cuo_Pagadas = Creditos.Plazo - 1
               CuoFalt              = 1.
    ELSE IF Creditos.Sdo_Capital LT Creditos.Monto THEN DO:
        DO NN = 1 TO Creditos.Plazo:   /*Con Sdo-Capital halla Faltantes*/
           ASSIGN KInt = ROUND(KSdo * W_Tasa,0)
                  KCap = ROUND(KCuo - KInt,0)
                  KSdo = KSdo - KCap.

           IF KSdo LE 0 THEN DO:  
              CuoFalt = NN.       
              LEAVE.
           END.

           CuoFalt = NN.
        END.

        Creditos.Cuo_Pagadas = Creditos.Plazo - CuoFalt.   /*Plazo menos faltantes son pagadas*/
    END.

    DO NN = 1 TO (Creditos.Cuo_Pagadas):  /*Halla Fecha próximo pago con base en Pagadas*/ 
       RUN Halla_FecVcto.p (INPUT  W_FecIni,W_DiaPdo,W_FecTra,   
                            OUTPUT W_FecTra).  
       Creditos.Fec_Pago = W_FecTra.
    END.

    ASSIGN W_FecTra   = xfecini  
           W_FecIni   = xfecini
           /*KSdo       = Creditos.Sdo_Capital*/
           KSdo       = Creditos.Monto
           KCuo       = Creditos.Cuota
           KIAcu      = 0
           KCAcu      = 0
           PdoTrans   = 0
           KInt       = 0
           KCap       = 0.

    DO NN = 1 TO Creditos.Plazo:    /*Halla la Cuota(Pdo.) que transcurre y Sdo_proy con base Monto*/ 
       ASSIGN KInt  = ROUND(KSdo * W_Tasa,0)
              KCap  = ROUND(KCuo - KInt,0) 
              KIAcu = KIAcu + KInt
              KCAcu = KCAcu + KCap
              KSdo  = KSdo  - KCap
              W_FecIniCont = W_FecTra.

       RUN Halla_FecVcto.p (INPUT  W_FecIni,W_DiaPdo,W_FecTra,   
                            OUTPUT W_FecTra). 

       IF W_FecTra GE TODAY THEN DO:
          PdoTrans = NN.
          LEAVE.
       END.  

       /*ASSIGN KInt  = ROUND(KSdo * W_Tasa,0)
              KCap  = ROUND(KCuo - KInt,0) 
              KIAcu = KIAcu + KInt
              KCAcu = KCAcu + KCap
              KSdo  = KSdo  - KCap.*/
    END.

    IF PdoTrans LE 0 AND xfecini LT (TODAY - W_DiaPdo) THEN
       ASSIGN PdoTrans = Creditos.Plazo + 1.
    ELSE IF PdoTrans LE 0 THEN
       PdoTrans = 1.

    ASSIGN W_FecTra   = xfecini  
           W_FecIni   = xfecini.             

    IF PdoTrans GT 1 THEN DO NN = 1 TO PdoTrans - 1:     /*Halla la Fec-Ini del que transcurre - 1*/ 
       ASSIGN W_FecIniCont = W_FecTra.

       RUN Halla_FecVcto.p (INPUT  W_FecIni,W_DiaPdo,W_FecTra,   
                            OUTPUT W_FecTra). 
    END.            

    ASSIGN W_SiPdoTr  = FALSE
           KSdo       = Creditos.Sdo_Capital
           KCuo       = Creditos.Cuota
           W_FecIni   = W_FecIniCont
           W_FecTra   = W_FecIniCont. 

    DO NN = (PdoTrans - 1) TO Creditos.Plazo + 1:  /*Genera los Reg.del PlanPagos*/ 
       RUN CrearPlan NO-ERROR.
       ASSIGN PlanPagos.Nro_Cuota = NN                      
              PlanPagos.Fec_Ini   = W_FecTra
              PlanPagos.Fec_Vcto  = W_FecTra.

       IF NN GT 0 THEN DO:
          RUN Halla_FecVcto.p (INPUT  W_FecIni,W_DiaPdo,W_FecTra,   
                               OUTPUT W_FecTra).
          PlanPagos.Fec_Vcto = W_FecTra.
       END.

       IF NN LT PdoTrans THEN
          ASSIGN PlanPagos.Id_PdoMes         = 2   /*Ya Cumplido*/
                 Creditos.Capital_Acum       = KCAcu - KCap
                 Creditos.Int_LiqAcum        = KIAcu - KInt
                 PlanPagos.Capital_Acum      = KCAcu - KCap
                 Creditos.Sdo_Proyectado     = Creditos.Monto - Creditos.Capital_Acum
                 PlanPagos.Int_LiqAcum       = KIAcu - KInt
                 PlanPagos.Int_LiqPdo        = KInt
                 PlanPagos.Capital_Pdo       = KCap
                 PlanPagos.Pagos_CapitalPdo  = Creditos.Sdo_CapPag.
       ELSE IF NN EQ PdoTrans THEN DO:
          ASSIGN PlanPagos.Id_PdoMes         = 1   /*Transcurre*/
                 Creditos.Capital_Acum       = KCAcu - KCap
                 Creditos.Sdo_Proyectado     = Creditos.Monto - Creditos.Capital_Acum
                 Creditos.Int_LiqAcum        = KIAcu - KInt
                 PlanPagos.Capital_Acum      = KCAcu - KCap
                 PlanPagos.Int_LiqAcum       = KIAcu - KInt
                 PlanPagos.Int_LiqPdo        = ROUND(((Creditos.Sdo_Proyectado * W_Tasa) / W_DiaPdo)
                                                     * (TODAY - PlanPagos.Fec_Ini),0)
                 /*PlanPagos.Int_LiqPdo        = (KInt / W_DiaPdo) * (TODAY - PlanPagos.Fec_Ini + 1)*/
                 PlanPagos.Capital_Pdo       = 0.
          IF NN EQ Creditos.Cuo_Pagadas AND Creditos.Sdo_Proyectado LE Creditos.Sdo_Capital THEN
             ASSIGN Creditos.Cuo_Pagadas  = Creditos.Cuo_Pagadas - 1
                    Creditos.Fec_Pago     = PlanPagos.Fec_Ini
                    PlanPagos.Fec_ProxPag = Creditos.Fec_Pago.
          ELSE IF  NN GT Creditos.Cuo_Pagadas AND Creditos.Sdo_Proyectado LE Creditos.Sdo_Capital
               AND (Creditos.Sdo_Capital - Creditos.Sdo_Proyectado) LT Creditos.Cuota THEN
             ASSIGN Creditos.Cuo_Pagadas  = Creditos.Cuo_Pagadas + 1
                    Creditos.Fec_Pago     = PlanPagos.Fec_Inic 
                    PlanPagos.Fec_ProxPag = Creditos.Fec_Pago.
       END.

       IF NN LE PdoTrans THEN
          ASSIGN PlanPagos.Fec_ProxPag       = Creditos.Fec_Pago
                 PlanPagos.Cuo_Pagas         = Creditos.Cuo_Pagadas
                 PlanPagos.Pagos_CapitalAcum = Creditos.Sdo_CapPag.                   
    END.

    FIND LAST PlanPagos WHERE PlanPagos.Agencia         EQ Creditos.Agencia    
                             AND PlanPagos.Nit          EQ Creditos.Nit                                    
                             AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito                            
                             AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito                            
                             AND PlanPagos.Id_PdoMes    EQ 1   /*Transc.*/  NO-ERROR.          
    IF NOT AVAIL(PlanPagos) THEN DO:
       RUN CrearPlan NO-ERROR.                        
       ASSIGN PlanPagos.Id_PdoMes     = 1
              PlanPagos.Nro_Cuota     = Creditos.Plazo + 1       
              PlanPagos.Fec_Ini       = TODAY
              PlanPagos.Fec_Vcto      = Creditos.Fec_Pago
              PlanPagos.Fec_ProxPago  = Creditos.Fec_Pago
              Creditos.Sdo_Proyectado = 0.
    END.

    ASSIGN Creditos.Sdo_IntPag     = Creditos.Int_LiqAcum - (Creditos.Int_Corrientes + Creditos.Int_DifCobro)
           PlanPagos.Pagos_IntAcum = Creditos.Sdo_IntPag.

    IF PlanPagos.Nro_Cuota EQ Creditos.Cuo_Pagadas AND Creditos.Sdo_Capital GT Creditos.Sdo_Proyectado THEN
       ASSIGN Creditos.Cuo_Pagadas = Creditos.Cuo_Pagadas - 1
              PlanPagos.Cuo_Pagas  = Creditos.Cuo_Pagadas.

    IF PlanPagos.Nro_Cuota GT Creditos.Plazo THEN
       ASSIGN Creditos.Sdo_Proyectado = 0
              Creditos.Capital_Acum   = Creditos.Monto
              Creditos.Cuo_Atraso     = Creditos.Plazo - Creditos.Cuo_Pagadas
              PlanPagos.Capital_Acum  = Creditos.Monto.
    ELSE IF PlanPagos.Nro_Cuota - 1 LE Creditos.Cuo_Pagadas THEN
       Creditos.Cuo_Atraso = 0.
    ELSE IF PlanPagos.Nro_Cuota - 1 GT Creditos.Cuo_Pagadas THEN
       Creditos.Cuo_Atraso = (PlanPagos.Nro_Cuota - 1) - Creditos.Cuo_Pagadas.

    ASSIGN Creditos.Val_Atraso = Creditos.Sdo_Capital - Creditos.Sdo_Proyectado WHEN
                                 Creditos.Sdo_Capital GT Creditos.Sdo_Proyectado.

    ASSIGN Creditos.Dias_Atraso = TODAY - Creditos.Fec_Pago WHEN 
                                  TODAY GT Creditos.Fec_Pago.

  END.
/*   SESSION:SET-WAIT-STATE(""). */

  OUTPUT CLOSE.

   MESSAGE "Generado log de ejecución, en caso de error." SKIP
       listado
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

   DYNAMIC-FUNCTION('setQueryWhere':U IN h_dplanpagos,
     INPUT "nit EQ '" + w_nit + "' AND num_credito EQ INTEGER(" + STRING(vinumcre) + ")" ).
   DYNAMIC-FUNCTION('setQuerySort':U IN h_dplanpagos,
     INPUT "by Nro_Cuota").
   DYNAMIC-FUNCTION('openQuery':U IN h_dplanpagos).



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Nit wWin
ON LEAVE OF Clientes.Nit IN FRAME fMain /* Nit */
DO:
  DO WITH FRAME F_Clientes:
     /* btn_procesar:SENSITIVE = FALSE. */
     FIND Clientes WHERE 
          (Clientes.Nit EQ Clientes.Nit:SCREEN-VALUE) AND
          Clientes.estado EQ 1 NO-LOCK NO-ERROR.

     IF AVAILABLE(Clientes) THEN DO:
        W_NomTitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
        ASSIGN W_Nit = Clientes.Nit.
     END.
     ELSE DO:
        MESSAGE "Cliente No Existe ó No esta Activo"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
     END.
     W_Credito:SCREEN-VALUE = "".
     btn_procesar:SENSITIVE = FALSE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w_Credito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w_Credito wWin
ON LEAVE OF w_Credito IN FRAME fMain /* Nro.Credito */
DO:
  ASSIGN w_credito.
  ASSIGN vinumcre = integer(w_credito:SCREEN-VALUE).
  DO WITH FRAME F_Clientes:
     FIND Creditos WHERE 
         Creditos.Nit         EQ Clientes.nit:SCREEN-VALUE AND 
         Creditos.num_credito EQ vinumcre AND
         Creditos.estado EQ 2 NO-ERROR.
     IF NOT AVAILABLE(Creditos) THEN DO:
        MESSAGE "El Credito No Existe o esta Cancelado"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        W_Credito:SCREEN-VALUE = "".
        btn_procesar:SENSITIVE = FALSE.
     END.
     ELSE
        btn_procesar:SENSITIVE = TRUE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'dplanpagos.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedplanpagosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dplanpagos ).
       RUN repositionObject IN h_dplanpagos ( 1.27 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsAgencia,Nit,Num_Credito,Nro_Cuota,Fec_Inic,Fec_ProxPago,Fec_Vcto,Pagos_CapitalAcum,Pagos_CapitalPdo,Int_LiqPdoEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdplanpagosUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser-2 ).
       RUN repositionObject IN h_dynbrowser-2 ( 4.50 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser-2 ( 6.46 , 86.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_dynbrowser-2. */
       RUN addLink ( h_dplanpagos , 'Data':U , h_dynbrowser-2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser-2 ,
             Btn_Consulta:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CrearPlan wWin 
PROCEDURE CrearPlan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CREATE PlanPagos.
ASSIGN PlanPagos.Agencia      = Creditos.Agencia          
       PlanPAgos.Nit          = Creditos.Nit              
       PlanPagos.Num_Credito  = Creditos.Num_credito          
       PlanPagos.Pagare       = Creditos.Pagare        
       PlanPagos.Cod_Credito  = Creditos.Cod_Credito           
       PlanPagos.Tip_Credito  = Creditos.Tip_Credito
       PlanPagos.Cuota        = Creditos.Cuota            
       PlanPagos.Tasa         = Creditos.Tasa             
       PlanPagos.Plazo        = Creditos.Plazo            
       PlanPagos.Monto_Actual = Creditos.Monto            
       PlanPagos.Id_PdoMes    = 0.                 /*Inicia en futuro*/
ASSIGN TotReg = TotReg + 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/

  {&OPEN-QUERY-fMain}
  GET FIRST fMain.
  DISPLAY W_NomTitular w_Credito 
      WITH FRAME fMain IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Nit 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE ECT-309 RECT-274 BUTTON-1 Clientes.Nit w_Credito Btn_Consulta 
         btn_procesar Btn_Cancelar BtnDone-2 BUTTON-95 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  DYNAMIC-FUNCTION('closeQuery':U IN h_dplanpagos).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

