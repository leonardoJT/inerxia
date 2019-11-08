&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-InfGer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-InfGer 
/*------------------------------------------------------------------------
  File:         W-InfGer.W
  Description:  Informes Gerenciales
  Author:       GAER
  Created:      Julio 24/05
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

  {Incluido\VARIABLE.I "SHARED"}
  DEFI VAR W_NomEnt   LIKE Entidad.Nombre.      
  DEFI VAR W_AnoAct   AS INTEG FORM "9999".
  DEFI VAR W_MesAct   AS INTEG FORM "99". 
  DEFI VAR W_MesIni   AS INTEG FORM "99".
  DEFI VAR W_AnoIni   AS INTEG FORM "9999".
  DEFI VAR W_TotIng   LIKE Ahorros.Sdo_Dispon INIT 0 EXTENT 3.
  DEFI VAR W_ArchVcto AS CHAR FORM "X(35)".
  DEFI VAR PromedDisp LIKE Ahorros.Sdo_Dispon INIT 0.
  DEFI VAR PromedAhV  LIKE Ahorros.Sdo_Dispon INIT 0.
  DEFI VAR VrFdoLiq   LIKE Ahorros.Sdo_Dispon INIT 0.
  DEFI VAR VrContDeu  LIKE Ahorros.Sdo_Dispon INIT 0.
  DEFI VAR ProyContr  LIKE Ahorros.Sdo_Dispon INIT 0 EXTENT 6.
  DEFI VAR ProyAport  LIKE Ahorros.Sdo_Dispon INIT 0.
  DEFI VAR W_IntCtesNoP LIKE Ahorros.Sdo_Dispon INIT 0.
  
  DEFINE VARIABLE Wk_TipoDocto  AS CHARACTER FORM "X(20)" EXTENT 10 INITIAL
  ["01-Cédula Ciudadanía","02-Cédula Extranjería","03-N.I.T.","04-Tarjeta Identidad","05-Pasaporte",~
   "06-Tarjeta Seguro Social Extranjero","07-Soc. Extranjera sin NIT en Colombia","08-Fideicomiso",~
   "09-Registro Civil","10-RUT PsonaNatural"].

  DEFI VAR W_ProrRen AS LOG INIT FALSE.
  DEFI VAR W_CedAho  LIKE Ahorros.Nit.
  DEFI VAR FecAho    LIKE Ahorros.Fec_UltTrans.
  DEFI VAR I_Tasa LIKE Ahorros.Tasa.
  DEFI VAR AnoIni AS INTEG FORM "9999".
  DEFI VAR MesIni AS INTEG FORM "99".
  DEFI VAR AnoFin AS INTEG FORM "9999".
  DEFI VAR MesFin AS INTEG FORM "99".
  DEFI VAR FecIni AS INTEG FORM "999999".
  DEFI VAR FecFin AS INTEG FORM "999999".
  DEFI VAR FecCor AS INTEG FORM "999999".
  DEFI VAR LiqInt LIKE Ahorros.Sdo_Dispon.
  DEFI VAR W_RowidTMes AS ROWID.
  DEFI VAR PrAVBda7    LIKE Ahorros.Sdo_Dispon  INIT 0 EXTENT 7.       
  DEFI VAR PromBda     LIKE Ahorros.Sdo_Dispon  INIT 0 EXTENT 12.
  DEFI VAR W_NroDiasFinan AS DECIMAL            INIT 0 NO-UNDO.


   DEFI TEMP-TABLE T_PromMes  /*  BORRAR   09072009 */                    
       FIELD Ident      AS CHAR     FORMAT "X(8)"            INITIAL ""   
       FIELD AnoMes     AS INTEGER  FORMAT "999999"          INITIAL "0"  
       FIELD Vr_Promed  AS DECIMAL  FORMAT "->>>>>>,>>>,>>9" INITIAL "0"  
       FIELD Nro_Regist AS INTEGER  FORMAT ">,>>>,>>9"       INITIAL "0". 
                                                                          
  DEFI TEMP-TABLE TProvee
      FIELD CedNit LIKE Clientes.Nit
      FIELD Suc    LIKE Agencias.Agencia
      FIELD Fec    AS DATE.

  DEFI TEMP-TABLE TDiaA
       FIELD FecD AS DATE
       FIELD TSdo LIKE Ahorros.Sdo_Dispon INIT 0
       FIELD TAcumes LIKE Ahorros.Sdo_Dispon INIT 0
       FIELD TProm LIKE Ahorros.Sdo_Dispon INIT 0.
    
  DEFI TEMP-TABLE TOf
       FIELD Ofi  LIKE Agencias.Agencia
       FIELD Cta  LIKE Cuentas.Cuenta
       FIELD Nom  LIKE Agencias.Nombre FORM "X(30)"
       FIELD Sdos LIKE Ahorros.Sdo_Dispon INIT 0 EXTENT 4 FORM "->>>>>>>,>>>,>>>,>>9".

  DEFI TEMP-TABLE TGtoof
       FIELD Ofi  LIKE Agencias.Agencia
       FIELD Cta  LIKE Cuentas.Cuenta
       FIELD Nom  LIKE Agencias.Nombre FORM "X(30)"
       FIELD Sdos LIKE Ahorros.Sdo_Dispon INIT 0 EXTENT 4 FORM "->>>>>>>,>>>,>>>,>>9".
    
  DEFI TEMP-TABLE TOfi
       FIELD Tip   AS INTEG FORM "9"
       FIELD Cta   LIKE Cuentas.Cuenta
       FIELD Clas  AS CHAR  FORM "X(30)"
       FIELD SCar  LIKE Ahorros.Sdo_Disponible EXTENT 18 FORMAT "->>>>>>>>>>>>>9" INIT 0.

  DEFI TEMP-TABLE TBrecha
       FIELD Mes   AS INTEG FORM "99"
       FIELD SCar  LIKE Ahorros.Sdo_Disponible EXTENT 9 FORMAT "->>>>>>>,>>>,>>>,>>9" INIT 0.

  DEFINE TEMP-TABLE W_MovTempo LIKE Mov_contable.

  DEFINE TEMP-TABLE MovTempo
      FIELD Cuenta       LIKE Mov_contable.Cuenta
      FIELD Fec_Contable LIKE Mov_contable.Fec_Contable
      FIELD Naturaleza   AS CHAR   /* LIKE Mov_contable.Naturaleza */
      FIELD Valor        LIKE Mov_contable.db.

  DEFI TEMP-TABLE TMes
     FIELD Tipo AS   CHAR  FORM "X"
     FIELD TA   AS   INTEG FORM "9"
     FIELD AMes AS   INTEG FORM "999999"
     FIELD SdoK LIKE Ahorros.Sdo_Dispon INIT 0 FORM "->>>>>>,>>>,>>>,>>9"
     FIELD SdoI LIKE Ahorros.Sdo_Dispon INIT 0 FORM "->>>>>>,>>>,>>>,>>9"
     FIELD Nro  AS   INTEG FORM "9999999"
     FIELD Tasa LIKE Ahorros.Tasa
     FIELD Banda AS INTEG FORM 9
     FIELD Comen AS CHAR  FORM "X(20)".

  DEFI TEMP-TABLE TEval
     FIELD Clas AS INTEG FORM "9"
     FIELD Reng AS INTEG FORM "999"
     FIELD Cpto AS CHAR  FORM "X(35)"
     FIELD Vlr  AS DEC   FORM "->>>>,>>>,>>>,>>9" INITIAL 0
     FIELD Bda  AS DEC   FORM "->>>>,>>>,>>>,>>9" EXTENT 7 LABEL "BANDA - " INITIAL 0.

  DEFI TEMP-TABLE TBanda
     FIELD Id-A  AS   CHAR  FORM "X(5)"
     FIELD Tipo  AS   CHAR  FORM "X"
     FIELD Banda AS INTEG FORM "9"
     FIELD SdoK  LIKE Ahorros.Sdo_Dispon INIT 0 FORM "->>>>>>,>>>,>>>,>>9"
     FIELD SdoI  LIKE Ahorros.Sdo_Dispon INIT 0 FORM "->>>>>>,>>>,>>>,>>9"
     FIELD Tasa  LIKE Ahorros.Tasa.

  DEF VAR W_CostoInv     AS DECIMAL INITIAL 0 NO-UNDO.
  DEF VAR W_CostoVtas    AS DECIMAL INITIAL 0 NO-UNDO.
  DEF VAR W_NroDiasPer   AS INTEGER INITIAL 0 NO-UNDO.
  DEF VAR W_NroDiasInv   AS INTEGER INITIAL 0 NO-UNDO.
  DEF VAR W_SdoForm1     AS DECIMAL INITIAL 0 NO-UNDO.
  DEF VAR W_SdoForm2     AS DECIMAL INITIAL 0 NO-UNDO.
  DEF VAR W_NroDiasRotac AS DECIMAL INITIAL 0 NO-UNDO.
  DEF VAR W_NroBandas    AS DECIMAL INITIAL 0 NO-UNDO.
  DEF VAR W_vlrxMesInv   AS DECIMAL INITIAL 0 NO-UNDO.
  DEF VAR wbda           AS DECIMAL INITIAL 0 EXTENT 7.


  DEFINE TEMP-TABLE PromedioMes
      FIELD PromMes AS INT
      FIELD PromVal AS DECIMAL FORMAT "->>>>>>,>>>,>>>,>>9".


  DEFINE TEMP-TABLE TCred
      FIELD CNit    LIKE creditos.nit
      FIELD CNumC   LIKE creditos.Num_Credito
      FIELD CLinea  AS CHAR
      FIELD cCod    LIKE Creditos.Cod_Credito
      FIELD cSaldo  LIKE Creditos.Sdo_Capital
      FIELD CSdoK   LIKE Ahorros.Sdo_Dispon INIT 0 FORM "->>>>>>,>>>,>>>,>>9" EXTENT 9
      FIELD CSdoI   LIKE Ahorros.Sdo_Dispon INIT 0 FORM "->>>>>>,>>>,>>>,>>9" EXTENT 9.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Inf

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-11 RECT-4 RECT-5 RECT-6 W_FecCorte ~
Cmb_Inf W_FecIniP W_FecFinP W_PorcVta Btn_Proc W_TasInv W_TasFdo ~
Btn_ImpProm W_TasAhV W_Tasreno PorcAsamb W_Bda1 W_Bda2 W_Bda3 W_Bda4 W_Bda5 ~
W_Bda6 W_Bda7 Btn_EvalRLiq 
&Scoped-Define DISPLAYED-OBJECTS W_FecCorte Cmb_Inf W_FecIniP W_FecFinP ~
W_PorcVta W_TasInv W_TasFdo W_TasAhV W_Tasreno PorcAsamb W_Bda1 W_Bda2 ~
W_Bda3 W_Bda4 W_Bda5 W_Bda6 W_Bda7 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD HallaBanda W-InfGer 
FUNCTION HallaBanda RETURNS INTEGER
  (/* INPUT XYMes AS INTEGER */)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SumatoriaCta W-InfGer 
FUNCTION SumatoriaCta RETURNS DECIMAL
  ( INPUT stopmes AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-InfGer AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_EvalRLiq 
     LABEL "&Evaluación del Riesgo de Liquidez" 
     SIZE 33.72 BY 1.15.

DEFINE BUTTON Btn_ImpProm 
     LABEL "Inf.Promedios Históricos" 
     SIZE 24.29 BY .96.

DEFINE BUTTON Btn_Proc 
     LABEL "&Procesar Informe" 
     SIZE 18.29 BY .88.

DEFINE VARIABLE Cmb_Inf AS CHARACTER FORMAT "X(50)":U 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "Fondo de Liquidez","Cartera Consolidada","Ejecución Presupuestal","Excedentes Acumulados","Gastos-Generales Acum.","Informe Ejecutivo","Ingresos-Egresos Caja","Brecha de Liquidez","Disponible: Promedio Dia/Año","Aho_Vista: Promedio Dia/Año","IContingente: Promedio Dia/Año","Proyeccion Recaudo Contractual","Próximos-Vencimientos - Resumen X Bandas","Inf-Proveedores","Inf-Psonas.Juridicas-RetFte" 
     DROP-DOWN-LIST
     SIZE 30.14 BY 1 NO-UNDO.

DEFINE VARIABLE PorcAsamb AS DECIMAL FORMAT "->9.99":U INITIAL 0 
     LABEL "% Revaloriz Exced." 
     VIEW-AS FILL-IN 
     SIZE 7.72 BY 1 NO-UNDO.

DEFINE VARIABLE W_Bda1 AS DECIMAL FORMAT "->>>>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88
     FONT 4 NO-UNDO.

DEFINE VARIABLE W_Bda2 AS DECIMAL FORMAT "->>>>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88
     FONT 4 NO-UNDO.

DEFINE VARIABLE W_Bda3 AS DECIMAL FORMAT "->>>>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88
     FONT 4 NO-UNDO.

DEFINE VARIABLE W_Bda4 AS DECIMAL FORMAT "->>>>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88
     FONT 4 NO-UNDO.

DEFINE VARIABLE W_Bda5 AS DECIMAL FORMAT "->>>>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88
     FONT 4 NO-UNDO.

DEFINE VARIABLE W_Bda6 AS DECIMAL FORMAT "->>>>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88
     FONT 4 NO-UNDO.

DEFINE VARIABLE W_Bda7 AS DECIMAL FORMAT "->>>>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88
     FONT 4 NO-UNDO.

DEFINE VARIABLE W_FecCorte AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha de Corte" 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY 1 NO-UNDO.

DEFINE VARIABLE W_FecFinP AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY 1 NO-UNDO.

DEFINE VARIABLE W_FecIniP AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY 1 NO-UNDO.

DEFINE VARIABLE W_PorcVta AS DECIMAL FORMAT "9.9999":U INITIAL .2 
     LABEL "Margen Util. Invent" 
     VIEW-AS FILL-IN 
     SIZE 12.29 BY .81 NO-UNDO.

DEFINE VARIABLE W_TasAhV AS DECIMAL FORMAT "->>9.9999":U INITIAL 0 
     LABEL "Tasa Int.(A.V.Prom)" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .81 NO-UNDO.

DEFINE VARIABLE W_TasFdo AS DECIMAL FORMAT "->>9.9999":U INITIAL 0 
     LABEL "Tasa Int.(Fdo.Liq)" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .81 NO-UNDO.

DEFINE VARIABLE W_TasInv AS DECIMAL FORMAT "->>9.9999":U INITIAL 0 
     LABEL "Tasa Int(Inversiones)" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .81 NO-UNDO.

DEFINE VARIABLE W_Tasreno AS DECIMAL FORMAT "->>9.9999":U INITIAL 0 
     LABEL "% No Renovado CDAT" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 4 GRAPHIC-EDGE    
     SIZE 29 BY 1.92
     BGCOLOR 1 FGCOLOR 15 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 4 GRAPHIC-EDGE    
     SIZE 38.86 BY 1.96
     BGCOLOR 1 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 4 GRAPHIC-EDGE    
     SIZE 23.57 BY 1.54
     BGCOLOR 1 FGCOLOR 15 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77.43 BY 1.42.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Inf
     W_FecCorte AT ROW 1.19 COL 63 COLON-ALIGNED
     Cmb_Inf AT ROW 1.27 COL 20 COLON-ALIGNED
     W_FecIniP AT ROW 2.35 COL 21.57 COLON-ALIGNED
     W_FecFinP AT ROW 2.35 COL 49 COLON-ALIGNED
     W_PorcVta AT ROW 3.69 COL 21.57 COLON-ALIGNED WIDGET-ID 4
     Btn_Proc AT ROW 4.77 COL 57.72
     W_TasInv AT ROW 4.81 COL 21.57 COLON-ALIGNED WIDGET-ID 2
     W_TasFdo AT ROW 5.88 COL 21.57 COLON-ALIGNED
     Btn_ImpProm AT ROW 6.58 COL 51.86
     W_TasAhV AT ROW 6.96 COL 21.57 COLON-ALIGNED
     W_Tasreno AT ROW 8 COL 21.57 COLON-ALIGNED WIDGET-ID 6
     PorcAsamb AT ROW 8.27 COL 68.72 COLON-ALIGNED
     W_Bda1 AT ROW 10.54 COL 1.86 NO-LABEL
     W_Bda2 AT ROW 10.54 COL 12.72 NO-LABEL
     W_Bda3 AT ROW 10.54 COL 23.72 NO-LABEL
     W_Bda4 AT ROW 10.54 COL 34.57 NO-LABEL
     W_Bda5 AT ROW 10.54 COL 45.43 NO-LABEL
     W_Bda6 AT ROW 10.54 COL 56.29 NO-LABEL
     W_Bda7 AT ROW 10.54 COL 67.29 NO-LABEL
     Btn_EvalRLiq AT ROW 12.77 COL 42.57
     "Act 16-06-2010  11:09" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 13.92 COL 2 WIDGET-ID 8
     "Vlrs.Bandas de Obligaciones Bancarias :" VIEW-AS TEXT
          SIZE 28.72 BY .62 AT ROW 9.65 COL 49.86
          BGCOLOR 1 FGCOLOR 8 FONT 4
     RECT-11 AT ROW 6.08 COL 49.57
     RECT-4 AT ROW 12.31 COL 40
     RECT-5 AT ROW 4.42 COL 55
     RECT-6 AT ROW 10.27 COL 1.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 78 BY 13.54
         BGCOLOR 17 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-InfGer ASSIGN
         HIDDEN             = YES
         TITLE              = "Informes Gerenciales, Programa W-InfGer.W"
         HEIGHT             = 13.54
         WIDTH              = 78
         MAX-HEIGHT         = 28.85
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 28.85
         VIRTUAL-WIDTH      = 182.86
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-InfGer
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Inf
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN W_Bda1 IN FRAME F_Inf
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_Bda2 IN FRAME F_Inf
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_Bda3 IN FRAME F_Inf
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_Bda4 IN FRAME F_Inf
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_Bda5 IN FRAME F_Inf
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_Bda6 IN FRAME F_Inf
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_Bda7 IN FRAME F_Inf
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-InfGer)
THEN W-InfGer:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Inf
/* Query rebuild information for FRAME F_Inf
     _Query            is NOT OPENED
*/  /* FRAME F_Inf */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-InfGer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-InfGer W-InfGer
ON END-ERROR OF W-InfGer /* Informes Gerenciales, Programa W-InfGer.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-InfGer W-InfGer
ON WINDOW-CLOSE OF W-InfGer /* Informes Gerenciales, Programa W-InfGer.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_EvalRLiq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_EvalRLiq W-InfGer
ON CHOOSE OF Btn_EvalRLiq IN FRAME F_Inf /* Evaluación del Riesgo de Liquidez */
DO:
    DEFINE VAR Tecla AS LOGICAL NO-UNDO.
    
    FOR EACH TBanda: DELETE TBanda. END.

    MESSAGE "Seleccione a Continuación el Archivo-Informe ProxVctos-DDMMAA.Txt," SKIP
            "Generado en la Fecha de Corte Respectiva."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    SYSTEM-DIALOG GET-FILE W_ArchVcto
                      TITLE      "Seleccione Archivo Prox.Vctos..."
                      FILTERS    "Archivos Texto (*.txt)"   "*.txt"
                      MUST-EXIST
                      USE-FILENAME
                      UPDATE Tecla
                      RETURN-TO-START-DIR.
    IF Tecla THEN 
       RUN Impor_VctosCred.
    ELSE DO: 
       MESSAGE "No Seleccionó Archivo para Madurar Créditos, Se genera con los Datos a Hoy."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

       SESSION:SET-WAIT-STATE("GENERAL"). 
         RUN Prox_Vctos.
         RUN Impor_VctosCred.
       SESSION:SET-WAIT-STATE(""). 
    END.

    RUN Arma_FtoRiesgo.
        
    MESSAGE "Finalizó Generación del Formato en C:\InfRed\EvRiesgoL-FCorte" 
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_ImpProm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ImpProm W-InfGer
ON CHOOSE OF Btn_ImpProm IN FRAME F_Inf /* Inf.Promedios Históricos */
DO:
   DEFI VAR ANOMES  AS INTEG FORM "999999".
   DEFI VAR AMC     AS CHAR  FORM "999999".
   DEFI VAR ANOMESF AS INTEG FORM "999999".
   DEFI VAR AMCF    AS CHAR  FORM "999999".
   DEFI VAR Mes     AS INTEG FORM "99".
   DEFI VAR AMesC   AS CHAR  FORM "999999".
   DEFI VAR MesC    AS CHAR  FORM "99".

   SESSION:SET-WAIT-STATE("GENERAL").
   
   OUTPUT TO C:\InfRed\Inf_promed.Txt.
   
   ASSIGN AMC    = STRING(YEAR(W_FecIniP),"9999") + STRING(MONTH(W_FecIniP),"99")
          AnoMes = INTEG(AMC).
         
  ASSIGN AMCF    = STRING(YEAR(W_FecFinP),"9999") + STRING(MONTH(W_FecFinP),"99")
         AnoMesF = INTEG(AMCF).
         
   FOR EACH T_PromMes WHERE T_PromMes.AnoMes GE AnoMes
                       AND  T_PromMes.AnoMes LE AnoMesF NO-LOCK
             BREAK BY T_PromMes.Ident BY T_PromMes.AnoMes:
       DISPL T_PromMes.Ident      LABEL  "Clase" 
             T_PromMes.AnoMes     LABEL  "Año-Mes"
             T_PromMes.Vr_Promed  LABEL  "Valor Promedio"
             T_PromMes.Nro_Reg    LABEL  "N.Registros"   
           WITH DOWN WIDTH 120 FRAME F1 NO-BOX NO-LABELS USE-TEXT STREAM-IO.
            
       IF LAST-OF(T_PromMes.Ident) then
          DISP SKIP (2).                                
  END.
  
  OUTPUT CLOSE.
  
  SESSION:SET-WAIT-STATE("").

  MESSAGE "El informe fuè generado en : C:\InfRed\Inf_promed.Txt."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Proc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Proc W-InfGer
ON CHOOSE OF Btn_Proc IN FRAME F_Inf /* Procesar Informe */
DO:
    SESSION:SET-WAIT-STATE("GENERAL").

    IF Cmb_Inf EQ "Fondo de Liquidez" THEN
        RUN Fdo_Liqui.
    ELSE
        IF Cmb_Inf EQ "Cartera Consolidada" THEN
            RUN Cartera_Consolidada.
        ELSE
            IF Cmb_Inf EQ "Ejecución Presupuestal" THEN
                RUN Ejec_Pptal.
            ELSE
                IF Cmb_Inf EQ "Excedentes Acumulados"  THEN
                    RUN Exced_Acum.

                /* oakley */
  ELSE IF Cmb_Inf EQ "Gastos-Generales Acum." THEN
     RUN GtoGral_Acum.
  ELSE IF Cmb_Inf EQ "Informe Ejecutivo"      THEN
     RUN Inf_Ejecutivo.
  ELSE IF Cmb_Inf EQ "Brecha de Liquidez"     THEN
     RUN Brecha_Liqui.
  ELSE IF Cmb_Inf EQ "Ingresos-Egresos Caja"  THEN
     RUN Flu_Caja.
  ELSE IF Cmb_Inf EQ "Disponible: Promedio Dia/Año" THEN  /*Caja y Bcos, Oct.19/2006 GAER*/
     RUN Dispon_PromDiaAno.
  ELSE IF Cmb_Inf EQ "Aho_Vista: Promedio Dia/Año" THEN  /*Oct.20/2006 GAER*/
     RUN AhoVta_PromDiaAno.
  ELSE IF Cmb_Inf EQ "IContingente: Promedio Dia/Año" THEN  /*Oct.20/2006 GAER*/
     RUN IConting_PromDiaAno.
  ELSE IF Cmb_Inf EQ "Proyeccion Recaudo Contractual" THEN  /*Oct.20/2006 GAER*/
     RUN Proy_RecContractual.
  ELSE IF Cmb_Inf EQ "Próximos-Vencimientos - Resumen X Bandas" THEN
     RUN Prox_Vctos.  
  ELSE IF Cmb_Inf EQ "Inf-Proveedores" THEN   /*Desde Dic.19/08*/
     RUN Inf_Proveed. 
 ELSE IF Cmb_Inf EQ "Inf-Psonas.Juridicas-RetFte" THEN    /*Desde Dic.19/08*/
     RUN Inf_PJurid. 
    
  MESSAGE "Finalizó Generación del Informe"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
  SESSION:SET-WAIT-STATE("").    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Inf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Inf W-InfGer
ON MOUSE-SELECT-CLICK OF Cmb_Inf IN FRAME F_Inf /* Informes Disponibles */
DO:
  ASSIGN Cmb_Inf.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Inf W-InfGer
ON VALUE-CHANGED OF Cmb_Inf IN FRAME F_Inf /* Informes Disponibles */
DO:
  ASSIGN Cmb_Inf.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PorcAsamb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PorcAsamb W-InfGer
ON LEAVE OF PorcAsamb IN FRAME F_Inf /* % Revaloriz Exced. */
DO:
   ASSIGN PorcAsamb.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Bda1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Bda1 W-InfGer
ON LEAVE OF W_Bda1 IN FRAME F_Inf
DO:
  ASSIGN W_Bda1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Bda2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Bda2 W-InfGer
ON LEAVE OF W_Bda2 IN FRAME F_Inf
DO:
  ASSIGN W_Bda2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Bda3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Bda3 W-InfGer
ON LEAVE OF W_Bda3 IN FRAME F_Inf
DO:
  ASSIGN W_Bda3.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Bda4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Bda4 W-InfGer
ON LEAVE OF W_Bda4 IN FRAME F_Inf
DO:
  ASSIGN W_Bda4.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Bda5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Bda5 W-InfGer
ON LEAVE OF W_Bda5 IN FRAME F_Inf
DO:
  ASSIGN W_Bda5.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Bda6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Bda6 W-InfGer
ON LEAVE OF W_Bda6 IN FRAME F_Inf
DO:
  ASSIGN W_Bda6.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Bda7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Bda7 W-InfGer
ON LEAVE OF W_Bda7 IN FRAME F_Inf
DO:
  ASSIGN W_Bda7.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecCorte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecCorte W-InfGer
ON LEAVE OF W_FecCorte IN FRAME F_Inf /* Fecha de Corte */
DO:
  ASSIGN W_FecCorte
         W_AnoAct = YEAR(W_FecCorte)
         W_MesAct = MONTH(W_FecCorte).

  IF MONTH(W_FecCorte) EQ 2 AND DAY(W_FecCorte) EQ 29 THEN
     ASSIGN W_FecCorte                             = W_FecCorte - 1
            W_FecCorte:SCREEN-VALUE IN FRAME F_Inf = STRING(W_FecCorte)
            W_AnoAct                               = YEAR(W_FecCorte)
            W_MesAct                               = MONTH(W_FecCorte).

  ASSIGN W_MesIni = MONTH(W_FecCorte) + 1
         W_AnoIni = YEAR(W_FecCorte)  - 1
         W_FecFinP              = W_FecCorte
         W_FecFinP:SCREEN-VALUE = STRING(W_FecCorte).

  IF W_MesIni EQ 13 THEN
     ASSIGN W_MesIni = 01
            W_AnoIni = YEAR(W_FecCorte).

  ASSIGN W_FecIniP              = DATE(W_MesIni,01,W_AnoIni)
         W_FecIniP:SCREEN-VALUE = STRING(W_FecIniP).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecFinP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecFinP W-InfGer
ON LEAVE OF W_FecFinP IN FRAME F_Inf /* Fecha Final */
DO:
  ASSIGN W_FecFinP.

  IF W_FecFinP LT W_FecIniP THEN
     ASSIGN W_FecFinP = W_FecIniP
            W_FecFinP:SCREEN-VALUE = STRING(W_FecIniP).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecIniP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecIniP W-InfGer
ON LEAVE OF W_FecIniP IN FRAME F_Inf /* Fecha Inicial */
DO:
  ASSIGN W_FecIniP.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_PorcVta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_PorcVta W-InfGer
ON LEAVE OF W_PorcVta IN FRAME F_Inf /* Margen Util. Invent */
DO:
  ASSIGN W_PorcVta.
  IF W_PorcVta GT 1 THEN DO:
     MESSAGE "El margen no puede ser mayor del 100% (1)" SKIP
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ASSIGN W_PorcVta:SCREEN-VALUE = "0.20"  W_PorcVta.
     RETURN.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_TasAhV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_TasAhV W-InfGer
ON LEAVE OF W_TasAhV IN FRAME F_Inf /* Tasa Int.(A.V.Prom) */
DO:
   ASSIGN W_TasAhV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_TasFdo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_TasFdo W-InfGer
ON LEAVE OF W_TasFdo IN FRAME F_Inf /* Tasa Int.(Fdo.Liq) */
DO:
   ASSIGN W_TasFdo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_TasInv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_TasInv W-InfGer
ON LEAVE OF W_TasInv IN FRAME F_Inf /* Tasa Int(Inversiones) */
DO:
  ASSIGN W_TasInv.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Tasreno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Tasreno W-InfGer
ON LEAVE OF W_Tasreno IN FRAME F_Inf /* % No Renovado CDAT */
DO:
   ASSIGN W_Tasreno.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-InfGer 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  ON RETURN TAB.
  FIND FIRST Entidad NO-LOCK NO-ERROR.
  IF AVAILABLE Entidad THEN W_NomEnt = UPPER(Entidad.Nombre).

  ASSIGN W_FecCorte                             = W_Fecha
         W_FecCorte:SCREEN-VALUE IN FRAME F_Inf = STRING(W_Fecha)
         W_AnoAct                               = YEAR(W_FecCorte)
         W_MesAct                               = MONTH(W_FecCorte).
                  

  IF MONTH(W_Fecha) EQ 2 AND DAY(W_Fecha) EQ 29 THEN
     ASSIGN W_FecCorte                             = W_Fecha - 1
            W_FecCorte:SCREEN-VALUE IN FRAME F_Inf = STRING(W_FecCorte)
            W_AnoAct                               = YEAR(W_FecCorte)
            W_MesAct                               = MONTH(W_FecCorte).

  ASSIGN W_MesIni = MONTH(W_FecCorte) + 1
         W_AnoIni = YEAR(W_FecCorte)  - 1
         W_FecFinP              = W_FecCorte
         W_FecFinP:SCREEN-VALUE = STRING(W_FecCorte).

  IF W_MesIni EQ 13 THEN
     ASSIGN W_MesIni = 01
            W_AnoIni = YEAR(W_FecCorte).

  ASSIGN W_FecIniP              = DATE(W_MesIni,01,W_AnoIni)
         W_FecIniP:SCREEN-VALUE = STRING(W_FecIniP).

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
     WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AhoVta_PromDiaAno W-InfGer 
PROCEDURE AhoVta_PromDiaAno :
/*------------------------------------------------------------------------------
  Purpose:  Promedio Dia/Año del Aho-Vta.   
 
------------------------------------------------------------------------------*/
   DEFI VAR Vlr0     LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR W_SdoCta LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR TDisp    LIKE Ahorros.Sdo_Dispon INIT 0.
   DEFI VAR TFin     LIKE Ahorros.Sdo_Dispon INIT 0.
   DEFI VAR TMes     LIKE Ahorros.Sdo_Dispon INIT 0.
   DEFI VAR TMesT    LIKE Ahorros.Sdo_Dispon INIT 0.
   DEFI VAR PorC     AS DEC FORM "-99999.99".
   DEFI VAR Arch     AS CHAR FORM "X(35)".
   DEFI VAR FecNN    AS DATE.
   DEFI VAR NN       AS INTEG FORM "99999999".
   DEFI VAR NFI      AS INTEG FORM "99999999".
   DEFI VAR NFF      AS INTEG FORM "99999999".

   FOR EACH TDiaA: DELETE TDiaA. END.
      
   Arch = "C:\InfRed\AhoVtaPromDA-" + STRING(W_FecCorte,"999999") + "_" + W_Usuario.

   OUTPUT TO VALUE(Arch).

   DISPLAY "    " + W_NomEnt + "  AHORRO A LA VISTA RETIROS Y PROMEDIO DIA/AÑO DE "
          + STRING(W_FecIniP,"99/99/9999") + " HASTA " + STRING(W_FecFinP,"99/99/9999") FORM "X(120)"
         SKIP(1)
       WITH DOWN WIDTH 150 FRAME Enc NO-BOX NO-LABELS STREAM-IO USE-TEXT.

   ASSIGN FecNN = W_FecIniP - 1.

   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 0, INPUT 999, INPUT "210505",          
                                   INPUT YEAR(FecNN), INPUT MONTH(FecNN),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).   
   TDisp = W_SdoCta.
    
   FOR EACH Mov_Contable   FIELDS(Mov_Contable.Fec_Contable Mov_Contable.Db Mov_Contable.Cr)
                           WHERE Mov_Contable.Cuenta     GE "2105050000"
                           AND Mov_Contable.Cuenta       LE "2105059999"
                           AND Mov_Contable.Fec_Contable GE W_FecIniP
                           AND Mov_Contable.Fec_Contable LE W_FecFinP NO-LOCK
            BREAK BY YEAR(Mov_Contable.Fec_Contable) BY MONTH(Mov_Contable.Fec_Contable)
                  BY Mov_Contable.Fec_Contable:
       IF FIRST-OF(Mov_Contable.Fec_Contable) THEN DO:
          CREATE TDiaA.
          ASSIGN TDiaA.FecD = Mov_Contable.Fec_Contable
                 TDiaA.TSdo = TDisp.
       END.

       IF Mov_Contable.db GT 0 THEN
          ASSIGN TDiaA.TSdo = TDiaA.TSdo - Mov_Contable.Db
                 TMes       = TMes  + Mov_Contable.Db
                 TMesT      = TMesT + Mov_Contable.Db.
       ELSE
          ASSIGN TDiaA.TSdo = TDiaA.TSdo + Mov_Contable.Cr.
       
        
       IF LAST-OF(Mov_Contable.Fec_Contable) THEN DO:
          DISPLAY TDiaA.FecD    LABEL "Fecha-Día"
                  TDiaA.TSdo    LABEL "Saldo del Día"
                  TMes          LABEL "Retiros Acumul.Mes"
              WITH DOWN WIDTH 150 FRAME Fd1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

          ASSIGN TDisp = TDiaA.TSdo
                 TFin  = TFin + TDiaA.TSdo.
       END.

       IF LAST-OF(MONTH(Mov_Contable.Fec_Contable)) THEN DO:
          DISPLAY "----------  ---------------------- -----------------------" SKIP
                  TDiaA.FecD    
                  ""
                  TDiaA.TSdo    
                  TMes    SKIP(1)      
              WITH DOWN WIDTH 150 FRAME Fd2 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

          ASSIGN Tmes = 0.
       END.
  END.
    
  DISPLAY SKIP(1)
          "               Total de Saldos Día Tot./365 = Sdo.Promedio     Retiros Totales" SKIP                 
          "----------  ---------------------- ----------------------- -------------------" SKIP
          "TOTAL       "
          TFin 
          "   "
          TFin / 365  FORM "->>>>>>>,>>>,>>9"
          TmesT  
     WITH DOWN WIDTH 150 FRAME Totl NO-BOX NO-LABELS STREAM-IO USE-TEXT.
    
  OUTPUT CLOSE.

  ASSIGN PromedAhV = TFin / 365 
         TmesT     = 0.

  FIND FIRST T_PromMes WHERE T_PromMes.Ident  EQ "AhoVista"
                         AND T_PromMes.AnoMes EQ INTEG(STRING(YEAR(W_FecFinP),"9999") + 
                                                       STRING(MONTH(W_FecFinP),"99")) NO-ERROR.
  IF NOT AVAILABLE (T_PromMes) THEN
     CREATE T_PromMes.
  ASSIGN T_PromMes.Ident      = "AhoVista"
         T_PromMes.AnoMes     = INTEG(STRING(YEAR(W_FecFinP),"9999") + STRING(MONTH(W_FecFinP),"99"))
         T_PromMes.Nro_Regist = 365
         T_PromMes.Vr_Promed  = PromedAhV.     
         
  RUN Difer_PromedHist (INPUT "AhoVista").         

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Arma_FtoRiesgo W-InfGer 
PROCEDURE Arma_FtoRiesgo :
/*------------------------------------------------------------------------------
  Purpose:     

------------------------------------------------------------------------------*/
   DEFI VAR Vlr0     LIKE Sal_Cuenta.Sal_Inicial.
   DEFI VAR W_SdoCta LIKE Sal_Cuenta.Sal_Inicial.
   DEFI VAR W_SdoBda LIKE Sal_Cuenta.Sal_Inicial.
   DEFI VAR W_SdoCre LIKE Sal_Cuenta.Sal_Inicial EXTENT 5.   

   SESSION:SET-WAIT-STATE("GENERAL"). 

   FOR EACH TEval: DELETE TEval. END.
   
   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "11",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta). 
   CREATE TEval.
   ASSIGN TEval.Clas   = 1
          TEval.Reng   = 005
          TEval.Cpto   = "Disponible"
          TEval.Vlr    = W_SdoCta
          TEval.Bda[7] = W_SdoCta.

   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                 INPUT 999, INPUT 999, INPUT "1120",          
                                 INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                 OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   ASSIGN TEval.Vlr    = TEval.Vlr - W_SdoCta
          TEval.Bda[7] = TEval.Vlr.

   RUN Dispon_PromDiaAno.

/*IF PromedDisp LT TEval.Vlr THEN
      ASSIGN TEval.Bda[7] = PromedDisp
             TEval.Bda[6] = TEval.Vlr - PromedDisp.*/
    
   IF TEval.Vlr GT PromedDisp THEN DO:    /*Oct.30/07*/
      ASSIGN TEval.Bda[7] = PromedDisp
             TEval.Bda[1] = PrAvBda7[1]
             TEval.Bda[2] = PrAvBda7[2]
             TEval.Bda[3] = PrAvBda7[3]
             TEval.Bda[4] = PrAvBda7[4]
             TEval.Bda[5] = PrAvBda7[5]
             TEval.Bda[6] = PrAvBda7[6].
             
      ASSIGN W_SdoBda = TEval.Bda[7] + TEval.Bda[2] + TEval.Bda[3] + TEval.Bda[4] + TEval.Bda[5] + TEval.Bda[6].  
      IF W_SdoBda GT TEval.Vlr THEN
         TEval.Bda[1]   = (W_SdoBda - TEval.Vlr).
      ELSE TEval.Bda[1] = (TEval.Vlr - W_SdoBda).
   END.     
  
       /* NEGOCIABLES 007*/
   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1204",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta). 
   CREATE TEval.
   ASSIGN TEval.Clas   = 1
          TEval.Reng   = 007
          TEval.Cpto   = "INVERSIONES NEGOCIABLES"
          TEval.Vlr    = W_SdoCta
          TEval.Bda[7] = W_SdoCta.
   
   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1206",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   
   ASSIGN TEval.Vlr    = TEval.Vlr + W_SdoCta
          TEval.Bda[7] = TEval.Vlr .

   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                 INPUT 999, INPUT 999, INPUT "1286",          
                                 INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                 OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   ASSIGN TEval.Vlr    = TEval.Vlr - W_SdoCta
          TEval.Bda[7] = TEval.Vlr.
   
   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                 INPUT 999, INPUT 999, INPUT "1287",          
                                 INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                 OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   ASSIGN TEval.Vlr    = TEval.Vlr - W_SdoCta
          TEval.Bda[7] = TEval.Vlr.

   /*RUN HALLAPROMEDIONEGOCIABLE.*/

   /*FONDO LIQUIDEZ 015*/
   RUN Fdo_Liqui.
   CREATE TEval.
   ASSIGN TEval.Clas   = 1
          TEval.Reng   = 015
          TEval.Cpto   = "Fondo de Liquidez"
          TEval.Vlr    = VrFdoLiq
          TEval.Bda[7] = VrFdoLiq + ((VrFdoLiq * W_TasFdo) / 100).

   /* COMPROMISO REVENTA  020*/
   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1201",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta). 
   CREATE TEval.
   ASSIGN TEval.Clas   = 1
          TEval.Reng   = 020
          TEval.Cpto   = "COMPROMISO REVENTA INVERSIONES"
          TEval.Vlr    = 0
          TEval.Bda[7] = 0.
   
   /* COMPROMISO REVENTA CARTERA 025*/
   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1202",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta). 
   CREATE TEval.
   ASSIGN TEval.Clas   = 1
          TEval.Reng   = 025
          TEval.Cpto   = "COMPROMISO REVENTA CARTERA"
          TEval.Vlr    = 0
          TEval.Bda[7] = 0.
   /* INVERSIONES PARA MANTENER AL VENCIMIENTO 028*/
   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1208",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta). 
   CREATE TEval.
   ASSIGN TEval.Clas   = 1
          TEval.Reng   = 028
          TEval.Cpto   = "INVERSIONES PARA MANTENER AL VENCIMIENTO"
          TEval.Vlr    = W_SdoCta
          TEval.Bda[7] = W_SdoCta + ((W_SdoCta * W_TasInv) / 100).

   /* INVERSIONES PARA MANTENER AL VENCIMIENTO 035*/
   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1231",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta). 
   CREATE TEval.
   ASSIGN TEval.Clas   = 1
          TEval.Reng   = 035
          TEval.Cpto   = "DERECHO DE RECOMPRA DE INVERSIONES"
          TEval.Vlr    = 0
          TEval.Bda[7] = 0.




      /* INVERSIONES DISPONIBLE PARA LA VENTA 038*/
   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1213",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta). 
   CREATE TEval.
   ASSIGN TEval.Clas   = 1
          TEval.Reng   = 038
          TEval.Cpto   = "INVERSIONES DISPONIBLE PARA LA VENTA"
          TEval.Vlr    = W_SdoCta
          TEval.Bda[7] = W_SdoCta.
   
   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1216",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   ASSIGN TEval.Vlr    = TEval.Vlr + W_SdoCta
          TEval.Bda[7] = TEval.Vlr.

   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1291",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   ASSIGN TEval.Vlr    = TEval.Vlr + W_SdoCta
          TEval.Bda[7] = TEval.Vlr.

   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1289",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   ASSIGN TEval.Vlr    = TEval.Vlr + W_SdoCta
          TEval.Bda[7] = TEval.Vlr.

/* INVENTARIO */
  RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "13",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   CREATE TEval.
   ASSIGN TEval.Clas   = 1                    
          TEval.Reng   = 045                      
          TEval.Cpto   = "Inventarios"
          TEval.Vlr    = W_SdoCta.
   RUN Madurar_Inventarios.  
   FOR EACH TBanda WHERE SUBSTRING(TBanda.Id-A,1,3) EQ "1-C" 
                     BREAK BY TBanda.Id-A:
       IF      TBanda.Id-A EQ "1-CCO" THEN
          ASSIGN W_SdoCre[1] = W_SdoCre[1] + TBanda.SdoK.
       ELSE IF TBanda.Id-A EQ "1-CCI" THEN
          ASSIGN W_SdoCre[2] = W_SdoCre[2] + TBanda.SdoK.
       ELSE IF TBanda.Id-A EQ "1-CVH" THEN
          ASSIGN W_SdoCre[3] = W_SdoCre[3] + TBanda.SdoK.
       ELSE IF TBanda.Id-A EQ "1-CMI" THEN
             ASSIGN W_SdoCre[4] = W_SdoCre[4] + TBanda.SdoK.
       ELSE IF TBanda.Id-A EQ "1-CEM" THEN
             ASSIGN W_SdoCre[5] = W_SdoCre[5] + TBanda.SdoK.
   END.
   CREATE TEval.
   ASSIGN TEval.Clas   = 1
          TEval.Reng   = 050
          TEval.Cpto   = "Cartera de Créditos Consumo"
          TEval.Vlr    = W_SdoCre[1].
   FOR EACH TBanda WHERE SUBSTRING(TBanda.Id-A,1,5) EQ "1-CCO" AND TBanda.Banda LT 8
                     BREAK BY TBanda.Banda:
       TEval.Bda[TBanda.Banda] = TBanda.SdoK + TBanda.SdoI.
   END.
   /* CARTERA */
   RUN HallarSdoMayor (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1491",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   ASSIGN TEval.Vlr = TEval.Vlr - ABS(W_SdoCta).  /*Resta provision que es Sdo Neg.*/
   RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1498",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   ASSIGN TEval.Vlr = TEval.Vlr - ABS(W_SdoCta).  /*Resta provision que es Sdo Neg.*/
   CREATE TEval.
   ASSIGN TEval.Clas   = 1
          TEval.Reng   = 055
          TEval.Cpto   = "Cartera de Créditos Comercial"
          TEval.Vlr    = W_SdoCre[2].          
   FOR EACH TBanda WHERE SUBSTRING(TBanda.Id-A,1,5) EQ "1-CCI" AND TBanda.Banda LT 8
                     BREAK BY TBanda.Banda:
       TEval.Bda[TBanda.Banda] = TBanda.SdoK + TBanda.SdoI.
   END.   
   RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1495",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   ASSIGN TEval.Vlr = TEval.Vlr - ABS(W_SdoCta).
   /* viv */
   CREATE TEval.
   ASSIGN TEval.Clas   = 1
          TEval.Reng   = 060
          TEval.Cpto   = "Cartera de Créditos VVda-Hipot."
          TEval.Vlr    = W_SdoCre[3].          
   FOR EACH TBanda WHERE SUBSTRING(TBanda.Id-A,1,5) EQ "1-CVH" AND TBanda.Banda LT 8
                     BREAK BY TBanda.Banda:
       TEval.Bda[TBanda.Banda] = TBanda.SdoK + TBanda.SdoI.
   END.
   RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1489",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   ASSIGN TEval.Vlr = TEval.Vlr - ABS(W_SdoCta).

   RUN HallarSdoMayor (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1454",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta). 
   CREATE TEval.
   ASSIGN TEval.Clas   = 1
          TEval.Reng   = 062
          TEval.Cpto   = "CARTERA MICROCREDITO"
          TEval.Vlr    = 0
          TEval.Bda[7] = 0.
   RUN HallarSdoMayor (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1493",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   ASSIGN TEval.Vlr = TEval.Vlr - ABS(W_SdoCta). 
   CREATE TEval.
   ASSIGN TEval.Clas   = 1
          TEval.Reng   = 065
          TEval.Cpto   = "Cartera de Créditos MiCroCred."
          TEval.Vlr    = W_SdoCre[4].          
   FOR EACH TBanda WHERE SUBSTRING(TBanda.Id-A,1,5) EQ "1-CMI" AND TBanda.Banda LT 8
                     BREAK BY TBanda.Banda:
       TEval.Bda[TBanda.Banda] = TBanda.SdoK + TBanda.SdoI.
   END.
   /* CTA X COBRAR*/
   RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "16",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
    
   CREATE TEval.
   ASSIGN TEval.Clas   = 1                    
          TEval.Reng   = 075                      
          TEval.Cpto   = "Cuentas por Cobrar"
          TEval.Vlr    = W_SdoCta.
   RUN CxcInteres.
   RUN HallarSdoMayor (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1675",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   ASSIGN TEval.Bda[7] = TEval.Bda[7] + W_SdoCta.

   RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1620",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   ASSIGN TEval.Bda[7] = TEval.Bda[7] + W_SdoCta.

   RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1625",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   ASSIGN TEval.Bda[7] = TEval.Bda[7] + W_SdoCta.

   RUN HallarSdoMayor (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1635",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   ASSIGN TEval.Bda[7] = TEval.Bda[7] + W_SdoCta.

  /* Llevar la cartera de empleados a cada banda ( Sumandola a las anteriores ) */   
   FOR EACH TBanda WHERE SUBSTRING(TBanda.Id-A,1,5) EQ "1-CEM" AND TBanda.Banda LT 8
                     BREAK BY TBanda.Banda:
       TEval.Bda[TBanda.Banda] = TEval.Bda[TBanda.Banda] + TBanda.SdoK + TBanda.SdoI.
   END.
   RUN Madurar_CxC.

   CREATE TEval.
   ASSIGN TEval.Clas   = 1                    
          TEval.Reng   = 080                      
          TEval.Cpto   = "Propied.Planta y Equipo". 
   RUN HallarSdoMayor (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "17",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta). 
   ASSIGN TEval.Vlr    = W_SdoCta 
          TEval.Bda[7] = TEval.Bda[7] + W_SdoCta.

   CREATE TEval.
   ASSIGN TEval.Clas   = 1                    
          TEval.Reng   = 085                      
          TEval.Cpto   = "Activos Diferidos      ". 
   RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "18",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta). 
   ASSIGN TEval.Vlr    = W_SdoCta 
          TEval.Bda[7] = TEval.Bda[7] + W_SdoCta.

   CREATE TEval.
   ASSIGN TEval.Clas   = 1                    
          TEval.Reng   = 090                      
          TEval.Cpto   = "Otros Activos".
   RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "19",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta). 
   ASSIGN TEval.Vlr    = W_SdoCta 
          TEval.Bda[7] = TEval.Bda[7] + W_SdoCta.


   CREATE TEval.
   ASSIGN TEval.Clas   = 1                    
          TEval.Reng   = 095                      
          TEval.Cpto   = "Contingentes Deudoras".

   RUN IConting_PromDiaAno.

   RUN HallarSdoMayor (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "81",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta). 

   ASSIGN TEval.Vlr    = W_SdoCta
          TEval.Bda[1] = VrContDeu WHEN VrContDeu LT W_SdoCta
          TEval.Bda[1] = W_SdoCta  WHEN W_SdoCta  LE VrContDeu
          Vlr0         = W_SdoCta - VrContDeu.

   IF Vlr0 GT VrContDeu THEN
      ASSIGN TEval.Bda[2] = VrContDeu
             Vlr0         = Vlr0 - VrContDeu.
   ELSE IF Vlr0 GT 0 THEN
      ASSIGN TEval.Bda[2] = Vlr0
             Vlr0         = 0.

   IF Vlr0 GT 0 THEN DO:
      IF Vlr0 GT VrContDeu THEN
         ASSIGN TEval.Bda[3] = VrContDeu
                Vlr0         = Vlr0 - VrContDeu.
      ELSE
         ASSIGN TEval.Bda[3] = Vlr0
                Vlr0         = 0.
   END.

   IF Vlr0 GT 0 THEN DO:
      IF Vlr0 GT (VrContDeu * 3) THEN
         ASSIGN TEval.Bda[4] = VrContDeu * 3
                Vlr0         = Vlr0 - (VrContDeu * 3).
      ELSE
         ASSIGN TEval.Bda[4] = Vlr0
                Vlr0         = 0.
   END.

   IF Vlr0 GT 0 THEN DO:
      IF Vlr0 GT (VrContDeu * 3) THEN
         ASSIGN TEval.Bda[5] = VrContDeu * 3
                Vlr0         = Vlr0 - (VrContDeu * 3).
      ELSE
         ASSIGN TEval.Bda[5] = Vlr0
                Vlr0         = 0.
   END.

   IF Vlr0 GT 0 THEN DO:
      IF Vlr0 GT (VrContDeu * 3) THEN
         ASSIGN TEval.Bda[6] = VrContDeu * 3
                Vlr0         = Vlr0 - (VrContDeu * 3).
      ELSE
         ASSIGN TEval.Bda[6] = Vlr0
                Vlr0         = 0.
   END.

   IF Vlr0 GT 0 THEN
      ASSIGN TEval.Bda[7] = Vlr0.

   RUN Imp_Evaluacion.

   SESSION:SET-WAIT-STATE(""). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Brecha_Liqui W-InfGer 
PROCEDURE Brecha_Liqui :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR Vlr0     LIKE Sal_Cuenta.Sal_Inicial.
  DEFI VAR TCar     LIKE Ahorros.Sdo_Dispon INIT 0 EXTENT 9 FORM "->>>>>>,>>>,>>>,>>9".
  DEFI VAR Arch     AS CHAR FORM "X(35)".
  DEFI VAR FecAnt   AS DATE.
  DEFI VAR K        AS INTEG FORM "99".
  DEFI VAR FPcaja   AS LOG INIT FALSE.
  DEFI VAR FecFinOk AS DATE.

  FOR EACH TBrecha:  DELETE TBrecha.  END.
  FOR EACH Movtempo: DELETE Movtempo. END.

  ASSIGN Arch     = "C:\InfRed\BrechaLiq-" + STRING(W_FecCorte,"999999") + "_" + W_Usuario
         FecFinOk = W_FecCorte.

  IF MONTH(W_FecCorte) EQ 2 THEN DO:
     IF DAY(W_FecCorte + 1) EQ 29 THEN
        FecFinOk = W_FecCorte + 1.
  END.
    
  FOR EACH Mov_Contable WHERE Mov_Contable.Fec_Contable GE DATE(MONTH(W_FecCorte),1,YEAR(W_FecCorte))
                          AND Mov_Contable.Fec_Contable LE FecFinOk NO-LOCK
                              BREAK BY Mov_Contable.Agencia
                                    BY Mov_Contable.Comprobante                
                                    BY Mov_Contable.Num_Documento
                                    BY Mov_Contable.Cuenta:            
          IF FIRST-OF(Mov_Contable.Num_Documento) AND Mov_contable.Cuenta = "11050501" THEN                         
             ASSIGN FPcaja = TRUE.                                          
                                                                               
          IF FPcaja AND Mov_Contable.Cuenta NE "11050501" THEN DO:
             CREATE Movtempo.                                                   
             BUFFER-COPY Mov_Contable TO Movtempo.              
          END.
                                                                               
          IF LAST-OF(Mov_Contable.Num_Documento) THEN   
             ASSIGN FPcaja = FALSE.
  END. /* FOR */                                                           

 /* DO K = 1 TO W_MesAct:*/
  CREATE TBrecha.                                                                
  ASSIGN TBrecha.Mes = W_MesAct.  /*K.*/                                            
  FOR EACH Movtempo:    /*WHERE MONTH(Movtempo.Fec_Contab) EQ K:*/                  
      IF Movtempo.Natur EQ "Cr" THEN DO:   /*Ingresos*/                             
         IF SUBSTRING(Movtempo.Cuenta,1,2) EQ "14"                                  
         OR SUBSTRING(Movtempo.Cuenta,1,4) EQ "1655"                                
         OR SUBSTRING(Movtempo.Cuenta,1,6) EQ "279505" THEN                         
            ASSIGN TBrecha.Scar[3] = TBrecha.Scar[3] + Movtempo.Valor.              
         ELSE IF SUBSTRING(Movtempo.Cuenta,1,2) EQ "16"  
              OR SUBSTRING(Movtempo.Cuenta,1,8) EQ "27055001" 
              OR SUBSTRING(Movtempo.Cuenta,1,4) EQ "3105"                           
              OR SUBSTRING(Movtempo.Cuenta,1,1) EQ "4" THEN                         
            ASSIGN TBrecha.Scar[5] = TBrecha.Scar[5] + Movtempo.Valor.              
         ELSE IF SUBSTRING(Movtempo.Cuenta,1,2) EQ "21" THEN                        
            ASSIGN TBrecha.Scar[1] = TBrecha.Scar[1] + Movtempo.Valor.              
      END.                                                                          
      ELSE DO:                            /*Egresos*/                               
         IF SUBSTRING(Movtempo.Cuenta,1,4) EQ "1655" THEN.                          
         ELSE IF SUBSTRING(Movtempo.Cuenta,1,2) EQ "16"   
              OR SUBSTRING(Movtempo.Cuenta,1,4) EQ "2435"
              OR SUBSTRING(Movtempo.Cuenta,1,4) EQ "2465"
              OR SUBSTRING(Movtempo.Cuenta,1,8) EQ "24959502"
              OR SUBSTRING(Movtempo.Cuenta,1,8) EQ "24959505"
              OR SUBSTRING(Movtempo.Cuenta,1,8) EQ "24959599"
              OR SUBSTRING(Movtempo.Cuenta,1,4) EQ "3105"                           
              OR SUBSTRING(Movtempo.Cuenta,1,1) EQ "5" THEN                         
                 ASSIGN TBrecha.Scar[6] = TBrecha.Scar[6] + Movtempo.Valor.         
         ELSE IF SUBSTRING(Movtempo.Cuenta,1,2) EQ "21" THEN                        
                 ASSIGN TBrecha.Scar[2] = TBrecha.Scar[2] + Movtempo.Valor.         
      END.                                                                          
  END.                                                                              
                                                                                 
  FOR EACH Creditos NO-LOCK WHERE (Creditos.Fec_Desemb     GE DATE(MONTH(W_FecCorte),1,YEAR(W_FecCorte)) /*K.*/    
                                   AND Creditos.Fec_Desemb LE FecFinOk)
                               OR  Creditos.Cod_Credito   EQ 570:              /*Rotativos*/
      IF (Creditos.Cod_Credito EQ 540 OR Creditos.Cod_Credito EQ 541) THEN NEXT.
      ASSIGN TBrecha.Scar[4] = TBrecha.Scar[4] + (Creditos.Monto - Creditos.Sdo_capital). /* Creditos.MontoNuevo */               
  END.                                                                              
  /*END.*/
  
  OUTPUT TO VALUE(Arch).

  DISPLAY "      " + W_NomEnt + "    B R E C H A   R E A L   D E   L I Q U I D E Z   A   "
          + STRING(W_FecCorte,"99/99/9999") FORM "X(120)"
         SKIP(2)
          STRING(W_AnoAct,"9999") + "  ENTRADAS POSICION    SALIDAS POSICION   ENTRADAS POSICION    SALIDAS POSICION" FORM "X(200)" SKIP
          "MES   PASIVA(DEPOSITOS)   PASIVA(DEPOSITOS)     ACTIVA(CARTERA)     ACTIVA(CARTERA)      ENTRADAS OTROS       SALIDAS OTROS      ENTRADAS TOTAL       SALIDAS TOTAL         BRECHA" SKIP
          "___ ___________________ ___________________ ___________________ ___________________ ___________________ ___________________ ___________________ ___________________ ___________________"
        WITH DOWN WIDTH 350 FRAME Enc NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  FOR EACH TBrecha BY TBrecha.Mes:
      ASSIGN TBrecha.SCar[7] = TBrecha.SCar[1] + TBrecha.SCar[3] + TBrecha.SCar[5] 
             TBrecha.SCar[8] = TBrecha.SCar[2] + TBrecha.SCar[4] + TBrecha.SCar[6]
             TBrecha.SCar[9] = TBrecha.SCar[7] - TBrecha.SCar[8].

      DO K = 1 TO 9:
         ASSIGN TCar[K] = TCar[K] + TBrecha.SCar[K].
      END.
             
      DISPLAY TBrecha.Mes  FORM "99"
              TBrecha.SCar SKIP
              /*"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"*/
              SKIP (0) WITH DOWN WIDTH 350 FRAME Detal NO-BOX NO-LABELS STREAM-IO USE-TEXT.
  END.

  /*DISPLAY SKIP(1)
          "ACUMULADO" SKIP
          "  "
          TCar SKIP
          "_______________________________________________________________________________________________________________________________________________________________________________________"
        WITH DOWN WIDTH 350 FRAME Totl NO-BOX NO-LABELS STREAM-IO USE-TEXT.*/

  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cartera_Consolidada W-InfGer 
PROCEDURE Cartera_Consolidada :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR Vlr0     LIKE Sal_Cuenta.Sal_INICIAL.
  DEFI VAR W_SdoCta LIKE Sal_Cuenta.Sal_INICIAL.
  DEFI VAR K        AS INTEG FORM "99".
  DEFI VAR TCar     LIKE Ahorros.Sdo_Disponible EXTENT 18 FORMAT "->>>>>>>>>>>>9" INIT 0.

  DEFI VAR Arch     AS CHAR FORM "X(35)".
    
  FOR EACH Tofi: DELETE Tofi. END.
    
  Arch = "C:\InfRed\ClaCar-" + STRING(W_FecCorte,"999999") + "_" + W_Usuario.
    
  FOR EACH Cuentas WHERE Cuentas.Cuenta GE "1401        "
                    AND  Cuentas.Cuenta LT "1489        " NO-LOCK 
                      BY Cuentas.Cuenta:
      CREATE Tofi.
      
      IF Cuentas.Tipo EQ 2 THEN
      FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK BY Agencias.Agencia:
          RUN HallarSdoMayor /* IN W_Manija */ (INPUT Agencias.Agencia, INPUT Agencias.Agencia, INPUT 0, INPUT 999,
                                          INPUT Cuentas.Cuenta, INPUT W_AnoAct, INPUT W_MesAct,
                                          INPUT Cuentas.Tipo, OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
          ASSIGN Tofi.Scar[Agencias.Agencia] = W_SdoCta.
      END.

      ASSIGN TOfi.Cta       = Cuentas.Cuenta
             Tofi.Clas      = Cuentas.Nombre
             Tofi.Scar [18] = Tofi.Scar [1]  + Tofi.Scar [2]  + Tofi.Scar [3]  + Tofi.Scar [4]  + Tofi.Scar [5]  +
                              Tofi.Scar [6]  + Tofi.Scar [7]  + Tofi.Scar [8]  + Tofi.Scar [9]  + Tofi.Scar [10] +
                              Tofi.Scar [11] + Tofi.Scar [12] + Tofi.Scar [13] + Tofi.Scar [14] + Tofi.Scar [15] +
                              Tofi.Scar [16] + Tofi.Scar [17].
      DO K = 1 TO 18:
         TCar [K] = TCar[K] + Tofi.Scar[K].
      END.
             
  END.

  OUTPUT TO VALUE(Arch).

  DISPLAY "     "  + W_NomEnt + "  C L A S I F I C A C I O N   D E   C A R T E R A   A   "
          + STRING(W_FecCorte,"99/99/9999") FORM "X(180)"
         SKIP(2)
          "DESCRIPCION                          MEDELLIN       APARTADO       MONTERIA         QUIBDO       RIONEGRO      SINCELEJO          TURBO        ISTMINA       CAUCASIA  PUERTO BERRIO       FRONTINO        NECOCLI         LORICA  SAN JUAN DE U      ALPUJARRA        SAHAGUN      ARBOLETES    CONSOLIDADO" SKIP
          "------------------------------ -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- --------------"
      WITH DOWN WIDTH 450 FRAME Enc NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  FOR EACH TOfi BY TOfi.Cta:
      IF SUBSTRING(TOfi.Cta,5,1) EQ " " THEN
         DISPLAY Tofi.Clas
                 SKIP WITH WIDTH 450 FRAME Ft1 NO-LABELS.

      IF Tofi.Scar[18] NE 0 THEN
         DISPLAY Tofi.Clas
              Tofi.Scar 
              SKIP (0) WITH DOWN WIDTH 450 FRAME Detal NO-BOX NO-LABELS STREAM-IO USE-TEXT.
  END.

  DISPLAY SKIP(1)
          "             TOTALES          "
          TCar WITH DOWN WIDTH 450 FRAME Totl NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cdats_Permanentes W-InfGer 
PROCEDURE Cdats_Permanentes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR W_RowIdCdat AS ROWID.

  ASSIGN W_RowIdCdat = ROWID(Ahorros).

  FOR EACH Ahorros WHERE Ahorros.Nit          EQ W_CedAho 
                    AND (Ahorros.Cod_Ahorro EQ 4 OR Ahorros.Cod_Ahorro EQ 5) NO-LOCK:
      IF  Ahorros.Estado  NE 1 
      AND ROWID(Ahorros)  NE W_RowIdCdat
      AND (FecAho - Ahorros.Fec_UltTran) GT 0 
      AND (FecAho - Ahorros.Fec_UltTran) LE 730 THEN DO:   /*730 dias para abrir el nuevo(FecAho)*/
          ASSIGN W_ProrRen = TRUE.
          LEAVE.
      END.
  END.

  FIND Ahorros WHERE ROWID(Ahorros) EQ W_RowIdCdat NO-LOCK NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxcInteres W-InfGer 
PROCEDURE CxcInteres :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFI VAR Vlr0     LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR W_SdoCta LIKE Sal_Cuenta.Sal_INICIAL.

   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "165506",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   IF W_SdoCta GE 0 THEN  
      ASSIGN TEval.Bda[1] = TEval.Bda[1] + ABS(W_SdoCta). 

    RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "165518",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   IF W_SdoCta GE 0 THEN  
      ASSIGN TEval.Bda[1] = TEval.Bda[1] + ABS(W_SdoCta). 

   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "165532",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   IF W_SdoCta GE 0 THEN  
      ASSIGN TEval.Bda[1] = TEval.Bda[1] + ABS(W_SdoCta). 

   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "165542",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   IF W_SdoCta GE 0 THEN  
      ASSIGN TEval.Bda[1] = TEval.Bda[1] + ABS(W_SdoCta). 

   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "165560",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   IF W_SdoCta GE 0 THEN  
      ASSIGN TEval.Bda[1] = TEval.Bda[1] + ABS(W_SdoCta). 

   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "1650",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   IF W_SdoCta GE 0 THEN  
      ASSIGN TEval.Bda[1] = TEval.Bda[1] + ABS(W_SdoCta). 

   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "165701",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   IF W_SdoCta GE 0 THEN  
      ASSIGN TEval.Bda[1] = TEval.Bda[1] + ABS(W_SdoCta). 

   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 999, INPUT 999, INPUT "169010",          
                                   INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   IF W_SdoCta GE 0 THEN  
      ASSIGN TEval.Bda[1] = TEval.Bda[1] + ABS(W_SdoCta). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Difer_PromedHist W-InfGer 
PROCEDURE Difer_PromedHist :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI INPUT PARAM Clase AS CHAR FORM "X(8)".
  
  DEFI VAR ANOMES  AS INTEG FORM "999999".
  DEFI VAR AMC     AS CHAR  FORM "999999".
  DEFI VAR ANOMESF AS INTEG FORM "999999".
  DEFI VAR AMCF    AS CHAR  FORM "999999".
  DEFI VAR Mes     AS INTEG FORM "99".
  DEFI VAR AMesC   AS CHAR  FORM "999999".
  DEFI VAR MesC    AS CHAR  FORM "99".

  DEFINE VAR WX_mes AS INTEGER.
  DEFINE VAR sale AS INTEGER.
  DEFINE VAR i AS INTEGER.
  DEFINE VAR W_prommes  AS DECIMAL.
  DEFINE VAR W_Faltabanda AS DECIMAL.
  DEFINE VAR W_Falta AS DECIMAL.
  DEFINE VAR nromes AS INTEGER.

  
  ASSIGN AMC    = STRING(YEAR(W_FecIniP),"9999") + STRING(MONTH(W_FecIniP),"99")
         AnoMes = INTEG(AMC)
         PrAvBda7 = 0
         PromBda  = 0.
         
  ASSIGN AMCF    = STRING(YEAR(W_FecFinP),"9999") + STRING(MONTH(W_FecFinP),"99")
         AnoMesF = INTEG(AMCF).
         
  FOR EACH T_PromMes WHERE T_PromMes.Ident  EQ Clase
                       AND T_PromMes.AnoMes GE AnoMes
                       AND T_PromMes.AnoMes LE AnoMesF NO-LOCK:
      ASSIGN AMesC = STRING(T_PromMes.AnoMes,"999999")
             MesC  = SUBSTRING(AMesC,5,2)
             Mes   = INTEG(MesC)
             PromBda[Mes] = T_PromMes.Vr_Promed.                                       
  END.
  Wx_mes = MONTH(W_FecFinP).
  /* modificado nelson y wmr 10/06/2010 */
  W_FALTA = Teval.Vlr - PromedDisp.
  IF W_falta LE 0 THEN W_falta = 0.
  
  IF Clase NE "Disponib" THEN DO:
      IF W_falta GT 0 THEN DO: /*  falta distribuir */
          DO i = 1 TO 6 :   /* Bandas a distribuir */
             /* HALLA PROMEDIO MES */
            DO sale = 1 TO 1:
             IF WX_MES = 12 THEN
                W_prommes    = W_prommes + PromBda[1].
             IF WX_MES = 11 THEN
                W_prommes    = W_prommes + PromBda[12].
             IF WX_MES = 10 THEN
                W_prommes    = W_prommes + PromBda[11].
             IF WX_MES = 9 THEN
                W_prommes    = W_prommes + PromBda[10].
             IF WX_MES = 8 THEN
                W_prommes    = W_prommes + PromBda[9].
             IF WX_MES = 7 THEN
                W_prommes    = W_prommes + PromBda[8].
             IF WX_MES = 6 THEN
                W_prommes    = W_prommes + PromBda[7].
             IF WX_MES = 5 THEN
                W_prommes    = W_prommes + PromBda[6].
             IF WX_MES = 4 THEN
                W_prommes    = W_prommes + PromBda[5].
             IF WX_MES = 3 THEN
                W_prommes    = W_prommes + PromBda[4].
             IF WX_MES = 2 THEN
                W_prommes    = W_prommes + PromBda[3].
             IF WX_MES = 1 THEN
                W_prommes    = W_prommes + PromBda[2].

             WX_mes = WX_mes - 1.
             IF i = 4 OR i = 5 OR i = 6  THEN do:
                 nromes = nromes + 1.
                 IF nromes = 4 THEN ASSIGN nromes = 1
                                           sale = 1.
             END.
            END.    
             W_Faltabanda = W_Faltabanda + (PromedDisp - W_prommes).
                 /* REPARTE EN BANDA */
                 IF W_faltabanda GT 0 THEN DO:
                     IF W_faltabanda GE W_falta THEN
                        ASSIGN W_faltabanda = W_falta
                               W_falta = 0.
                     ELSE 
                        W_falta = W_falta - W_faltabanda.
                     /* aca tengo W_faltabanda lo que maduro */
                     /* en W_falta : lo que queda para la siguiente banda */
                     PrAvBda7[i] = PrAvBda7[i] + W_faltabanda.
                 END.
          END.
      END.
  END.
  /* Calculo para el disponible */
  ELSE DO:
      IF W_falta GT 0 THEN DO:
          DO i = 1 TO 6 :   /* Bandas a distribuir */
              IF WX_mes EQ 12 THEN ASSIGN WX_mes = 0.
              FIND FIRST PromedioMes WHERE PromedioMes.PromMes EQ (WX_mes + 1) NO-LOCK NO-ERROR.
              IF AVAILABLE PromedioMes THEN ASSIGN W_prommes = PromedioMes.PromVal.
              ELSE ASSIGN W_prommes = 0.
              IF PromedDisp GT W_prommes THEN
                 ASSIGN PrAvBda7[i] = PrAvBda7[i] + 0.
              ELSE DO:
                 ASSIGN W_Faltabanda = W_prommes - PromedDisp.
                 IF W_Faltabanda LE W_falta THEN ASSIGN PrAvBda7[i] = PrAvBda7[i] + W_Faltabanda.
                 ELSE ASSIGN PrAvBda7[i] = PrAvBda7[i] + W_falta.
                 ASSIGN W_falta = W_falta - W_Faltabanda.
                 IF W_falta LT 0 THEN W_falta = 0.
              END.
              ASSIGN WX_mes = WX_mes + 1.              
          END.
      END.
  END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-InfGer  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-InfGer)
  THEN DELETE WIDGET W-InfGer.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dispon_PromDiaAno W-InfGer 
PROCEDURE Dispon_PromDiaAno :
/*------------------------------------------------------------------------------
  Purpose:  Promedio Dia/Año del Disponible (Caja y Bancos).   
  
------------------------------------------------------------------------------*/
   DEFI VAR Vlr0     LIKE Sal_Cuenta.Sal_Inicial.
   DEFI VAR W_SdoCta LIKE Sal_Cuenta.sal_Inicial.
   DEFI VAR TDisp    LIKE Ahorros.Sdo_Dispon INIT 0.
   DEFI VAR TFin     LIKE Ahorros.Sdo_Dispon INIT 0.
   DEFI VAR PorC     AS DEC FORM "-99999.99".
   DEFI VAR Arch     AS CHAR FORM "X(35)".
   DEFI VAR FecNN    AS DATE.
   DEFI VAR NN       AS INTEG FORM "99999999".
   DEFI VAR NFI      AS INTEG FORM "99999999".
   DEFI VAR NFF      AS INTEG FORM "99999999".

   DEFINE VARIABLE tSumaMes AS DECIMAL     NO-UNDO.

   FOR EACH TDiaA: DELETE TDiaA. END.
      
   Arch = "C:\InfRed\DispPromDA-" + STRING(W_FecCorte,"999999") + "_" + W_Usuario.

   OUTPUT TO VALUE(Arch).

   DISPLAY "   " + W_NomEnt + "   DISPONIBLE PROMEDIO DIA/AÑO DE "
          + STRING(W_FecIniP,"99/99/9999") + " HASTA " + STRING(W_FecFinP,"99/99/9999") FORM "X(120)"
         SKIP(1)
       WITH DOWN WIDTH 150 FRAME Enc NO-BOX NO-LABELS STREAM-IO USE-TEXT.

   ASSIGN FecNN = W_FecIniP - 1.

   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 0, INPUT 999, INPUT "1105",          
                                   INPUT YEAR(FecNN), INPUT MONTH(FecNN),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).   
   TDisp = W_SdoCta.
   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,                             
                                   INPUT 0, INPUT 999, INPUT "1110",              
                                   INPUT YEAR(FecNN), INPUT MONTH(FecNN),INPUT 1,        
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).    
   TDisp = TDisp + W_SdoCta.

   RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,                             
                                   INPUT 0, INPUT 999, INPUT "1115",              
                                   INPUT YEAR(FecNN), INPUT MONTH(FecNN),INPUT 1,        
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).    
   TDisp = TDisp + W_SdoCta.
    
   /* Nuevo wmr para controlar promedios acumulados */
   EMPTY TEMP-TABLE PromedioMes.
   DEFINE VARIABLE DiaMes AS INTEGER EXTENT 12    NO-UNDO.
   ASSIGN    diaMes[1]  = 31
             diaMes[2]  = 28
             diaMes[3]  = 31
             diaMes[4]  = 30
             diaMes[5]  = 31
             diaMes[6]  = 30
             diaMes[7]  = 31
             diaMes[8]  = 31
             diaMes[9]  = 30
             diaMes[10] = 31
             diaMes[11] = 30
             diaMes[12] = 31.
   /************************************************/
   FOR EACH Mov_Contable WHERE Mov_Contable.Cuenta       GE "1105000000"
                           AND Mov_Contable.Cuenta       LE "1115999999"
                           AND Mov_Contable.Fec_Contable GE W_FecIniP
                           AND Mov_Contable.Fec_Contable LE W_FecFinP NO-LOCK
            BREAK BY Mov_Contable.Fec_Contable BY MONTH(Mov_Contable.Fec_Contable):

           IF FIRST-OF(Mov_Contable.Fec_Contable) THEN DO:
              CREATE TDiaA.
              ASSIGN TDiaA.FecD     = Mov_Contable.Fec_Contable
                     TDiaA.TSdo     = TDisp.
              IF DAY(Mov_Contable.Fec_Contable) EQ 1 THEN ASSIGN tSumaMes = 0.
           END.
    
    
           IF Mov_Contable.Db GT 0 THEN
              ASSIGN TDiaA.TSdo = TDiaA.TSdo + Mov_Contable.Db.
           ELSE
              ASSIGN TDiaA.TSdo = TDiaA.TSdo - Mov_Contable.Cr.
            
           
           IF LAST-OF(Mov_Contable.Fec_Contable) THEN DO:
              ASSIGN tSumaMes       = tSumaMes + TDiaA.TSdo
                     TDiaA.Tacumes  = tSumaMes.
              /* nuevo promedio mes */
              IF DAY(fec_contable) EQ diaMes[MONTH(fec_contable)] THEN DO:
                  CREATE PromedioMes.
                  ASSIGN PromedioMes.PromMes = MONTH(fec_contable)
                         PromedioMes.PromVal = TDiaA.TAcumes / DAY(fec_contable).
              END.
              /* ****************** */
              DISPLAY TDiaA.FecD    LABEL "Fecha-Día"
                      TDiaA.TSdo    LABEL "Saldo del Día"
                      TDiaA.Tacumes LABEL "Acumulado "
                      (TDiaA.TAcumes / DAY(fec_contable)) LABEL "promedio dia mes "  FORMAT "->>>,>>>,>>>,>>>,>>>"
                  WITH DOWN WIDTH 120 FRAME Fd1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.
    
              ASSIGN TDisp = TDiaA.TSdo
                     TFin  = TFin + TDiaA.TSdo.
           END.

       
  END.
    
  DISPLAY SKIP(1)
          "               Total de Saldos Día  Tot./365 = Sdo.Promedio" SKIP                 
          "----------  ----------------------  -----------------------" SKIP
          "TOTAL       "
          TFin 
          "  "
          TFin / 365  FORM "->,>>>,>>>,>>>,>>9"
     WITH DOWN WIDTH 150 FRAME Totl NO-BOX NO-LABELS STREAM-IO USE-TEXT.
    
  OUTPUT CLOSE.

  PromedDisp = TFin / 365.

  FIND FIRST T_PromMes WHERE T_PromMes.Ident  EQ "Disponib"
                         AND T_PromMes.AnoMes EQ INTEG(STRING(YEAR(W_FecFinP),"9999") + 
                                                       STRING(MONTH(W_FecFinP),"99")) NO-ERROR.
  IF NOT AVAILABLE (T_PromMes) THEN
     CREATE T_PromMes.
  ASSIGN T_PromMes.Ident      = "Disponib"
         T_PromMes.AnoMes     = INTEG(STRING(YEAR(W_FecFinP),"9999") + STRING(MONTH(W_FecFinP),"99"))         
         T_PromMes.Nro_Regist = 365
         T_PromMes.Vr_Promed  = PromedDisp.
  
  RUN Difer_PromedHist (INPUT "Disponib").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ejec_Pptal W-InfGer 
PROCEDURE Ejec_Pptal :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
   DEFI VAR Vlr0      LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR W_SdoCta  LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR W_SdoCta2 LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR Tcar      LIKE Ahorros.Sdo_Dispon INIT 0 EXTENT 4 FORM "->>>>>>,>>>,>>>,>>9".
   DEFI VAR TFin      LIKE Ahorros.Sdo_Dispon INIT 0 EXTENT 4 FORM "->>>>>>,>>>,>>>,>>9".
   DEFI VAR PorC      AS DEC FORM "-99999.99".
   DEFI VAR Arch      AS CHAR FORM "X(35)".
   DEFI VAR FecAnt    AS DATE.
    
   FOR EACH Tof: DELETE Tof. END.
    
   ASSIGN Arch   = "C:\InfRed\EjPptal-" + STRING(W_FecCorte,"999999") + "_" + W_Usuario
          FecAnt = DATE(MONTH(W_FecCorte),DAY(W_FecCorte),YEAR(W_FecCorte) - 1).
              
   FOR EACH Cuentas WHERE SUBSTRING(Cuentas.Cuenta,1,1) GE "4" AND
                          SUBSTRING(Cuentas.Cuenta,1,1) LE "7"  NO-LOCK 
                          BY Cuentas.Cuenta:  
       IF Cuentas.Tipo EQ 2
       OR SUBSTRING(Cuentas.Cuenta,5,1) EQ " "  THEN.
       ELSE NEXT.
       
       RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                       INPUT 0, INPUT 999, INPUT Cuentas.Cuenta,   
                                       INPUT W_AnoAct, INPUT W_MesAct,INPUT Cuentas.Tipo,    
                                       OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
              
       RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                       INPUT 0, INPUT 999, INPUT Cuentas.Cuenta,   
                                       INPUT W_AnoAct - 1, INPUT W_MesAct,INPUT Cuentas.Tipo,    
                                       OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta2).
       IF W_SdoCta NE 0 OR W_SdoCta2 NE 0 THEN DO:
          CREATE Tof.                      
          ASSIGN TOf.Cta     = Cuentas.Cuenta  
                 TOf.Nom     = Cuentas.Nombre
                 Tof.Sdos[1] = W_SdoCta
                 Tof.Sdos[2] = W_SdoCta2
                 Tof.Sdos[3] = Tof.Sdos[1]  - Tof.Sdos[2]
                 Tof.Sdos[4] = ((Tof.Sdos[1] / Tof.Sdos[2]) - 1) * 100. 
       END.
  END.

  OUTPUT TO VALUE(Arch). 
    
  DISPLAY "          " + W_NomEnt + "     E J E C U C I O N   P R E S U P U E S T A L   A   "
          + STRING(W_FecCorte,"99/99/9999") FORM "X(120)"
         SKIP(1)
          "          R U B R O S                    EJECUTADO           EJECUTADO           VARIACION   VARIACION" SKIP
          "                                        " +
          STRING(W_FecCorte,"99/99/9999") + "          " + STRING(FecAnt,"99/99/9999") + 
          "            ABSOLUTA    RELATIVA"  FORM "X(120)" SKIP
          "______________________________ ___________________ ___________________ ___________________ ___________"
        WITH DOWN WIDTH 150 FRAME Enc NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  FOR EACH TOf BREAK BY SUBSTRING(TOf.Cta,1,1) BY SUBSTRING(TOf.Cta,1,2) BY TOf.Cta:
      DISPLAY Tof.Nom
              Tof.Sdos [1]
              Tof.Sdos [2]
              Tof.Sdos [3]
              Tof.Sdos [4]        FORM "-99999.99 %" SKIP(0)
              /*"------------------------------------------------------------------------------------------------"*/
            WITH DOWN WIDTH 150 FRAME Detal NO-BOX NO-LABELS STREAM-IO USE-TEXT.

      IF FIRST-OF(SUBSTRING(TOf.Cta,1,1)) THEN DO:
         DISPLAY SKIP(0)
                 "------------------------------" 
                 WITH WIDTH 100 FRAME FNxx NO-LABELS.

         IF SUBSTRING(TOf.Cta,1,1) EQ "4" THEN
            ASSIGN TCar[1] = Tof.Sdos [1]
                   TCar[2] = Tof.Sdos [2].
         ELSE 
            ASSIGN TCar[1] = TCar[1] - Tof.Sdos [1]
                   TCar[2] = TCar[2] - Tof.Sdos [2].
      END.

      IF LAST-OF(SUBSTRING(TOf.Cta,1,2)) THEN 
         DISPLAY SKIP(1)
               WITH FRAME FN1xxx NO-LABELS.  

      IF LAST-OF(SUBSTRING(TOf.Cta,1,1)) THEN 
         DISPLAY SKIP
               WITH FRAME FNxxx NO-LABELS.      

  END.

  ASSIGN TCar[3] = TCar[1] - TCar[2]
         TCar[4] = ((TCar[1] / TCar[2]) - 1) * 100.

  DISPLAY SKIP(1)
          "______________________________________________________________________________________________________" SKIP
          "EXCEDENTES DEL EJERCICIO      "
          TCar [1]                        
          TCar [2]                        
          TCar [3]                        
          TCar [4]        FORM "-99999.99 %" SKIP
          "______________________________________________________________________________________________________"
          WITH DOWN WIDTH 150 FRAME Totl NO-BOX NO-LABELS STREAM-IO USE-TEXT.
    
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-InfGer  _DEFAULT-ENABLE
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
  DISPLAY W_FecCorte Cmb_Inf W_FecIniP W_FecFinP W_PorcVta W_TasInv W_TasFdo 
          W_TasAhV W_Tasreno PorcAsamb W_Bda1 W_Bda2 W_Bda3 W_Bda4 W_Bda5 W_Bda6 
          W_Bda7 
      WITH FRAME F_Inf IN WINDOW W-InfGer.
  ENABLE RECT-11 RECT-4 RECT-5 RECT-6 W_FecCorte Cmb_Inf W_FecIniP W_FecFinP 
         W_PorcVta Btn_Proc W_TasInv W_TasFdo Btn_ImpProm W_TasAhV W_Tasreno 
         PorcAsamb W_Bda1 W_Bda2 W_Bda3 W_Bda4 W_Bda5 W_Bda6 W_Bda7 
         Btn_EvalRLiq 
      WITH FRAME F_Inf IN WINDOW W-InfGer.
  {&OPEN-BROWSERS-IN-QUERY-F_Inf}
  VIEW W-InfGer.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Exced_Acum W-InfGer 
PROCEDURE Exced_Acum :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
   DEFI VAR Vlr0     LIKE Sal_Cuenta.Sal_Inicial.
   DEFI VAR W_SdoCta LIKE Sal_Cuenta.Sal_Inicial.
   DEFI VAR Tcar     LIKE Ahorros.Sdo_Dispon INIT 0 EXTENT 4 FORM "->>>>>>,>>>,>>>,>>9".
   DEFI VAR TFin     LIKE Ahorros.Sdo_Dispon INIT 0 EXTENT 4 FORM "->>>>>>,>>>,>>>,>>9".
   DEFI VAR PorC     AS DEC FORM "-99999.99".
   DEFI VAR Arch     AS CHAR FORM "X(35)".
   DEFI VAR FecAnt   AS DATE.
    
   FOR EACH Tof: DELETE Tof. END.
    
   ASSIGN Arch   = "C:\InfRed\ExcAcum-" + STRING(W_FecCorte,"999999") + "_" + W_Usuario
          FecAnt = DATE(MONTH(W_FecCorte),DAY(W_FecCorte),YEAR(W_FecCorte) - 1).
    
   FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK BY Agencias.Agencia:
       CREATE Tof.
       ASSIGN TOf.Ofi = Agencias.Agencia
              TOf.Nom = Agencias.Nombre.
        
       RUN HallarSdoMayor  /* IN W_Manija */ (INPUT Agencias.Agencia, INPUT Agencias.Agencia,
                                       INPUT 0, INPUT 999, INPUT "4",   
                                       INPUT W_AnoAct, INPUT W_MesAct,INPUT 1,    
                                       OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
       ASSIGN Tof.Sdos[1] = W_SdoCta.

       RUN HallarSdoMayor  /* IN W_Manija */ (INPUT Agencias.Agencia, INPUT Agencias.Agencia,
                                       INPUT 0, INPUT 999, INPUT "4",   
                                       INPUT W_AnoAct - 1, INPUT W_MesAct,INPUT 1,    
                                       OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).

       ASSIGN Tof.Sdos[2] = W_SdoCta.

       RUN HallarSdoMayor  /* IN W_Manija */ (INPUT Agencias.Agencia, INPUT Agencias.Agencia,
                                       INPUT 0, INPUT 999, INPUT "5",   
                                       INPUT W_AnoAct, INPUT W_MesAct,INPUT 1,    
                                       OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
       ASSIGN Tof.Sdos[1] = Tof.Sdos[1] - W_SdoCta.

       RUN HallarSdoMayor  /* IN W_Manija */ (INPUT Agencias.Agencia, INPUT Agencias.Agencia,
                                       INPUT 0, INPUT 999, INPUT "5",   
                                       INPUT W_AnoAct - 1, INPUT W_MesAct,INPUT 1,    
                                       OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
       
       ASSIGN Tof.Sdos[2] = Tof.Sdos[2] - W_SdoCta.

       IF W_MesAct EQ 12 THEN DO:
          RUN HallarSdoMayor  /* IN W_Manija */ (INPUT Agencias.Agencia, INPUT Agencias.Agencia,
                                          INPUT 0, INPUT 999, INPUT "59",   
                                          INPUT W_AnoAct, INPUT W_MesAct,INPUT 1,    
                                          OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
          ASSIGN Tof.Sdos[1] = Tof.Sdos[1] + W_SdoCta.

          RUN HallarSdoMayor  /* IN W_Manija */ (INPUT Agencias.Agencia, INPUT Agencias.Agencia,
                                          INPUT 0, INPUT 999, INPUT "59",   
                                          INPUT W_AnoAct - 1, INPUT W_MesAct,INPUT 1,    
                                          OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
          ASSIGN Tof.Sdos[2] = Tof.Sdos[2] + W_SdoCta.
       END.

       RUN HallarSdoMayor  /* IN W_Manija */ (INPUT Agencias.Agencia, INPUT Agencias.Agencia,
                                       INPUT 0, INPUT 999, INPUT "6",   
                                       INPUT W_AnoAct, INPUT W_MesAct,INPUT 1,    
                                       OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
       ASSIGN Tof.Sdos[1] = Tof.Sdos[1] - W_SdoCta.

       RUN HallarSdoMayor  /* IN W_Manija */ (INPUT Agencias.Agencia, INPUT Agencias.Agencia,
                                       INPUT 0, INPUT 999, INPUT "6",   
                                       INPUT W_AnoAct - 1, INPUT W_MesAct,INPUT 1,    
                                       OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
       ASSIGN Tof.Sdos[2] = Tof.Sdos[2] - W_SdoCta.

       ASSIGN Tof.Sdos[3] = ((Tof.Sdos[1] - Tof.Sdos[2]) / Tof.Sdos[2]) * 100
              TCar[1]     = TCar[1] + Tof.Sdos[1]
              TCar[2]     = TCar[2] + Tof.Sdos[2].
              
  END.

  ASSIGN TCar[3] = ((TCar[1] - TCar[2]) / TCar[2]) * 100.

  FOR EACH TOf:
      IF Tof.Sdos[3] EQ ? THEN
         Tof.Sdos[3] = 0.
  END.
  OUTPUT TO VALUE(Arch). 
    
  DISPLAY "  " + W_NomEnt + "  E X C E D E N T E S - A C U M U L A D O S  A  "
          + STRING(W_FecCorte,"99/99/9999") FORM "X(120)"
         SKIP(2)
          "SUCURSAL                    " + STRING(W_FecCorte,"99/99/9999") + "          " +
                                        STRING(FecAnt,"99/99/9999") + "   % VAR"  FORM "X(120)" SKIP
          "__________________ ___________________ ___________________ ___________" SKIP(1)
        WITH DOWN WIDTH 150 FRAME Enc NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  FOR EACH TOf BY Tof.Sdos [3] DESCEN:
      DISPLAY Tof.Nom  FORM "X(18)"
              Tof.Sdos [1]
              Tof.Sdos [2]
              Tof.Sdos [3] FORM "->>9.99 %" SKIP
              "__________________ ___________________ ___________________ ___________"
             
          SKIP (0) WITH DOWN WIDTH 150 FRAME Detal NO-BOX NO-LABELS STREAM-IO USE-TEXT.
  END.

  DISPLAY SKIP(1)
          "       TOTALES    "
          TCar [1]
          TCar [2]
          TCar [3] FORM "->>9.99 %" SKIP
          /*"_______________________________________________________________________"*/
          SKIP(2)
       WITH DOWN WIDTH 150 FRAME Totl NO-BOX NO-LABELS STREAM-IO USE-TEXT.
    
  OUTPUT CLOSE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Fdo_Liqui W-InfGer 
PROCEDURE Fdo_Liqui :
DEFI VAR Vlr0 LIKE Sal_Cuenta.Sal_INICIAL.
DEFI VAR W_SdoCta LIKE Sal_Cuenta.Sal_INICIAL.
DEFI VAR Tcar LIKE Ahorros.Sdo_Dispon INIT 0 EXTENT 4 FORM "->>>>>>,>>>,>>>,>>9".
DEFI VAR TFin LIKE Ahorros.Sdo_Dispon INIT 0 EXTENT 4 FORM "->>>>>>,>>>,>>>,>>9".
DEFI VAR PorC AS DEC FORM "-99999.99".
DEFI VAR Arch AS CHAR FORM "X(35)".

EMPTY TEMP-TABLE TOf.

Arch = "C:\Info_Fodun\FondoLiquidez" + STRING(W_FecCorte,"99999999") + "_" + W_Usuario.

FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK BY Agencias.Agencia:
    CREATE Tof.
    ASSIGN TOf.Ofi = Agencias.Agencia
           TOf.Nom = Agencias.Nombre.

    FOR EACH Cuentas WHERE (SUBSTRING(Cuentas.Cuenta,1,4) EQ "1120" OR
                            SUBSTRING(Cuentas.Cuenta,1,4) EQ "1203" OR
                            SUBSTRING(Cuentas.Cuenta,1,2) EQ "21")
                       AND Cuentas.Tipo EQ 2 NO-LOCK BY Cuentas.Cuenta:
        RUN HallarSdoMayor (INPUT Agencias.Agencia,
                            INPUT Agencias.Agencia,
                            INPUT 0,
                            INPUT 999,
                            INPUT Cuentas.Cuenta,
                            INPUT W_AnoAct,
                            INPUT W_MesAct,
                            INPUT Cuentas.Tipo,
                            OUTPUT Vlr0,
                            OUTPUT Vlr0,
                            OUTPUT W_SdoCta).

        IF SUBSTRING(Cuentas.Cuenta,1,4) EQ "1120" OR SUBSTRING(Cuentas.Cuenta,1,4) EQ "1203" THEN
            Tof.Sdos[3] = Tof.Sdos[3] + W_SdoCta.
        ELSE
            Tof.Sdos[1] = Tof.Sdos[1] + W_SdoCta.
    END.

    ASSIGN Tof.Sdos[2] = Tof.Sdos[1] / 10
           Tof.Sdos[4] = Tof.Sdos[3] - Tof.Sdos[2]
           Tcar[1] = Tcar [1] + Tof.Sdos[1]
           Tcar[2] = Tcar [2] + Tof.Sdos[2]
           Tcar[3] = Tcar [3] + Tof.Sdos[3]
           Tcar[4] = Tcar [4] + Tof.Sdos[4].
END.

ASSIGN TFin[1] = TCar[1]
       TFin[2] = TCar[2]
       TFin[3] = TCar[3]
       TFin[4] = TCar[4]
       PorC = ROUND((TFin[3] / TFin[1] * 100),2).

OUTPUT TO VALUE(Arch).

DISPLAY "  " + W_NomEnt +  "  F O N D O   D E   L I Q U I D E Z   A   " + STRING(W_FecCorte,"99/99/9999") FORM "X(120)" SKIP(2)
        "SUCURSAL            Depósitos Asociados    Fondo de Liquidez    Fondo de Liquidez           Diferencia" SKIP
        "                         Cuenta 21         Decreto 2886/2001   Cuenta 1120 + 1203" SKIP
        "__________________  ___________________  ___________________  ___________________  ___________________"
    WITH DOWN WIDTH 150 FRAME Enc NO-BOX NO-LABELS STREAM-IO USE-TEXT.

FOR EACH TOf BY TOf.Ofi:
    DISPLAY Tof.Nom  FORM "X(18)"
            Tof.Sdos SKIP
            "------------------------------------------------------------------------------------------------------" SKIP (0)
        WITH DOWN WIDTH 150 FRAME Detal NO-BOX NO-LABELS STREAM-IO USE-TEXT.
END.

DISPLAY SKIP(1)
        "       TOTALES    "
        TCar SKIP
        "______________________________________________________________________________________________________" SKIP(2)
        "Valor Fondo de Liquidez en Bancos                        "
        TFin[3] SKIP
        "10% del Total Saldos en Depósitos                        "
        TFin[2] SKIP
        "Diferencia                                               "
        TFin[4] SKIP(1)
        "El Fondo de Liquidez está en                             "
        PorC FORM "-99.99%"
    WITH DOWN WIDTH 150 FRAME Totl NO-BOX NO-LABELS STREAM-IO USE-TEXT.

OUTPUT CLOSE.

ASSIGN VrFdoLiq = TFin[3].

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Flu_Caja W-InfGer 
PROCEDURE Flu_Caja :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR FPcaja      AS LOGICAL INIT FALSE.
  DEFI VAR Vlr0        LIKE Sal_Cuenta.Sal_INICIAL.
  DEFI VAR W_SdoCta    LIKE Sal_Cuenta.Sal_INICIAL.
  DEFI VAR W_AnoIni    AS INTEG FORM "9999".
  DEFI VAR W_MesIni    AS INTEG FORM "99".
  DEFI VAR TCar        LIKE Ahorros.Sdo_Disponible EXTENT 18 FORMAT "->>>>>>>>>>>>9" INIT 0.
  DEFI VAR TTCar       LIKE Ahorros.Sdo_Disponible EXTENT 18 FORMAT "->>>>>>>>>>>>9" INIT 0.
  DEFI VAR K           AS INTEG FORM "99".
  DEFI VAR Arch        AS CHAR FORM "X(35)".
  DEFI VAR FecFinOk    AS DATE.

  FOR EACH W_Movtempo: DELETE W_Movtempo. END.
  FOR EACH Tofi:       DELETE Tofi.       END.

  Arch = "C:\InfRed\FluCaj-" + STRING(W_FecCorte,"999999") + "_" + W_Usuario.
    
  ASSIGN FecFinOk = W_FecCorte
         W_AnoIni = W_AnoAct
         W_MesIni = W_MesAct - 1.

  IF W_MesAct - 1 EQ 0 THEN
     ASSIGN W_AnoIni = W_AnoAct - 1
            W_MesIni = 12.

  IF MONTH(W_FecCorte) EQ 2 THEN DO:
     IF DAY(W_FecCorte + 1) EQ 29 THEN
        FecFinOk = W_FecCorte + 1.
  END.

  CREATE TOfi.
  FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK BY Agencias.Agencia:
      RUN HallarSdoMayor  /* IN W_Manija */ (INPUT Agencias.Agencia,INPUT Agencias.Agencia,INPUT 0,INPUT 999,
                                      INPUT "11050501",INPUT W_AnoIni,INPUT W_MesIni,INPUT 1,
                                      OUTPUT Vlr0,OUTPUT Vlr0,OUTPUT W_SdoCta).
      ASSIGN Tofi.Tip                    = 1
             Tofi.Cta                    = "   "
             Tofi.Clas                   = "    SALDO INICIAL"
             Tofi.SCar[Agencias.Agencia] = W_SdoCta.
  END.
  
  CREATE TOfi.
  FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK BY Agencias.Agencia:
      RUN HallarSdoMayor  /* IN W_Manija */ (INPUT Agencias.Agencia,INPUT Agencias.Agencia,INPUT 0,INPUT 999,
                                      INPUT "11050501",INPUT W_AnoAct,INPUT W_MesAct,INPUT 1,
                                      OUTPUT Vlr0,OUTPUT Vlr0,OUTPUT W_SdoCta).
      ASSIGN Tofi.Tip                    = 2
             Tofi.Cta                    = "999"
             Tofi.Clas                   = "    SALDO FINAL"
             Tofi.SCar[Agencias.Agencia] = W_SdoCta.
  END.

  FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK BY Agencias.Agencia:
      FOR EACH Mov_Contable WHERE Mov_Contable.Fec_Contable  GE DATE(MONTH(W_FecCorte),1,YEAR(W_FecCorte))
                              AND Mov_Contable.Fec_Contable  LE FecFinOk
                              AND Mov_Contable.Agencia       EQ Agencias.Agencia  NO-LOCK
                              BREAK BY Mov_Contable.Comprobante                
                                    BY Mov_Contable.Num_Documento:             
          IF FIRST-OF(Mov_Contable.Num_Documento) THEN                         
             ASSIGN FPcaja = FALSE.                                          
                                                                               
          IF Mov_contable.Cuenta = "11050501" THEN                             
             ASSIGN FPcaja = TRUE.                                             
                                                                               
          CREATE W_Movtempo.                                                   
          BUFFER-COPY Mov_Contable TO W_Movtempo.              
                                                                               
          IF LAST-OF(Mov_Contable.Num_Documento) THEN DO:                      
             IF NOT FPCaja THEN DO:                                        
                FOR EACH W_Movtempo WHERE W_Movtempo.Comprobante   = Mov_Contable.Comprobante  
                                      AND W_Movtempo.Num_documento = Mov_Contable.Num_Documento
                                      AND W_Movtempo.Agencia       = Agencias.Agencia:    
                    DELETE W_Movtempo.                                         
                END.                                                           
             END.                                                              
          END.                                                                 
                                                                               
      END. /* FOR */                                                           
  END.

  /*Ingresos*/
  FOR EACH W_Movtempo WHERE W_Movtempo.Cuenta NE "11050501"
                    /*  AND W_Movtempo.Natur  EQ "Cr"   */
                        AND W_Movtempo.Cr  GT 0
                   BREAK BY SUBSTRING(W_Movtempo.Cuenta,1,6) BY SUBSTRING(W_Movtempo.Cuenta,1,8)
                         BY W_Movtempo.Agencia:
      IF FIRST-OF(SUBSTRING(W_Movtempo.Cuenta,1,8))
      AND SUBSTRING(W_Movtempo.Cuenta,1,6) EQ "249595" THEN DO:
          FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_Movtempo.Cuenta NO-LOCK NO-ERROR.
          CREATE TOfi.
          ASSIGN Tofi.Tip  = 1
                 TOfi.Cta  = SUBSTRING(W_Movtempo.Cuenta,1,6)
                 Tofi.Clas = Cuentas.Nombre WHEN AVAIL Cuentas.
      END.
      ELSE IF  FIRST-OF(SUBSTRING(W_Movtempo.Cuenta,1,6))
      AND SUBSTRING(W_Movtempo.Cuenta,1,6) NE "249595" THEN DO:
         FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ SUBSTRING(W_Movtempo.Cuenta,1,6) NO-LOCK NO-ERROR.
         CREATE TOfi.
         ASSIGN Tofi.Tip  = 1
                TOfi.Cta  = SUBSTRING(W_Movtempo.Cuenta,1,6)
                Tofi.Clas = Cuentas.Nombre WHEN AVAIL Cuentas.
      END.
      
      Tofi.SCar[W_Movtempo.Agencia] = Tofi.SCar[W_Movtempo.Agencia] + W_Movtempo.Cr.
  END.

  /*Egresos*/
  FOR EACH W_Movtempo WHERE W_Movtempo.Cuenta NE "11050501"
/*                         AND W_Movtempo.Natur  EQ "Db"  */
                        AND W_Movtempo.Db  GT 0
                   BREAK BY SUBSTRING(W_Movtempo.Cuenta,1,6) BY SUBSTRING(W_Movtempo.Cuenta,1,8)
                         BY W_Movtempo.Agencia:
      IF  FIRST-OF(SUBSTRING(W_Movtempo.Cuenta,1,8))           
      AND SUBSTRING(W_Movtempo.Cuenta,1,6) EQ "249595" THEN DO:
          FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_Movtempo.Cuenta NO-LOCK NO-ERROR.
          CREATE TOfi.                                         
          ASSIGN Tofi.Tip  = 2                                 
                 TOfi.Cta  = SUBSTRING(W_Movtempo.Cuenta,1,6)  
                 Tofi.Clas = Cuentas.Nombre WHEN AVAIL Cuentas.
      END.                                            
      ELSE IF  FIRST-OF(SUBSTRING(W_Movtempo.Cuenta,1,6)) 
      AND SUBSTRING(W_Movtempo.Cuenta,1,6) NE "249595" THEN DO:
         FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ SUBSTRING(W_Movtempo.Cuenta,1,6) NO-LOCK NO-ERROR.
         CREATE TOfi.
         ASSIGN Tofi.Tip  = 2
                TOfi.Cta  = SUBSTRING(W_Movtempo.Cuenta,1,6)
                Tofi.Clas = Cuentas.Nombre WHEN AVAIL Cuentas.
      END.
              
      Tofi.SCar[W_Movtempo.Agencia] = Tofi.SCar[W_Movtempo.Agencia] + W_Movtempo.Db.
  END.

  OUTPUT TO VALUE(Arch).

  DISPLAY "  " +  W_NomEnt + "  I N G R E S O S   Y   E G R E S O S   D E   C A J A   A   "
          + STRING(W_FecCorte,"99/99/9999") FORM "X(180)"
         SKIP(2)
          "I N G R E S O S                      MEDELLIN       APARTADO       MONTERIA         QUIBDO       RIONEGRO      SINCELEJO          TURBO        ISTMINA       CAUCASIA  PUERTO BERRIO       FRONTINO        NECOCLI         LORICA  SAN JUAN DE U      ALPUJARRA        SAHAGUN      ARBOLETES    CONSOLIDADO" SKIP
          "------------------------------ -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- --------------"
     WITH DOWN WIDTH 500 FRAME Enc NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  FOR EACH TOfi BREAK BY TOfi.Tip BY TOfi.Cta:
      DO K = 1 TO 17:
         ASSIGN Tofi.Scar [18] = Tofi.Scar [18] + Tofi.Scar[K].
      END.

      DISPLAY SUBSTRING(TOfi.Cta,1,6) FORM "X(6)"
              Tofi.Clas  FORM "X(23)"
              Tofi.Scar 
              SKIP (0) WITH DOWN WIDTH 450 FRAME Detal NO-BOX NO-LABELS STREAM-IO USE-TEXT.

      DO K = 1 TO 18:
         ASSIGN TCar [K] = TCar[K]  + Tofi.Scar[K].

         IF TOfi.Tip EQ 1 THEN
            TTCar[K] = TTCar[K] + Tofi.Scar[K].
         ELSE
            TTCar[K] = TTCar[K] - Tofi.Scar[K].
      END.

      IF LAST-OF(TOfi.Tip) THEN DO:
         DISPLAY "------------------------------ -------------- --------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- --------------"
                 SKIP
                 "S U B T O T A L "    FORM "X(30)"
                 TCar
                 SKIP(2)
             WITH DOWN WIDTH 450 FRAME Totl NO-BOX NO-LABELS STREAM-IO USE-TEXT.
         TCar = 0.

         IF TOfi.Tip EQ 1 THEN
            DISPLAY "E G R E S O S" SKIP
                    "-------------" SKIP(0)
              WITH DOWN WIDTH 450 FRAME Tot22 NO-BOX NO-LABELS STREAM-IO USE-TEXT.
      END.
  END.

  DISPLAY SKIP(1)
          "------------------------------ -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- -------------- --------------  -------------- -------------- -------------- --------------"
          SKIP
          "      D I F E R E N C I A"  FORM "X(30)"
          TTCar
        WITH DOWN WIDTH 450 FRAME Tot33 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GtoGral_Acum W-InfGer 
PROCEDURE GtoGral_Acum :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
   DEFI VAR Vlr0     LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR VlrMDb   LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR VlrMCr   LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR W_SdoCta LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR TCar     LIKE Ahorros.Sdo_Dispon INIT 0 EXTENT 4 FORM "->>>>>>,>>>,>>>,>>9".   
   DEFI VAR TPorC    AS DEC FORM "->>>9.99 %"  INIT 0.
   DEFI VAR Arch     AS CHAR FORM "X(35)".
   DEFI VAR FecAnt   AS DATE.
   DEFI VAR K        AS INTEG FORM "99".
   DEFI VAR J        AS INTEG FORM "99".
   DEFI VAR MesIni   AS INTEG FORM "99".
   DEFI VAR MesFin   AS INTEG FORM "99".
   DEFI VAR W_Titul  AS CHAR FORM "X(100)" INIT "ENERO             FEBRERO               MARZO         FEB-MAR".
    
   FOR EACH TGtoOf: DELETE TGtoOf. END.
    
   ASSIGN Arch   = "C:\InfRed\GtoGralAcum-" + STRING(W_FecCorte,"999999") + "_" + W_Usuario
          FecAnt = DATE(MONTH(W_FecCorte),DAY(W_FecCorte),YEAR(W_FecCorte) - 1)
          MesFin = MONTH(W_FecCorte).

   IF MesFin GE 3 THEN
      MesIni = MesFin - 2.
   ELSE IF MesFin EQ 2 THEN
      MesIni = 12.
   ELSE IF MesFin EQ 1 THEN
      MesIni = 11.

   IF MesIni = 2 THEN
      W_Titul = "FEBRERO               MARZO               ABRIL       MAR-ABR".
   ELSE IF MesIni = 3 THEN
      W_Titul = "MARZO               ABRIL                MAYO         ABR-MAY".
   ELSE IF MesIni = 4 THEN
      W_Titul = "ABRIL                MAYO               JUNIO         MAY-JUN".
   ELSE IF MesIni = 5 THEN
      W_Titul = "MAYO               JUNIO               JULIO          JUN-JUL".
   ELSE IF MesIni = 6 THEN
      W_Titul = "JUNIO               JULIO              AGOSTO         JUL-AGO".
   ELSE IF MesIni = 7 THEN
      W_Titul = "JULIO              AGOSTO           SEPTIEMBRE        AGO-SEP".
   ELSE IF MesIni = 8 THEN
      W_Titul = "AGOSTO              SEPTIEMBRE            OCTUBRE     SEP-OCT".
   ELSE IF MesIni = 9 THEN
      W_Titul = "SEPTIEMBRE              OCTUBRE           NOVIEMBRE   OCT-NOV".
   ELSE IF MesIni = 10 THEN
      W_Titul = "OCTUBRE              NOVIEMBRE            DICIEMBRE   NOV-DIC".
   ELSE IF MesIni = 11 THEN
      W_Titul = "NOVIEMBRE            DICIEMBRE               ENERO    DIC-ENE".
   ELSE IF MesIni = 12 THEN
      W_Titul = "DICIEMBRE            ENERO              FEBRERO       ENE-FEB".

   FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK BY Agencias.Agencia:
       CREATE TGtoOf.
       ASSIGN TGtoOf.Ofi = Agencias.Agencia
              TGtoOf.Nom = Agencias.Nombre.

       IF MesIni = 11 OR MesIni = 12 THEN DO: /*Año anterior y actual*/
          RUN HallarSdoMayor  /* IN W_Manija */ (INPUT Agencias.Agencia, INPUT Agencias.Agencia,
                                          INPUT 0, INPUT 999, INPUT "5110",   
                                          INPUT W_AnoAct - 1,INPUT MesIni,INPUT 1,  /*Novbre o Diciembre*/
                                          OUTPUT VlrMDb, OUTPUT VlrMCr, OUTPUT Vlr0).
          TGtoOf.Sdos[1] = VlrMDb - VlrMCr.

          IF MesIni = 11 THEN DO:
             RUN HallarSdoMayor  /* IN W_Manija */ (INPUT Agencias.Agencia, INPUT Agencias.Agencia,
                                             INPUT 0, INPUT 999, INPUT "5110",   
                                             INPUT W_AnoAct - 1,INPUT 12,INPUT 1,    /*Diciembre*/
                                             OUTPUT VlrMDb, OUTPUT VlrMCr, OUTPUT Vlr0).
             TGtoOf.Sdos[2] = VlrMDb - VlrMCr.
          END.

          RUN HallarSdoMayor  /* IN W_Manija */ (INPUT Agencias.Agencia, INPUT Agencias.Agencia,
                                          INPUT 0, INPUT 999, INPUT "5110",   
                                          INPUT W_AnoAct ,INPUT 1,INPUT 1,          /*Enero*/
                                          OUTPUT VlrMDb, OUTPUT VlrMCr, OUTPUT TGtoOf.Sdos[4]).
          IF MesIni = 11 THEN
             ASSIGN TGtoOf.Sdos[3] = VlrMDb - VlrMCr.
          ELSE DO:
             TGtoOf.Sdos[2] = VlrMDb - VlrMCr.

             RUN HallarSdoMayor  /* IN W_Manija */ (INPUT Agencias.Agencia, INPUT Agencias.Agencia,
                                             INPUT 0, INPUT 999, INPUT "5110",   
                                             INPUT W_AnoAct ,INPUT 2,INPUT 1,          /*Febrero*/
                                             OUTPUT VlrMDb, OUTPUT VlrMCr, OUTPUT TGtoOf.Sdos[4]).
             ASSIGN TGtoOf.Sdos[3] = VlrMDb - VlrMCr.
          END.

       END.
       ELSE DO: /*Solo año actual*/
          J = 1.
          DO K = MesIni TO MesFin BY 1:
             RUN HallarSdoMayor  /* IN W_Manija */ (INPUT Agencias.Agencia, INPUT Agencias.Agencia,
                                             INPUT 0, INPUT 999, INPUT "5110",   
                                             INPUT W_AnoAct,INPUT K,INPUT 1,    
                                             OUTPUT VlrMDb, OUTPUT VlrMCr, OUTPUT TGtoOf.Sdos[4]).
             TGtoOf.Sdos[J] = VlrMDb - VlrMCr.
             J = J + 1.
          END.
       END.                    
  END.

  OUTPUT TO VALUE(Arch). 
    
  DISPLAY "   " + W_NomEnt +  "  -  G A S T O S  G E N E R A L E S  Y  A C U M U L A D O  A  "
          + STRING(W_FecCorte,"99/99/9999") FORM "X(120)"
         SKIP(2)
          "                                    G A S T O   M E N S U A L                   VARIACION          ACUMULADO" SKIP
          "SUCURSAL                    " + 
          W_Titul + "          " + STRING(W_FecCorte,"99/99/9999") FORM "X(300)" SKIP
          "__________________ ___________________ ___________________ ___________________ ____________ ___________________" SKIP(1)
        WITH DOWN WIDTH 420 FRAME Enc NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  FOR EACH TGtoOf BY TGtoOf.Ofi:
      ASSIGN Tcar[1] = Tcar[1] + TGtoOf.Sdos[1]
             Tcar[2] = Tcar[2] + TGtoOf.Sdos[2]
             Tcar[3] = Tcar[3] + TGtoOf.Sdos[3]
             Tcar[4] = Tcar[4] + TGtoOf.Sdos[4]
             TPorC   = ((TGtoOf.Sdos[3] / TGtoOf.Sdos[2]) - 1) * 100.
      

      DISPLAY TGtoOf.Nom  FORM "X(18)"
              TGtoOf.Sdos [1]
              TGtoOf.Sdos [2]
              TGtoOf.Sdos [3]
              TPorC
              TGtoOf.Sdos [4]
           SKIP
          "------------------ ------------------- ------------------- ------------------- ----------- --------------------"
             SKIP (0) WITH DOWN WIDTH 420 FRAME Detal NO-BOX NO-LABELS STREAM-IO USE-TEXT.
  END.

  TPorC = ((TCar[3] / TCar[2]) - 1) * 100.

  DISPLAY SKIP(0)
          "       TOTALES    "
          TCar[1]
          TCar[2]
          TCar[3]
          TPorC
          TCar[4]
          SKIP
          "------------------ ------------------- ------------------- ------------------- ------------ -------------------"
         WITH DOWN WIDTH 420 FRAME Totl NO-BOX NO-LABELS STREAM-IO USE-TEXT.
    
  OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallarSaldo W-InfGer 
PROCEDURE HallarSaldo :
DEFINE INPUT PARAMETER P_OfiIni LIKE Agencias.Agencia.
DEFINE INPUT PARAMETER P_OfiFin LIKE Agencias.Agencia.
DEFINE INPUT PARAMETER P_CenIni LIKE Cen_Costos.Cen_Costos.
DEFINE INPUT PARAMETER P_CenFin LIKE Cen_Costos.Cen_Costos.
DEFINE INPUT PARAMETER P_CtaTra LIKE Cuentas.cuenta.
DEFINE INPUT PARAMETER P_Ano LIKE Sal_Cuenta.Ano.
DEFINE INPUT PARAMETER P_Mes AS INTEGER.
DEFINE OUTPUT PARAMETER P_SdoDeb LIKE Sal_Cuenta.Sal_Inicial INITIAL 0.
DEFINE OUTPUT PARAMETER P_SdoCre LIKE Sal_Cuenta.Sal_Inicial INITIAL 0.
DEFINE OUTPUT PARAMETER P_SdoAct LIKE Sal_Cuenta.Sal_Inicial INITIAL 0.

FOR EACH Sal_Cuenta FIELDS(sal_Cuenta.Agencia
                           Sal_Cuenta.Sal_Inicial
                           Sal_Cuenta.CR
                           Sal_Cuenta.DB
                           Sal_Cuenta.Cuenta)
    WHERE Sal_Cuenta.Agencia GE P_OfiIni
      AND Sal_Cuenta.Agencia LE P_OfiFin
      AND Sal_Cuenta.Cuenta EQ P_CtaTra
      AND Sal_Cuenta.Ano EQ P_Ano NO-LOCK:
    
    ASSIGN P_SdoAct = P_SdoAct + SumatoriaCta(P_MES)
           P_SdoCre = P_SdoCre + Sal_Cuenta.CR[P_Mes]
           P_SdoDeb = P_SdoDeb + Sal_Cuenta.DB[P_Mes].
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallarSdoMayor W-InfGer 
PROCEDURE HallarSdoMayor :
DEFINE INPUT PARAMETER P_OfiIni LIKE Agencias.Agencia.
DEFINE INPUT PARAMETER P_OfiFin LIKE Agencias.Agencia.
DEFINE INPUT PARAMETER P_CenIni LIKE Cen_Costos.Cen_Costos.
DEFINE INPUT PARAMETER P_CenFin LIKE Cen_Costos.Cen_Costos.
DEFINE INPUT PARAMETER P_CtaTra LIKE Cuentas.cuenta.
DEFINE INPUT PARAMETER P_Ano LIKE Sal_Cuenta.Ano.
DEFINE INPUT PARAMETER P_Mes AS INTEGER.
DEFINE INPUT PARAMETER P_Tipo LIKE Cuentas.Tipo.
DEFINE OUTPUT PARAMETER P_SdoDeb LIKE Sal_Cuenta.Sal_Inicial   INITIAL 0.
DEFINE OUTPUT PARAMETER P_SdoCre AS DECIMAL INITIAL 0.
DEFINE OUTPUT PARAMETER P_SdoAct AS DECIMAL INITIAL 0.
DEFINE VARIABLE W_Natur_Mayor LIKE Cuentas.naturaleza.
DEF VAR zsdocta AS DECIMAL INITIAL 0.
DEF VAR WSWAge AS LOGICAL INITIAL FALSE.

/* ******************************************************************************
*  Tipo = 1 Cuentas Mayor                                                       *
*  Tipo = 2 Cuentas Movimiento                                                  *
******************************************************************************* */

DEF VAR W-ROWIDAge  AS ROWID.

IF P_Tipo EQ 2 THEN DO:
    RUN HallarSaldo (INPUT P_OfiIni,
                     INPUT P_OfiFin,
                     INPUT P_CenIni,
                     INPUT P_CenFin,
                     INPUT P_CtaTra,
                     INPUT P_Ano,
                     INPUT P_Mes,
                     OUTPUT P_SdoDeb,
                     OUTPUT P_SdoCre,
                     OUTPUT P_SdoAct).

    RETURN.
END.

FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ P_CtaTra NO-LOCK NO-ERROR.
IF NOT AVAILABLE cuentas THEN DO:
    /*      MESSAGE "No se encontro la cuenta " P_CtaTra SKIP */
    /*          VIEW-AS ALERT-BOX INFO BUTTONS OK.            */
    RETURN.
END.

ASSIGN W_Natur_Mayor = Cuentas.Naturaleza.

IF AVAILABLE agencias THEN
    ASSIGN W-RowidAge = ROWID(Agencias)
           WSWAge = TRUE.

FOR EACH Agencias FIELDS(Agencias.Agencia) WHERE Agencias.Agencia GE P_OfiIni
                                             AND Agencias.Agencia LE P_OfiFin NO-LOCK:
    FOR EACH Sal_Cuenta FIELDS(Sal_Cuenta.Sal_Inicial
                               Sal_Cuenta.CR
                               Sal_Cuenta.Cen_Costos
                               sal_Cuenta.Agencia
                               Sal_Cuenta.DB
                               Sal_Cuenta.Cuenta
                               Sal_cuenta.ano) WHERE Sal_Cuenta.Agencia EQ Agencias.Agencia
                                                 AND Sal_Cuenta.Cuenta BEGINS P_CtaTra
                                                 AND Sal_Cuenta.Ano EQ P_Ano NO-LOCK:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Sal_Cuenta.Cuenta NO-LOCK NO-ERROR.
        IF NOT AVAILABLE cuentas THEN
            MESSAGE "No se encontro cuenta: " Sal_cuenta.Cuenta
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

        IF Cuentas.Naturaleza EQ W_Natur_Mayor THEN
            ASSIGN P_SdoAct = P_SdoAct + sumatoriaCta(P_Mes). /* Sal_Cuenta.Sdo_Final. */
        ELSE
            ASSIGN P_SdoAct = P_SdoAct - sumatoriaCta(P_Mes).

        ASSIGN P_SdoCre = P_SdoCre + Sal_Cuenta.CR[P_Mes]
               P_SdoDeb = P_SdoDeb + Sal_Cuenta.DB[P_Mes].
    END.
END.

IF WSWAge THEN
    FIND FIRST agencias WHERE ROWID(Agencias) EQ W-RowidAge NO-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Halla_Inter W-InfGer 
PROCEDURE Halla_Inter :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
  DEFI OUTPUT PARAMETER W_SdoCta1 LIKE Sal_Cuenta.Sal_Inicial INIT 0.
  DEFI OUTPUT PARAMETER W_SdoCta2 LIKE Sal_Cuenta.Sal_Inicial INIT 0.
  DEFI OUTPUT PARAMETER W_SdoCta3 LIKE Sal_Cuenta.Sal_Inicial INIT 0.

  DEFI VAR TotCU LIKE Sal_Cuenta.Sal_INICIAL INIT 0.
  DEFI VAR Vlr0  LIKE Sal_Cuenta.Sal_INICIAL.

  FOR EACH Cuentas WHERE (Cuentas.Cuenta EQ "615005"
                      OR  Cuentas.Cuenta EQ "615010"
                      OR  Cuentas.Cuenta EQ "615015"
                      OR  Cuentas.Cuenta EQ "61509501") NO-LOCK:
      RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                      INPUT 0, INPUT 999, INPUT Cuentas.Cuenta,   
                                      INPUT W_AnoAct, INPUT W_MesAct,INPUT 2,    
                                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT TotCU).     
      W_SdoCta1 = W_SdoCta1 + TotCU.
  END.

  FOR EACH Cuentas WHERE (Cuentas.Cuenta EQ "615005"
                      OR  Cuentas.Cuenta EQ "615010"
                      OR  Cuentas.Cuenta EQ "615015"
                      OR  Cuentas.Cuenta EQ "61509501") NO-LOCK:
      RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                      INPUT 0, INPUT 999, INPUT Cuentas.Cuenta,   
                                      INPUT W_AnoAct - 1, INPUT W_MesAct,INPUT 2,    
                                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT TotCU).     
      W_SdoCta2 = W_SdoCta2 + TotCU.
  END.

  FOR EACH Cuentas WHERE (Cuentas.Cuenta EQ "615005"
                      OR  Cuentas.Cuenta EQ "615010"
                      OR  Cuentas.Cuenta EQ "615015"
                      OR  Cuentas.Cuenta EQ "61509501") NO-LOCK:
      RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                      INPUT 0, INPUT 999, INPUT Cuentas.Cuenta,   
                                      INPUT W_AnoAct - 1, INPUT 12,INPUT 2,    
                                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT TotCU).     
      W_SdoCta3 = W_SdoCta3 + TotCU.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Halla_Provis W-InfGer 
PROCEDURE Halla_Provis :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
  DEFI OUTPUT PARAMETER W_SdoCta1 LIKE Sal_Cuenta.Sal_INICIAL INIT 0.
  DEFI OUTPUT PARAMETER W_SdoCta2 LIKE Sal_Cuenta.Sal_INICIAL INIT 0.
  DEFI OUTPUT PARAMETER W_SdoCta3 LIKE Sal_Cuenta.Sal_INICIAL INIT 0.

  DEFI VAR TotCU LIKE Sal_Cuenta.Sal_INICIAL INIT 0.
  DEFI VAR Vlr0  LIKE Sal_Cuenta.Sal_INICIAL.

  FOR EACH Cuentas WHERE Cuentas.Cuenta GE "1489000000"
                     AND Cuentas.Cuenta LE "1499999999"
                     AND Cuentas.Tipo   EQ 2
                     AND Cuentas.Estado EQ 1 NO-LOCK:
      RUN HallarSdoMayor /* IN W_Manija  */ (INPUT 1, INPUT 20,
                                      INPUT 0, INPUT 999, INPUT Cuentas.Cuenta,   
                                      INPUT W_AnoAct, INPUT W_MesAct,INPUT 2,    
                                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT TotCU).     
      W_SdoCta1 = W_SdoCta1 + TotCU.
  END.

  FOR EACH Cuentas WHERE Cuentas.Cuenta GE "1489000000"
                     AND Cuentas.Cuenta LE "1499999999"
                     AND Cuentas.Tipo   EQ 2
                     AND Cuentas.Estado EQ 1 NO-LOCK:
      RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                      INPUT 0, INPUT 999, INPUT Cuentas.Cuenta,   
                                      INPUT W_AnoAct - 1, INPUT W_MesAct,INPUT 1,    
                                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT TotCU).     
      W_SdoCta2 = W_SdoCta2 + TotCU.
  END.

  FOR EACH Cuentas WHERE Cuentas.Cuenta GE "1489000000"
                     AND Cuentas.Cuenta LE "1499999999"
                     AND Cuentas.Tipo   EQ 2
                     AND Cuentas.Estado EQ 1 NO-LOCK:
      RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                      INPUT 0, INPUT 999, INPUT Cuentas.Cuenta,   
                                      INPUT W_AnoAct - 1, INPUT 12,INPUT 1,    
                                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT TotCU).
      W_SdoCta3 = W_SdoCta3 + TotCU.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Halla_PyG W-InfGer 
PROCEDURE Halla_PyG :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
  DEFI OUTPUT PARAMETER W_SdoCta1 LIKE Sal_Cuenta.Sal_INICIAL INIT 0.
  DEFI OUTPUT PARAMETER W_SdoCta2 LIKE Sal_Cuenta.Sal_INICIAL INIT 0.
  DEFI OUTPUT PARAMETER W_SdoCta3 LIKE Sal_Cuenta.Sal_INICIAL INIT 0.

  DEFI VAR TotCU LIKE Sal_Cuenta.Sal_INICIAL INIT 0.
  DEFI VAR Vlr0  LIKE Sal_Cuenta.Sal_INICIAL.

  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                  INPUT 0, INPUT 999, INPUT "5",   
                                  INPUT W_AnoAct, INPUT W_MesAct,INPUT 1,        
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT TotCU).         
  W_SdoCta1 = W_TotIng[1] - TotCU.
  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                  INPUT 0, INPUT 999, INPUT "6",   
                                  INPUT W_AnoAct, INPUT W_MesAct,INPUT 1,        
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT TotCU).         
  W_SdoCta1 = W_SdoCta1 - TotCU.

  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                  INPUT 0, INPUT 999, INPUT "5",   
                                  INPUT W_AnoAct - 1, INPUT W_MesAct,INPUT 1,        
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT TotCU).         
  W_SdoCta2 = W_TotIng[2] - TotCU.
  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                  INPUT 0, INPUT 999, INPUT "6",   
                                  INPUT W_AnoAct - 1, INPUT W_MesAct,INPUT 1,        
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT TotCU).         
  W_SdoCta2 = W_SdoCta2 - TotCU.

  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                  INPUT 0, INPUT 999, INPUT "5",   
                                  INPUT W_AnoAct - 1, INPUT 12,INPUT 1,        
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT TotCU).         
  W_SdoCta3 = W_TotIng[3] - TotCU.
  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                  INPUT 0, INPUT 999, INPUT "6",   
                                  INPUT W_AnoAct - 1, INPUT 12,INPUT 1,        
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT TotCU).         
  W_SdoCta3 = W_SdoCta3 - TotCU.
  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                  INPUT 0, INPUT 999, INPUT "59",   /*Cierre ejerc. Dic.cada año*/
                                  INPUT W_AnoAct - 1, INPUT 12,INPUT 1,        
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT TotCU).         
  W_SdoCta3 = W_SdoCta3 + TotCU.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Halla_SdoCta W-InfGer 
PROCEDURE Halla_SdoCta :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
  DEFI INPUT PARAMETER W_Cta LIKE Sal_Cuenta.Cuenta.
  
  DEFI VAR Vlr0 LIKE Sal_Cuenta.Sal_INICIAL.

  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,                             
                                  INPUT 0, INPUT 999, INPUT W_Cta,               
                                  INPUT W_AnoAct, INPUT W_MesAct,INPUT 1,         
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT Tof.Sdos[1]).  
  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,                              
                                  INPUT 0, INPUT 999, INPUT W_Cta,               
                                  INPUT W_AnoAct - 1, INPUT W_MesAct,INPUT 1,     
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT Tof.Sdos[2]).  
  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,                              
                                  INPUT 0, INPUT 999, INPUT W_Cta,               
                                  INPUT W_AnoAct - 1, INPUT 12,INPUT 1,           
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT Tof.Sdos[3]).  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Halla_TempVcto W-InfGer 
PROCEDURE Halla_TempVcto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF FecCor GT FecIni AND SUBSTRING(STRING(FecCor),5,2) GT "12" THEN
     ASSIGN FecCor = FecCor + 1000
            FecCor = FecCor - 12.
  
  IF FecCor GT FecFin THEN                                                                  
     FIND FIRST TMes WHERE TMes.Tipo EQ "A"                                                 
                       AND TMes.Ta   EQ 5                            
                       AND TMes.AMes EQ FecFin + 1 NO-ERROR.                                
  ELSE DO:                                                                                  
     IF FecCor LT FecIni THEN                                                               
        FIND FIRST TMes WHERE TMes.Tipo EQ "A"                                              
                          AND TMes.Ta   EQ 5                         
                          AND TMes.AMes EQ FecIni NO-ERROR.                                 
     ELSE                                                                                   
        FIND FIRST TMes WHERE TMes.Tipo EQ "A"                                              
                          AND TMes.Ta   EQ 5                         
                          AND TMes.AMes EQ FecCor NO-ERROR.                                 
  END.                                                                                      
      
  IF NOT AVAIL(Tmes) THEN DO:                                                               
     CREATE TMes.                                                                           
     ASSIGN TMes.Tipo = "A"                                                                 
            TMes.Ta   = 5                                            
            TMes.AMes = FecCor.                                                             
                                                                                                
     IF FecCor GT FecFin THEN                                                        
        ASSIGN TMes.AMes = FecFin + 1.                                                      
     ELSE IF FecCor LT FecIni THEN                                                          
        ASSIGN TMes.AMes = FecIni.                                                          
  END.

  ASSIGN LiqInt      = ((Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) * ( Ahorros.Tasa / 12 )) / 100
         TMes.Nro    = TMes.Nro  + 1
         TMes.Tasa   = TMes.Tasa + Ahorros.Tasa  /* (Ahorros.Tasa * 12) */                              
         TMes.SdoI   = TMes.SdoI + LiqInt
         W_RowidTMes = ROWID(TMes).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IConting_PromDiaAno W-InfGer 
PROCEDURE IConting_PromDiaAno :
/*------------------------------------------------------------------------------
  Purpose:  Promedio Dia/Año Interés-contingente   
  Notes:    Oct.19/2006 GAER.   
------------------------------------------------------------------------------*/
   DEFI VAR Vlr0     LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR W_SdoCta LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR TDisp    LIKE Ahorros.Sdo_Dispon INIT 0.
   DEFI VAR TFin     LIKE Ahorros.Sdo_Dispon INIT 0.
   DEFI VAR TMes     LIKE Ahorros.Sdo_Dispon INIT 0.
   DEFI VAR TMesT    LIKE Ahorros.Sdo_Dispon INIT 0.
   DEFI VAR PorC     AS DEC FORM "-99999.99".
   DEFI VAR Arch     AS CHAR FORM "X(35)".
   DEFI VAR FecNN    AS DATE.
   DEFI VAR NN       AS INTEG FORM "99999999".
   DEFI VAR NFI      AS INTEG FORM "99999999".
   DEFI VAR NFF      AS INTEG FORM "99999999".

   FOR EACH TDiaA: DELETE TDiaA. END.
      
   Arch = "C:\InfRed\IContingPromDA-" + STRING(W_FecCorte,"999999") + "_" + W_Usuario.

   OUTPUT TO VALUE(Arch).

   DISPLAY "  " + W_NomEnt + "  INT-CONTINGENTE RECUPERACION Y PROMEDIO DIA/AÑO DE "
          + STRING(W_FecIniP,"99/99/9999") + " HASTA " + STRING(W_FecFinP,"99/99/9999") FORM "X(120)"
         SKIP(1)
       WITH DOWN WIDTH 150 FRAME Enc NO-BOX NO-LABELS STREAM-IO USE-TEXT.

   ASSIGN FecNN = W_FecIniP - 1.

  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 0, INPUT 999, INPUT "81",          
                                   INPUT YEAR(FecNN), INPUT MONTH(FecNN),INPUT 1,            
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).   
   TDisp = W_SdoCta.
    
   FOR EACH Mov_Contable WHERE Mov_Contable.Cuenta       GE "8100000000"
                           AND Mov_Contable.Cuenta       LE "8199999999"
                           AND Mov_Contable.Fec_Contable GE W_FecIniP
                           AND Mov_Contable.Fec_Contable LE W_FecFinP NO-LOCK
            BREAK BY YEAR(Mov_Contable.Fec_Contable) BY MONTH(Mov_Contable.Fec_Contable)
                  BY Mov_Contable.Fec_Contable:
       IF FIRST-OF(Mov_Contable.Fec_Contable) THEN DO:
          CREATE TDiaA.
          ASSIGN TDiaA.FecD = Mov_Contable.Fec_Contable
                 TDiaA.TSdo = TDisp.
       END.

       IF Mov_Contable.CR GT 0 THEN
          ASSIGN TDiaA.TSdo = TDiaA.TSdo - Mov_Contable.CR
                 TMes       = TMes  + Mov_Contable.CR
                 TMesT      = TMesT + Mov_Contable.CR.
       ELSE
          ASSIGN TDiaA.TSdo = TDiaA.TSdo + Mov_Contable.DB.
       
        
       IF LAST-OF(Mov_Contable.Fec_Contable) THEN DO:
          DISPLAY TDiaA.FecD    LABEL "Fecha-Día"
                  TDiaA.TSdo    LABEL "Saldo del Día"
                  TMes          LABEL "Recuperac.Acumul.Mes"
                  TMes / DAY(Mov_Contable.Fec_Contable) FORMAT "->>>,>>>,>>>,>>9" LABEL "Promedio recuperacion acumulada"
              WITH DOWN WIDTH 150 FRAME Fd1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

          ASSIGN TDisp = TDiaA.TSdo
                 TFin  = TFin + TDiaA.TSdo.
       END.

       IF LAST-OF(MONTH(Mov_Contable.Fec_Contable)) THEN DO:
          DISPLAY "----------  ---------------------- -----------------------" SKIP
                  TDiaA.FecD    
                  ""
                  TDiaA.TSdo    
                  TMes    SKIP(1)      
              WITH DOWN WIDTH 150 FRAME Fd2 NO-BOX NO-LABELS STREAM-IO USE-TEXT.
          ASSIGN Tmes = 0.
       END.
  END.
    
  DISPLAY SKIP(1)
          "               Total de Saldos Día Tot./365 = Sdo.Promedio  Recuperación Total  Promed.Recup.Mes" SKIP                 
          "----------  ---------------------- ----------------------- -------------------  ----------------" SKIP
          "TOTAL       "
          TFin 
          "   "
          TFin / 365  FORM "->>>>>>>,>>>,>>9"
          TmesT  
          "  "
          TMesT / 12  FORM "->>>>>>>,>>>,>>9"
     WITH DOWN WIDTH 170 FRAME Totl NO-BOX NO-LABELS STREAM-IO USE-TEXT.
    
  OUTPUT CLOSE.

  FIND FIRST T_PromMes WHERE T_PromMes.Ident  EQ "IntContin"
                         AND T_PromMes.AnoMes EQ INTEG(STRING(YEAR(W_FecFinP),"9999") + 
                                                       STRING(MONTH(W_FecFinP),"99")) NO-ERROR.
  IF NOT AVAILABLE (T_PromMes) THEN
     CREATE T_PromMes.
  ASSIGN T_PromMes.Ident      = "IntContin"
         T_PromMes.Ano        = INTEG(STRING(YEAR(W_FecFinP),"9999") + STRING(MONTH(W_FecFinP),"99"))         
         T_PromMes.Nro_Regist = 12
         T_PromMes.Vr_Promed  = ROUND(TMesT / 12,0).

  ASSIGN VrContDeu = ROUND(TMesT / 12,0)
         TmesT     = 0.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Impor_VctosCred W-InfGer 
PROCEDURE Impor_VctosCred :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR Datos  AS CHARACTER FORMAT "X(80)".
  DEFINE VAR Vlr    AS CHARACTER FORMAT "X(18)" NO-UNDO.

  W_IntCtesNoP = 0.

  INPUT FROM VALUE(W_ArchVcto).
  REPEAT:                                                 
     IMPORT UNFORMATTED Datos.                                 
     IF SUBSTRING(Datos,1,2) EQ "1-" THEN DO:                  
        CREATE TBanda.                                         
        ASSIGN TBanda.Id-A  = SUBSTRING(Datos,1,5)             
               TBanda.Tipo  = SUBSTRING(Datos,7,1)             
               TBanda.Banda = INTEG(SUBSTRING(Datos,13,1))     
               Vlr          = SUBSTRING(Datos,15,18)           
               Vlr          = REPLACE(Vlr,",","")              
               TBanda.SdoK  = DEC(Vlr)                         
               Vlr          = SUBSTRING(Datos,34,18)           
               Vlr          = REPLACE(Vlr,",","")              
               TBanda.SdoI  = DEC(Vlr)                         
               TBanda.Tasa  = DEC(SUBSTRING(Datos,53,13)).                    
     END. 
     ELSE IF SUBSTRING(Datos,1,7) EQ "C     8" THEN 
        ASSIGN Vlr          = SUBSTRING(Datos,28,18)
               Vlr          = REPLACE(Vlr,",","")
               W_IntCtesNoP = DEC(Vlr).
  END.                                                         
  INPUT CLOSE.                                                 
                                                          
  MESSAGE "El Archivo-Informe Prox.Vctos Fué Importado OK."  SKIP
          "Vlr.Intereses No-Provis.: " W_IntCtesNoP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.                       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_EvalCont1 W-InfGer 
PROCEDURE Imp_EvalCont1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR Vlr0     LIKE Sal_Cuenta.Sal_INICIAL.
  DEFI VAR W_SdoCta LIKE Sal_Cuenta.Sal_INICIAL.
    
  RUN HallarSdoMayor (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "24",
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas   = 2
         TEval.Reng   = 035
         TEval.Cpto   = "Cuentas por Pagar"
         TEval.Vlr    = W_SdoCta.

  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "2405",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  ASSIGN TEval.Bda[1] = W_SdoCta.
  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "2442",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
  RUN HallarSdoMayor   (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "2445",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "2450",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "2460",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
  RUN HallarSdoMayor   (INPUT 1, INPUT 20,
                                INPUT 999, INPUT 999, INPUT "24350501",          
                                INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
  RUN HallarSdoMayor   (INPUT 1, INPUT 20,
                                INPUT 999, INPUT 999, INPUT "24350502",          
                                INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  ASSIGN TEval.Bda[1] = TEval.Bda[1] + (W_SdoCta / 2).
  ASSIGN TEval.Bda[2] = TEval.Bda[2] + (W_SdoCta / 2).

  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                INPUT 999, INPUT 999, INPUT "24959519",          
                                INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  ASSIGN TEval.Bda[7] = TEval.Bda[7] + W_SdoCta.
  RUN HallarSdoMayor   (INPUT 1, INPUT 20,
                                INPUT 999, INPUT 999, INPUT "24959521",          
                                INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  ASSIGN TEval.Bda[7] = TEval.Bda[7] + W_SdoCta.
  RUN HallarSdoMayor   (INPUT 1, INPUT 20,
                                INPUT 999, INPUT 999, INPUT "24959522",          
                                INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  ASSIGN TEval.Bda[7] = TEval.Bda[7] + W_SdoCta.


  /* Feb, May, Ago, Nov */
  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "244020",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).  
  CASE MONTH(W_FecCorte):
      WHEN 1  THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 2  THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 3  THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 4  THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 5  THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 6  THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 7  THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 8  THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 9  THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 10 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 11 THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 12 THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
  END CASE.

  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "2447",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).  
  CASE MONTH(W_FecCorte):
      WHEN 1  THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 2  THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 3  THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 4  THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 5  THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 6  THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 7  THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 8  THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 9  THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 10 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 11 THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 12 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
  END CASE.


  /* Feb,Ago */
  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "244005",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).  
  CASE MONTH(W_FecCorte):
      WHEN 1  THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 2  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 3  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 4  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 5  THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 6  THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 7  THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 8  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 9  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 10 THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 11 THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 12 THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_EvalCont2 W-InfGer 
PROCEDURE Imp_EvalCont2 :
DEFI VAR Vlr0     LIKE Sal_Cuenta.Sal_INICIAL.
  DEFI VAR W_SdoCta LIKE Sal_Cuenta.Sal_INICIAL.
  DEFI VAR W_MesResta AS INTEG FORM "99".
  DEFI VAR W_VlrResta LIKE Sal_Cuenta.Sal_INICIAL.

  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                       INPUT 999, INPUT 999, INPUT "25",          
                       INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                       OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas   = 2
         TEval.Reng   = 040
         TEval.Cpto   = "Impuestos,Gravamenes y Tasas"
         TEval.Vlr    = W_SdoCta.
/*          TEval.Bda[1] = W_SdoCta. */
  IF W_SdoCta LT 0 THEN
     ASSIGN TEval.Vlr    = 0
            TEval.Bda[1] = 0.  

  RUN Imp_EvalCont4.

  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                       INPUT 999, INPUT 999, INPUT "26",          
                       INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                       OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas   = 2
         TEval.Reng   = 045
         TEval.Cpto   = "Fondos Sociales"
         TEval.Vlr    = W_SdoCta.
  RUN madurar_FdosSociales(INPUT W_SdoCta).
  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                       INPUT 999, INPUT 999, INPUT "27",          
                       INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                       OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas   = 2
         TEval.Reng   = 050
         TEval.Cpto   = "Otros Pasivos"
         TEval.Vlr    = W_SdoCta.
    
  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                       INPUT 999, INPUT 999, INPUT "271005",          
                       INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                       OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  ASSIGN TEval.Bda[1] = W_SdoCta.
    
  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                       INPUT 999, INPUT 999, INPUT "2740",          
                       INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                       OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
    
  RUN HallarSdoMayor (INPUT 1, INPUT 20,
                      INPUT 999, INPUT 999, INPUT "2745",          
                      INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  ASSIGN TEval.Bda[7] = W_SdoCta.

  RUN HallarSdoMayor (INPUT 1, INPUT 20,
                      INPUT 999, INPUT 999, INPUT "279505",          
                      INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  ASSIGN TEval.Bda[7] = TEval.Bda[7] + W_SdoCta.


  RUN HallarSdoMayor (INPUT 1, INPUT 20,
                      INPUT 999, INPUT 999, INPUT "28",          
                      INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas   = 2
         TEval.Reng   = 055
         TEval.Cpto   = "Pasivos Estimados y Provisiones"
         TEval.Vlr    = W_SdoCta.
    
  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                       INPUT 999, INPUT 999, INPUT "2805",          
                       INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                       OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  ASSIGN TEval.Bda[7] = W_SdoCta.

  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                       INPUT 999, INPUT 999, INPUT "28109501",          
                       INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                       OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CASE MONTH(W_FecCorte):
      WHEN 1  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 2  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 3  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 4  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 5  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 6  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 7  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 8  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 9  THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 10 THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 11 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 12 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
  END CASE.
    
  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "28109502",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CASE MONTH(W_FecCorte):
      WHEN 1  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 2  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 3  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 4  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 5  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 6  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 7  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 8  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 9  THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 10 THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 11 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 12 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
  END CASE.
        
  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "282505",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CASE MONTH(W_FecCorte):
      WHEN 1  THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 2  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 3  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 4  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 5  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 6  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 7  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 8  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 9  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 10 THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 11 THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 12 THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
  END CASE.
    
  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "282510",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CASE MONTH(W_FecCorte):
      WHEN 1  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 2  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 3  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 4  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 5  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 6  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 7  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 8  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 9  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 10 THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 11 THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 12 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
  END CASE.
    
  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "282515",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CASE MONTH(W_FecCorte):
      WHEN 1  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 2  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 3  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 4  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 5  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 6  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 7  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 8  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 9  THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 10 THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 11 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 12 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
  END CASE.
    

  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "282520",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CASE MONTH(W_FecCorte):
      WHEN 1  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 2  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 3  THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 4  THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 5  THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 6  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 7  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 8  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 9  THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 10 THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 11 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 12 THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
  END CASE.
    


  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "282535",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CASE MONTH(W_FecCorte):
      WHEN 1  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 2  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 3  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 4  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 5  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 6  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 7  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 8  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 9  THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 10 THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 11 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 12 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
  END CASE.


  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "282540",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  IF MONTH(W_FecCorte) EQ 11 THEN
     ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
  ELSE IF MONTH(W_FecCorte) EQ 12 THEN
     ASSIGN TEval.Bda[1] = TEval.Bda[1] + (W_SdoCta / 12)
            TEval.Bda[2] = TEval.Bda[2] + (W_SdoCta / 12)
            TEval.Bda[3] = TEval.Bda[3] + (W_SdoCta / 12)
            TEval.Bda[4] = TEval.Bda[4] + (W_SdoCta / 12 * 3)
            TEval.Bda[5] = TEval.Bda[5] + (W_SdoCta / 12 * 3)
            TEval.Bda[6] = TEval.Bda[6] + (W_SdoCta / 12 * 3).
  ELSE DO:
     ASSIGN W_MesResta = 12 - MONTH(W_FecCorte)  
            W_VlrResta = W_SdoCta / W_MesResta
            TEval.Bda[1] = TEval.Bda[1] + W_VlrResta
            TEval.Bda[2] = TEval.Bda[2] + W_VlrResta.
     IF W_MesResta GE 3 THEN DO:
        TEval.Bda[3] = TEval.Bda[3] + W_VlrResta.
        IF W_MesResta GE 4 THEN DO:
           IF W_MesResta EQ 4 THEN
              TEval.Bda[4] = TEval.Bda[4] + W_VlrResta.
           ELSE IF W_MesResta EQ 5 THEN
              TEval.Bda[4] = TEval.Bda[4] + (W_VlrResta * 2).
           ELSE IF W_MesResta EQ 6 THEN
              TEval.Bda[4] = TEval.Bda[4] + (W_VlrResta * 3).
           ELSE IF W_MesResta EQ 7 THEN
              ASSIGN TEval.Bda[4] = TEval.Bda[4] + (W_VlrResta * 3)
                     TEval.Bda[5] = TEval.Bda[5] + W_VlrResta.
           ELSE IF W_MesResta EQ 8 THEN
              ASSIGN TEval.Bda[4] = TEval.Bda[4] + (W_VlrResta * 3)
                     TEval.Bda[5] = TEval.Bda[5] + (W_VlrResta * 2).
           ELSE IF W_MesResta EQ 9 THEN
              ASSIGN TEval.Bda[4] = TEval.Bda[4] + (W_VlrResta * 3)
                     TEval.Bda[5] = TEval.Bda[5] + (W_VlrResta * 3).           
           ELSE IF W_MesResta EQ 10 THEN
              ASSIGN TEval.Bda[4] = TEval.Bda[4] + (W_VlrResta * 3)
                     TEval.Bda[5] = TEval.Bda[5] + (W_VlrResta * 3)
                     TEval.Bda[6] = TEval.Bda[6] + W_VlrResta.
           ELSE IF W_MesResta EQ 11 THEN
              ASSIGN TEval.Bda[4] = TEval.Bda[4] + (W_VlrResta * 3)
                     TEval.Bda[5] = TEval.Bda[5] + (W_VlrResta * 3)
                     TEval.Bda[6] = TEval.Bda[6] + (W_VlrResta * 2).
        END.
     END.
  END.  


  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "282565",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CASE MONTH(W_FecCorte):
      WHEN 1  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 2  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 3  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 4  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 5  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 6  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 7  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 8  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 9  THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 10 THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 11 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 12 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
  END CASE.

    
  RUN HallarSdoMayor  (INPUT 1, INPUT 20,  
                                  INPUT 999, INPUT 999, INPUT "282595",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  IF MONTH(W_FecCorte) LE 2 OR MONTH(W_FecCorte) EQ 12 THEN
     ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
  ELSE IF MONTH(W_FecCorte) GE 3 AND MONTH(W_FecCorte) LE 5 THEN
     ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
  ELSE IF MONTH(W_FecCorte) GE 6 AND MONTH(W_FecCorte) LE 8 THEN
     ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
  ELSE IF MONTH(W_FecCorte) EQ 9 THEN
     ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
  ELSE IF MONTH(W_FecCorte) EQ 10 THEN
     ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
  ELSE IF MONTH(W_FecCorte) EQ 11 THEN
     ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.

  
  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "2835",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CASE MONTH(W_FecCorte):
      WHEN 1  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 2  THEN ASSIGN TEval.Bda[6] = TEval.Bda[6] + W_SdoCta.
      WHEN 3  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 4  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 5  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 6  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 7  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 8  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 9  THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 10 THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 11 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 12 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
  END CASE.
    


  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "29",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas   = 2
         TEval.Reng   = 060
         TEval.Cpto   = "TITULOS DE INVERSIÒN EN CIRCUILACION"
         TEval.Vlr    = W_SdoCta.
  
  RUN HallarSdoMayor (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "9125",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas   = 2
         TEval.Reng   = 065
         TEval.Cpto   = "Contingentes Acreedores"
         TEval.Vlr    = W_SdoCta.
  
  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "9145",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  ASSIGN TEval.Vlr    = TEval.Vlr  + W_SdoCta
         TEval.Bda[1] = W_SdoCta / 2
         TEval.Bda[2] = W_SdoCta / 2.


  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "91250502",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).

  ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_EvalCont3 W-InfGer 
PROCEDURE Imp_EvalCont3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR Vlr0     LIKE Sal_Cuenta.Sal_INICIAL.
  DEFI VAR W_SdoCta LIKE Sal_Cuenta.Sal_INICIAL.
  DEFI VAR TPromAp  LIKE Sal_Cuenta.Sal_INICIAL.
    
  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "3105",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas   = 3
         TEval.Reng   = 005
         TEval.Cpto   = "Aportes Sociales"
         TEval.Vlr    = W_SdoCta
         TEval.Bda[7] = W_SdoCta.
    
  FOR EACH Mov_Contable WHERE Mov_Contable.Cuenta EQ "310505" NO-LOCK:             
      IF Mov_Contable.Fec_Contab GE W_FecCorte - 365 AND 
         Mov_Contable.Fec_Contab LE W_FecCorte THEN DO:
            ASSIGN TPromAp = TPromAp - Mov_Contable.db + mov_contable.cr.                            
      END.
  END.
  MESSAGE "aportes " tpromap VIEW-AS ALERT-BOX.
       
  ASSIGN TPromAp     = TPromAp / 12
        ProyAport    = ProyAport - TPromAp
        TEval.Bda[1] = ProyAport 
        TEval.Bda[2] = ProyAport  
        TEval.Bda[3] = ProyAport  
        TEval.Bda[4] = (ProyAport * 3) 
        TEval.Bda[5] = (ProyAport * 3) 
        TEval.Bda[6] = (ProyAport * 3).
        
  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "32",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas  = 3
        TEval.Reng   = 010
        TEval.Cpto   = "Reservas"
        TEval.Vlr    = W_SdoCta
        TEval.Bda[7] = W_SdoCta.
    
  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "33",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas  = 3
        TEval.Reng   = 015
        TEval.Cpto   = "Fondos Patrimoniales"
        TEval.Vlr    = W_SdoCta
        TEval.Bda[7] = W_SdoCta.
    
  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "34",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas  = 3
        TEval.Reng   = 020
        TEval.Cpto   = "Superávit"
        TEval.Vlr    = W_SdoCta
        TEval.Bda[7] = W_SdoCta.

  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                 INPUT 999, INPUT 999, INPUT "4",          
                                 INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                 OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas  = 3
        TEval.Reng   = 025
        TEval.Cpto   = "Excedentes Y/O Pérdidas"    
        TEval.Vlr    = W_SdoCta.

  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                 INPUT 999, INPUT 999, INPUT "5",
                                 INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                 OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  TEval.Vlr = TEval.Vlr - W_SdoCta.

  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                 INPUT 999, INPUT 999, INPUT "6",
                                 INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                 OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  TEval.Vlr = TEval.Vlr - W_SdoCta.
    
  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                 INPUT 999, INPUT 999, INPUT "3605",          
                                 INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                 OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  ASSIGN TEval.Vlr    = TEval.Vlr + W_SdoCta.
  

  DEFINE VARIABLE PorcFondoEducacion    AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE PorcFondoSolidaridad  AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE PorcReservaProteccion AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE PorcSeguroDeudor      AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE PorcFondoPrevencion   AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE PorcFondoSeguroVida   AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE PorcExcedenteOtras    AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE PorcRevalorizaAportes AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE PorcEducaFormalEdu    AS DECIMAL     NO-UNDO.     /* Porcentaje del fondo de educacion formal */
  DEFINE VARIABLE PorcEducaFormalSol    AS DECIMAL     NO-UNDO.     /* Porcentaje del fondo de solidaridad */

  ASSIGN PorcFondoEducacion     = TEval.Vlr * 0.2089
         PorcFondoSolidaridad   = TEval.Vlr * 0.1116
         PorcReservaProteccion  = TEval.Vlr * 0.1899
         PorcSeguroDeudor       = TEval.Vlr * 0.1470
         PorcFondoPrevencion    = TEval.Vlr * 0.0588
         PorcFondoSeguroVida    = TEval.Vlr * 0.0412
         PorcExcedenteOtras     = TEval.Vlr * 0.0505
         PorcRevalorizaAportes  = TEval.Vlr * 0.1921.

  ASSIGN PorcEducaFormalEdu     = PorcFondoEducacion    * 0.61      /* Dic -  Porcentaje que se saca del fondo de educacion para educacion formal */
         PorcEducaFormalSol     = PorcFondoSolidaridad  * 0.56      /* Dic */
         PorcFondoEducacion     = PorcFondoEducacion - PorcEducaFormalEdu       /* Abr - Dic */
         PorcFondoSolidaridad   = PorcFondoSolidaridad - PorcEducaFormalSol.    /* Abr - Dic */

  /* PorcEducaFormalEdu */
  CASE MONTH(W_FecCorte):
      WHEN 1 OR WHEN 2 OR WHEN 3 THEN DO: 
          ASSIGN TEval.Bda[7] = TEval.Bda[7] + PorcEducaFormalEdu + PorcEducaFormalSol + PorcFondoEducacion + PorcFondoSolidaridad + 
                 PorcReservaProteccion + PorcSeguroDeudor + PorcFondoPrevencion + PorcFondoSeguroVida + PorcExcedenteOtras + PorcRevalorizaAportes.
      END.
      WHEN 4  THEN DO:
          ASSIGN TEval.Bda[7] = TEval.Bda[7]          +
                 (PorcEducaFormalEdu)       +
                 (PorcEducaFormalSol)     +
                 (PorcFondoEducacion    * (11 / 12))     +
                 (PorcFondoSolidaridad  * (11 / 12))   + 
                 PorcReservaProteccion  +
                 (PorcSeguroDeudor  * (11 / 12))       +
                 (PorcFondoPrevencion   * (11 / 12))    +
                 (PorcFondoSeguroVida   * (11 / 12))    +
                 PorcExcedenteOtras                   +
                 PorcRevalorizaAportes.
          ASSIGN TEval.Bda[6] = TEval.Bda[6]    +                 
                 (PorcFondoEducacion / 12)      +
                 (PorcFondoSolidaridad / 12)    + 
                 (PorcFondoPrevencion / 12)     +
                 (PorcFondoSeguroVida / 12)     +
                 (PorcSeguroDeudor / 12).
      END.
      WHEN 5  THEN DO:
          ASSIGN TEval.Bda[7] = TEval.Bda[7]        +
                 (PorcEducaFormalEdu)     +
                 (PorcEducaFormalSol)     +
                 (PorcFondoEducacion    * (10 / 12))     +
                 (PorcFondoSolidaridad  * (10 / 12))     + 
                 PorcReservaProteccion  +
                 PorcSeguroDeudor       * (10 / 12)     +
                 (PorcFondoPrevencion   * (10 / 12))    +
                 (PorcFondoSeguroVida   * (10 / 12))    +
                 PorcExcedenteOtras                     +
                 PorcRevalorizaAportes.

          ASSIGN TEval.Bda[6] = TEval.Bda[6]            +
                 (PorcFondoEducacion    * (2 / 12))      +
                 (PorcFondoSolidaridad  * (2 / 12))      + 
                 (PorcFondoPrevencion   * (2 / 12))      +
                 (PorcSeguroDeudor      * (2 / 12))     +
                 (PorcFondoSeguroVida   * (2 / 12)).
      END.
      WHEN 6  THEN DO:
          ASSIGN TEval.Bda[7] = TEval.Bda[7]        +
                 (PorcEducaFormalEdu)     +
                 (PorcEducaFormalSol)     +
                 (PorcFondoEducacion    * (9 / 12))     +
                 (PorcFondoSolidaridad  * (9 / 12))     + 
                 PorcReservaProteccion                  +
                 (PorcSeguroDeudor  * (9 / 12))       +
                 (PorcFondoPrevencion   * (9 / 12))    +
                 (PorcFondoSeguroVida   * (9 / 12))    +
                 PorcExcedenteOtras                     +
                 PorcRevalorizaAportes.

          ASSIGN TEval.Bda[6] = TEval.Bda[6]            +
                 (PorcFondoEducacion    * (3 / 12))      +
                 (PorcFondoSolidaridad  * (3 / 12))      + 
                 (PorcFondoPrevencion   * (3 / 12))      +
                 (PorcSeguroDeudor      * (3 / 12))     +
                 (PorcFondoSeguroVida   * (3 / 12)).
      END.
      WHEN 7  THEN DO:
          ASSIGN TEval.Bda[7] = TEval.Bda[7]            +
                 (PorcEducaFormalEdu)     +
                 (PorcEducaFormalSol)     +
                 (PorcFondoEducacion    * (8 / 12))     +
                 (PorcFondoSolidaridad  * (8 / 12))     + 
                 PorcReservaProteccion  +
                 (PorcSeguroDeudor      * (8 / 12))       +
                 (PorcFondoPrevencion   * (8 / 12))     +
                 (PorcFondoSeguroVida   * (8 / 12))     +
                 PorcExcedenteOtras                     +
                 PorcRevalorizaAportes.

          ASSIGN TEval.Bda[6] = TEval.Bda[6]            +
                 (PorcFondoEducacion    * (3 / 12))     +
                 (PorcFondoSolidaridad  * (3 / 12))     + 
                 (PorcFondoPrevencion   * (3 / 12))     +
                 (PorcSeguroDeudor      * (3 / 12))     +
                 (PorcFondoSeguroVida   * (3 / 12)).

          ASSIGN TEval.Bda[5] = TEval.Bda[5]            +
                 (PorcFondoEducacion    * (1 / 12))     +
                 (PorcFondoSolidaridad  * (1 / 12))     + 
                 (PorcFondoPrevencion   * (1 / 12))     +
                 (PorcSeguroDeudor      * (1 / 12))     +
                 (PorcFondoSeguroVida   * (1 / 12)).
      END.
      WHEN 8  THEN DO:
          ASSIGN TEval.Bda[7] = TEval.Bda[7]            +
                 (PorcEducaFormalEdu)     +
                 (PorcEducaFormalSol)     +
                 (PorcFondoEducacion    * (7 / 12))     +
                 (PorcFondoSolidaridad  * (7 / 12))     + 
                 PorcReservaProteccion  +  
                 (PorcSeguroDeudor      * (7 / 12))     +  
                 (PorcFondoPrevencion   * (7 / 12))     +
                 (PorcFondoSeguroVida   * (7 / 12))     +
                 PorcExcedenteOtras                     +
                 PorcRevalorizaAportes.

          ASSIGN TEval.Bda[6] = TEval.Bda[6]            +
                 (PorcFondoEducacion    * (3 / 12))     +
                 (PorcFondoSolidaridad  * (3 / 12))     + 
                 (PorcFondoPrevencion   * (3 / 12))     +
                 (PorcSeguroDeudor      * (3 / 12))     +
                 (PorcFondoSeguroVida   * (3 / 12)).

          ASSIGN TEval.Bda[5] = TEval.Bda[5]            +
                 (PorcFondoEducacion    * (2 / 12))     +
                 (PorcFondoSolidaridad  * (2 / 12))     + 
                 (PorcFondoPrevencion   * (2 / 12))     +
                 (PorcSeguroDeudor      * (2 / 12))     +
                 (PorcFondoSeguroVida   * (2 / 12)).
      END.          
      WHEN 9  THEN DO:
          ASSIGN TEval.Bda[7] = TEval.Bda[7]            +
                 (PorcEducaFormalEdu)     +
                 (PorcEducaFormalSol)     +
                 (PorcFondoEducacion    * (6 / 12))     +
                 (PorcFondoSolidaridad  * (6 / 12))     + 
                 PorcReservaProteccion  +  
                 (PorcSeguroDeudor      * (6 / 12))     +  
                 (PorcFondoPrevencion   * (6 / 12))     +
                 (PorcFondoSeguroVida   * (6 / 12))     +
                 PorcExcedenteOtras                     +
                 PorcRevalorizaAportes.

          ASSIGN TEval.Bda[6] = TEval.Bda[6]            +
                 (PorcFondoEducacion    * (3 / 12))     +
                 (PorcFondoSolidaridad  * (3 / 12))     + 
                 (PorcFondoPrevencion   * (3 / 12))     +
                 (PorcSeguroDeudor      * (3 / 12))     +
                 (PorcFondoSeguroVida   * (3 / 12)).

          ASSIGN TEval.Bda[5] = TEval.Bda[5]            +
                 (PorcFondoEducacion    * (3 / 12))     +
                 (PorcFondoSolidaridad  * (3 / 12))     + 
                 (PorcFondoPrevencion   * (3 / 12))     +
                 (PorcSeguroDeudor      * (3 / 12))     +
                 (PorcFondoSeguroVida   * (3 / 12)).
      END.
      WHEN 10 THEN DO:
          ASSIGN TEval.Bda[7] = TEval.Bda[7]            +
                 (PorcEducaFormalEdu)     +
                 (PorcEducaFormalSol)     +
                 (PorcFondoEducacion    * (5 / 12))     +
                 (PorcFondoSolidaridad  * (5 / 12))     + 
                 PorcReservaProteccion  +  
                 (PorcSeguroDeudor      * (5 / 12))     +  
                 (PorcFondoPrevencion   * (5 / 12))     +
                 (PorcFondoSeguroVida   * (5 / 12))     +
                 PorcExcedenteOtras                     +
                 PorcRevalorizaAportes.

          ASSIGN TEval.Bda[6] = TEval.Bda[6]            +
                 (PorcFondoEducacion    * (3 / 12))     +
                 (PorcFondoSolidaridad  * (3 / 12))     + 
                 (PorcFondoPrevencion   * (3 / 12))     +
                 (PorcSeguroDeudor      * (3 / 12))     +
                 (PorcFondoSeguroVida   * (3 / 12)).

          ASSIGN TEval.Bda[5] = TEval.Bda[5]            +
                 (PorcFondoEducacion    * (3 / 12))     +
                 (PorcFondoSolidaridad  * (3 / 12))     + 
                 (PorcFondoPrevencion   * (3 / 12))     +
                 (PorcSeguroDeudor      * (3 / 12))     +
                 (PorcFondoSeguroVida   * (3 / 12)).
          ASSIGN TEval.Bda[4] = TEval.Bda[4]            +
                 (PorcFondoEducacion    * (1 / 12))     +
                 (PorcFondoSolidaridad  * (1 / 12))     + 
                 (PorcFondoPrevencion   * (1 / 12))     +
                 (PorcSeguroDeudor      * (1 / 12))     +
                 (PorcFondoSeguroVida   * (1 / 12)).
      END.
      WHEN 11 THEN DO:
          ASSIGN TEval.Bda[7] = TEval.Bda[7]            +
                 (PorcEducaFormalEdu)     +
                 (PorcEducaFormalSol)     +
                 (PorcFondoEducacion    * (4 / 12))     +
                 (PorcFondoSolidaridad  * (4 / 12))     + 
                 PorcReservaProteccion  +  
                 (PorcSeguroDeudor      * (4 / 12))     +  
                 (PorcFondoPrevencion   * (4 / 12))     +
                 (PorcFondoSeguroVida   * (4 / 12))     +
                 PorcExcedenteOtras                     +
                 PorcRevalorizaAportes.

          ASSIGN TEval.Bda[6] = TEval.Bda[6]            +
                 (PorcFondoEducacion    * (3 / 12))     +
                 (PorcFondoSolidaridad  * (3 / 12))     + 
                 (PorcFondoPrevencion   * (3 / 12))     +
                 (PorcSeguroDeudor      * (3 / 12))     +
                 (PorcFondoSeguroVida   * (3 / 12)).

          ASSIGN TEval.Bda[5] = TEval.Bda[5]            +
                 (PorcFondoEducacion    * (3 / 12))     +
                 (PorcFondoSolidaridad  * (3 / 12))     + 
                 (PorcFondoPrevencion   * (3 / 12))     +
                 (PorcSeguroDeudor      * (3 / 12))     +
                 (PorcFondoSeguroVida   * (3 / 12)).
          ASSIGN TEval.Bda[4] = TEval.Bda[4]            +
                 (PorcFondoEducacion    * (2 / 12))     +
                 (PorcFondoSolidaridad  * (2 / 12))     + 
                 (PorcFondoPrevencion   * (2 / 12))     +
                 (PorcSeguroDeudor      * (2 / 12))     +
                 (PorcFondoSeguroVida   * (2 / 12)).
      END.
      WHEN 12 THEN DO:
          ASSIGN TEval.Bda[7] = TEval.Bda[7]            +
                 (PorcEducaFormalEdu)     +
                 (PorcEducaFormalSol)     +
                 (PorcFondoEducacion    * (3 / 12))     +
                 (PorcFondoSolidaridad  * (3 / 12))     + 
                 PorcReservaProteccion  +  
                 (PorcSeguroDeudor      * (3 / 12))     +  
                 (PorcFondoPrevencion   * (3 / 12))     +
                 (PorcFondoSeguroVida   * (3 / 12))     +
                 PorcExcedenteOtras                     +
                 PorcRevalorizaAportes.

          ASSIGN TEval.Bda[6] = TEval.Bda[6]            +
                 (PorcFondoEducacion    * (3 / 12))     +
                 (PorcFondoSolidaridad  * (3 / 12))     + 
                 (PorcFondoPrevencion   * (3 / 12))     +
                 (PorcSeguroDeudor      * (3 / 12))     +
                 (PorcFondoSeguroVida   * (3 / 12)).

          ASSIGN TEval.Bda[5] = TEval.Bda[5]            +
                 (PorcFondoEducacion    * (3 / 12))     +
                 (PorcFondoSolidaridad  * (3 / 12))     + 
                 (PorcFondoPrevencion   * (3 / 12))     +
                 (PorcSeguroDeudor      * (3 / 12))     +
                 (PorcFondoSeguroVida   * (3 / 12)).
          ASSIGN TEval.Bda[4] = TEval.Bda[4]            +
                 (PorcFondoEducacion    * (3 / 12))     +
                 (PorcFondoSolidaridad  * (3 / 12))     + 
                 (PorcFondoPrevencion   * (3 / 12))     +
                 (PorcSeguroDeudor      * (3 / 12))     +
                 (PorcFondoSeguroVida   * (3 / 12)).
      END.
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_EvalCont4 W-InfGer 
PROCEDURE Imp_EvalCont4 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFI VAR Vlr0     LIKE Sal_Cuenta.Sal_INICIAL.
  DEFI VAR W_SdoCta LIKE Sal_Cuenta.Sal_INICIAL.
  DEFI VAR TPromAp  LIKE Sal_Cuenta.Sal_INICIAL.
  DEFINE VARIABLE j AS INTEGER     NO-UNDO.


  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "2510",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).

  CASE MONTH(W_FecCorte):
      WHEN 1  THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 2  THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 3  THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 4  THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 5  THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 6  THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 7  THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 8  THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 9  THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 10 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 11 THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 12 THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
  END CASE.
/*   REPEAT j = 1 TO 7:                                   */
/*       ASSIGN TEval.Bda[j] = 0.                         */
/*   END.                                                 */
/*                                                        */
/*                                                        */
/*   CASE MONTH(W_FecCorte):                              */
/*       WHEN 1  THEN ASSIGN TEval.Bda[2] =  + W_SdoCta.  */
/*       WHEN 2  THEN ASSIGN TEval.Bda[1] =  + W_SdoCta.  */
/*       WHEN 3  THEN ASSIGN TEval.Bda[2] =  + W_SdoCta.  */
/*       WHEN 4  THEN ASSIGN TEval.Bda[1] =  + W_SdoCta.  */
/*       WHEN 5  THEN ASSIGN TEval.Bda[2] =  + W_SdoCta.  */
/*       WHEN 6  THEN ASSIGN TEval.Bda[1] =  + W_SdoCta.  */
/*       WHEN 7  THEN ASSIGN TEval.Bda[2] =  + W_SdoCta.  */
/*       WHEN 8  THEN ASSIGN TEval.Bda[1] =  + W_SdoCta.  */
/*       WHEN 9  THEN ASSIGN TEval.Bda[2] =  + W_SdoCta.  */
/*       WHEN 10 THEN ASSIGN TEval.Bda[1] =  + W_SdoCta.  */
/*       WHEN 11 THEN ASSIGN TEval.Bda[2] =  + W_SdoCta.  */
/*       WHEN 12 THEN ASSIGN TEval.Bda[1] =  + W_SdoCta.  */
/*   END CASE.                                            */

  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "259502",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).

  /* may, sep */
  CASE MONTH(W_FecCorte):
      WHEN 1  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 2  THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 3  THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 4  THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 5  THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 6  THEN ASSIGN TEval.Bda[3] = TEval.Bda[3] + W_SdoCta.
      WHEN 7  THEN ASSIGN TEval.Bda[2] = TEval.Bda[2] + W_SdoCta.
      WHEN 8  THEN ASSIGN TEval.Bda[1] = TEval.Bda[1] + W_SdoCta.
      WHEN 9  THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 10 THEN ASSIGN TEval.Bda[5] = TEval.Bda[5] + W_SdoCta.
      WHEN 11 THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
      WHEN 12 THEN ASSIGN TEval.Bda[4] = TEval.Bda[4] + W_SdoCta.
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_Evaluacion W-InfGer 
PROCEDURE Imp_Evaluacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR W_ArchEval AS CHAR FORM "X(35)".
  DEFI VAR Vlr0     LIKE Sal_Cuenta.Sal_INICIAL.
  DEFI VAR W_SdoCta LIKE Sal_Cuenta.Sal_INICIAL.
  DEFI VAR W_SdoBda LIKE Sal_Cuenta.Sal_INICIAL.
  DEFI VAR Tot LIKE Ahorros.Sdo_Dispon EXTENT 8.
  DEFI VAR TTp LIKE Ahorros.Sdo_Dispon EXTENT 8.
  DEFI VAR NN  AS INTEG FORM "99".
  DEFINE VARIABLE W_SumaUltBanda AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE W_TasaEfectiva AS DECIMAL     NO-UNDO.

  ASSIGN W_ArchEval = "C:\InfRed\EvRiesgoL-" + STRING(W_FecCorte,"999999") + ".Txt".
    
  OUTPUT TO VALUE(W_ArchEval). 

  DISPLAY "        " + W_NomEnt " -   EVALUACION DEL RIESGO DE LIQUIDEZ" 
          + "    Fecha " +
          STRING(W_FecCorte,"99/99/9999") FORM "X(120)" SKIP(1)
    WITH DOWN WIDTH 250 FRAME Tit1EvRL NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  FOR EACH TEval WHERE TEval.Clas EQ 1 BY TEval.Reng: 
      ASSIGN Tot [1] = Tot [1] + TEval.Bda [1]
             Tot [2] = Tot [2] + TEval.Bda [2]
             Tot [3] = Tot [3] + TEval.Bda [3]
             Tot [4] = Tot [4] + TEval.Bda [4]
             Tot [5] = Tot [5] + TEval.Bda [5]
             Tot [6] = Tot [6] + TEval.Bda [6]
             Tot [7] = Tot [7] + TEval.Bda [7].
      IF TEval.Reng NE 095 THEN
             Tot [8] = Tot [8] + TEval.Vlr.
      DISPL TEval.Reng    LABEL "RENG"
            TEval.Cpto    LABEL "CONCEPTO"
            TEval.Vlr     LABEL "SALDO A LA FECHA"
            TEval.Bda [1] LABEL "Menor/Igual 1 Mes"    
            TEval.Bda [2] LABEL "> 1 Y <= 2 Meses"
            TEval.Bda [3] LABEL "> 2 Y <= 3 Meses"
            TEval.Bda [4] LABEL "> 3 Y <= 6 Meses"
            TEval.Bda [5] LABEL "> 6 Y <= 9 Meses"
            TEval.Bda [6] LABEL "> 9 Y <= 12 Meses"
            TEval.Bda [7] LABEL "Mayor a 12 Meses"
        WITH DOWN WIDTH 200 FRAME DetEvRL NO-BOX NO-LABELS STREAM-IO USE-TEXT.
  END.

  DISPLAY "T O T A L  POSICIONES ACTIVAS           "
          Tot [8]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [1]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [2]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [3]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [4]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [5]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [6]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [7]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          SKIP(2)
     WITH WIDTH 200 FRAME Tot1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  ASSIGN Tot = 0.

  RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "2105",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas   = 2
         TEval.Reng   = 005
         TEval.Cpto   = "Depósitos de Ahorro a la Vista"
         TEval.Vlr    = W_SdoCta
         TEval.Bda[7] = W_SdoCta.

  RUN AhoVta_PromDiaAno.

  IF TEval.Vlr GT PromedAhV THEN DO:    
     ASSIGN TEval.Bda[7] = PromedAhV
            TEval.Bda[1] = PrAvBda7[1]
            TEval.Bda[2] = PrAvBda7[2]
            TEval.Bda[3] = PrAvBda7[3]
            TEval.Bda[4] = PrAvBda7[4]
            TEval.Bda[5] = PrAvBda7[5]
            TEval.Bda[6] = PrAvBda7[6].

     ASSIGN W_SdoBda = TEval.Bda[7] + TEval.Bda[2] + TEval.Bda[3] + TEval.Bda[4] + TEval.Bda[5] + TEval.Bda[6].  
     IF W_SdoBda GT TEval.Vlr THEN
        TEval.Bda[1]   = (W_SdoBda - TEval.Vlr).
     ELSE TEval.Bda[1] = (TEval.Vlr - W_SdoBda).
  END.     

  RUN HallarSdoMayor   (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "2110",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  
  CREATE TEval.
  ASSIGN TEval.Clas   = 2
         TEval.Reng   = 010
         TEval.Cpto   = "Certificados de Ahorro a Término"
         TEval.Vlr    = W_SdoCta
         TEval.Bda[7] = W_SdoCta.

  ASSIGN W_SumaUltBanda = 0.
  FOR EACH TBanda WHERE SUBSTRING(TBanda.Id-A,1,5) EQ "1-ATE" 
                     BREAK BY TBanda.Banda:
        TEval.Bda[TBanda.Banda] = (TBanda.SdoK + TBanda.SdoI) * W_Tasreno / 100.
        IF TBanda.Banda LT 7 THEN W_SumaUltBanda = W_SumaUltBanda + TEval.Bda[TBanda.Banda].
  END.  
  RUN NVEF IN W_ManFin (INPUT TBanda.Tasa / 1200, 12, OUTPUT W_TasaEfectiva) NO-ERROR.
  ASSIGN TEval.Bda[7] = (W_SdoCta - W_SumaUltBanda) + ((W_SdoCta - W_SumaUltBanda) * (W_TasaEfectiva / 100)) .
/*   ASSIGN TEval.Bda[7] = (W_SdoCta - W_SumaUltBanda).  */


  /* contractual */
  RUN HallarSdoMayor   (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "2125",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas   = 2
         TEval.Reng   = 015
         TEval.Cpto   = "Depósitos de Ahorro Contractual"
         TEval.Vlr    = W_SdoCta
         TEval.Bda[7] = W_SdoCta.
  FOR EACH TBanda WHERE SUBSTRING(TBanda.Id-A,1,5) EQ "1-ACO" 
                     BREAK BY TBanda.Banda:
      TEval.Bda[TBanda.Banda] = TBanda.SdoK + TBanda.SdoI.
  END.

  RUN Proy_RecContractual.

  DO NN = 1 TO 6:
     TEval.Bda[NN] = TEval.Bda[NN] - ProyContr[NN].
  END.
  /* permanetes */
  /* contractual */
  RUN HallarSdoMayor   (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "2130",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas   = 2
         TEval.Reng   = 020
         TEval.Cpto   = "Depósitos de Ahorro Permanente"
         TEval.Vlr    = W_SdoCta
         TEval.Bda[7] = W_SdoCta.
  
  /* compromiso de recompra */
  /* contractual */
  RUN HallarSdoMayor   (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "2205",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas   = 2
         TEval.Reng   = 025
         TEval.Cpto   = "Depósitos de Ahorro Contractual"
         TEval.Vlr    = W_SdoCta
         TEval.Bda[7] = W_SdoCta.
  /*recompra cartera negociada */
  /* contractual */
  RUN HallarSdoMayor   (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "2210",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas   = 2
         TEval.Reng   = 027
         TEval.Cpto   = "RECOMPRA CARTERA NEGOCIADA"
         TEval.Vlr    = W_SdoCta
         TEval.Bda[7] = W_SdoCta.
  /*obligaciones financieras */
  RUN HallarSdoMayor  (INPUT 1, INPUT 20,
                                  INPUT 999, INPUT 999, INPUT "23",          
                                  INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                  OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  CREATE TEval.
  ASSIGN TEval.Clas   = 2
         TEval.Reng   = 030
         TEval.Cpto   = "Créditos de Bco.y Oblig.Financieras"
         TEval.Vlr    = W_SdoCta.
    
  RUN HallarSdoMayor  (INPUT 1, INPUT 20, INPUT 999, INPUT 999, INPUT "230505" /*cta*/, INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1, OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  TEval.Bda[1] = W_SdoCta.
  RUN HallarSdoMayor  (INPUT 1, INPUT 20, INPUT 999, INPUT 999, INPUT "230525" /*cta*/, INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1, OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
  TEval.Bda[7] = W_SdoCta.

  RUN Imp_EvalCont1.
    
  RUN Imp_EvalCont2.    

  FOR EACH TEval WHERE TEval.Clas EQ 2 BY TEval.Reng: 
      ASSIGN Tot [1] = Tot [1] + TEval.Bda [1]
             Tot [2] = Tot [2] + TEval.Bda [2]
             Tot [3] = Tot [3] + TEval.Bda [3]
             Tot [4] = Tot [4] + TEval.Bda [4]
             Tot [5] = Tot [5] + TEval.Bda [5]
             Tot [6] = Tot [6] + TEval.Bda [6]
             Tot [7] = Tot [7] + TEval.Bda [7].
      IF TEval.Reng NE 065 THEN
             Tot [8] = Tot [8] + TEval.Vlr.
      DISPL TEval.Reng   FORM ">999" 
            TEval.Cpto    
            TEval.Vlr     
            TEval.Bda [1]     
            TEval.Bda [2] 
            TEval.Bda [3] 
            TEval.Bda [4] 
            TEval.Bda [5] 
            TEval.Bda [6] 
            TEval.Bda [7] 
        WITH DOWN WIDTH 200 FRAME DetEvRLPas NO-BOX NO-LABELS STREAM-IO USE-TEXT.
  END.

  DO NN = 1 TO 8:
     ASSIGN TTp[NN] = Tot[NN].
  END.
  
  DISPLAY "T O T A L  POSICIONES PASIVAS           "
          Tot [8]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [1]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [2]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [3]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [4]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [5]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [6]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [7]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          SKIP(2)
     WITH WIDTH 200 FRAME TotPas1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  ASSIGN Tot = 0.
    
  RUN Imp_EvalCont3.
    
  FOR EACH TEval WHERE TEval.Clas EQ 3 BY TEval.Reng: 
      ASSIGN Tot [1] = Tot [1] + TEval.Bda [1]
             Tot [2] = Tot [2] + TEval.Bda [2]
             Tot [3] = Tot [3] + TEval.Bda [3]
             Tot [4] = Tot [4] + TEval.Bda [4]
             Tot [5] = Tot [5] + TEval.Bda [5]
             Tot [6] = Tot [6] + TEval.Bda [6]
             Tot [7] = Tot [7] + TEval.Bda [7]
             Tot [8] = Tot [8] + TEval.Vlr.
      DISPL TEval.Reng   FORM ">999" 
            TEval.Cpto    
            TEval.Vlr     
            TEval.Bda [1]     
            TEval.Bda [2] 
            TEval.Bda [3] 
            TEval.Bda [4] 
            TEval.Bda [5] 
            TEval.Bda [6] 
            TEval.Bda [7] 
        WITH DOWN WIDTH 200 FRAME DetEvRLPatt NO-BOX NO-LABELS STREAM-IO USE-TEXT.
  END.
    
  DISPLAY "T O T A L  POSICIONES DEL PATRIMONIO    "
          Tot [8]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [1]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [2]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [3]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [4]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [5]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [6]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          Tot [7]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          SKIP(1)
     WITH WIDTH 200 FRAME TotPas1Pat NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  DO NN = 1 TO 8:
     ASSIGN TTp[NN] = TTp[NN] + Tot[NN].
  END.

  DISPLAY "TOTAL POSICIONES DEL PASIVO Y PATRIMONIO"
          TTp [8]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          TTp [1]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          TTp [2]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          TTp [3]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          TTp [4]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          TTp [5]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          TTp [6]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          TTp [7]  FORM "->>>>,>>>,>>>,>>9"  LABEL "-----------------"
          SKIP(2)
     WITH WIDTH 200 FRAME TotPas1Pat NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  ASSIGN Tot = 0
         TTp = 0.

  OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_ProxVctos W-InfGer 
PROCEDURE Imp_ProxVctos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR T1    AS DEC FORM "->>>,>>>,>>>,>>9" INIT 0 EXTENT 2. 
  DEFI VAR T2    AS DEC FORM "->>>,>>>,>>>,>>9" INIT 0 EXTENT 2. 
  DEFI VAR W_Mes AS CHAR FORM "99".
  DEFI VAR W_Ano AS CHAR FORM "9999".

  DEFI VAR TT  AS DEC FORM "9999999999999.9999999".
  DEFI VAR TN  AS INTEG FORM "999999999".
  DEFI VAR TTT AS DEC FORM "9999999999999.9999999".
  DEFI VAR TTN AS INTEG FORM "999999999".
    
  /*W_Msaje:SCREEN-VALUE IN FRAME F_RiesgoL = "Generando El Archivo con los Próximos Vencimientos...C:\InfRed\ProxVctos-FECHA.Txt".*/

  FOR EACH TMes WHERE TMes.Banda LT 8:
      W_Mes = SUBSTRING(STRING(TMes.AMes),5,2).
      IF W_Mes GT "12" THEN
         ASSIGN TMes.Banda = (TMes.AMes - FecIni) + 1
                W_Mes      = STRING((INTEG(W_Mes) - 12),"99")
                W_Ano      = STRING(INTEG(SUBSTRING(STRING(TMes.AMes),1,4)) + 1)
                TMes.AMes  = INTEG(STRING(W_Ano,"9999") + STRING(W_Mes,"99")).
      ELSE IF INTEG(SUBSTRING(STRING(TMes.AMes),1,4)) GT AnoIni THEN
         TMes.Banda = (TMes.AMes - 88 - FecIni) + 1.
      ELSE   
         TMes.Banda = (TMes.AMes - FecIni) + 1.

      IF TMes.Banda LE 1 OR TMes.Banda EQ ? THEN 
         TMes.Banda = 1.
      ELSE IF TMes.Banda EQ 2 THEN
         TMes.Banda = 2.
      ELSE IF TMes.Banda EQ 3 THEN
         TMes.Banda = 3.
      ELSE IF TMes.Banda LE 6 THEN
         TMes.Banda = 4.
      ELSE IF TMes.Banda LE 9 THEN
         TMes.Banda = 5.
      ELSE IF TMes.Banda LE 12 THEN
         TMes.Banda = 6.
      ELSE IF TMes.Banda GT 12 THEN
         TMes.Banda = 7.
  END.

  DISPLAY  "  " + W_NomEnt + " -   Próximos Vencimientos de Ahorros y Créditos    Desde " +
          STRING(W_FecCorte,"99/99/9999") FORM "X(120)" SKIP(1)
       WITH DOWN WIDTH 250 FRAME Tit1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  /*El TMes.Ta = 3,(cdats) fue reemplazado por el 5*/
  FOR EACH TMes WHERE TMes.Tipo EQ "A" AND TMes.Ta NE 3 BREAK BY TMes.Tipo BY TA BY AMes:
      IF FIRST-OF(TMes.TA) THEN
         RUN Tit_ProxVcto.
               
      DISPLAY TMes.Tipo   LABEL "T"
              TMes.TA     LABEL "C"
              TMes.AMes   LABEL "Año-Mes Vcto"
              TMes.SdoK   LABEL "Valores de Capital"
              TMes.SdoI   LABEL "Vlrs. de Intereses"
              TMes.Nro    LABEL "No.Vctos"
              TMes.Tasa / TMes.Nro   LABEL "Tasa Promedio" FORM "->99.999"              
         WITH DOWN WIDTH 250 FRAME Det1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

      ASSIGN T1[1] = T1[1] + TMes.SdoK
             T1[2] = T1[2] + TMes.SdoI
             TT    = TT + (TMes.Tasa / TMes.Nro)
             TN    = TN + 1.

      IF LAST-OF(TMes.TA) THEN DO:
         DISPLAY "                               "
                 T1[1]
                 T1[2]
                 "             "
                 TT / TN FORM "->99.999"
                 SKIP(2) WITH NO-LABELS.

         ASSIGN T1[1] = 0
                T1[2] = 0
                TT    = 0
                TN    = 0.
      END.
  END.

  ASSIGN T1[1] = 0
         T1[2] = 0
         TT    = 0
         TN    = 0.

  /*Continua el Resumen por bandas*/
  DISPLAY SKIP(2)
            "   Resumen Bandas de Ahorros(Contractuales)"
            SKIP(1)
         WITH DOWN WIDTH 250 FRAME Tit1BC NO-BOX NO-LABELS STREAM-IO USE-TEXT. /*Solo Contractuales*/

    FOR EACH TMes WHERE TMes.Tipo EQ "A" AND TMes.Ta EQ 2 BREAK BY TMes.Banda:
        ASSIGN T1[1] = T1[1] + TMes.SdoK
               T1[2] = T1[2] + TMes.SdoI
               T2[1] = T2[1] + TMes.SdoK
               T2[2] = T2[2] + TMes.SdoI
               TT    = TT    + (TMes.Tasa / TMes.Nro)
               TTT   = TTT   + (TMes.Tasa / TMes.Nro)
               TN    = TN    + 1
               TTN   = TTN   + 1.

        IF LAST-OF(TMes.Banda) THEN DO:
           DISPLAY "1-ACO"     LABEL "ID.A"
                   TMes.Tipo   LABEL "T"
                   TMes.Banda  LABEL "Banda"
                   T1[1]       LABEL "Valores de Capital"
                   T1[2]       LABEL "Vlrs. de Intereses"
                   TT / TN     LABEL "Tasa Promedio" FORM "->99.999"
              WITH DOWN WIDTH 250 FRAME Det1BC NO-BOX NO-LABELS STREAM-IO USE-TEXT.
           ASSIGN T1[1] = 0 
                  T1[2] = 0 
                  TT    = 0 
                  TN    = 0.
        END.
    END.

    DISPLAY SKIP(1)
            "Tot.Ahorros     " 
            T2[1] 
            "  "
            T2[2]
            "    "
            TTT / TTN FORM "->99.999"
        WITH DOWN WIDTH 250 FRAME Tit1TBC NO-BOX NO-LABELS STREAM-IO USE-TEXT. 

    ASSIGN T1[1] = 0 
           T1[2] = 0 
           TT    = 0 
           TN    = 0
           T2[1] = 0 
           T2[2] = 0 
           TTT   = 0 
           TTN   = 0.


    DISPLAY SKIP(2)
              "   Resumen Bandas de Ahorros(A-TNO CDATs)"
              SKIP(1)
           WITH DOWN WIDTH 250 FRAME Tit1BA NO-BOX NO-LABELS STREAM-IO USE-TEXT. /*Solo A-Termino*/

      FOR EACH TMes WHERE TMes.Tipo EQ "A" AND TMes.Ta EQ 5 BREAK BY TMes.Banda:
          ASSIGN T1[1] = T1[1] + TMes.SdoK
                 T1[2] = T1[2] + TMes.SdoI
                 T2[1] = T2[1] + TMes.SdoK
                 T2[2] = T2[2] + TMes.SdoI
                 TT    = TT    + (TMes.Tasa / TMes.Nro)
                 TTT   = TTT   + (TMes.Tasa / TMes.Nro)
                 TN    = TN    + 1
                 TTN   = TTN   + 1.

          IF LAST-OF(TMes.Banda) THEN DO:
             DISPLAY "1-ATE"     LABEL "ID.A"
                     TMes.Tipo   LABEL "T"
                     TMes.Banda  LABEL "Banda"
                     T1[1]       LABEL "Valores de Capital"
                     T1[2]       LABEL "Vlrs. de Intereses"
                     TT / TN     LABEL "Tasa Promedio" FORM "->99.999"
                WITH DOWN WIDTH 250 FRAME Det1BA NO-BOX NO-LABELS STREAM-IO USE-TEXT.
             ASSIGN T1[1] = 0 
                    T1[2] = 0 
                    TT    = 0 
                    TN    = 0.
          END.
      END.

      DISPLAY SKIP(1)
              "Tot.Ahorros" 
              T2[1] 
              " "
              T2[2]
              "    "
              TTT / TTN FORM "->99.999"
          WITH DOWN WIDTH 250 FRAME Tit1TBA NO-BOX NO-LABELS STREAM-IO USE-TEXT. 

      ASSIGN T1[1] = 0 
             T1[2] = 0 
             TT    = 0 
             TN    = 0
             T2[1] = 0 
             T2[2] = 0 
             TTT   = 0 
             TTN   = 0.


  DISPLAY SKIP(2)
          "   Resumen Bandas de Ahorros. (Contractuales + A-Termino)"
          SKIP(1)
       WITH DOWN WIDTH 250 FRAME Tit1B NO-BOX NO-LABELS STREAM-IO USE-TEXT. /*Solo Contractuales y A-Termino*/

  FOR EACH TMes WHERE TMes.Tipo EQ "A" AND (TMes.Ta EQ 2 OR 
                                            TMes.Ta EQ 5) BREAK BY TMes.Banda:
      ASSIGN T1[1] = T1[1] + TMes.SdoK
             T1[2] = T1[2] + TMes.SdoI
             T2[1] = T2[1] + TMes.SdoK
             T2[2] = T2[2] + TMes.SdoI
             TT    = TT    + (TMes.Tasa / TMes.Nro)
             TTT   = TTT   + (TMes.Tasa / TMes.Nro)
             TN    = TN    + 1
             TTN   = TTN   + 1.

      IF LAST-OF(TMes.Banda) THEN DO:
         DISPLAY TMes.Tipo   LABEL "T"
                 TMes.Banda  LABEL "Banda"
                 T1[1]       LABEL "Valores de Capital"
                 T1[2]       LABEL "Vlrs. de Intereses"
                 TT / TN     LABEL "Tasa Promedio" FORM "->99.999"
            WITH DOWN WIDTH 250 FRAME Det1B NO-BOX NO-LABELS STREAM-IO USE-TEXT.
         ASSIGN T1[1] = 0 
                T1[2] = 0 
                TT    = 0 
                TN    = 0.
      END.
  END.

  DISPLAY SKIP(1)
          "Tot.Ahorros" 
          T2[1] 
          " "
          T2[2]
          "    "
          TTT / TTN FORM "->99.999"
      WITH DOWN WIDTH 250 FRAME Tit1TB NO-BOX NO-LABELS STREAM-IO USE-TEXT. 
 
  ASSIGN T1[1] = 0 
         T1[2] = 0 
         TT    = 0 
         TN    = 0
         T2[1] = 0 
         T2[2] = 0 
         TTT   = 0 
         TTN   = 0.
    
  /*Créditos Vctos 12 meses*/
  DEF VAR W-Tipo AS CHAR FORMAT "X(20)".
  DEF VAR W-Dett AS CHAR FORMAT "X(5)".
  DISPLAY SKIP(2)
          "     CREDITOS"
          SKIP(1)
      WITH DOWN WIDTH 250 FRAME Tit2bCCCRB NO-BOX NO-LABELS STREAM-IO USE-TEXT.  
    
  FOR EACH TMes WHERE TMes.Tipo EQ "C" BREAK BY AMes:
      DISPLAY TMes.Tipo   LABEL "T"
              TMes.TA     LABEL "C"
              TMes.AMes   LABEL "Año-Mes Vcto"
              TMes.SdoK   LABEL "Valores de Capital"
              TMes.SdoI   LABEL "Vlrs. de Intereses"
              TMes.Nro    LABEL "No.Vctos"
              TMes.Tasa / TMes.Nro   LABEL "Tasa Promedio" FORM "->99.999"
         WITH DOWN WIDTH 250 FRAME Det1Cr NO-BOX NO-LABELS STREAM-IO USE-TEXT.

      ASSIGN T1[1] = T1[1] + TMes.SdoK
             T1[2] = T1[2] + TMes.SdoI
             TT    = TT + (TMes.Tasa / TMes.Nro)
             TN    = TN + 1.      
  END.

  DISPLAY "                               "
          T1[1]                  
          T1[2]                         
          "            "                     
          TT / TN FORM "->99.999"       
          SKIP(2) WITH NO-LABELS.       

  ASSIGN T1[1] = 0 
         T1[2] = 0        
         TT    = 0        
         TN    = 0.        

  DISPLAY SKIP(2)
          "   Resumen Bandas de Créditos."
          SKIP(1)
      WITH DOWN WIDTH 250 FRAME Tit2bCC NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  FOR EACH TMes WHERE TMes.Tipo EQ "C" BREAK BY TMes.Banda:
       ASSIGN T1[1] = T1[1] + TMes.SdoK
              T1[2] = T1[2] + TMes.SdoI
              T2[1] = T2[1] + TMes.SdoK
              T2[2] = T2[2] + TMes.SdoI
              TT    = TT    + (TMes.Tasa / TMes.Nro)
              TN    = TN    + 1
              TTT   = TTT   + (TMes.Tasa / TMes.Nro)
              TTN   = TTN   + 1.

      IF LAST-OF(TMes.Banda) THEN DO:
         DISPLAY TMes.Tipo   LABEL "T"
                 TMes.Banda  LABEL "Banda"
                 T1[1]       LABEL "Valores de Capital"
                 T1[2]       LABEL "Vlrs. de Intereses"
                 TT / TN     LABEL "Tasa Promedio" FORM "->99.999"
                 TMes.Comen  LABEL "Comentario"
            WITH DOWN WIDTH 250 FRAME Det1BCC NO-BOX NO-LABELS STREAM-IO USE-TEXT.
         ASSIGN T1[1] = 0 
                T1[2] = 0 
                TT    = 0 
                TN    = 0.
      END.
  END.

  DISPLAY SKIP(1)
          "Tot.Creditos     " 
          T2[1]  
          "  "
          T2[2]
          "   "
          TTT / TTN FORM "->99.999"
      WITH DOWN WIDTH 250 FRAME Tit1TBCrC NO-BOX NO-LABELS STREAM-IO USE-TEXT. 
 
  ASSIGN T1[1] = 0 
         T1[2] = 0 
         TT    = 0 
         TN    = 0
         T2[1] = 0 
         T2[2] = 0 
         TTT   = 0 
         TTN   = 0.


  FOR EACH TMes WHERE TMes.Tipo EQ "C" BREAK BY TMes.TA BY TMes.Banda:
      IF FIRST-OF(TMes.TA) THEN DO:
         CASE TMes.TA:
             WHEN 1 THEN ASSIGN W-Dett = "1-CCO"  W-Tipo = "CONSUMO).". 
             WHEN 2 THEN ASSIGN W-Dett = "1-CCI"  W-Tipo = "COMERCIAL).".
             WHEN 3 THEN ASSIGN W-Dett = "1-CVH"  W-Tipo = "HIPOTECARIO).".
             WHEN 4 THEN ASSIGN W-Dett = "1-CMI"  W-Tipo = "MICROCREDITO).".
             WHEN 6 THEN ASSIGN W-Dett = "1-CEM"  W-Tipo = "EMPLEADOS).".
               OTHERWISE ASSIGN W-Dett = "1-CXX"  W-Tipo = "NO DEFINIDA: " + STRING(TMes.TA).
         END CASE.
         DISPLAY SKIP(2) "   Resumen Bandas de Créditos(Por Tipo-Credito "  W-Tipo  
                 SKIP(1) WITH DOWN WIDTH 250 FRAME Tit2bCCC NO-BOX NO-LABELS STREAM-IO USE-TEXT.
      END.
       ASSIGN T1[1] = T1[1] + TMes.SdoK
              T1[2] = T1[2] + TMes.SdoI
              T2[1] = T2[1] + TMes.SdoK
              T2[2] = T2[2] + TMes.SdoI
              TT    = TT    + (TMes.Tasa / TMes.Nro)
              TN    = TN    + 1
              TTT   = TTT   + (TMes.Tasa / TMes.Nro)
              TTN   = TTN   + 1.

      IF LAST-OF(TMes.Banda) THEN DO:
         DISPLAY W-Dett     LABEL "ID.A"
                 TMes.Tipo   LABEL "T"
                 TMes.Banda  LABEL "Banda"
                 T1[1]       LABEL "Valores de Capital"
                 T1[2]       LABEL "Vlrs. de Intereses"
                 TT / TN     LABEL "Tasa Promedio" FORM "->99.999"
            WITH DOWN WIDTH 250 FRAME Det1BCCC NO-BOX NO-LABELS STREAM-IO USE-TEXT.
         ASSIGN T1[1] = 0    T1[2] = 0 
                TT    = 0    TN    = 0.
      END.
      IF LAST-OF(Tmes.TA) THEN DO:
          DISPLAY SKIP(1) "Tot.Creditos    "  T2[1]   "  "
                  T2[2]   "    "           TTT / TTN FORM "->99.999"
              WITH DOWN WIDTH 250 FRAME Tit1TBCrCC NO-BOX NO-LABELS STREAM-IO USE-TEXT. 

          ASSIGN T1[1] = 0   T1[2] = 0   TT  = 0    TN  = 0
                 T2[1] = 0   T2[2] = 0   TTT = 0    TTN = 0.
      END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_Ejecutivo W-InfGer 
PROCEDURE Inf_Ejecutivo :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
   DEFI VAR Vlr0      LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR W_SdoCta  LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR W_SdoCta1 LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR W_SdoCta2 LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR W_SdoCta3 LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR TCar      LIKE Ahorros.Sdo_Dispon INIT 0 EXTENT 3 FORM "->>>>>>,>>>,>>>,>>9".
   DEFI VAR TFin      LIKE Ahorros.Sdo_Dispon INIT 0 EXTENT 3 FORM "->>>>>>,>>>,>>>,>>9".
   DEFI VAR PorC1     AS DEC FORM "-99999.99".
   DEFI VAR PorC2     AS DEC FORM "-99999.99".
   DEFI VAR Arch      AS CHAR FORM "X(35)".
   DEFI VAR FecAnt    AS DATE.
   DEFI VAR FecDiciem AS DATE.
    
   FOR EACH Tof: DELETE Tof. END.
    
   ASSIGN Arch      = "C:\InfRed\IEjecuti-" + STRING(W_FecCorte,"999999") + "_" + W_Usuario
          FecAnt    = DATE(MONTH(W_FecCorte),DAY(W_FecCorte),YEAR(W_FecCorte) - 1)
          FecDiciem = DATE(12,31,YEAR(W_FecCorte) - 1).

   CREATE Tof.                      
   ASSIGN TOf.Cta     = "11 menos 1120"  
          TOf.Nom     = "CAJA Y BANCOS". 
   RUN Halla_SdoCta(INPUT "11").   
   RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 0, INPUT 999, INPUT "1120",   
                                   INPUT W_AnoAct, INPUT W_MesAct,INPUT 1,    
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   Tof.Sdos[1] = Tof.Sdos[1] - W_SdoCta.   
   RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 0, INPUT 999, INPUT "1120",   
                                   INPUT W_AnoAct - 1, INPUT W_MesAct,INPUT 1,    
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   Tof.Sdos[2] = Tof.Sdos[2] - W_SdoCta.   
   RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 0, INPUT 999, INPUT "1120",   
                                   INPUT W_AnoAct - 1, INPUT 12,INPUT 1,    
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   Tof.Sdos[3] = Tof.Sdos[3] - W_SdoCta.

   CREATE Tof.                      
   ASSIGN TOf.Cta     = "1120+1203"  
          TOf.Nom     = "FONDO DE LIQUIDEZ".
   RUN Halla_SdoCta(INPUT "1120").
   RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 0, INPUT 999, INPUT "1203",   
                                   INPUT W_AnoAct, INPUT W_MesAct,INPUT 1,    
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   Tof.Sdos[1] = Tof.Sdos[1] + W_SdoCta.
   RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 0, INPUT 999, INPUT "1203",   
                                   INPUT W_AnoAct - 1, INPUT W_MesAct,INPUT 1,    
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   Tof.Sdos[2] = Tof.Sdos[2] + W_SdoCta.
   RUN HallarSdoMayor  /* IN W_Manija */ (INPUT 1, INPUT 20,
                                   INPUT 0, INPUT 999, INPUT "1203",   
                                   INPUT W_AnoAct - 1, INPUT 12,INPUT 1,    
                                   OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
   Tof.Sdos[3] = Tof.Sdos[3] + W_SdoCta.

   CREATE Tof.                      
   ASSIGN TOf.Cta     = "14 menos Provis"  
          TOf.Nom     = "CARTERA".
   RUN Halla_Provis (OUTPUT W_SdoCta1, OUTPUT W_SdoCta2, OUTPUT W_SdoCta3).
   RUN Halla_SdoCta (INPUT "14").
   
   ASSIGN Tof.Sdos[1] = Tof.Sdos[1] - W_SdoCta1
          Tof.Sdos[2] = Tof.Sdos[2] - W_SdoCta2
          Tof.Sdos[3] = Tof.Sdos[3] - W_SdoCta3.
   
   CREATE Tof.                      
   ASSIGN TOf.Cta     = "1489 Provis"  
          TOf.Nom     = "PROVISION CARTERA"
          Tof.Sdos[1] = W_SdoCta1
          Tof.Sdos[2] = W_SdoCta2
          Tof.Sdos[3] = W_SdoCta3.

   CREATE Tof.                      
   ASSIGN TOf.Cta     = "1999 Activos"  
          TOf.Nom     = "TOTAL ACTIVOS".
   RUN Halla_SdoCta(INPUT "1").
   
   CREATE Tof.                      
   ASSIGN TOf.Cta     = "2105"  
          TOf.Nom     = "MULTIDIARIO".
   RUN Halla_SdoCta(INPUT "2105").

   CREATE Tof.                      
   ASSIGN TOf.Cta     = "2110"  
          TOf.Nom     = "C D A T".
   RUN Halla_SdoCta(INPUT "2110").

   CREATE Tof.                      
   ASSIGN TOf.Cta     = "2125"  
          TOf.Nom     = "CONTRACTUAL".
   RUN Halla_SdoCta(INPUT "2125").

   CREATE Tof.                      
   ASSIGN TOf.Cta     = "23"  
          TOf.Nom     = "OBLIGACIONES FINANCIERAS".
   RUN Halla_SdoCta(INPUT "23").

   CREATE Tof.                      
   ASSIGN TOf.Cta     = "3105"  
          TOf.Nom     = "APORTES SOCIALES".
   RUN Halla_SdoCta(INPUT "3105").

   CREATE Tof.                      
   ASSIGN TOf.Cta     = "4    Ingresos"  
          TOf.Nom     = "INGRESOS TOTALES".
   RUN Halla_SdoCta(INPUT "4").
   ASSIGN W_TotIng [1] = Tof.Sdos[1]
          W_TotIng [2] = Tof.Sdos[2]
          W_TotIng [3] = Tof.Sdos[3].

   CREATE Tof.                      
   ASSIGN TOf.Cta     = "5105"  
          TOf.Nom     = "GASTOS DE PERSONAL".
   RUN Halla_SdoCta(INPUT "5105").

   CREATE Tof.                      
   ASSIGN TOf.Cta     = "6150 - Inter"  
          TOf.Nom     = "INTERESES AHORRO/DATACREDITO".
   RUN Halla_Inter (OUTPUT Tof.Sdos[1], OUTPUT Tof.Sdos[2], OUTPUT Tof.Sdos[3]).

   CREATE Tof.                      
   ASSIGN TOf.Cta     = "654 - PyG"  
          TOf.Nom     = "RESULTADO DEL EJERCICIO".
   RUN Halla_PyG (OUTPUT Tof.Sdos[1], OUTPUT Tof.Sdos[2], OUTPUT Tof.Sdos[3]).
                    
   OUTPUT TO VALUE(Arch). 

   DISPLAY "        " + W_NomEnt + "  I N F O R M E    E J E C U T I V O   A   "
          + STRING(W_FecCorte,"99/99/9999") FORM "X(120)"
         SKIP(2)
          "          C U E N T A S                 " +
          STRING(W_FecCorte,"99/99/9999") + "          " + STRING(FecAnt,"99/99/9999") + "          " +
          STRING(FecDiciem,"99/99/9999")  + " VARIACION MES VARIACION DIC" FORM "X(150)" SKIP
          "______________________________ ___________________ ___________________ ___________________ _____________ _____________"
        WITH DOWN WIDTH 200 FRAME Enc NO-BOX NO-LABELS STREAM-IO USE-TEXT.

   FOR EACH Tof:
       ASSIGN Porc1 = ((Tof.Sdos[1] / Tof.Sdos[2]) - 1) * 100  
              Porc2 = ((Tof.Sdos[1] / Tof.Sdos[3]) - 1) * 100. 

       DISPLAY TOf.Nom
               Tof.Sdos[1]
               Tof.Sdos[2]
               Tof.Sdos[3]
               " "
               Porc1  FORM "->>>>9.99 %"
               " "
               Porc2  FORM "->>>>9.99 %" SKIP
               "______________________________ ___________________ ___________________ ___________________ _____________ _____________"
               SKIP(0)
 
           WITH DOWN WIDTH 200 FRAME Det NO-BOX NO-LABELS STREAM-IO USE-TEXT.
   END.

   OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_PJurid W-InfGer 
PROCEDURE Inf_PJurid :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR TRet   LIKE Mov_Contable.db.
  DEFI VAR Arch   AS CHAR FORM "X(35)".
  DEFI VAR TipIde AS CHAR FORM "X(20)".

  ASSIGN Arch = "C:\InfRed\IPsJurid-" + STRING(W_FecCorte,"999999") + "_" + W_Usuario.
  OUTPUT TO VALUE(Arch). 

  DISPLAY "            "   +  W_NomEnt +  "     I N F O R M E  D E  PERSONAS JURIDICAS - ReteFuente  "
          + STRING(W_FecCorte,"99/99/9999") FORM "X(120)"
         SKIP(1)
          "     RETEFUENTE X RDTOS-FINANCIEROS DESDE " + STRING(W_FecIniP,"99/99/9999") + " HASTA " + 
                                                         STRING(W_FecFinP,"99/99/9999") FORM "X(120)"
         SKIP(2)                                                  
     WITH DOWN WIDTH 200 FRAME Enc NO-BOX NO-LABELS STREAM-IO USE-TEXT.

/*   FOR EACH Clientes WHERE NOT Clientes.Tipo_Persona NO-LOCK BY Clientes.Nit:                                                */
/*       FIND FIRST Clientes WHERE Clientes.Nit    EQ Clientes.Nit                                                             */
/*                             AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.                                                      */
/*       IF NOT AVAIL(Clientes) THEN                                                                                           */
/*          NEXT.                                                                                                              */
/*                                                                                                                             */
/*       ASSIGN TipIde = "ERROR EN TIPO-IDENT."                                                                                */
/*              TRet   = 0.                                                                                                    */
/*                                                                                                                             */
/*       FOR EACH Mov_Contab WHERE Mov_Contab.Nit        EQ Clientes.Nit                                                       */
/*                             AND Mov_Contab.Cuenta     EQ "244535"                                                           */
/*                             AND Mov_Contab.Fec_contab GE W_FecIniP                                                          */
/*                             AND Mov_Contab.Fec_contab LE W_FecFinP NO-LOCK:                                                 */
/*           IF Mov_Contable.Cr GT 0 THEN                                                                                      */
/*              TRet = TRet + Mov_Contable.cr.                                                                                 */
/*           ELSE                                                                                                              */
/*              TRet = TRet - Mov_Contable.dB.                                                                                 */
/*       END.                                                                                                                  */
/*                                                                                                                             */
/*       IF Clientes.Tipo_Identificacion GE 1 AND Clientes.Tipo_Identificacion LE 10 THEN                                      */
/*          TipIde = Wk_TipoDocto[Clientes.Tipo_Identificacion].                                                               */
/*                                                                                                                             */
/*       DISPL Clientes.Ofic   LABEL "Suc."                                                                                    */
/*             Clientes.Nit                     LABEL "Ced./Nit"                                                               */
/*             Clientes.Dig_Verificacion        LABEL "D.V"                                                                    */
/*             TipIde                           LABEL "T.Identificación"                                                       */
/*             Clientes.Tipo_Persona            LABEL "T.Psna"                                                                 */
/*             TRIM(Clientes.Nombre) + " " + TRIM(REPLACE(Clientes.Apellido,"_"," ")) LABEL "Nombre-Razón Social" FORM "X(40)" */
/*             Clientes.Dir_Comercial           LABEL "Dirección Comercial"                                                    */
/*             Clientes.Tel_Comercial           LABEL "Teléfono"                                                               */
/*             Clientes.Id_Retencion            LABEL "Id.RetFte"                                                              */
/*             TRet                             LABEL "Vr.RetFte Rend-Finac"                                                   */
/*           WITH DOWN WIDTH 250 FRAME F1 USE-TEXT STREAM-IO NO-LABELS NO-BOX.                                                 */
/*   END.                                                                                                                      */

  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_Proveed W-InfGer 
PROCEDURE Inf_Proveed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR Arch   AS CHAR FORM "X(35)".
  DEFI VAR TipIde AS CHAR FORM "X(20)".

  FOR EACH TProvee: DELETE TProvee. END.

  FOR EACH Mov_Contab WHERE Mov_Contab.Cuenta EQ "243505" NO-LOCK
                      BREAK BY Mov_Contab.Nit BY Mov_Contab.Fec_contab DESCEND:
      IF FIRST-OF(Mov_Contab.Nit) THEN DO:
         IF TODAY - 1080 LE Mov_Contab.Fec_contab THEN DO:
            CREATE TProvee.
            ASSIGN TProvee.CedNit = Mov_Contab.Nit
                   TProvee.Suc    = Mov_Contab.Agencia
                   TProvee.Fec    = Mov_Contab.Fec_contab.
         END.
      END.
  END.

  ASSIGN Arch = "C:\InfRed\IProveed-" + STRING(W_FecCorte,"999999") + "_" + W_Usuario.
  OUTPUT TO VALUE(Arch). 

  DISPLAY "    " + W_NomEnt + " I N F O R M E  D E  P R O V E E D O R E S   "
          + STRING(W_FecCorte,"99/99/9999") FORM "X(120)"
         SKIP(2)
     WITH DOWN WIDTH 200 FRAME Enc NO-BOX NO-LABELS STREAM-IO USE-TEXT.

/*   FOR EACH TProvee:                                                                                                          */
/*       FIND FIRST Clientes WHERE Clientes.Nit EQ TProvee.CedNit NO-LOCK NO-ERROR.                                             */
/*       IF NOT AVAIL(Clientes) THEN                                                                                            */
/*          NEXT.                                                                                                               */
/*                                                                                                                              */
/*       TipIde = "ERROR EN TIPO-IDENT.".                                                                                       */
/*       IF Clientes.Tipo_Identificacion GE 1 AND Clientes.Tipo_Identificacion LE 10 THEN                                       */
/*          TipIde = Wk_TipoDocto[Clientes.Tipo_Identificacion].                                                                */
/*                                                                                                                              */
/*       DISPL Clientes.Ofic   LABEL "Suc."                                                                                     */
/*             Clientes.Nit    LABEL "Ced./Nit"                                                                                 */
/*             Clientes.Dig_Verificacion        LABEL "D.V"                                                                     */
/*             TipIde                           LABEL "T.Identificación"                                                        */
/*             Clientes.Tipo_Persona            LABEL "T.Psna"                                                                  */
/*             TRIM(Clientes.Nombre) + " " + TRIM(REPLACE(Clientes.Apellido,"_"," ")) LABEL "Nombre-Razón Social" FORM "X(40)"  */
/*             Clientes.Dir_Comercial           LABEL "Dirección Comercial"                                                     */
/*             Clientes.Tel_Comercial           LABEL "Teléfono"                                                                */
/*             TProvee.Suc                      LABEL "En la Suc"                                                               */
/*             TProvee.Fec                      LABEL "Fec.Ult.Movto" FORM "99/99/9999" SKIP                                    */
/*           WITH DOWN WIDTH 250 FRAME F1 USE-TEXT STREAM-IO NO-LABELS NO-BOX.                                                  */
/*   END.                                                                                                                       */

  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Madurar_CxC W-InfGer 
PROCEDURE Madurar_CxC :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR W_NrobandasCxC AS INTEGER INITIAL 0.
DEF VAR Vlr0           AS DECIMAL INITIAL 0.    

ASSIGN W_NrobandasCxC = TRUNCATE (W_NroDiasFinan / 30, 0)   wbda = 0.
IF W_NrobandasCxC LT 1 THEN
   W_NrobandasCxC = 1.

DEF VAR W_SdoCta   AS DECIMAL INITIAL 0.
RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                INPUT 999, INPUT 999, INPUT "164502",          
                                INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta). 


IF ABS(W_SdoCta) GT 0 THEN DO:
    DEF VAR i       AS INTEGER INITIAL 0.
    DEF VAR Wresto  AS DECIMAL INITIAL 0.
    DEF VAR Wvlrxxx AS DECIMAL INITIAL 0.

    ASSIGN  Wresto  = ABS(W_SdoCta)  
            Wvlrxxx = ROUND( ABS(W_SdoCta) / W_NroBandasCxC,0).

    REPEAT i = 1 TO 13:
       IF (Wresto + 10) LT W_vlrxMesInv THEN
          Wvlrxxx = Wresto.
       IF i LT 4 THEN 
          ASSIGN wbda[i] = Wvlrxxx
                 Wresto  = Wresto - Wvlrxxx.
       ELSE
         IF i LT 7 THEN
            ASSIGN wbda[4] = wbda[4] + Wvlrxxx
                   Wresto  = Wresto - Wvlrxxx.
         ELSE
           IF i LT 10 THEN
              ASSIGN wbda[5] = wbda[5] + Wvlrxxx
                     Wresto  = Wresto - Wvlrxxx.
           ELSE
               IF i LT 13 THEN
                  ASSIGN wbda[6] = wbda[6] + Wvlrxxx
                      Wresto  = Wresto - Wvlrxxx.

       IF i EQ 13 THEN ASSIGN wbda[7] = wbda[7] + Wresto   Wresto = 0.
       IF Wresto EQ 0 THEN i = 1000.
    END.

    ASSIGN TEval.Bda[1] = TEval.Bda[1] + wbda[1]
           TEval.Bda[2] = TEval.Bda[2] + wbda[2]
           TEval.Bda[3] = TEval.Bda[3] + wbda[3]
           TEval.Bda[4] = TEval.Bda[4] + wbda[4]
           TEval.Bda[5] = TEval.Bda[5] + wbda[5]
           TEval.Bda[6] = TEval.Bda[6] + wbda[6]
           TEval.Bda[7] = TEval.Bda[7] + wbda[7].
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Madurar_FdosSociales W-InfGer 
PROCEDURE Madurar_FdosSociales :
/*------------------------------------------------------------------------------
  Purpose:   Madurar solo hasta el mes de diciembre   
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER W_SdoCta AS DECIMAL.
CASE MONTH(W_FecFinP):

    WHEN  1 THEN ASSIGN TEval.Bda[1] = W_SdoCta / 12      TEval.Bda[2] = W_SdoCta / 12      TEval.Bda[3] = W_SdoCta / 12  
                        TEval.Bda[4] = W_SdoCta / 4       TEval.Bda[5] = W_SdoCta / 4       TEval.Bda[6] = W_SdoCta / 4. 
                                                                                            
    WHEN  2 THEN ASSIGN TEval.Bda[1] = W_SdoCta / 11      TEval.Bda[2] = W_SdoCta / 11      TEval.Bda[3] = W_SdoCta / 11 
                        TEval.Bda[4] = W_SdoCta / 11 * 3  TEval.Bda[5] = W_SdoCta / 11 * 3  TEval.Bda[6] = W_SdoCta / 11 * 2.  
          
    WHEN  3 THEN ASSIGN TEval.Bda[1] = W_SdoCta / 10      TEval.Bda[2] = W_SdoCta / 10      TEval.Bda[3] = W_SdoCta / 10 
                        TEval.Bda[4] = W_SdoCta / 10 * 3  TEval.Bda[5] = W_SdoCta / 10 * 3  TEval.Bda[6] = W_SdoCta / 10.  
          
    WHEN  4 THEN ASSIGN TEval.Bda[1] = W_SdoCta / 9      TEval.Bda[2] = W_SdoCta / 9        TEval.Bda[3] = W_SdoCta / 9 
                        TEval.Bda[4] = W_SdoCta / 9 * 3  TEval.Bda[5] = W_SdoCta / 9 * 3.  
          
    WHEN  5 THEN ASSIGN TEval.Bda[1] = W_SdoCta / 8      TEval.Bda[2] = W_SdoCta / 8        TEval.Bda[3] = W_SdoCta / 8 
                        TEval.Bda[4] = W_SdoCta / 8 * 3  TEval.Bda[5] = W_SdoCta / 8 * 2.    
                                                                                            
    WHEN  6 THEN ASSIGN TEval.Bda[1] = W_SdoCta / 7      TEval.Bda[2] = W_SdoCta / 7        TEval.Bda[3] = W_SdoCta / 7 
                        TEval.Bda[4] = W_SdoCta / 7 * 3  TEval.Bda[5] = W_SdoCta / 7.       
                                                                                            
    WHEN  7 THEN ASSIGN TEval.Bda[1] = W_SdoCta / 6      TEval.Bda[2] = W_SdoCta / 6        TEval.Bda[3] = W_SdoCta / 6 
                        TEval.Bda[4] = W_SdoCta / 6 * 3.                                    
                                                                                            
    WHEN  8 THEN ASSIGN TEval.Bda[1] = W_SdoCta / 5      TEval.Bda[2] = W_SdoCta / 5        TEval.Bda[3] = W_SdoCta / 5 
                        TEval.Bda[4] = W_SdoCta / 5 * 2.                                    
                                                                                            
    WHEN  9 THEN ASSIGN TEval.Bda[1] = W_SdoCta / 4      TEval.Bda[2] = W_SdoCta / 4        TEval.Bda[3] = W_SdoCta / 4
                        TEval.Bda[4] = W_SdoCta / 4.                                        
                                                                                            
    WHEN 10 THEN ASSIGN TEval.Bda[1] = W_SdoCta / 3      TEval.Bda[2] = W_SdoCta / 3        TEval.Bda[3] = W_SdoCta / 3.

    WHEN 11 THEN ASSIGN TEval.Bda[1] = W_SdoCta / 2      TEval.Bda[2] = W_SdoCta / 2.

    WHEN 12 THEN ASSIGN TEval.Bda[1] = W_SdoCta.

END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Madurar_Inventarios W-InfGer 
PROCEDURE Madurar_Inventarios :
DEF VAR Vlr0      AS DECIMAL INITIAL 0.
DEF VAR W_SdoCta  AS DECIMAL INITIAL 0.
ASSIGN W_CostoInv     = 0   W_CostoVtas    = 0  W_NroDiasPer   = 0
       W_NroDiasInv   = 0   W_SdoForm1     = 0  W_SdoForm2     = 0
       W_NroDiasRotac = 0   W_NroBandas    = 0  W_vlrxMesInv   = 0
       wbda = 0.

RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                      INPUT 999, INPUT 999, INPUT "13",          
                                      INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_CostoInv).

RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                      INPUT 999, INPUT 999, INPUT "6120",          
                                      INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
ASSIGN W_CostoVtas = W_CostoVtas + W_SdoCta.

RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                      INPUT 999, INPUT 999, INPUT "6135",          
                                      INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
ASSIGN W_CostoVtas = W_CostoVtas + W_SdoCta.

RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                      INPUT 999, INPUT 999, INPUT "6155",          
                                      INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
ASSIGN W_CostoVtas = W_CostoVtas + W_SdoCta.

RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                      INPUT 999, INPUT 999, INPUT "6170",          
                                      INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
ASSIGN W_CostoVtas = W_CostoVtas + W_SdoCta.

W_NroDiasPer = MONTH(W_FecCorte) * 30.

/* W_NroDiasInventario = Inventario / Costo de Ventas *  Nro Dias Periodo   */
  ASSIGN W_NroDiasInv =  TRUNCATE( W_CostoInv  / W_CostoVtas * W_NroDiasPer, 0).


/*   NUMERO DE DIAS DE FINANCIACION SE APLICA LA SIGUIENTE FORMULA */
RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                      INPUT 999, INPUT 999, INPUT "164502",
                                      INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
ASSIGN W_SdoForm1 = W_SdoForm1 + W_SdoCta.
RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                      INPUT 999, INPUT 999, INPUT "1648",          
                                      INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
ASSIGN W_SdoForm1 = W_SdoForm1 + W_SdoCta.
RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                      INPUT 999, INPUT 999, INPUT "1691",          
                                      INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
ASSIGN W_SdoForm1 = W_SdoForm1 - W_SdoCta.
        
RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                      INPUT 999, INPUT 999, INPUT "4120",          
                                      INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
ASSIGN W_SdoForm2 = W_SdoForm2 + W_SdoCta.

RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                      INPUT 999, INPUT 999, INPUT "4135",          
                                      INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).
ASSIGN W_SdoForm2 = W_SdoForm2 + W_SdoCta.
RUN HallarSdoMayor /* IN W_Manija */ (INPUT 1, INPUT 20,
                                      INPUT 999, INPUT 999, INPUT "4155",          
                                      INPUT YEAR(W_FecCorte), INPUT MONTH(W_FecCorte),INPUT 1,            
                                      OUTPUT Vlr0, OUTPUT Vlr0, OUTPUT W_SdoCta).

ASSIGN W_SdoForm2 = W_SdoForm2 + W_SdoCta
       W_NroDiasFinan = TRUNCATE(W_SdoForm1 / W_SdoForm2 * W_NroDiasPer, 0)
       W_NroDiasRotac = W_NroDiasFinan + W_NroDiasInv
       W_NroBandas    = TRUNCATE(W_NroDiasRotac / 30, 0).

W_vlrxMesInv = ROUND (W_CostoInv / W_NroBandas / ( 1 - W_PorcVta),0).

MESSAGE "W_NroDiasFinan"   W_NroDiasFinan  SKIP
        "W_NroDiasRotac"   W_NroDiasRotac  SKIP
        "W_NroBandas   "   W_NroBandas     SKIP
        "W_CostoInv    "   W_CostoInv      SKIP
        "W_PorcVta     "   W_PorcVta       SKIP
        "W_vlrxMes     "   W_vlrxMesinv    SKIP
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

DEF VAR i       AS INTEGER INITIAL 0.
DEF VAR Wresto  AS DECIMAL INITIAL 0.
DEF VAR Wvlrxxx AS DECIMAL INITIAL 0.

ASSIGN  Wresto  = W_CostoInv  
        Wvlrxxx = W_vlrxMesinv.
/* Correccion 16-06-2010 */
ASSIGN Wresto =  W_vlrxMesinv * W_NroBandas.
/* ********************* */
REPEAT i = 1 TO 13:
   IF (Wresto + 10) LT W_vlrxMesInv THEN
      Wvlrxxx = Wresto.
   IF i LT 4 THEN 
      ASSIGN wbda[i] = Wvlrxxx
             Wresto  = Wresto - Wvlrxxx.
   ELSE
     IF i LT 7 THEN
        ASSIGN wbda[4] = wbda[4] + Wvlrxxx
               Wresto  = Wresto - Wvlrxxx.
     ELSE
       IF i LT 10 THEN
          ASSIGN wbda[5] = wbda[5] + Wvlrxxx
                 Wresto  = Wresto - Wvlrxxx.
       ELSE
           IF i LT 13 THEN
              ASSIGN wbda[6] = wbda[6] + Wvlrxxx
                  Wresto  = Wresto - Wvlrxxx.

   IF i EQ 13 THEN ASSIGN wbda[7] = wbda[7] + Wresto   Wresto = 0.
   IF Wresto EQ 0 THEN i = 1000.
END.

ASSIGN TEval.Bda[1] = wbda[1]
       TEval.Bda[2] = wbda[2]
       TEval.Bda[3] = wbda[3]
       TEval.Bda[4] = wbda[4]
       TEval.Bda[5] = wbda[5]
       TEval.Bda[6] = wbda[6]
       TEval.Bda[7] = wbda[7].

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProxVctos W-InfGer 
PROCEDURE ProxVctos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH TMes: DELETE TMes. END.

  ASSIGN AnoIni = YEAR(W_FecCorte)
         MesIni = MONTH(W_FecCorte) + 1
         AnoFin = AnoIni + 1         
         MesFin = MesIni.            

  IF MesIni GT 12 THEN                                               
     ASSIGN AnoIni = AnoIni + 1                                      
            MesIni = 1.                                              
                                                                     
  ASSIGN FecFin = INTEG(STRING(AnoFin,"9999") + STRING(MesFin,"99")) 
         FecIni = INTEG(STRING(AnoIni,"9999") + STRING(MesIni,"99")).

  FOR EACH Pro_Ahorros WHERE /* Pro_Ahorros.Agencia EQ 1   *Solo los Pdctos de la ppal como control AND */ 
                           Pro_Ahorros.Estado  EQ 1 NO-LOCK
                  BREAK BY Pro_Ahorros.Tip_Ahorro BY Pro_Ahorros.Cod_Ahorro:
      IF Pro_Ahorros.Tip_Ahorro EQ 1 OR Pro_Ahorros.Tip_Ahorro EQ 4 THEN DO:
         FIND FIRST TMes WHERE TMes.Tipo EQ "A"                                 
                           AND TMes.Ta   EQ Pro_Ahorros.Tip_Ahorro            
                           AND TMes.AMes EQ FecIni NO-ERROR.                    
         IF NOT AVAIL(Tmes) THEN DO:                                            
            CREATE TMes.                                                        
            ASSIGN TMes.Tipo = "A"                                              
                   TMes.Ta   = Pro_Ahorros.Tip_Ahorro                         
                   TMes.AMes = FecIni.                                          
         END.                                                                   
      END.                                                                      

      FOR EACH Ahorros WHERE Ahorros.Cod_Ahorro EQ Pro_Ahorros.Cod_Ahorro
                         AND Ahorros.Estado       EQ 1 NO-LOCK:
          /*ASSIGN W_Cont = W_Cont + 1  
                 W_Cont:SCREEN-VALUE IN FRAME F_RiesgoL = STRING(W_Cont).*/
            
          IF Pro_Ahorros.Tip_Ahorro EQ 1 OR Pro_Ahorros.Tip_Ahorro EQ 4 THEN DO:          
             ASSIGN TMes.SdoK = TMes.SdoK + (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje)
                    TMes.Nro  = TMes.Nro  + 1.

             IF Pro_Ahorros.Tip_Ahorro EQ 1 THEN DO:
                RUN Halla_TasaInd.   /*Al final de este mismo procedimiento*/
                TMes.Tasa = TMes.Tasa + (I_Tasa * 365).
             END.
          END.
          ELSE DO:
             ASSIGN FecCor = INTEG(STRING(YEAR(Ahorros.Fec_Vencimiento),"9999") +  
                                      STRING(MONTH(Ahorros.Fec_Vencimiento),"99")).
             IF FecCor GT FecFin THEN                                              
                FIND FIRST TMes WHERE TMes.Tipo EQ "A"                             
                                  AND TMes.Ta   EQ Pro_Ahorros.Tip_Ahorro        
                                  AND TMes.AMes EQ FecFin + 1 NO-ERROR.            
             ELSE DO:                                                              
                IF FecCor LT FecIni THEN                                           
                   FIND FIRST TMes WHERE TMes.Tipo EQ "A"                          
                                     AND TMes.Ta   EQ Pro_Ahorros.Tip_Ahorro     
                                     AND TMes.AMes EQ FecIni NO-ERROR.             
                ELSE                                                               
                   FIND FIRST TMes WHERE TMes.Tipo EQ "A"                          
                                     AND TMes.Ta   EQ Pro_Ahorros.Tip_Ahorro     
                                     AND TMes.AMes EQ FecCor NO-ERROR.             
             END.  
                
             IF NOT AVAIL(Tmes) THEN DO:                   
                CREATE TMes.                               
                ASSIGN TMes.Tipo = "A"                     
                       TMes.Ta   = Pro_Ahorros.Tip_Ahorro
                       TMes.AMes = FecCor.                 
                                                           
                IF FecCor GT FecFin THEN                   
                   ASSIGN TMes.AMes = FecFin + 1.          
                ELSE IF FecCor LT FecIni THEN              
                   ASSIGN TMes.AMes = FecIni.              
             END.                                          

             ASSIGN TMes.SdoK = TMes.SdoK + (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje)
                    TMes.Nro  = TMes.Nro  + 1.

             IF Pro_Ahorros.Tip_Ahorro EQ 2 THEN DO:
                ASSIGN /*TMes.SdoI = TMes.SdoI + Ahorros.INT_Pagar No porque se toman en CxP 24*/
                       TMes.Tasa = TMes.Tasa + Ahorros.Tasa.
                IF FecCor GT FecIni THEN   /*Solo los que no se han vencido, Liq.Int*/
                   TMes.SdoI = TMes.SdoI + (
                               ((Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) * (Ahorros.Tasa / 12 )) / 100).
             END.
             ELSE DO:
                ASSIGN LiqInt    = ((Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) * (Ahorros.Tasa / 12)) / 100
                       TMes.Tasa = TMes.Tasa + Ahorros.Tasa 
                       TMes.SdoI = TMes.SdoI + LiqInt.      
             END.             
          END.         
      END.
  END.

  RUN ProxVctos_ATno.   /*De los Cdats con base en las Prorrogas y Renovaciones*/

  RUN ProxVctos_Creditos.  /*Halla los prox.vctos de Créditos*/

END PROCEDURE.

PROCEDURE Halla_TasaInd:
  DEFI VAR I_TasAux LIKE Ahorros.Tasa.

  FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Pro_Ahorros.Indicador 
                         AND   Indicadores.Estado    EQ 1 NO-LOCK NO-ERROR.
  IF AVAILABLE(Indicadores) THEN DO:
     ASSIGN I_Tasa = Indicadores.Tasa.
     IF Indicadores.Rangos EQ TRUE THEN DO:
        FIND FIRST Ran_Intereses WHERE    Ran_Intereses.Indicador   EQ Indicador.Indicador
                                    AND   Ran_Intereses.Val_Inicial LE Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje
                                    AND   Ran_Intereses.Val_Final   GE Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje
                                    AND   Ran_Intereses.Pla_Inicial LE 9999
                                    AND   Ran_Intereses.Pla_Final   GE 9999
                                    AND   Ran_Interes.Estado        EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE(Ran_Intereses) THEN 
           ASSIGN I_Tasa = I_Tasa + Ran_Intereses.Puntos_Asoc.

        RUN EFNV IN W_ManFin (INPUT (I_Tasa / 100),INPUT 365,OUTPUT I_TasAux).
        ASSIGN I_Tasa = I_TasAux * 100.
     END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProxVctos_ATno W-InfGer 
PROCEDURE ProxVctos_ATno :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI   VAR FecContVcto LIKE Ahorros.Fec_ProLiqui.
  DEFINE VAR K           AS INTEGER.
  DEFINE VAR INTFAL      AS DECIMAL.

  FOR EACH Ahorros WHERE Ahorros.Tip_Ahorro EQ 3 AND Ahorros.Estado  EQ 1 NO-LOCK BY Ahorros.Nit:
      ASSIGN FecCor = INTEG(STRING(YEAR(Ahorros.Fec_ProLiqui),"9999") +                   
                            STRING(MONTH(Ahorros.Fec_ProLiqui),"99")).
             /*W_Cont = W_Cont + 1  
             W_Cont:SCREEN-VALUE IN FRAME F_RiesgoL = STRING(W_Cont).*/
            
      /*Control de liq.int.x c/mes*/
      IF  Ahorros.Per_Liquid GE 4 AND Ahorros.Per_Liquid LE 8
/*       IF  Ahorros.Per_Liquid GE 2 AND Ahorros.Per_Liquid LE 4  */
      AND Plazo GT 1 THEN DO:
          IF  Ahorros.Fec_Vencimiento GT W_FecCorte  
          AND (Ahorros.Fec_Vencimiento - W_FecCorte) GE 31 THEN DO: 
             IF Ahorros.Fec_ProLiqui + 2 GT Ahorros.Fec_Vencimiento THEN. /*Prox vcto con ult.intereses lo controla el código sgte.*/
             ELSE DO:
                 ASSIGN FecContVcto = Ahorros.Fec_ProLiqui.
                 IF Ahorros.Per_Liquid EQ 4 THEN DO K = 1 TO Plazo / 30:
                    RUN Halla_TempVcto.
                    ASSIGN FecCor      = FecCor + 1
                           FecContVcto = FecContVcto + 31.
                    IF FecContVcto GE Ahorros.Fec_Vencimiento THEN
                       LEAVE.                    
                 END.
                 ELSE IF Ahorros.Per_Liquid EQ 5 THEN DO K = 1 TO Plazo / 30:
                    RUN Halla_TempVcto.
                    ASSIGN FecCor      = FecCor + 2
                           FecContVcto = FecContVcto + 61.
                    IF FecContVcto GE Ahorros.Fec_Vencimiento THEN
                       LEAVE.                    
                 END.
                 ELSE IF Ahorros.Per_Liquid EQ 6 THEN DO K = 1 TO Plazo / 30:
                    RUN Halla_TempVcto.
                    ASSIGN FecCor      = FecCor + 3
                           FecContVcto = FecContVcto + 92.
                    IF FecContVcto GE Ahorros.Fec_Vencimiento THEN
                       LEAVE.                 
                 END.
                 ELSE IF Ahorros.Per_Liquid EQ 8 THEN DO K = 1 TO Plazo / 30:
                    RUN Halla_TempVcto.
                    ASSIGN FecCor      = FecCor + 6
                           FecContVcto = FecContVcto + 184.
                    IF FecContVcto GE Ahorros.Fec_Vencimiento THEN
                       LEAVE.          
                 END.
             END.
         END.
         /*de lo contrario: Prox vcto con ult.intereses lo controla el código sgte.*/
      END.
      /*de lo contrario: Prox vcto con ult.intereses lo controla el código sgte.*/

      ASSIGN FecCor = INTEG(STRING(YEAR(Ahorros.Fec_Vencimiento),"9999") +                   
                      STRING(MONTH(Ahorros.Fec_Vencimiento),"99"))
             W_ProrRen = FALSE.

/*       IF Ahorros.Fec_Prorroga NE ? AND (W_Fecha - Ahorros.Fec_Prorroga) GT 3 THEN */
/*          W_ProrRen = TRUE.                                                        */
/*       ELSE DO:                                                                    */
         ASSIGN W_CedAho = Ahorros.Nit
                FecAho   = Ahorros.Fec_Apertura.
/*          RUN Cdats_Permanentes.   /*Halla los Cdats Renovados en Otros*/ comentado por nelson 19-04-2010  */
/*       END. */

      /*Fecha para Los intereses en el vcto normal*/
      IF FecCor GT FecFin THEN                                                                  
         FIND FIRST TMes WHERE TMes.Tipo EQ "A"                                                 
                           AND TMes.Ta   EQ 5                            
                           AND TMes.AMes EQ FecFin + 1 NO-ERROR.                                
      ELSE DO:                                                                                  
         IF FecCor LT FecIni THEN                                                               
            FIND FIRST TMes WHERE TMes.Tipo EQ "A"                                              
                              AND TMes.Ta   EQ 5                         
                              AND TMes.AMes EQ FecIni NO-ERROR.                                 
         ELSE                                                                                   
            FIND FIRST TMes WHERE TMes.Tipo EQ "A"                                              
                              AND TMes.Ta   EQ 5                         
                              AND TMes.AMes EQ FecCor NO-ERROR.                                 
      END.                                                                                      
      IF NOT AVAIL(Tmes) THEN DO:                                                               
         CREATE TMes.                                                                           
         ASSIGN TMes.Tipo = "A"                                                                 
                TMes.Ta   = 5                                            
                TMes.AMes = FecCor.                                                             
                                                                                                
         IF FecCor GT FecFin THEN                                                        
            ASSIGN TMes.AMes = FecFin + 1.                                                      
         ELSE IF FecCor LT FecIni THEN                                                          
            ASSIGN TMes.AMes = FecIni.                                                          
      END.

    /********************************************************** Nuevo **********/

      /*Los intereses en el vcto normal*/
      /* wmr agregó abs en interes */
      ASSIGN LiqInt   = ((Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) * (Ahorros.Tasa / 12)) / 100
             IntFal   = ABS(((TODAY - ahorros.fec_vencimiento) * LiqInt / 30))
             TMes.Nro = TMes.Nro  + 1
             TMes.Tasa = TMes.Tasa + Ahorros.Tasa 
             TMes.SdoI = TMes.SdoI + IntFal. /*LiqInt.       */

      IF NOT W_ProrRen THEN DO:  /*No Son prorrogas Ni Renovaciones, K en el vcto normal*/
         ASSIGN TMes.SdoK = TMes.SdoK + (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje).
         NEXT.
      END.

      /*Continua K solo para Renovac.y Prorrogas en el Ultimo Mes*/
      ASSIGN FecCor = FecFin + 1.

      FIND FIRST TMes WHERE TMes.Tipo EQ "A"                                                 
                           AND TMes.Ta   EQ 5                            
                           AND TMes.AMes EQ FecFin + 1 NO-ERROR.                                
      IF NOT AVAIL(Tmes) THEN DO:                                                               
         CREATE TMes.                                                                           
         ASSIGN TMes.Tipo = "A"                                                                 
                TMes.Ta   = 5                                            
                TMes.AMes = FecFin    + 1
                TMes.Nro  = TMes.Nro  + 1
                TMes.Tasa = TMes.Tasa + (Ahorros.Tasa / 12).
      END.                                                                                      
                                                                                                
      ASSIGN TMes.SdoK = TMes.SdoK + (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProxVctos_Creditos W-InfGer 
PROCEDURE ProxVctos_Creditos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR FecCre   AS INTEG FORM "999999".
  DEFI VAR W_EPag   AS INTEG FORM "99".
  DEFI VAR SdoK     LIKE Creditos.Sdo_Capital.
  DEFI VAR IntP     LIKE Creditos.Sdo_Capital.
  DEFI VAR K        AS INTEG FORM "99999".
  DEFI VAR FecCont  AS DATE.
  DEFI VAR W_CuoExT AS DECIMAL INITIAL 0.
  DEFI VAR W_TipCre LIKE Creditos.Tip_credito.
  
  ASSIGN FecFin  = FecIni + 12.
/*   OUTPUT TO VALUE("c:\InfRed\ProxVctosListaCreditos-" + STRING(W_FecCorte,"999999") + "_" + W_Usuario + ".csv").          */
/*   EXPORT DELIMITER ";" "nit" "Código" "Num_Credito" "Pagare" "Sdo_Capital" "Intereses" "Cuota" "Plazo" "fec_desembolso".  */
  FOR EACH Creditos WHERE Creditos.Estado       EQ 2 
                      AND Creditos.Sdo_Capital  GT 0 
/*                    AND (Creditos.Cod_Credito EQ 540 OR Creditos.Cod_Credito EQ 541)  */
      NO-LOCK BREAK BY Creditos.Cod_Credito :
      ASSIGN FecCont = W_FecCorte - DAY(W_FecCorte) + 1.
      IF FIRST-OF( Creditos.Cod_credito ) THEN DO:
          FIND Pro_creditos WHERE Pro_creditos.cod_credito EQ Creditos.cod_credito AND
                                  Pro_creditos.Tip_credito EQ Creditos.Tip_credito NO-LOCK NO-ERROR.
          IF AVAILABLE Pro_creditos THEN 
            FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Pro_creditos.Cod_tasamora NO-LOCK NO-ERROR.
          ELSE /* Se toma la máxima de usura */  
               FIND FIRST Indicadores WHERE Indicadores.Indicador EQ 1 AND Indicadores.Estado EQ 1 NO-LOCK NO-ERROR.
      END.
      ASSIGN W_EPag = 1.  /*Inicia mensual*/
             
      CASE Creditos.Per_pago:
           WHEN 1 THEN w_EPag = 4.33.
           WHEN 2 THEN w_EPag = 3.
           WHEN 3 THEN w_EPag = 2.
           WHEN 4 THEN w_EPag = 1.
           WHEN 5 THEN w_EPag = 0.5.
           WHEN 6 THEN w_EPag = 1 / 3.
           WHEN 7 THEN w_EPag = 1 / 4.
           WHEN 8 THEN w_EPag = 1 / 6.
           WHEN 9 THEN w_EPag = 1 / 12.
           OTHERWISE W_EPag = 1.
      END CASE.

      FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
        
      /* Listado de créditos */
      DEFINE VARIABLE NuevaBanda AS INTEGER     NO-UNDO.
      FIND FIRST TCred WHERE TCred.CNit EQ Creditos.Nit AND TCred.CNumC EQ Creditos.Num_Credito NO-ERROR.
      IF NOT AVAILABLE TCred THEN DO:
          CREATE TCred.
          ASSIGN  cNit   = Creditos.Nit
                  cNumC  = Creditos.Num_Credito
                  cSaldo = Creditos.Sdo_Capital
                  cCod   = Creditos.Cod_Credito.
      END.
      /**********************/

      IF Creditos.Cod_Califica LT 4 THEN DO:      /*Los Intereses causados en TMes.AMes EQ 8*/
         FIND FIRST TMes WHERE TMes.Tipo EQ "C"                     
                           /*AND TMes.Ta   EQ Pro_Creditos.Tip_Credito */
                           AND TMes.AMes EQ 8 NO-ERROR.            
         IF NOT AVAIL(Tmes) THEN
            CREATE TMes.  
                
         ASSIGN TMes.Tipo  = "C"                                                              
                /*TMes.Ta   = Pro_Creditos.Tip_Credito WHEN AVAIL(Pro_Creditos)*/              
                TMes.AMes  = 8 
                TMes.Banda = 8
                TMes.Nro   = TMes.Nro  + 1                                                       
                TMes.SdoI  = TMes.SdoI + (Creditos.Int_Corrientes +  Creditos.Int_MorCobrar)
                TMes.Comen = "Int-Causados,No-Provisionados".
         IF Creditos.Tasa LE Indicadores.Tasa THEN                                              
            TMes.Tasa = TMes.Tasa + Creditos.Tasa.                                       
         ELSE                                                                                   
            TMes.Tasa = TMes.Tasa + Indicadores.Tasa.                                    

        /* Listado de créditos */
        ASSIGN TCred.CSdoK[8]  = 0
               TCred.CSdoI[8]  = (Creditos.Int_Corrientes +  Creditos.Int_MorCobrar).
      END. 
      IF Creditos.Cod_Credito EQ 540 OR Creditos.Cod_Credito EQ 541 THEN
         W_TipCre = 6. /* Empleados */
      ELSE
         W_TipCre = Creditos.Tip_credito.


      CASE Creditos.Cod_Credito:
          WHEN 524 THEN              ASSIGN CLinea = "Comercial". 
          WHEN  15 THEN              ASSIGN CLinea = "Hipotecario".
          WHEN 540 OR WHEN 541 THEN  ASSIGN CLinea = "Empleados".
          OTHERWISE                  ASSIGN CLinea = "Consumo".
      END CASE.


      IF Creditos.Cod_Califica GT 2 THEN DO:    /*Solo los Calif.mayores a A, en TMes.AMes EQ 9*/
         FIND FIRST TMes WHERE TMes.Tipo EQ "C"                     
                        AND TMes.Ta   EQ W_TipCre    
                        AND TMes.AMes EQ 9 NO-ERROR.            
         IF NOT AVAIL(Tmes) THEN                                                         
            CREATE TMes. 

         ASSIGN TMes.Tipo  = "C"                                                             
                TMes.Ta    = W_TipCre WHEN AVAIL(Pro_Creditos)                 
                TMes.AMes  = 9                                                                  
                TMes.Banda = 9                                                                  
                TMes.Nro   = TMes.Nro  + 1                                                      
                TMes.SdoK  = TMes.SdoK + Creditos.Sdo_Capital                                   
                TMes.SdoI  = TMes.SdoI + (Creditos.Int_Corrientes +  Creditos.Int_MorCobrar)
                TMes.Comen = "Cartera Superior a A".
         IF Creditos.Tasa LE Indicadores.Tasa THEN                                              
            TMes.Tasa = TMes.Tasa + Creditos.Tasa.                                       
         ELSE                                                                                   
            TMes.Tasa = TMes.Tasa + Indicadores.Tasa.                                    
         
            /* Listado de créditos */
            ASSIGN TCred.CSdoK[9]  = Creditos.Sdo_Capital
                   TCred.CSdoI[9]  = (Creditos.Int_Corrientes +  Creditos.Int_MorCobrar).

         NEXT.  /*Pasa Al Sgte registro de Creditos*/
      END.

     ASSIGN  SdoK   = Creditos.Sdo_Capital
             FecCre = FecIni - 1.

/*      EXPORT DELIMITER ";" Creditos.nit Creditos.Num_Credito Creditos.Pagare Creditos.Sdo_Capital (Creditos.Int_Corrientes +  Creditos.Int_MorCobrar) Creditos.Cuota Creditos.Plazo Creditos.fec_desembolso. */
      DO K = 1 TO Creditos.Plazo:  
         IF SdoK GT 0 THEN DO:
            ASSIGN W_CuoExT = 0.
            IF (MONTH(feccont) + 1) GT 12 THEN
               ASSIGN FecCont = DATE(1,1,YEAR(FecCont) + 1).
            ELSE
               ASSIGN FecCont = DATE(MONTH(FecCont) + 1,1,YEAR(FecCont)).     
            IF K GT 12 THEN 
               FIND FIRST TMes WHERE TMes.Tipo EQ "C"                     
                                 AND TMes.Ta   EQ W_TipCre    
                                 AND TMes.AMes EQ FecCre + 13 NO-ERROR.  
            ELSE
               FIND FIRST TMes WHERE TMes.Tipo EQ "C"                     
                                 AND TMes.Ta   EQ W_TipCre    
                                 AND TMes.AMes EQ FecCre + K NO-ERROR.
            IF NOT AVAIL(Tmes) THEN DO:                                    
               CREATE TMes.                                                 
               ASSIGN TMes.Tipo = "C"                                      
                      TMes.Ta   = W_TipCre                 
                      TMes.AMes = FecCre + K.
               IF K GT 12 THEN
                  TMes.AMes = FecCre + 13.
            END.
            FIND FIRST Extras WHERE Extras.Nit      EQ Creditos.Nit
                         AND Extras.Num_Solicitud   EQ Creditos.Num_solicit
                         AND MONTH(Extras.Fec_Vcto) EQ MONTH(FecCont) 
                         AND YEAR(Extras.Fec_Vcto)  EQ YEAR(FecCont) NO-LOCK NO-ERROR. 
            IF AVAIL(Extras) THEN
              ASSIGN W_CuoExT = Extras.Vr_CuoExtra.

            ASSIGN TMes.Nro  = TMes.Nro  + 1
                   TMes.Tasa = TMes.Tasa + Creditos.Tasa 
                   IntP      = SdoK * ((Creditos.Tasa / 12) / 100)
                   TMes.SdoI = TMes.SdoI + IntP.

            /* Listado de créditos */
            NuevaBanda = HallaBanda().
            ASSIGN TCred.CSdoI[NuevaBanda] = TCred.CSdoI[NuevaBanda] + IntP.

            IF SdoK - ((Creditos.Cuota * W_EPag) + W_CuoExT - IntP) GE 0 THEN DO:
               ASSIGN SdoK      = SdoK      - ((Creditos.Cuota * W_EPag) + W_CuoExT - IntP)
                      TMes.SdoK = TMes.SdoK + ((Creditos.Cuota * W_EPag) + W_CuoExT - IntP).

               /* Listado de créditos */
               ASSIGN TCred.CSdoK[NuevaBanda] = TCred.CSdoK[NuevaBanda] + ((Creditos.Cuota * W_EPag) + W_CuoExT - IntP).

               IF K EQ Creditos.Plazo AND SdoK GT 0 THEN DO:
                  ASSIGN TMes.SdoK = TMes.SdoK + SdoK.
                         
                  /* Listado de créditos */
                  ASSIGN TCred.CSdoK[NuevaBanda] = TCred.CSdoK[NuevaBanda] + SdoK.

                  ASSIGN SdoK      = 0.
                  LEAVE.
               END.
            END.
            ELSE DO:
               ASSIGN TMes.SdoK = TMes.SdoK + SdoK.
                      
               /* Listado de créditos */
               ASSIGN TCred.CSdoK[NuevaBanda] = TCred.CSdoK[NuevaBanda] + SdoK.

               SdoK      = 0.
               LEAVE.
            END.
         END.
         ELSE 
            LEAVE.
      END.
  END.
/*   OUTPUT CLOSE.  */


  OUTPUT TO VALUE("c:\InfRed\ProxVctosListaCreditos-" + STRING(W_FecCorte,"999999") + "_" + W_Usuario + ".csv").
  EXPORT DELIMITER ";" "Nit" "Num cred" "Linea" "Código" "Saldo capital"
      "SdoK 1" "SdoK 2" "SdoK 3" "SdoK 4" "SdoK 5" "SdoK 6" "SdoK 7" "SdoK 8" "SdoK 9"
      "SdoI 1" "SdoI 2" "SdoI 3" "SdoI 4" "SdoI 5" "SdoI 6" "SdoI 7" "SdoI 8" "SdoI 9".
  FOR EACH TCred NO-LOCK:
    EXPORT DELIMITER ";" TCred.
  END.
  OUTPUT CLOSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prox_Vctos W-InfGer 
PROCEDURE Prox_Vctos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN W_ArchVcto = "C:\InfRed\ProxVctos-" + STRING(W_FecCorte,"999999") + ".Txt".
  /*W_Msaje:SCREEN-VALUE IN FRAME F_RiesgoL = "Generando desde Archivos, Temporales Próximos Vencimientos...".*/
    
  OUTPUT TO VALUE(W_ArchVcto).     
        RUN ProxVctos. 
        RUN Imp_ProxVctos.
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proy_RecContractual W-InfGer 
PROCEDURE Proy_RecContractual :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFI VAR Vlr0     LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR W_SdoCta LIKE Sal_Cuenta.Sal_INICIAL.
   DEFI VAR TDisp    LIKE Ahorros.Sdo_Dispon INIT 0.
   DEFI VAR TFin     LIKE Ahorros.Sdo_Dispon INIT 0.
   DEFI VAR TMes     LIKE Ahorros.Sdo_Dispon INIT 0 EXTENT 12.
   DEFI VAR TMesT    LIKE Ahorros.Sdo_Dispon INIT 0.
   DEFI VAR PorC     AS DEC FORM "-99999.99".
   DEFI VAR Arch     AS CHAR FORM "X(35)".
   DEFI VAR FecNN    AS DATE.
   DEFI VAR NN       AS INTEG FORM "99999999".
   DEFI VAR PdoP     AS INTEG FORM "99".
   DEFI VAR TControl AS DEC FORM "-9999.99".

   FOR EACH TDiaA: DELETE TDiaA. END.
      
   ASSIGN Arch      = "C:\InfRed\ProyContract-" + STRING(W_FecCorte,"999999") + "_" + W_Usuario
          ProyContr = 0.

   OUTPUT TO VALUE(Arch).

   DISPLAY "   " + W_NomEnt + "  PROYECCION RECAUDOS CONTRACTUALES A UN AÑO DE "
          + STRING(W_FecIniP,"99/99/9999") + " HASTA " + STRING(W_FecFinP,"99/99/9999") FORM "X(120)"
         SKIP(1)
       WITH DOWN WIDTH 150 FRAME Enc NO-BOX NO-LABELS STREAM-IO USE-TEXT.



   FOR EACH Ahorros WHERE (Ahorros.Cod_Ahorro EQ 217 OR Ahorros.Cod_Ahorro EQ 5) AND 
                           Ahorros.Estado EQ 1 NO-LOCK:
       
     CASE Ahorros.Per_Deduccion:
       WHEN 1 THEN ASSIGN W_SdoCta = W_SdoCta + (Ahorros.Cuota * 4.33)   PdoP = 4.
       WHEN 2 THEN ASSIGN W_SdoCta = W_SdoCta + (Ahorros.Cuota * 3)      PdoP = 3.
       WHEN 3 THEN ASSIGN W_SdoCta = W_SdoCta + (Ahorros.Cuota * 2)      PdoP = 2. 
       WHEN 4 THEN ASSIGN W_SdoCta = W_SdoCta + Ahorros.Cuota            PdoP = 1.
       OTHERWISE   ASSIGN W_SdoCta = W_SdoCta + Ahorros.Cuota            PdoP = 1.
     END CASE.
   END.

   DISPLAY SKIP(1)
           "Proyección Aportes-Sociales :" SKIP
           "(Con Base en la Cuota de Aportes y con Créditos Vigentes)." SKIP
           "Total Esperado x Mes: "
           W_SdoCta    
       WITH WIDTH 150 NO-BOX NO-LABELS STREAM-IO USE-TEXT.   




   ASSIGN ProyAport = W_SdoCta
          W_SdoCta  = 0.
    
   FOR EACH Ahorros WHERE(Ahorros.Cod_Ahorro EQ 215 OR Ahorros.Cod_Ahorro EQ 205)
                      AND Ahorros.Estado       EQ 1
                      AND Ahorros.Fec_Apertura LE W_FecIniP NO-LOCK:

       CASE Ahorros.Per_Deduccion:
         WHEN 1 THEN ASSIGN W_SdoCta = W_SdoCta + (Ahorros.Cuota * 4.33)   PdoP = 4.
         WHEN 2 THEN ASSIGN W_SdoCta = W_SdoCta + (Ahorros.Cuota * 3)      PdoP = 3.
         WHEN 3 THEN ASSIGN W_SdoCta = W_SdoCta + (Ahorros.Cuota * 2)      PdoP = 2. 
         WHEN 4 THEN ASSIGN W_SdoCta = W_SdoCta + Ahorros.Cuota            PdoP = 1.
         OTHERWISE   ASSIGN W_SdoCta = W_SdoCta + Ahorros.Cuota            PdoP = 1.
       END CASE.

/*        FIND FIRST Inf_Laboral WHERE Inf_Laboral.Nit   EQ Ahorros.Nit               */
/*                                AND Inf_Laboral.Estado EQ 1 NO-LOCK NO-ERROR.       */
/*        IF AVAIL(Inf_Laboral) THEN                                                  */
/*          FIND FIRST Empresas WHERE Empresas.Cod_empresa EQ Inf_Laboral.Cod_Empresa */
/*                                AND Empresas.Estado      EQ 1 NO-LOCK NO-ERROR.     */
/*        IF AVAIL(Inf_Laboral) AND AVAIL(Empresas) THEN DO:                          */
/*           IF Empresas.FOR_Pago EQ 1 THEN                                           */
/*              ASSIGN W_SdoCta = (Ahorros.Cuota * 4.33)                              */
/*                     PdoP     = 4.                                                  */
/*           ELSE IF Empresas.FOR_Pago EQ 2 THEN                                      */
/*              ASSIGN W_SdoCta = (Ahorros.Cuota * 3)                                 */
/*                     PdoP     = 3.                                                  */
/*           ELSE IF Empresas.FOR_Pago EQ 3 THEN                                      */
/*              ASSIGN W_SdoCta = (Ahorros.Cuota * 2)                                 */
/*                     PdoP     = 2.                                                  */
/*           ELSE IF Empresas.FOR_Pago EQ 4 THEN                                      */
/*              ASSIGN W_SdoCta = Ahorros.Cuota                                       */
/*                     PdoP     = 1.                                                  */
/*        END.                                                                        */
/*        ELSE                                                                        */
/*          ASSIGN W_SdoCta = Ahorros.Cuota                                           */
/*                 PdoP     = 1.                                                      */

       ASSIGN TControl = (Ahorros.Fec_Vencimiento - W_FecIniP) / 30
              TControl = ROUND(TControl + .49,0).

       IF TControl GT 12 THEN
          TControl = 12.
       ELSE IF TControl LT 1 THEN
          TControl = 1.

       DO NN = 1 TO TControl:
          TMes[NN] = TMes[NN] + W_SdoCta.
       END.
   END.

   DISPLAY SKIP(3)
           "Proyección 12 Meses Ahorro-Contractual desde " 
           W_FecIniP SKIP
           "Total Esperado x Mes: " SKIP (1)              
       WITH WIDTH 150 FRAME F1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

   DISPLAY TMes   LABEL "Valor X C/Mes" FORM "->>>>>>,>>>,>>9"
          WITH DOWN WIDTH 420 FRAME FDet NO-BOX NO-LABELS STREAM-IO USE-TEXT.
   
   DO NN = 1 TO 12:
      IF NN LE 3 THEN
         ProyContr[NN] = TMes[NN].
      ELSE IF NN LE 6 THEN
         ProyContr[4] = ProyContr[4] + TMes[NN].
      ELSE IF NN LE 9 THEN
         ProyContr[5] = ProyContr[5] + TMes[NN].
      ELSE 
         ProyContr[6] = ProyContr[6] + TMes[NN].   
   END.

   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tit_ProxVcto W-InfGer 
PROCEDURE Tit_ProxVcto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF TMes.Tipo = "C" THEN
         DISPLAY "       CREDITOS"
                 SKIP (0) WITH FRAME F1 NO-LABELS.
  ELSE DO:
      IF TMes.TA EQ 1 THEN
         DISPLAY "   AHORROS A LA VISTA" 
                 SKIP (0) WITH FRAME F2 NO-LABELS.
      ELSE IF TMes.TA EQ 2 THEN
         DISPLAY "   AHORROS CONTRACTUALES" 
                 SKIP (0) WITH FRAME F3 NO-LABELS.
      ELSE IF TMes.TA EQ 3 THEN
         DISPLAY "   AHORROS A TERMINO" 
                 SKIP (0) WITH FRAME F4 NO-LABELS.
      ELSE IF TMes.TA EQ 4 THEN
         DISPLAY "   APORTES"
                 SKIP (0) WITH FRAME F5 NO-LABELS.
      ELSE IF TMes.TA EQ 5 THEN
         DISPLAY "A-TERMINO (Prórrogas y Renovaciones)"
                 SKIP (0) WITH WIDTH 80 FRAME F6 NO-LABELS.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION HallaBanda W-InfGer 
FUNCTION HallaBanda RETURNS INTEGER
  (/* INPUT XYMes AS INTEGER */) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE W_Mes AS CHARACTER NO-UNDO.
  DEFINE VARIABLE NuevaBanda AS INTEGER     NO-UNDO.
  DEFINE VARIABLE XAno AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE XAmes AS INTEGER     NO-UNDO.

  ASSIGN XAmes = TMes.AMes.

  W_Mes = SUBSTRING(STRING(XAmes),5,2).
  IF W_Mes GT "12" THEN
     ASSIGN NuevaBanda = (XAmes - FecIni) + 1
            W_Mes      = STRING((INTEG(W_Mes) - 12),"99")
            XAno      = STRING(INTEG(SUBSTRING(STRING(XAmes),1,4)) + 1)
            XAmes  = INTEG(STRING(XAno,"9999") + STRING(W_Mes,"99")).
  ELSE IF INTEG(SUBSTRING(STRING(XAmes),1,4)) GT AnoIni THEN
     NuevaBanda = (XAmes - 88 - FecIni) + 1.
  ELSE   
     NuevaBanda = (XAmes - FecIni) + 1.

  IF NuevaBanda LE 1 OR NuevaBanda EQ ? THEN 
     NuevaBanda = 1.
  ELSE IF NuevaBanda EQ 2 THEN
     NuevaBanda = 2.
  ELSE IF NuevaBanda EQ 3 THEN
     NuevaBanda = 3.
  ELSE IF NuevaBanda LE 6 THEN
     NuevaBanda = 4.
  ELSE IF NuevaBanda LE 9 THEN
     NuevaBanda = 5.
  ELSE IF NuevaBanda LE 12 THEN
     NuevaBanda = 6.
  ELSE IF NuevaBanda GT 12 THEN
     NuevaBanda = 7.


  RETURN NuevaBanda.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SumatoriaCta W-InfGer 
FUNCTION SumatoriaCta RETURNS DECIMAL
  ( INPUT stopmes AS INTEGER ) :
/*------------------------------------------------------------------------------
  Toma los saldos de una cuenta, según su naturaleza, y la suma hasta el mes dado
----------------------------------------------------------------------------------*/
  DEF VAR zsdocta AS DECIMAL INITIAL 0.00.
  DEF VAR i       AS INTEGER INITIAL 0.
    
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Sal_cuenta.Cuenta NO-LOCK NO-ERROR.
  IF NOT AVAILABLE cuentas THEN
      MESSAGE "No se encuentra " Sal_cuenta.cuenta
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  IF UPPER(Cuentas.Naturaleza) EQ "DB" THEN DO:
    zsdocta = Sal_cuenta.Sal_inicial.    
    REPEAT i = 1 TO stopmes:
      ASSIGN zsdocta = zsdocta + Sal_Cuenta.DB[i] - Sal_Cuenta.CR[i].
    END.
  END.
  ELSE 
  DO:
      zsdocta = Sal_cuenta.Sal_inicial.    
      REPEAT i = 1 TO stopmes:
        ASSIGN zsdocta = zsdocta - Sal_Cuenta.DB[i] + Sal_Cuenta.CR[i].
      END.
  END.  
    
  RETURN zsdocta.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

