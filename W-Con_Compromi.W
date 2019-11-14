&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME W-Con_Compromi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Con_Compromi 
/*------------------------------------------------------------------------
  File:        W-Con_Compromi.W
  Description: Consulta de Compromisos de Cobro
  Author:      GAER
  Created:     Octubre 27/05

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

    ON RETURN TAB.

   {Incluido/Variable.I "SHARED"}
   {Incluido/VARCON.I   "SHARED"}
        
   DEFI VAR W_Age        LIKE Agencias.Agencia.
   DEFI VAR W_SiIngr     AS LOG INIT FALSE.
   DEFI VAR W_RowIdCobro AS ROWID.
   DEFI VAR UsuIni LIKE Usuarios.Usuario. 
   DEFI VAR UsuFin LIKE Usuarios.Usuario.  

   DEFI TEMP-TABLE TCreditos LIKE Creditos.

   DEFI TEMP-TABLE Cred_Ag
        FIELD Age   LIKE Agencias.Agencia
        FIELD Cat   LIKE Creditos.Categoria
        FIELD Sdo   LIKE Creditos.Sdo_Capital
        FIELD Vdo   LIKE Creditos.Val_Atraso
        FIELD Cuo   LIKE Creditos.Cuota
        FIELD Ncre  AS INTEG FORM "9999999"
        FIELD SdoH  LIKE Creditos.Sdo_Capital
        FIELD VdoH  LIKE Creditos.Val_Atraso
        FIELD CuoH  LIKE Creditos.Cuota
        FIELD NcreH AS INTEG FORM "9999999".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Compromi
&Scoped-define BROWSE-NAME BROWSE-11

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Cobros Creditos TCreditos

/* Definitions for BROWSE BROWSE-11                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-11 Cobros.Agencia Cobros.Num_Credito Cobros.Fec_Acuerdo Cobros.Fec_Compromiso Cobros.Nit Cobros.Num_Credito Cobros.Val_Compromiso Cobros.Val_Cumplido Cobros.Vr_Vencido Cobros.Fec_Cumpli Cobros.Usuario Cobros.Instanc Cobros.Estado Cobros.Usu_Recaudo Cobros.Observacion Cobros.Age_Recaudo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-11   
&Scoped-define SELF-NAME BROWSE-11
&Scoped-define QUERY-STRING-BROWSE-11 FOR EACH Cobros WHERE Cobros.Nit    EQ W_CedNit                                           AND Cobros.Estado GE 2 NO-LOCK, ~
               EACH Creditos WHERE Creditos.Nit         EQ Cobros.Nit                         AND Creditos.Num_Credito EQ Cobros.Num_Credito                         /*AND Creditos.Sdo_Capital GT 0*/ NO-LOCK  INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-11 OPEN QUERY {&SELF-NAME} FOR EACH Cobros WHERE Cobros.Nit    EQ W_CedNit                                           AND Cobros.Estado GE 2 NO-LOCK, ~
               EACH Creditos WHERE Creditos.Nit         EQ Cobros.Nit                         AND Creditos.Num_Credito EQ Cobros.Num_Credito                         /*AND Creditos.Sdo_Capital GT 0*/ NO-LOCK  INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-11 Cobros Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-11 Cobros
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-11 Creditos


/* Definitions for BROWSE Br_Cobros                                     */
&Scoped-define FIELDS-IN-QUERY-Br_Cobros Cobros.Agencia Cobros.Fec_Acuerdo Cobros.Fec_Compromiso Cobros.Nit Cobros.Num_Credito Cobros.Val_Compromiso Cobros.Val_Cumplido Cobros.Vr_Vencido Cobros.Usuario Cobros.Instanc Cobros.Estado Cobros.Usu_Recaudo Cobros.Observacion Cobros.Categoria   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Cobros   
&Scoped-define SELF-NAME Br_Cobros
&Scoped-define QUERY-STRING-Br_Cobros FOR EACH Cobros WHERE Cobros.Nit    EQ W_CedNit                                           AND Cobros.Estado EQ 1 NO-LOCK, ~
               EACH Creditos WHERE Creditos.Nit         EQ Cobros.Nit                         AND Creditos.Num_Credito EQ Cobros.Num_Credito                         AND Creditos.Sdo_Capital GT 0 NO-LOCK  INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Cobros OPEN QUERY {&SELF-NAME} FOR EACH Cobros WHERE Cobros.Nit    EQ W_CedNit                                           AND Cobros.Estado EQ 1 NO-LOCK, ~
               EACH Creditos WHERE Creditos.Nit         EQ Cobros.Nit                         AND Creditos.Num_Credito EQ Cobros.Num_Credito                         AND Creditos.Sdo_Capital GT 0 NO-LOCK  INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Cobros Cobros Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Cobros Cobros
&Scoped-define SECOND-TABLE-IN-QUERY-Br_Cobros Creditos


/* Definitions for BROWSE Br_cred                                       */
&Scoped-define FIELDS-IN-QUERY-Br_cred TCreditos.Agencia TCreditos.Nit TCreditos.Cod_Credito TCreditos.Num_Credito TCreditos.Val_Atraso TCreditos.Dias_Atraso TCreditos.Sdo_Capital TCreditos.Categoria TCreditos.Fec_Pago TCreditos.Fec_UltPago TCreditos.Abogado TCreditos.Cuota TCreditos.Cuo_Atraso TCreditos.Cuo_Pagadas TCreditos.Fec_Desembolso   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_cred   
&Scoped-define SELF-NAME Br_cred
&Scoped-define QUERY-STRING-Br_cred FOR EACH TCreditos NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_cred OPEN QUERY {&SELF-NAME} FOR EACH TCreditos NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_cred TCreditos
&Scoped-define FIRST-TABLE-IN-QUERY-Br_cred TCreditos


/* Definitions for BROWSE Br_Tramites                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Tramites Cobros.Agencia Cobros.Cod_Credito Cobros.Num_Credito Cobros.Cod_Tipo Cobros.Fec_Acuerdo Cobros.Usuario Cobros.Instanc Cobros.Vr_Vencido Cobros.Observacion Cobros.Categoria   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Tramites   
&Scoped-define SELF-NAME Br_Tramites
&Scoped-define QUERY-STRING-Br_Tramites FOR EACH Cobros WHERE Cobros.Nit    EQ W_CedNit                                           AND Cobros.Estado EQ 0 NO-LOCK, ~
               EACH Creditos WHERE Creditos.Nit         EQ Cobros.Nit                         AND Creditos.Num_Credito EQ Cobros.Num_Credito                         /*AND Creditos.Sdo_Capital GT 0*/ NO-LOCK  INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Tramites OPEN QUERY {&SELF-NAME} FOR EACH Cobros WHERE Cobros.Nit    EQ W_CedNit                                           AND Cobros.Estado EQ 0 NO-LOCK, ~
               EACH Creditos WHERE Creditos.Nit         EQ Cobros.Nit                         AND Creditos.Num_Credito EQ Cobros.Num_Credito                         /*AND Creditos.Sdo_Capital GT 0*/ NO-LOCK  INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Tramites Cobros Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Tramites Cobros
&Scoped-define SECOND-TABLE-IN-QUERY-Br_Tramites Creditos


/* Definitions for FRAME F_Compromi                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Compromi ~
    ~{&OPEN-QUERY-BROWSE-11}~
    ~{&OPEN-QUERY-Br_Cobros}~
    ~{&OPEN-QUERY-Br_Tramites}

/* Definitions for FRAME F_SinGestion                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_SinGestion ~
    ~{&OPEN-QUERY-Br_cred}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Cobros.Estado 
&Scoped-define ENABLED-TABLES Cobros
&Scoped-define FIRST-ENABLED-TABLE Cobros
&Scoped-Define ENABLED-OBJECTS BROWSE-11 Br_Tramites Btn_InfG ~
Btn_SinGestion Rs_FAcuerdOCompro W_CedNit W_CmbOfi FecIni FecFin EDITOR-1 ~
Btn_IngComp Btn_Salva Br_Cobros W_CmbUsu Mora_Ini Mora_Fin W_CodCpto ~
Cmb_Cpto RECT-321 RECT-322 
&Scoped-Define DISPLAYED-FIELDS Cobros.Fec_Compromiso Cobros.Val_Compromiso ~
Cobros.Estado Cobros.Num_Credito 
&Scoped-define DISPLAYED-TABLES Cobros
&Scoped-define FIRST-DISPLAYED-TABLE Cobros
&Scoped-Define DISPLAYED-OBJECTS Rs_FAcuerdOCompro W_CedNit W_CmbOfi FecIni ~
FecFin EDITOR-1 W_NomCte W_CmbUsu Mora_Ini Mora_Fin W_CodCpto Cmb_Cpto 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Con_Compromi AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_InfG 
     LABEL "Informes de &Gestión" 
     SIZE 24 BY 1.35.

DEFINE BUTTON Btn_IngComp 
     LABEL "&Ingresar Compromiso/Gestión" 
     SIZE 23.57 BY 1.35.

DEFINE BUTTON Btn_Salva 
     LABEL "&Salvar Gestión/Compromiso" 
     SIZE 23.72 BY 1.35.

DEFINE BUTTON Btn_SinGestion 
     LABEL "&Créditos Vencidos sin Gestión" 
     SIZE 21.72 BY 1.31.

DEFINE VARIABLE Cmb_Cpto AS CHARACTER FORMAT "X(40)":U 
     LABEL "Gestión (Concepto)" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "00000 - Ninguno" 
     DROP-DOWN-LIST
     SIZE 34.29 BY 1 TOOLTIP "Concepto-Trámite realizado"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(30)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 12
     DROP-DOWN-LIST
     SIZE 30.72 BY 1 TOOLTIP "Agencias Activas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_CmbUsu AS CHARACTER FORMAT "X(100)":U 
     LABEL "Usuario" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 30.72 BY 1 TOOLTIP "Usuarios Activos"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 500 SCROLLBAR-VERTICAL
     SIZE 48 BY 1.46
     BGCOLOR 15 FGCOLOR 18  NO-UNDO.

DEFINE VARIABLE FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha   Final" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .73 TOOLTIP "Fecha final del Compromiso para el filtro"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .73 TOOLTIP "Fecha inicial del Compromiso para el filtro"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Mora_Fin AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "Final" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .77
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Mora_Ini AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "Inicial" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .73
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CedNit AS CHARACTER FORMAT "X(12)":U 
     LABEL "Cédula/Nit" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .88 TOOLTIP "Con doble click consulta Clientes"
     BGCOLOR 15 FGCOLOR 18  NO-UNDO.

DEFINE VARIABLE W_CodCpto AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Código de la Gestión" 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY .73
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCte AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 48 BY .85 TOOLTIP "Nombre del Cliente"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rs_FAcuerdOCompro AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "X Fec-Compromiso", 0,
"X Fec-Acuerdo", 1
     SIZE 32 BY .46 NO-UNDO.

DEFINE RECTANGLE RECT-321
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 33.29 BY 5.96.

DEFINE RECTANGLE RECT-322
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 33.29 BY .85.

DEFINE BUTTON Btn_Impresion 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "I&mprimir" 
     SIZE 10.29 BY 1.62
     FONT 15.

DEFINE BUTTON Btn_OutCon 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_outcon" 
     SIZE 9.72 BY 1.62 TOOLTIP "Regresa a la Ventana Principal".

DEFINE VARIABLE Cmb_Inf AS CHARACTER FORMAT "X(40)":U 
     LABEL "Informes Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "Resumen por Usuario","Créditos por Agencia","Detallado X Usuario","Créditos Vdos sin Gestión" 
     DROP-DOWN-LIST
     SIZE 26.57 BY 1
     BGCOLOR 15 FGCOLOR 18  NO-UNDO.

DEFINE VARIABLE I_FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN 
     SIZE 12.29 BY .77 TOOLTIP "Fecha final del filtro"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE I_FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .77 TOOLTIP "Fecha inicial del filtro"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE BUTTON Btn_OutConAcuerdo 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 179" 
     SIZE 9 BY 1.31 TOOLTIP "Regresa a la Ventana Principal".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-11 FOR 
      Cobros, 
      Creditos SCROLLING.

DEFINE QUERY Br_Cobros FOR 
      Cobros, 
      Creditos SCROLLING.

DEFINE QUERY Br_cred FOR 
      TCreditos SCROLLING.

DEFINE QUERY Br_Tramites FOR 
      Cobros, 
      Creditos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-11 W-Con_Compromi _FREEFORM
  QUERY BROWSE-11 NO-LOCK DISPLAY
      Cobros.Agencia FORMAT "999":U                 COLUMN-LABEL "Ag."
      Cobros.Num_Credito FORMAT "999999999":U
      Cobros.Fec_Acuerdo FORMAT "99/99/9999":U
      Cobros.Fec_Compromiso FORMAT "99/99/9999":U   COLUMN-LABEL "F-Compromiso"
      Cobros.Nit FORMAT "X(12)":U
      Cobros.Num_Credito FORMAT "999999999":U
      Cobros.Val_Compromiso FORMAT ">>>,>>>,>>9":U
      Cobros.Val_Cumplido FORMAT ">>>,>>>,>>9":U
      Cobros.Vr_Vencido FORMAT ">>>,>>>,>>9":U      COLUMN-LABEL "Vencido Inicial"
      Cobros.Fec_Cumpli   FORMAT "99/99/9999":U     COLUMN-LABEL "F-Cumplido"
      Cobros.Usuario FORMAT "X(4)":U
      Cobros.Instanc                                COLUMN-LABEL "Inst."
      Cobros.Estado FORMAT "9"                      COLUMN-LABEL "Est" COLUMN-BGCOLOR 1
      Cobros.Usu_Recaudo                            COLUMN-LABEL "Usu.M"
      Cobros.Observacion FORMAT "X(80)":U
      Cobros.Age_Recaudo COLUMN-LABEL "Ag.Rec."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 108.29 BY 5
         FONT 4
         TITLE "Compromisos Cumplidos y No-Vigentes del Cliente Seleccionado" ROW-HEIGHT-CHARS .42 EXPANDABLE.

DEFINE BROWSE Br_Cobros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Cobros W-Con_Compromi _FREEFORM
  QUERY Br_Cobros NO-LOCK DISPLAY
      Cobros.Agencia FORMAT "999":U                 COLUMN-LABEL "Ag."
      Cobros.Fec_Acuerdo FORMAT "99/99/9999":U
      Cobros.Fec_Compromiso FORMAT "99/99/9999":U   COLUMN-LABEL "FCompromiso"
      Cobros.Nit FORMAT "X(12)":U
      Cobros.Num_Credito FORMAT "999999999":U
      Cobros.Val_Compromiso FORMAT ">>>>,>>>,>>9":U
      Cobros.Val_Cumplido   FORMAT ">>>>,>>>,>>9":U COLUMN-LABEL "Vr.Cumplido"
      Cobros.Vr_Vencido     FORMAT ">>>>,>>>,>>9":U COLUMN-LABEL "Vencido Inicial"
      Cobros.Usuario FORMAT "X(4)":U
      Cobros.Instanc                                COLUMN-LABEL "Inst."
      Cobros.Estado FORMAT "9"                      COLUMN-LABEL "Est" COLUMN-BGCOLOR 1
      Cobros.Usu_Recaudo                            COLUMN-LABEL "Usu.M"
      Cobros.Observacion FORMAT "X(120)":U
      Cobros.Categoria
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 108.43 BY 5.31
         FONT 4
         TITLE "COMPROMISOS PENDIENTES" ROW-HEIGHT-CHARS .46 EXPANDABLE.

DEFINE BROWSE Br_cred
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_cred W-Con_Compromi _FREEFORM
  QUERY Br_cred NO-LOCK DISPLAY
      TCreditos.Agencia FORMAT "999":U                        COLUMN-LABEL "Ag."
      TCreditos.Nit FORMAT "X(12)":U
      TCreditos.Cod_Credito FORMAT "999":U                    COLUMN-LABEL "Pdto"
      TCreditos.Num_Credito FORMAT "999999999":U
      TCreditos.Val_Atraso FORMAT ">>,>>>,>>>,>>9":U          COLUMN-LABEL "Vr.Capital Vencido"
      TCreditos.Dias_Atraso FORMAT "99999":U                  COLUMN-LABEL "Dias-Vdos"
      TCreditos.Sdo_Capital FORMAT "->>>>>,>>>,>>9":U
      TCreditos.Categoria FORMAT "X":U                        COLUMN-LABEL "Cat"
      TCreditos.Fec_Pago FORMAT "99/99/9999":U                COLUMN-LABEL "F-ProxPago"
      TCreditos.Fec_UltPago FORMAT "99/99/9999":U             COLUMN-LABEL "F-UltPago"
      TCreditos.Abogado FORMAT "yes/no":U
      TCreditos.Cuota         FORMAT ">>,>>>,>>>,>>9":U       COLUMN-LABEL "Valor Cuota"
      TCreditos.Cuo_Atraso                                    COLUMN-LABEL "Cuo-Vdas"
      TCreditos.Cuo_Pagadas                                   COLUMN-LABEL "Cuo-Pagas"
      TCreditos.Fec_Desembolso                                COLUMN-LABEL "F-Desemb"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 108.14 BY 11.92
         BGCOLOR 15 FGCOLOR 18 FONT 4 EXPANDABLE.

DEFINE BROWSE Br_Tramites
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Tramites W-Con_Compromi _FREEFORM
  QUERY Br_Tramites NO-LOCK DISPLAY
      Cobros.Agencia FORMAT "999":U
      Cobros.Cod_Credito FORMAT "999":U         COLUMN-LABEL "Pdcto"
      Cobros.Num_Credito FORMAT "999999999":U
      Cobros.Cod_Tipo FORMAT "99999":U          COLUMN-LABEL "C-Gestion"
      Cobros.Fec_Acuerdo FORMAT "99/99/9999":U  COLUMN-LABEL "F-Tramite"
      Cobros.Usuario FORMAT "X(4)":U
      Cobros.Instanc                            COLUMN-LABEL "Inst."
      Cobros.Vr_Vencido FORMAT ">>,>>>,>>>,>>9":U
      Cobros.Observacion FORMAT "X(120)":U
      Cobros.Categoria
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 108.43 BY 4.46
         FONT 4
         TITLE "Gestiones de Cobranza para el Crédito seleccionado" ROW-HEIGHT-CHARS .46 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_SinGestion
     Br_cred AT ROW 1 COL 1
     Btn_OutConAcuerdo AT ROW 13.12 COL 97.72
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.72 ROW 7.88
         SIZE 108.43 BY 14.35
         BGCOLOR 18 FGCOLOR 15 FONT 4
         TITLE "Créditos Vencidos Sin Gestión".

DEFINE FRAME F_InfG
     I_FecIni AT ROW 1.15 COL 31.29 COLON-ALIGNED
     I_FecFin AT ROW 2 COL 31.29 COLON-ALIGNED
     Cmb_Inf AT ROW 3.27 COL 17.14 COLON-ALIGNED
     Btn_Impresion AT ROW 5.15 COL 9.14 HELP
          "Imprime la Información que desea consultar"
     Btn_OutCon AT ROW 5.15 COL 33.29
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.72 ROW 9.27
         SIZE 46.14 BY 7.35
         BGCOLOR 17 FONT 4
         TITLE "Informes de Gestión de Cobranza".

DEFINE FRAME F_Compromi
     BROWSE-11 AT ROW 13.23 COL 2.57
     Br_Tramites AT ROW 18.38 COL 2.57
     Btn_InfG AT ROW 6.12 COL 2.86
     Btn_SinGestion AT ROW 4.08 COL 78
     Rs_FAcuerdOCompro AT ROW 2.88 COL 78.29 NO-LABEL
     W_CedNit AT ROW 1.12 COL 9.86 COLON-ALIGNED
     W_CmbOfi AT ROW 2.08 COL 43.72 COLON-ALIGNED
     FecIni AT ROW 1.12 COL 85.57 COLON-ALIGNED
     FecFin AT ROW 1.85 COL 97.14 COLON-ALIGNED
     EDITOR-1 AT ROW 4 COL 28.57 NO-LABEL
     Cobros.Fec_Compromiso AT ROW 6.12 COL 43.43 COLON-ALIGNED NO-LABEL FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 13.72 BY .77
          BGCOLOR 15 FGCOLOR 0 
     Cobros.Val_Compromiso AT ROW 6.12 COL 60.43 COLON-ALIGNED NO-LABEL FORMAT ">>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 14 BY .77 TOOLTIP "Vlr.del nuevo Compromiso"
          BGCOLOR 15 FGCOLOR 0 
     Cobros.Estado AT ROW 6.27 COL 79.29 HELP
          "Estado en que se encuentra la agencia" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Pendiente", 1,
"No Vigente", 3
          SIZE 31 BY .54
     Cobros.Num_Credito AT ROW 6.12 COL 26.57 COLON-ALIGNED NO-LABEL FORMAT "999999999"
          VIEW-AS FILL-IN 
          SIZE 14 BY .73
          BGCOLOR 1 FGCOLOR 15 
     Btn_IngComp AT ROW 2.5 COL 3
     Btn_Salva AT ROW 4.35 COL 3
     W_NomCte AT ROW 1.15 COL 26.43 COLON-ALIGNED NO-LABEL
     Br_Cobros AT ROW 7.77 COL 2.57
     W_CmbUsu AT ROW 3.04 COL 43.86 COLON-ALIGNED
     Mora_Ini AT ROW 4.08 COL 104.14 COLON-ALIGNED
     Mora_Fin AT ROW 4.77 COL 104.14 COLON-ALIGNED
     W_CodCpto AT ROW 7.04 COL 90.86 COLON-ALIGNED
     Cmb_Cpto AT ROW 6.96 COL 40.29 COLON-ALIGNED
     RECT-321 AT ROW 1 COL 77.86
     RECT-322 AT ROW 2.69 COL 77.86
     "Número_Crédito               Fecha-Compromiso          Valor Compromiso" VIEW-AS TEXT
          SIZE 49.29 BY .58 AT ROW 5.54 COL 28.57
          BGCOLOR 18 FGCOLOR 15 
     "El Filtro es :" VIEW-AS TEXT
          SIZE 8.43 BY .5 AT ROW 2.31 COL 78.14
          BGCOLOR 18 FGCOLOR 15 
     "Observación" VIEW-AS TEXT
          SIZE 9.29 BY .5 AT ROW 3.58 COL 28.57
          BGCOLOR 18 FGCOLOR 15 
     "       El Compromiso lo Actualiza como :" VIEW-AS TEXT
          SIZE 34.29 BY .62 AT ROW 5.54 COL 76.57
          BGCOLOR 18 FGCOLOR 15 
     " Dias de Mora" VIEW-AS TEXT
          SIZE 10.86 BY .46 AT ROW 3.62 COL 99.86
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.72 BY 21.96
         BGCOLOR 17 FONT 4.


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
  CREATE WINDOW W-Con_Compromi ASSIGN
         HIDDEN             = YES
         TITLE              = "Administración Compromisos/Gestión Cobranza, Prog.W-Con_Compromi.W"
         HEIGHT             = 21.96
         WIDTH              = 110.86
         MAX-HEIGHT         = 22.35
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.35
         VIRTUAL-WIDTH      = 114.29
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
/* SETTINGS FOR WINDOW W-Con_Compromi
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Compromi
   Custom                                                               */
/* BROWSE-TAB BROWSE-11 1 F_Compromi */
/* BROWSE-TAB Br_Tramites BROWSE-11 F_Compromi */
/* BROWSE-TAB Br_Cobros W_NomCte F_Compromi */
ASSIGN 
       Btn_Salva:HIDDEN IN FRAME F_Compromi           = TRUE.

ASSIGN 
       EDITOR-1:HIDDEN IN FRAME F_Compromi           = TRUE.

/* SETTINGS FOR RADIO-SET Cobros.Estado IN FRAME F_Compromi
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN Cobros.Fec_Compromiso IN FRAME F_Compromi
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Cobros.Num_Credito IN FRAME F_Compromi
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Cobros.Val_Compromiso IN FRAME F_Compromi
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN W_NomCte IN FRAME F_Compromi
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_InfG
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_InfG:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_SinGestion
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_cred 1 F_SinGestion */
ASSIGN 
       FRAME F_SinGestion:HIDDEN           = TRUE
       FRAME F_SinGestion:MOVABLE          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Con_Compromi)
THEN W-Con_Compromi:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-11
/* Query rebuild information for BROWSE BROWSE-11
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Cobros WHERE Cobros.Nit    EQ W_CedNit
                                          AND Cobros.Estado GE 2 NO-LOCK,
        EACH Creditos WHERE Creditos.Nit         EQ Cobros.Nit
                        AND Creditos.Num_Credito EQ Cobros.Num_Credito
                        /*AND Creditos.Sdo_Capital GT 0*/ NO-LOCK  INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-11 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Cobros
/* Query rebuild information for BROWSE Br_Cobros
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Cobros WHERE Cobros.Nit    EQ W_CedNit
                                          AND Cobros.Estado EQ 1 NO-LOCK,
        EACH Creditos WHERE Creditos.Nit         EQ Cobros.Nit
                        AND Creditos.Num_Credito EQ Cobros.Num_Credito
                        AND Creditos.Sdo_Capital GT 0 NO-LOCK  INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Cobros */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_cred
/* Query rebuild information for BROWSE Br_cred
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TCreditos NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_cred */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Tramites
/* Query rebuild information for BROWSE Br_Tramites
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Cobros WHERE Cobros.Nit    EQ W_CedNit
                                          AND Cobros.Estado EQ 0 NO-LOCK,
        EACH Creditos WHERE Creditos.Nit         EQ Cobros.Nit
                        AND Creditos.Num_Credito EQ Cobros.Num_Credito
                        /*AND Creditos.Sdo_Capital GT 0*/ NO-LOCK  INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Tramites */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Compromi
/* Query rebuild information for FRAME F_Compromi
     _Query            is NOT OPENED
*/  /* FRAME F_Compromi */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_InfG
/* Query rebuild information for FRAME F_InfG
     _Query            is NOT OPENED
*/  /* FRAME F_InfG */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_SinGestion
/* Query rebuild information for FRAME F_SinGestion
     _Query            is NOT OPENED
*/  /* FRAME F_SinGestion */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Con_Compromi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Con_Compromi W-Con_Compromi
ON END-ERROR OF W-Con_Compromi /* Administración Compromisos/Gestión Cobranza, Prog.W-Con_Compromi.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Con_Compromi W-Con_Compromi
ON WINDOW-CLOSE OF W-Con_Compromi /* Administración Compromisos/Gestión Cobranza, Prog.W-Con_Compromi.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-11
&Scoped-define SELF-NAME BROWSE-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-11 W-Con_Compromi
ON MOUSE-SELECT-CLICK OF BROWSE-11 IN FRAME F_Compromi /* Compromisos Cumplidos y No-Vigentes del Cliente Seleccionado */
DO:
  APPLY "Mouse-Select-Click" TO BROWSE Br_Cobros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Cobros
&Scoped-define SELF-NAME Br_Cobros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Cobros W-Con_Compromi
ON MOUSE-SELECT-CLICK OF Br_Cobros IN FRAME F_Compromi /* COMPROMISOS PENDIENTES */
DO:
  IF AVAIL(Cobros) AND Cobros.Estado NE 0 AND Cobros.Estado NE 2 THEN 
     ASSIGN Cobros.Estado:SCREEN-VALUE         = STRING(Cobros.Estado)
            Cobros.Fec_Compromiso:SCREEN-VALUE = STRING(Cobros.Fec_Compromiso)
            Cobros.Val_Compromiso:SCREEN-VALUE = STRING(Cobros.Val_Compromiso)
            Cobros.Num_Credito:SCREEN-VALUE    = STRING(Cobros.Num_Credito)
            W_CedNit:SCREEN-VALUE              = Cobros.Nit
            W_CedNit
            Cobros.Val_Compromiso:SENSITIVE    = FALSE
            Cobros.Fec_Compromi:SENSITIVE      = FALSE
            Btn_Salva:VISIBLE                  = TRUE
            EDITOR-1:VISIBLE                   = TRUE
            EDITOR-1:SCREEN-VALUE              = ""
            EDITOR-1
            Cobros.Estado:SENSITIVE            = TRUE
            W_SiIngr = FALSE
            W_RowIdCobro                       = ROWID(Cobros).  
  ELSE DO:
     IF AVAIL(Cobros) THEN
        ASSIGN W_CedNit:SCREEN-VALUE = Cobros.Nit
               W_CedNit.

     ASSIGN Cobros.Fec_Compromiso:SCREEN-VALUE = ""
            Cobros.Val_Compromiso:SCREEN-VALUE = "0"
            Cobros.Num_Credito:SCREEN-VALUE    = ""
            W_RowIdCobro                       = ?.
  END.

  RUN NO-Pendientes.
  RUN Tramites.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_InfG
&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-Con_Compromi
ON CHOOSE OF Btn_Impresion IN FRAME F_InfG /* Imprimir */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  Listado = W_PathSpl + W_Usuario + "_GestCobra.LST".
  {incluido/Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Compromi
&Scoped-define SELF-NAME Btn_InfG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_InfG W-Con_Compromi
ON CHOOSE OF Btn_InfG IN FRAME F_Compromi /* Informes de Gestión */
DO:
   ASSIGN FRAME F_Compromi:SENSITIVE  = FALSE
          FRAME F_InfG:VISIBLE        = TRUE
          I_FecIni:SCREEN-VALUE       = STRING(W_Fecha)
          I_FecIni
          I_FecFin:SCREEN-VALUE       = STRING(W_Fecha)
          I_FecFin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_IngComp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_IngComp W-Con_Compromi
ON CHOOSE OF Btn_IngComp IN FRAME F_Compromi /* Ingresar Compromiso/Gestión */
DO:
    DEFINE VAR W_Age LIKE Creditos.Agencia.
    DEFINE VAR W_Pro LIKE Creditos.Cod_Credito.
    DEFINE VAR W_Cue LIKE Creditos.Num_Credito.  

    ASSIGN W_CodCpto               = 0
           W_CodCpto:SCREEN-VALUE  = "0".
        
    IF W_CedNit GT "0" THEN DO:
       RELEASE Cobros.
        
       MESSAGE "Seleccione el Num-Credito/Pagare para Asignarle el Compromiso"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
       ASSIGN W-Con_Compromi:SENSITIVE = FALSE. 
        
       RUN C-Creditos.R (INPUT W_CedNit, OUTPUT W_Age,    OUTPUT W_Pro, 
                                         OUTPUT W_CedNit, OUTPUT W_Cue).
        
       ASSIGN W-Con_Compromi:SENSITIVE           = TRUE
              Cobros.Estado:SENSITIVE            = FALSE
              Cobros.Estado:SCREEN-VALUE         = "1"
              W_CedNit:SCREEN-VALUE              = W_CedNit
              Cobros.Num_Credito:SCREEN-VALUE    = STRING(W_Cue)
              Cobros.Val_Compromiso:SENSITIVE    = TRUE
              Cobros.Val_Compromiso:SCREEN-VALUE = "0"
              Cobros.Fec_Compromi:SENSITIVE      = TRUE
              Cobros.Fec_Compromi:SCREEN-VALUE   = ""
              Btn_Salva:VISIBLE                  = TRUE
              EDITOR-1:VISIBLE                   = TRUE
              EDITOR-1:SCREEN-VALUE              = ""
              EDITOR-1
              W_SiIngr = TRUE.
            
       APPLY "ENTRY" TO EDITOR-1. 
    END.
    ELSE
       MESSAGE "Debe Teclear o Consultar una Cedula/Nit, para Ingresar Compromisos."
           VIEW-AS ALERT-BOX ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_InfG
&Scoped-define SELF-NAME Btn_OutCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutCon W-Con_Compromi
ON CHOOSE OF Btn_OutCon IN FRAME F_InfG /* Btn_outcon */
DO:
   HIDE FRAME F_InfG.
   FRAME F_Compromi:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_SinGestion
&Scoped-define SELF-NAME Btn_OutConAcuerdo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutConAcuerdo W-Con_Compromi
ON CHOOSE OF Btn_OutConAcuerdo IN FRAME F_SinGestion /* Button 179 */
DO:
   HIDE FRAME F_SinGestion.
   FRAME F_Compromi:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Compromi
&Scoped-define SELF-NAME Btn_Salva
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salva W-Con_Compromi
ON CHOOSE OF Btn_Salva IN FRAME F_Compromi /* Salvar Gestión/Compromiso */
DO:
   ASSIGN EDITOR-1.     
   IF EDITOR-1 LE " " OR W_CedNit LE "0" THEN DO:
      MESSAGE "Falta la Cedula/Nit Y/O el Comentario del Compromiso/Gestion."
          VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
    
   IF W_SiIngr THEN DO: 
      IF (Cobros.Fec_Compromiso:SCREEN-VALUE LE " " OR Cobros.Fec_Compromiso:SCREEN-VALUE EQ ?) 
      AND DEC(Cobros.Val_Compromiso:SCREEN-VALUE)  LE 0 THEN DO:
          IF W_CodCpto LE 0 THEN DO:
             MESSAGE "Falta el Concepto de la Gestion."
                   VIEW-AS ALERT-BOX ERROR.
             RETURN.
          END.
      END.
      ELSE IF Cobros.Fec_Compromiso:SCREEN-VALUE LE " " OR Cobros.Fec_Compromiso:SCREEN-VALUE EQ ? 
      OR DEC(Cobros.Val_Compromiso:SCREEN-VALUE)  LE 0           
      OR DATE(Cobros.Fec_Compromiso:SCREEN-VALUE) LT W_Fecha THEN DO:
         MESSAGE "Falta el Valor Y/O la Fecha del Compromiso(Igual O Posterior a Hoy)."
              VIEW-AS ALERT-BOX ERROR.
         RETURN.
      END.

      FIND FIRST Creditos WHERE Creditos.Nit     EQ W_CedNit
                        AND Creditos.Num_Credito EQ INTEG(Cobros.Num_Credito:SCREEN-VALUE)
                        AND Creditos.Sdo_Capital GT 0 NO-LOCK NO-ERROR.
      IF NOT AVAIL(Creditos) THEN DO:
         MESSAGE "No existe el Credito vigente con Sdo-Capital para asignarle el Compromiso."
             VIEW-AS ALERT-BOX ERROR.
         RETURN.
      END.

      CREATE Cobros.
      ASSIGN Cobros.Nro_Cobro        = NEXT-VALUE(Sec_NumCobro)
             Cobros.Agencia          = Creditos.Agencia
             Cobros.Estado           = 1
             Cobros.Fec_Acuerdo      = W_Fecha
             Cobros.Fec_Compromiso   = DATE(Cobros.Fec_Compromiso:SCREEN-VALUE)
             Cobros.Nit              = W_CedNit
             Cobros.Num_Credito      = INTEG(Cobros.Num_Credito:SCREEN-VALUE)
             Cobros.Observacion      = EDITOR-1
             Cobros.Usuario          = W_Usuario
             Cobros.Val_Compromiso   = DEC(Cobros.Val_Compromiso:SCREEN-VALUE)
             Cobros.Categoria        = Creditos.Categoria
             Cobros.Cod_Credito      = Creditos.Cod_Credito
             Cobros.Cuo_Vencidas     = Creditos.Cuo_Atraso 
             Cobros.Dias_Vencido     = Creditos.Dias_Atraso
             Cobros.Hora_Grabacion   = TIME
             Cobros.Sdo_Capital      = Creditos.Sdo_Capital
             Cobros.Vr_Vencido       = Creditos.Val_Atraso.

      IF Cobros.Val_Compromiso LE 0 THEN
         ASSIGN Cobros.Fec_Compromiso = ?
                Cobros.Estado         = 0
                Cobros.Cod_Tipo       = W_CodCpto.

      FIND FIRST Mov_Inst WHERE Mov_Inst.Nit           EQ Creditos.Nit                 
                            AND Mov_Inst.Cuenta        EQ STRING(Creditos.Num_Credito) 
                            AND Mov_Inst.Estado        EQ NO                           
                            AND Mov_Inst.Num_Solicitud EQ Creditos.Num_Solicitud NO-LOCK NO-ERROR.
      IF AVAIL(Mov_Inst) THEN 
         Cobros.Instancia = Mov_Inst.Instancia. 
   END.
   ELSE DO:
      FIND FIRST Cobros WHERE ROWID(Cobros) EQ W_RowIdCobro NO-ERROR.
      IF NOT AVAIL(Cobros) THEN DO:
         MESSAGE "Debe seleccionar un Compromiso Valido."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN.
      END.

      IF Cobros.Estado EQ 0 OR Cobros.Estado EQ 2 THEN DO:
         MESSAGE "Compromisos Pagados o Gestiones de Cobranza, no se permiten modificar."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN.
      END.

      ASSIGN Cobros.Estado      = INTEG(Cobros.Estado:SCREEN-VALUE)
             Cobros.Usu_Recaudo = W_Usuario
             Cobros.Observacion = Cobros.Observacion + "; F-" + STRING(W_Fecha,"999999") + ": " + EDITOR-1.
      FIND CURRENT Cobros NO-LOCK NO-ERROR.
   END.
    
   CLOSE QUERY Br_Cobros.
    
   RUN Compromi_Cte.    
    
   ASSIGN Btn_Salva:VISIBLE       = FALSE
          EDITOR-1:VISIBLE        = FALSE
          Cobros.Estado:SENSITIVE = FALSE
          W_CodCpto               = 0
          W_CodCpto:SCREEN-VALUE  = "0"
          W_SiIngr = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_SinGestion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SinGestion W-Con_Compromi
ON CHOOSE OF Btn_SinGestion IN FRAME F_Compromi /* Créditos Vencidos sin Gestión */
DO:
  IF Mora_Ini GT Mora_Fin THEN
     ASSIGN Mora_Fin              = Mora_Ini
            Mora_Fin:SCREEN-VALUE = STRING(Mora_Ini).

  IF Mora_Ini LE 0 OR Mora_Fin LE 0 THEN DO:
     MESSAGE "Los dias de mora Inicial y Final para el filtro, son necesarios."
         VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  ASSIGN FRAME F_Compromi:SENSITIVE = FALSE
         FRAME F_SinGestion:VISIBLE = TRUE.

  SESSION:SET-WAIT-STATE("General").
  RUN QUERY_Cred.
  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Cpto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Cpto W-Con_Compromi
ON VALUE-CHANGED OF Cmb_Cpto IN FRAME F_Compromi /* Gestión (Concepto) */
DO:
  ASSIGN W_CodCpto:SCREEN-VALUE = SUBSTRING(Cmb_Cpto:SCREEN-VALUE,1,5)
         W_CodCpto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EDITOR-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDITOR-1 W-Con_Compromi
ON LEAVE OF EDITOR-1 IN FRAME F_Compromi
DO:
  ASSIGN EDITOR-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FecFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FecFin W-Con_Compromi
ON LEAVE OF FecFin IN FRAME F_Compromi /* Fecha   Final */
DO:
  ASSIGN FecFin. 
    
  IF FecFin LT FecIni THEN
     ASSIGN FecFin:SCREEN-VALUE = STRING(FecIni)
            FecFin.
    
  APPLY "Mouse-Select-Click" TO Rs_FAcuerdOCompro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FecIni W-Con_Compromi
ON LEAVE OF FecIni IN FRAME F_Compromi /* Fecha Inicial */
DO:
  ASSIGN FecIni.                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_InfG
&Scoped-define SELF-NAME I_FecFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL I_FecFin W-Con_Compromi
ON LEAVE OF I_FecFin IN FRAME F_InfG /* Fecha Final */
DO:
  ASSIGN I_FecFin. 
    
  IF I_FecFin LT I_FecIni THEN
     ASSIGN I_FecFin:SCREEN-VALUE = STRING(I_FecIni)
            I_FecFin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME I_FecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL I_FecIni W-Con_Compromi
ON LEAVE OF I_FecIni IN FRAME F_InfG /* Fecha Inicial */
DO:
  ASSIGN I_FecIni.                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Compromi
&Scoped-define SELF-NAME Mora_Fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Mora_Fin W-Con_Compromi
ON LEAVE OF Mora_Fin IN FRAME F_Compromi /* Final */
DO:
  ASSIGN Mora_Fin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Mora_Ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Mora_Ini W-Con_Compromi
ON LEAVE OF Mora_Ini IN FRAME F_Compromi /* Inicial */
DO:
  ASSIGN Mora_Ini.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rs_FAcuerdOCompro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_FAcuerdOCompro W-Con_Compromi
ON MOUSE-SELECT-CLICK OF Rs_FAcuerdOCompro IN FRAME F_Compromi
DO:
  CLOSE QUERY BROWSE-11.   
  CLOSE QUERY Br_Tramites.

  ASSIGN Rs_FAcuerdOCompro.

  IF SUBSTRING(W_CmbUsu:SCREEN-VALUE,1,4) EQ "0000" OR 
     SUBSTRING(W_CmbUsu:SCREEN-VALUE,1,4) EQ ?      OR 
     SUBSTRING(W_CmbUsu:SCREEN-VALUE,1,4) EQ "    " THEN
     ASSIGN UsuIni = "0000"
            UsuFin = "9999".
  ELSE 
     ASSIGN UsuIni = SUBSTRING(W_CmbUsu:SCREEN-VALUE,1,4)
            UsuFin = SUBSTRING(W_CmbUsu:SCREEN-VALUE,1,4).

  IF Rs_FAcuerdOCompro EQ 0 THEN DO:
     Br_Cobros:TITLE = "COMPROMISOS PENDIENTES".
     CLOSE QUERY Br_Cobros.
    
     OPEN QUERY Br_Cobros FOR EACH Cobros WHERE Cobros.Agencia        GE W_OfiIni 
                                            AND Cobros.Agencia        LE W_OfiFin 
                                            AND Cobros.Usuario        GE UsuIni
                                            AND Cobros.Usuario        LE UsuFin
                                            AND Cobros.Fec_Compromiso GE FecIni
                                            AND Cobros.Fec_Compromiso LE FecFin            
                                            AND Cobros.Estado EQ 1 NO-LOCK,
                          EACH Creditos WHERE Creditos.Nit         EQ Cobros.Nit
                                          AND Creditos.Num_Credito EQ Cobros.Num_Credito
                                          AND Creditos.Sdo_Capital GT 0 NO-LOCK 
                              BY Cobros.Fec_Compromiso DESCEND INDEXED-REPOSITION.
  END.
  ELSE DO:
     Br_Cobros:TITLE = "COMPROMISOS Y GESTIONES POR FECHA DE MATRICULA".
     CLOSE QUERY Br_Cobros.
    
     OPEN QUERY Br_Cobros FOR EACH Cobros WHERE Cobros.Agencia        GE W_OfiIni 
                                            AND Cobros.Agencia        LE W_OfiFin 
                                            AND Cobros.Usuario        GE UsuIni
                                            AND Cobros.Usuario        LE UsuFin
                                            AND Cobros.Fec_Acuerdo    GE FecIni
                                            AND Cobros.Fec_Acuerdo    LE FecFin  NO-LOCK,
                          EACH Creditos WHERE Creditos.Nit         EQ Cobros.Nit
                                          AND Creditos.Num_Credito EQ Cobros.Num_Credito NO-LOCK 
                              BY Cobros.Fec_Acuerdo DESCEND INDEXED-REPOSITION.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_FAcuerdOCompro W-Con_Compromi
ON VALUE-CHANGED OF Rs_FAcuerdOCompro IN FRAME F_Compromi
DO:
   APPLY "Mouse-Select-Click" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CedNit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CedNit W-Con_Compromi
ON LEAVE OF W_CedNit IN FRAME F_Compromi /* Cédula/Nit */
DO:
   ASSIGN W_CedNit
          W_NomCte:SCREEN-VALUE = ""
          Btn_Salva:VISIBLE     = FALSE.

   FIND FIRST Clientes WHERE Clientes.Nit         EQ W_CedNit
                         AND Clientes.Estado      EQ 1 NO-LOCK NO-ERROR.
   IF NOT AVAIL(Clientes) AND W_CedNit GT "0" THEN DO:                                                                    
      ASSIGN FRAME F_Compromi:SENSITIVE = FALSE.                                                           
                                                                                                    
      RUN C-Clientes.R (INPUT  1,W_Agencia,                                                           
                        OUTPUT W_CedNit, OUTPUT W_NomCte, OUTPUT W_NomCte, OUTPUT W_Age).       
                                                                                                    
      ASSIGN FRAME F_Compromi:SENSITIVE = TRUE.                                                            
                                                                                                    
      FIND FIRST Clientes WHERE Clientes.Nit         EQ W_CedNit
                            AND Clientes.Estado      EQ 1 NO-LOCK NO-ERROR.                            
      IF AVAIL(Clientes) THEN DO:                                                                        
         ASSIGN W_CedNit:SCREEN-VALUE = W_CedNit                                                      
                W_NomCte:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +   
                                        " " + TRIM(Clientes.Nombre). 
         RUN Compromi_Cte.
      END.
      ELSE                                                                                            
         MESSAGE "El Nit debe existir Activo en Clientes." VIEW-AS ALERT-BOX.                                 
   END.                                                                                               
   ELSE IF W_CedNit GT "0" AND AVAIL(Clientes) THEN DO:
           ASSIGN W_NomCte:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +    
                                          " " + TRIM(Clientes.Nombre). 
           RUN Compromi_Cte.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CedNit W-Con_Compromi
ON MOUSE-SELECT-DBLCLICK OF W_CedNit IN FRAME F_Compromi /* Cédula/Nit */
DO:
   ASSIGN FRAME F_Compromi:SENSITIVE = FALSE.                                                           

   RUN C-Clientes.R (INPUT  1,W_Agencia,                                                           
                     OUTPUT W_CedNit, OUTPUT W_NomCte, OUTPUT W_NomCte, OUTPUT W_Age).

   ASSIGN W_CedNit:SCREEN-VALUE      = W_CedNit
          FRAME F_Compromi:SENSITIVE = TRUE.

   APPLY "Entry" TO SELF.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi W-Con_Compromi
ON VALUE-CHANGED OF W_CmbOfi IN FRAME F_Compromi /* Agencia */
DO:
  ASSIGN W_CmbUsu:LIST-ITEMS = "".
  W_CmbUsu:ADD-LAST("0000 Todos los Usuarios").

  IF INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) EQ 0 THEN DO:
     FIND FIRST Agencias WHERE Agencias.Agencia GT 0
                           AND Agencias.Estado  NE 3 NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN 
        ASSIGN W_OfiIni = Agencias.Agencia.
        
     FIND LAST Agencias WHERE Agencias.Agencia GT 0
                          AND Agencias.Estado  NE 3 NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN 
        ASSIGN W_OfiFin = Agencias.Agencia.

     FOR EACH Usuarios WHERE Usuarios.Estado EQ 1 NO-LOCK BY Usuarios.Agencia
                                                          BY Usuarios.Nombre:
         W_CmbUsu:ADD-LAST(STRING(Usuarios.Usuario,"X(4)") + " " + STRING(Usuarios.Nombre,"X(30)")).
     END.
  END.
  ELSE DO:
     ASSIGN W_OfiIni = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) 
            W_OfiFin = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)).

     FOR EACH Usuarios WHERE Usuarios.Agencia EQ W_OfiIni
                         AND Usuarios.Estado  EQ 1 NO-LOCK BY Usuarios.Nombre:
         W_CmbUsu:ADD-LAST(STRING(Usuarios.Usuario,"X(4)") + " " + STRING(Usuarios.Nombre,"X(30)")).
     END.
  END.

  APPLY "Entry" TO FecIni.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbUsu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbUsu W-Con_Compromi
ON VALUE-CHANGED OF W_CmbUsu IN FRAME F_Compromi /* Usuario */
DO:
  APPLY "Mouse-Select-Click" TO Rs_FAcuerdOCompro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CodCpto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CodCpto W-Con_Compromi
ON LEAVE OF W_CodCpto IN FRAME F_Compromi /* Código de la Gestión */
DO:
  ASSIGN W_CodCpto.

  FIND FIRST Varios WHERE Varios.Tipo EQ 30
                    AND Varios.Codigo EQ W_CODCpto
                    AND Varios.Estado EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Varios) THEN
     ASSIGN Cmb_Cpto:SCREEN-VALUE = (STRING(Varios.Codigo,"99999") + " - " + Varios.Descrip).
  ELSE
     ASSIGN Cmb_Cpto:SCREEN-VALUE  = Cmb_Cpto:ENTRY(1)
            W_CodCpto              = 0
            W_CodCpto:SCREEN-VALUE = "0".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-11
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Con_Compromi 


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
    
  W_CmbOfi:ADD-LAST("000 CONSOLIDADO").
  
  ASSIGN W_OfiIni                                = W_Agencia
         W_OfiFin                                = W_Agencia
         FecIni:SCREEN-VALUE IN FRAME F_Compromi = STRING(W_Fecha)
         FecIni
         FecFin:SCREEN-VALUE                     = STRING(W_Fecha)
         FecFin
         Btn_Salva:VISIBLE       = FALSE
         EDITOR-1:VISIBLE        = FALSE
         Cobros.Estado:SENSITIVE = FALSE.
                  
  FOR EACH Agencias WHERE Agencias.Estado  NE 3 
                      AND Agencias.Agencia GT 0 NO-LOCK:
      W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + 
                        "-" + STRING(Agencias.Nombre,"X(25)")).      
  END.   

  FOR EACH Varios WHERE Varios.Tipo   EQ 30
                    AND Varios.Estado EQ 1 NO-LOCK:
      Cmb_Cpto:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descrip).
  END.
     
  ASSIGN W_CmbOfi:SCREEN-VALUE = "000 CONSOLIDADO".
     
  APPLY "VALUE-CHANGED" TO W_CmbOfi.
  APPLY "Entry" TO W_CedNit.
    
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
     WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Compromi_Cte W-Con_Compromi 
PROCEDURE Compromi_Cte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 CLOSE QUERY Br_Cobros.
    
 OPEN QUERY Br_Cobros FOR EACH Cobros NO-LOCK WHERE Cobros.Nit    EQ W_CedNit 
                                                AND Cobros.Estado EQ 1,
                          EACH Creditos WHERE Creditos.Nit         EQ Cobros.Nit
                                          AND Creditos.Num_Credito EQ Cobros.Num_Credito
                                          AND Creditos.Sdo_Capital GT 0 NO-LOCK 
                               BY Cobros.Fec_Compromiso DESCEND INDEXED-REPOSITION.
        
 RUN NO-Pendientes. 
 RUN Tramites.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Detalle_UsuarioFA W-Con_Compromi 
PROCEDURE Detalle_UsuarioFA :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR Nro_Ges AS INTEG FORM "9999999"       INIT 0 EXTENT 3.
  DEFI VAR Nro_Cob AS INTEG FORM "9999999"       INIT 0 EXTENT 3.
  DEFI VAR Nro_Cum AS INTEG FORM "9999999"       INIT 0 EXTENT 3.
  DEFI VAR Vlr_Cob AS DEC FORM ">>>,>>>,>>>,>>>" INIT 0 EXTENT 3.
  DEFI VAR Vlr_Rec AS DEC FORM ">>>,>>>,>>>,>>>" INIT 0 EXTENT 3.

  DISPLAY "                                COOPERATIVA BELEN" SKIP(1)
          "REPORTE   : Detalle Gestion-Cobranza por Usuarios  -   FECHA: " + STRING(TODAY,"99/99/9999") + 
                                                              " - " + STRING(TIME,"hh:mm am") 
                            FORM "X(100)"
           SKIP
          "Desde     : " + STRING(I_FecIni,"99/99/9999") + " Hasta : " + STRING(I_FecFin,"99/99/9999") +
          "    Fechas de Acuerdo"
                            FORM "X(100)"
             SKIP
          "-------------------------------------------------------------------------------------"
      WITH WIDTH 120 NO-LABELS.

  FOR EACH Cobros WHERE Cobros.Usuario     GE UsuIni
                    AND Cobros.Usuario     LE UsuFin
                    AND Cobros.Fec_Acuerdo GE I_FecIni
                    AND Cobros.Fec_Acuerdo LE I_FecFin NO-LOCK 
               BREAK BY Cobros.Usuario BY Cobros.Fec_Acuerdo:
      IF FIRST-OF(Cobros.Usuario) THEN DO:
         FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Cobros.Usuario NO-LOCK NO-ERROR. 
         DISPLAY SKIP
                 "Usuario : "
                 Cobros.Usuario
                 Usuarios.Nombre WHEN AVAIL(Usuarios)
              WITH WIDTH 120 NO-LABELS.
      END.
        
      ASSIGN Nro_Ges[1] = Nro_Ges[1] + 1
             Nro_Ges[2] = Nro_Ges[2] + 1
             Nro_Ges[3] = Nro_Ges[3] + 1
             Vlr_Cob[1] = Vlr_Cob[1] + Cobros.Val_Compromi
             Vlr_Cob[2] = Vlr_Cob[2] + Cobros.Val_Compromi
             Vlr_Cob[3] = Vlr_Cob[3] + Cobros.Val_Compromi
             Vlr_Rec[1] = Vlr_Rec[1] + Cobros.Val_Cumplido
             Vlr_Rec[2] = Vlr_Rec[2] + Cobros.Val_Cumplido
             Vlr_Rec[3] = Vlr_Rec[3] + Cobros.Val_Cumplido.

      IF Cobros.Val_Compromi GT 0 THEN
         ASSIGN Nro_Cob[1] = Nro_Cob[1] + 1
                Nro_Cob[2] = Nro_Cob[2] + 1
                Nro_Cob[3] = Nro_Cob[3] + 1.

      IF Cobros.Val_Compromi GT 0 AND Cobros.Val_Cumplido GT 0 THEN
         ASSIGN Nro_Cum[1] = Nro_Cum[1] + 1
                Nro_Cum[2] = Nro_Cum[2] + 1
                Nro_Cum[3] = Nro_Cum[3] + 1.

      DISPLAY Cobros.Fec_Acuerdo   LABEL "F-Acuerdo"
              Cobros.Nit           LABEL "Ced/Nit Deudor"
              Cobros.Fec_Compromi  LABEL "F-Comprom"
              Cobros.Estado        LABEL "Est"
              Cobros.Val_Compromi  LABEL "Vr.Compromiso"
              Cobros.Val_Cumplido  LABEL "Vlr.Cumplido"
              Cobros.Num_Credito   LABEL "Nro.Credito"
              Cobros.Vr_Vencido    LABEL "V.Vencido Inic"
              Cobros.Observac      LABEL "Observacion sobre la Gestion" FORM "X(40)"
           WITH DOWN WIDTH 160 FRAME F1 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

      IF LAST-OF(Cobros.Usuario) THEN DO:
         DISPLAY "           -----------     ------------ ---------------   ---------- ---------------" SKIP
                 "TOT." 
                 Cobros.Usuario  
                 "   "
                 Nro_Ges[2]
                 Nro_Cob[2]  FORM ">>>>>9999999"   
                 "   "
                 Vlr_Cob[2] 
                 "  "
                 Nro_Cum[2]  FORM ">>>9999999"
                 Vlr_Rec[2]         
            WITH DOWN WIDTH 120 FRAME F2 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
            
         ASSIGN Nro_Ges[2] = 0
                Nro_Cob[2] = 0
                Vlr_Cob[2] = 0
                Vlr_Rec[2] = 0
                Nro_Cum[2] = 0.
      END.
  END.
    
  DISPLAY SKIP(1)
          "           -----------     ------------ ---------------   ---------- ---------------" SKIP

          "T.GENERAL"  
          "   "
           Nro_Ges[3]
           Nro_Cob[3]   FORM ">>>>>9999999" 
           "   "
           Vlr_Cob[3] 
           "  "
           Nro_Cum[3]   FORM ">>>9999999"
           Vlr_Rec[3]         
      WITH DOWN WIDTH 120 FRAME F3 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
            
  ASSIGN Nro_Ges[3] = 0
         Nro_Cob[3] = 0  
         Vlr_Cob[3] = 0         
         Vlr_Rec[3] = 0
         Nro_Cum[3] = 0.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Detalle_UsuarioFC W-Con_Compromi 
PROCEDURE Detalle_UsuarioFC :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR Nro_Ges AS INTEG FORM "9999999"       INIT 0 EXTENT 3.
  DEFI VAR Nro_Cob AS INTEG FORM "9999999"       INIT 0 EXTENT 3.
  DEFI VAR Nro_Cum AS INTEG FORM "9999999"       INIT 0 EXTENT 3.
  DEFI VAR Vlr_Cob AS DEC FORM ">>>,>>>,>>>,>>>" INIT 0 EXTENT 3.
  DEFI VAR Vlr_Rec AS DEC FORM ">>>,>>>,>>>,>>>" INIT 0 EXTENT 3.

  DISPLAY "                                COOPERATIVA BELEN" SKIP(1)
          "REPORTE   : Detalle Gestion-Cobranza por Usuarios  -   FECHA: " + STRING(TODAY,"99/99/9999") + 
                                                              " - " + STRING(TIME,"hh:mm am") 
                            FORM "X(100)"
           SKIP
          "Desde     : " + STRING(I_FecIni,"99/99/9999") + " Hasta : " + STRING(I_FecFin,"99/99/9999") +
          "    Fechas de Compromiso"
                            FORM "X(100)"
             SKIP
          "-------------------------------------------------------------------------------------"
      WITH WIDTH 120 NO-LABELS.

  FOR EACH Cobros WHERE Cobros.Usuario     GE UsuIni
                    AND Cobros.Usuario     LE UsuFin
                    AND Cobros.Fec_Comprom GE I_FecIni
                    AND Cobros.Fec_Comprom LE I_FecFin NO-LOCK 
               BREAK BY Cobros.Usuario BY Cobros.Fec_Comprom:
      IF FIRST-OF(Cobros.Usuario) THEN DO:
         FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Cobros.Usuario NO-LOCK NO-ERROR. 
         DISPLAY SKIP
                 "Usuario : "
                 Cobros.Usuario
                 Usuarios.Nombre WHEN AVAIL(Usuarios)
              WITH WIDTH 120 NO-LABELS.
      END.
        
      ASSIGN Nro_Ges[1] = Nro_Ges[1] + 1
             Nro_Ges[2] = Nro_Ges[2] + 1
             Nro_Ges[3] = Nro_Ges[3] + 1
             Vlr_Cob[1] = Vlr_Cob[1] + Cobros.Val_Compromi
             Vlr_Cob[2] = Vlr_Cob[2] + Cobros.Val_Compromi
             Vlr_Cob[3] = Vlr_Cob[3] + Cobros.Val_Compromi
             Vlr_Rec[1] = Vlr_Rec[1] + Cobros.Val_Cumplido
             Vlr_Rec[2] = Vlr_Rec[2] + Cobros.Val_Cumplido
             Vlr_Rec[3] = Vlr_Rec[3] + Cobros.Val_Cumplido.

      IF Cobros.Val_Compromi GT 0 THEN
         ASSIGN Nro_Cob[1] = Nro_Cob[1] + 1
                Nro_Cob[2] = Nro_Cob[2] + 1
                Nro_Cob[3] = Nro_Cob[3] + 1.

      IF Cobros.Val_Compromi GT 0 AND Cobros.Val_Cumplido GT 0 THEN
         ASSIGN Nro_Cum[1] = Nro_Cum[1] + 1
                Nro_Cum[2] = Nro_Cum[2] + 1
                Nro_Cum[3] = Nro_Cum[3] + 1.

      DISPLAY Cobros.Fec_Acuerdo   LABEL "F-Acuerdo"
              Cobros.Nit           LABEL "Ced/Nit Deudor"
              Cobros.Fec_Compromi  LABEL "F-Comprom"
              Cobros.Estado        LABEL "Est"
              Cobros.Val_Compromi  LABEL "Vr.Compromiso"
              Cobros.Val_Cumplido  LABEL "Vlr.Cumplido"
              Cobros.Num_Credito   LABEL "Nro.Credito"
              Cobros.Vr_Vencido    LABEL "V.Vencido Inic"
              Cobros.Observac      LABEL "Observacion sobre la Gestion" FORM "X(40)"
         WITH DOWN WIDTH 160 FRAME F1 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

      IF LAST-OF(Cobros.Usuario) THEN DO:
         DISPLAY "           -----------     ------------ ---------------   ---------- ---------------" SKIP
                 "TOT." 
                 Cobros.Usuario  
                 "   "
                 Nro_Ges[2]
                 Nro_Cob[2]  FORM ">>>>>9999999"   
                 "   "
                 Vlr_Cob[2] 
                 "  "
                 Nro_Cum[2]  FORM ">>>9999999"
                 Vlr_Rec[2]         
            WITH DOWN WIDTH 120 FRAME F2 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
            
         ASSIGN Nro_Ges[2] = 0
                Nro_Cob[2] = 0
                Vlr_Cob[2] = 0
                Vlr_Rec[2] = 0
                Nro_Cum[2] = 0.
      END.
  END.
    
  DISPLAY SKIP(1)
          "           -----------     ------------ ---------------   ---------- ---------------" SKIP

          "T.GENERAL"  
          "   "
           Nro_Ges[3]
           Nro_Cob[3]   FORM ">>>>>9999999" 
           "   "
           Vlr_Cob[3] 
           "  "
           Nro_Cum[3]   FORM ">>>9999999"
           Vlr_Rec[3]         
      WITH DOWN WIDTH 120 FRAME F3 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
            
  ASSIGN Nro_Ges[3] = 0
         Nro_Cob[3] = 0  
         Vlr_Cob[3] = 0         
         Vlr_Rec[3] = 0
         Nro_Cum[3] = 0.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Con_Compromi  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Con_Compromi)
  THEN DELETE WIDGET W-Con_Compromi.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Con_Compromi  _DEFAULT-ENABLE
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
  DISPLAY Rs_FAcuerdOCompro W_CedNit W_CmbOfi FecIni FecFin EDITOR-1 W_NomCte 
          W_CmbUsu Mora_Ini Mora_Fin W_CodCpto Cmb_Cpto 
      WITH FRAME F_Compromi IN WINDOW W-Con_Compromi.
  IF AVAILABLE Cobros THEN 
    DISPLAY Cobros.Fec_Compromiso Cobros.Val_Compromiso Cobros.Estado 
          Cobros.Num_Credito 
      WITH FRAME F_Compromi IN WINDOW W-Con_Compromi.
  ENABLE BROWSE-11 Br_Tramites Btn_InfG Btn_SinGestion Rs_FAcuerdOCompro 
         W_CedNit W_CmbOfi FecIni FecFin EDITOR-1 Cobros.Estado Btn_IngComp 
         Btn_Salva Br_Cobros W_CmbUsu Mora_Ini Mora_Fin W_CodCpto Cmb_Cpto 
         RECT-321 RECT-322 
      WITH FRAME F_Compromi IN WINDOW W-Con_Compromi.
  {&OPEN-BROWSERS-IN-QUERY-F_Compromi}
  ENABLE Br_cred Btn_OutConAcuerdo 
      WITH FRAME F_SinGestion IN WINDOW W-Con_Compromi.
  {&OPEN-BROWSERS-IN-QUERY-F_SinGestion}
  DISPLAY I_FecIni I_FecFin Cmb_Inf 
      WITH FRAME F_InfG IN WINDOW W-Con_Compromi.
  ENABLE I_FecIni I_FecFin Cmb_Inf Btn_Impresion Btn_OutCon 
      WITH FRAME F_InfG IN WINDOW W-Con_Compromi.
  {&OPEN-BROWSERS-IN-QUERY-F_InfG}
  VIEW W-Con_Compromi.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jkjkj W-Con_Compromi 
PROCEDURE jkjkj :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  TCreditos.Agencia FORMAT "999":U
      TCreditos.Nit FORMAT "X(12)":U
      TCreditos.Cod_Credito FORMAT "999":U
      TCreditos.Num_Credito FORMAT "999999999":U
      TCreditos.Val_Atraso FORMAT ">>,>>>,>>>,>>9":U
      TCreditos.Dias_Atraso FORMAT "99999":U
      TCreditos.Sdo_Capital FORMAT "->>>>>,>>>,>>>,>>9.99":U
      TCreditos.Categoria FORMAT "X":U
      TCreditos.Fec_Pago FORMAT "99/99/9999":U
      TCreditos.Fec_UltPago FORMAT "99/99/9999":U
      TCreditos.Abogado FORMAT "yes/no":U
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE No-Pendientes W-Con_Compromi 
PROCEDURE No-Pendientes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 CLOSE QUERY BROWSE-11.   
        
 OPEN QUERY BROWSE-11 FOR EACH Cobros WHERE Cobros.Nit    EQ W_CedNit
                                        AND Cobros.Estado GT 1 NO-LOCK,
        EACH Creditos WHERE Creditos.Nit         EQ Cobros.Nit
                        AND Creditos.Num_Credito EQ Cobros.Num_Credito
                        /*AND Creditos.Sdo_Capital GT 0*/ NO-LOCK  INDEXED-REPOSITION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Con_Compromi 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF Cmb_Inf:SCREEN-VALUE IN FRAME F_InfG EQ "Resumen por Usuario" THEN DO:     
     IF Rs_FAcuerdOCompro EQ 0 THEN
        RUN Resumen_UsuarioFC.
     ELSE
        RUN Resumen_UsuarioFA.
  END.
  ELSE IF Cmb_Inf:SCREEN-VALUE EQ "Créditos por Agencia" THEN      
     RUN Resumen_CredXAg.
  ELSE IF Cmb_Inf:SCREEN-VALUE EQ "Detallado X Usuario" THEN DO:     
     IF Rs_FAcuerdOCompro EQ 0 THEN
        RUN Detalle_UsuarioFC.   /*Por fechas de Compromiso*/
     ELSE 
        RUN Detalle_UsuarioFA.   /*Por fechas de Acuerdo*/
  END.
  ELSE IF Cmb_Inf:SCREEN-VALUE EQ "Créditos Vdos sin Gestión" THEN DO:
     IF Mora_Ini GT Mora_Fin THEN
        ASSIGN Mora_Fin              = Mora_Ini
               Mora_Fin:SCREEN-VALUE IN FRAME F_Compromi = STRING(Mora_Ini).

     IF Mora_Ini LE 0 OR Mora_Fin LE 0 THEN DO:                                   
        MESSAGE "Los dias de mora Inicial y Final para el filtro, son necesarios."
            VIEW-AS ALERT-BOX ERROR.                                              
        RETURN.                                                                   
     END. 

     RUN Vdos_SinGestion.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Query_Cred W-Con_Compromi 
PROCEDURE Query_Cred :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH TCreditos: DELETE TCreditos. END.
  CLOSE QUERY Br_Cred.

  FOR EACH Creditos WHERE Creditos.Agencia     GE W_OfiIni
                      AND Creditos.Agencia     LE W_OfiFin
                      AND Creditos.Estado      EQ 2 
                      AND Creditos.Dias_Atraso GE Mora_Ini
                      AND Creditos.Dias_Atraso LE Mora_Fin NO-LOCK:
      FIND FIRST Cobros WHERE Cobros.Nit       EQ Creditos.Nit
                        AND Cobros.Num_Credito EQ Creditos.Num_Credito
                        AND Cobros.Fec_Acuerdo GE W_Fecha - Creditos.Dias_Atraso NO-LOCK NO-ERROR.
      IF NOT AVAIL(Cobros) THEN DO:
         CREATE TCreditos.
         BUFFER-COPY Creditos TO TCreditos.
      END.
  END.

  OPEN QUERY Br_Cred FOR EACH TCreditos NO-LOCK BY TCreditos.Agencia
                                                BY TCreditos.Dias_Atraso DESCEND  INDEXED-REPOSITION.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen_CredXAg W-Con_Compromi 
PROCEDURE Resumen_CredXAg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR W_FecIni AS DATE.

  DEFI VAR Nro_Cre  AS INTEG FORM "9999999"       INIT 0.
  DEFI VAR Vlr_Sdo  LIKE Creditos.Sdo_Capital     INIT 0.
  DEFI VAR Vlr_Vdo  LIKE Creditos.Val_Atraso      INIT 0.
  DEFI VAR Vlr_Cuo  LIKE Creditos.Cuota           INIT 0.
  DEFI VAR Nro_CreH  AS INTEG FORM "9999999"      INIT 0.
  DEFI VAR Vlr_SdoH  LIKE Creditos.Sdo_Capital    INIT 0. 
  DEFI VAR Vlr_VdoH  LIKE Creditos.Val_Atraso     INIT 0. 
  DEFI VAR Vlr_CuoH  LIKE Creditos.Cuota          INIT 0. 

  ASSIGN W_FecIni = DATE(MONTH(W_Fecha),01,YEAR(W_Fecha))
         W_FecIni = W_FecIni - 1.    /*Ultimo dia del mes anterior*/


  DISPLAY "                                COOPERATIVA BELEN" SKIP(1)
          "REPORTE   : Resumen Creditos por Agencia Comparado  -                                 " 
          + "FECHA: " + STRING(TODAY,"99/99/9999") + 
                                                              " - " + STRING(TIME,"hh:mm am") 
                            FORM "X(120)"
          SKIP
          "                    V A L O R E S    A   " + STRING(W_FecIni,"99/99/9999") + 
          "                 I                 V A L O R E S   A   H O Y                      I     "
          + "  D I F E R E N C I A"   FORM "X(200)" 
          SKIP
          "--------------------------------------------------------------------I----------------------------------------------------------------I-------------------------------"
      WITH WIDTH 250 NO-LABELS.

  FOR EACH Cred_Ag: DELETE Cred_Ag. END.

  FOR EACH PlanPagos WHERE PlanPagos.Id_PdoMes EQ 3
                       AND PlanPagos.Fec_Ini   EQ W_FecIni NO-LOCK
                  BREAK BY PlanPagos.Agencia   BY PlanPagos.Categoria:
      ASSIGN Nro_Cre = Nro_Cre + 1
             Vlr_Sdo = Vlr_Sdo + (PlanPagos.Monto - PlanPagos.Pagos_CapitalAcum)
             Vlr_Cuo = Vlr_Cuo + PlanPagos.Cuota.

      ASSIGN Vlr_Vdo = Vlr_Vdo + (PlanPagos.Capital_Acum - PlanPagos.Pagos_CapitalAcum) WHEN 
                                  PlanPagos.Capital_Acum GT PlanPagos.Pagos_CapitalAcum.

      IF LAST-OF(PlanPagos.Categoria) THEN DO:
         CREATE Cred_Ag.
         ASSIGN Cred_Ag.Age  = PlanPagos.Agencia
                Cred_Ag.Cat  = PlanPagos.Categoria
                Cred_Ag.Sdo  = Vlr_Sdo
                Cred_Ag.Vdo  = Vlr_Vdo
                Cred_Ag.Cuo  = Vlr_Cuo
                Cred_Ag.Ncre = Nro_Cre
                Vlr_Sdo      = 0
                Vlr_Vdo      = 0
                Vlr_Cuo      = 0
                Nro_Cre      = 0.
      END.
  END.

  ASSIGN Vlr_Sdo      = 0
         Vlr_Vdo      = 0
         Vlr_Cuo      = 0
         Nro_Cre      = 0.

  FOR EACH Creditos WHERE Creditos.Estado  EQ 2 AND Creditos.Sdo_Capital GT 0 NO-LOCK
                 BREAK BY Creditos.Agencia BY Creditos.Categoria:
      ASSIGN Nro_Cre = Nro_Cre + 1
             Vlr_Sdo = Vlr_Sdo + Creditos.Sdo_Capital
             Vlr_Cuo = Vlr_Cuo + Creditos.Cuota
             Vlr_Vdo = Vlr_Vdo + Creditos.Val_Atraso.

      IF LAST-OF(Creditos.Categoria) THEN DO:
         FIND FIRST Cred_Ag WHERE Cred_Ag.Age EQ Creditos.Agencia
                              AND Cred_Ag.Cat EQ Creditos.Categoria NO-ERROR.
         IF NOT AVAIL(Cred_Ag) THEN
            CREATE Cred_Ag.

         ASSIGN Cred_Ag.Age   = Creditos.Agencia
                Cred_Ag.Cat   = Creditos.Categoria
                Cred_Ag.SdoH  = Vlr_Sdo
                Cred_Ag.VdoH  = Vlr_Vdo
                Cred_Ag.CuoH  = Vlr_Cuo
                Cred_Ag.NcreH = Nro_Cre
                Vlr_Sdo       = 0
                Vlr_Vdo       = 0
                Vlr_Cuo       = 0
                Nro_Cre       = 0.
      END.
  END.

  ASSIGN Vlr_Sdo      = 0
         Vlr_Vdo      = 0
         Vlr_Cuo      = 0
         Nro_Cre      = 0.

  FOR EACH Cred_Ag BREAK BY Cred_Ag.Age BY Cred_Ag.Cat:
      IF FIRST-OF(Cred_Ag.Age) THEN DO:
         FIND FIRST Agencias WHERE Agencias.Agencia EQ Cred_Ag.Age NO-LOCK NO-ERROR.

         DISPLAY "AGENCIA : " Cred_Ag.Age
                 Agencias.Nombre WHEN AVAIL(Agencias)
             WITH NO-LABELS.
      END.

      ASSIGN Nro_Cre  = Nro_Cre  + Cred_Ag.Ncre
             Vlr_Sdo  = Vlr_Sdo  + Cred_Ag.Sdo
             Vlr_Cuo  = Vlr_Cuo  + Cred_Ag.Cuo
             Vlr_Vdo  = Vlr_Vdo  + Cred_Ag.Vdo
             Nro_CreH = Nro_CreH + Cred_Ag.NcreH
             Vlr_SdoH = Vlr_SdoH + Cred_Ag.SdoH 
             Vlr_CuoH = Vlr_CuoH + Cred_Ag.CuoH 
             Vlr_VdoH = Vlr_VdoH + Cred_Ag.VdoH. 

      DISPLAY Cred_Ag.Cat      LABEL "Cat"
              Cred_Ag.Ncre     LABEL "Creditos"
              Cred_Ag.Sdo      LABEL "Vlr.Saldos Capital"   FORM "->>>>>>>>,>>>,>>9"
              Cred_Ag.Vdo      LABEL "Vr.Vencido Capital"
              Cred_Ag.Cuo      LABEL "Valor de Cuotas"      FORM "->>>>>>>>,>>>,>>9"
              Cred_Ag.NcreH    LABEL "Creditos"        
              Cred_Ag.SdoH     LABEL "Vlr.Saldos Capital"   FORM "->>>>>>>>,>>>,>>9"
              Cred_Ag.VdoH     LABEL "Vr.Vencido Capital" 
              Cred_Ag.CuoH     LABEL "Valor de Cuotas"      FORM "->>>>>>>>,>>>,>>9"
              Cred_Ag.Vdo  - Cred_Ag.VdoH  LABEL "Difer.Vlr.Vencido" FORM "->>>>>>>,>>>,>>9"
              Cred_Ag.Ncre - Cred_Ag.NcreH LABEL "Difer.No.Vdos" FORM "-9999999" 
           WITH DOWN WIDTH 200 FRAME F1 NO-BOX NO-LABELS USE-TEXT STREAM-IO.
        
      IF LAST-OF(Cred_Ag.Age) THEN DO:         
         DISPLAY "    -------- ------------------ ------------------ ----------------- -------- ------------------ ------------------ ----------------- ----------------- -------------" SKIP
                 "   "
                 Nro_Cre    FORM ">999999"
                 " "
                 Vlr_Sdo    FORM "->>>>>>>>,>>>,>>9"
                 "   "
                 Vlr_Vdo  
                 Vlr_Cuo    FORM "->>>>>>>>,>>>,>>9"
                 Nro_CreH   FORM ">>999999"
                 Vlr_SdoH   FORM "->>>>>,>>>,>>>,>>9"
                 "   "
                 Vlr_VdoH 
                 Vlr_CuoH   FORM "->>>>>>>>,>>>,>>9" 
                 Vlr_Vdo - Vlr_VdoH   FORM "->>>>>>>>,>>>,>>9"
                 Nro_Cre - Nro_CreH   FORM "->>>>>9999999"  SKIP(1)      
              WITH DOWN WIDTH 200 FRAME F2 NO-BOX NO-LABELS USE-TEXT STREAM-IO.
                                        
         ASSIGN Nro_Cre  = 0                               
                Vlr_Sdo  = 0                               
                Vlr_Cuo  = 0                               
                Vlr_Vdo  = 0                        
                Nro_CreH = 0                        
                Vlr_SdoH = 0                        
                Vlr_CuoH = 0                        
                Vlr_VdoH = 0.                                                                 
      END.  
  END.

  RUN Tot_Ag.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen_UsuarioFA W-Con_Compromi 
PROCEDURE Resumen_UsuarioFA :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR Nro_Ges     AS INTEG FORM "9999999"       INIT 0 EXTENT 3.
  DEFI VAR Nro_Cob     AS INTEG FORM "9999999"       INIT 0 EXTENT 3.
  DEFI VAR Nro_Cum     AS INTEG FORM "9999999"       INIT 0 EXTENT 3.
  DEFI VAR Nro_Parcial AS INTEG FORM "9999999"       INIT 0 EXTENT 3.
  DEFI VAR Vlr_Cob     AS DEC FORM ">>>,>>>,>>>,>>>" INIT 0 EXTENT 3.
  DEFI VAR Vlr_Rec     AS DEC FORM ">>>,>>>,>>>,>>>" INIT 0 EXTENT 3.

  DISPLAY "                                COOPERATIVA BELEN" SKIP(1)
          "REPORTE   : Resumen Gestion-Cobranza por Usuarios  -   FECHA: " + STRING(TODAY,"99/99/9999") + 
                                                              " - " + STRING(TIME,"hh:mm am") 
                            FORM "X(100)"
           SKIP
          "Desde     : " + STRING(I_FecIni,"99/99/9999") + " Hasta : " + STRING(I_FecFin,"99/99/9999") + 
          "    Fechas de Acuerdo"
                            FORM "X(100)"
          SKIP
          "----------------------------------------------------------------------------------------"
      WITH WIDTH 120 NO-LABELS.

  FOR EACH Cobros WHERE Cobros.Fec_Acuerdo GE I_FecIni
                    AND Cobros.Fec_Acuerdo LE I_FecFin NO-LOCK 
               BREAK BY Cobros.Usuario BY Cobros.Agencia BY Cobros.Num_Credito:
      IF FIRST-OF(Cobros.Usuario) THEN DO:
         FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Cobros.Usuario NO-LOCK NO-ERROR. 
         DISPLAY SKIP
                 "Usuario : "
                 Cobros.Usuario
                 Usuarios.Nombre WHEN AVAIL(Usuarios)
              WITH WIDTH 120 NO-LABELS.
      END.
        
      ASSIGN Nro_Ges[1] = Nro_Ges[1] + 1
             Nro_Ges[2] = Nro_Ges[2] + 1
             Nro_Ges[3] = Nro_Ges[3] + 1
             Vlr_Cob[1] = Vlr_Cob[1] + Cobros.Val_Compromi
             Vlr_Cob[2] = Vlr_Cob[2] + Cobros.Val_Compromi
             Vlr_Cob[3] = Vlr_Cob[3] + Cobros.Val_Compromi
             Vlr_Rec[1] = Vlr_Rec[1] + Cobros.Val_Cumplido
             Vlr_Rec[2] = Vlr_Rec[2] + Cobros.Val_Cumplido
             Vlr_Rec[3] = Vlr_Rec[3] + Cobros.Val_Cumplido.

      IF Cobros.Val_Compromi GT 0 THEN
         ASSIGN Nro_Cob[1] = Nro_Cob[1] + 1
                Nro_Cob[2] = Nro_Cob[2] + 1
                Nro_Cob[3] = Nro_Cob[3] + 1.

      IF Cobros.Val_Compromi GT 0 AND Cobros.Val_Cumplido GE Cobros.Val_Compromi THEN
         ASSIGN Nro_Cum[1] = Nro_Cum[1] + 1
                Nro_Cum[2] = Nro_Cum[2] + 1
                Nro_Cum[3] = Nro_Cum[3] + 1.
      ELSE IF Cobros.Val_Compromi GT 0 AND Cobros.Val_Cumplido GT 0
      AND     Cobros.Val_Cumplido LT Cobros.Val_Compromi THEN
         ASSIGN Nro_Parcial[1] = Nro_Parcial[1] + 1
                Nro_Parcial[2] = Nro_Parcial[2] + 1
                Nro_Parcial[3] = Nro_Parcial[3] + 1. 

      IF LAST-OF(Cobros.Agencia) THEN DO:
         DISPLAY Cobros.Agencia     LABEL "Age."
                 Nro_Ges[1]         LABEL "No.Gestiones"
                 Nro_Cob[1]         LABEL "#Compromisos"                 
                 Vlr_Cob[1]         LABEL "Vlr-Compromisos"
                 Nro_Cum[1]         LABEL "#Cumplidos"
                 Nro_Parcial[1]     LABEL "#Cumpl.Parcial"
                 Vlr_Rec[1]         LABEL "Valor-Recaudos"
            WITH DOWN WIDTH 120 FRAME F1 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
            
         ASSIGN Nro_Ges[1] = 0
                Nro_Cob[1] = 0
                Vlr_Cob[1] = 0
                Vlr_Rec[1] = 0
                Nro_Cum[1] = 0
                Nro_Parcial[1] = 0.
      END.
        
      IF LAST-OF(Cobros.Usuario) THEN DO:
         DISPLAY "       ----------- ----------- --------------- ---------- -------------- ---------------" SKIP
                 "TOT." 
                 Cobros.Usuario  
                 Nro_Ges[2]
                 Nro_Cob[2]      FORM ">>>>>9999999"       
                 Vlr_Cob[2] 
                 Nro_Cum[2]      FORM ">>>9999999"
                 Nro_Parcial[2]  FORM ">>>>>>>9999999"
                 Vlr_Rec[2]         
            WITH DOWN WIDTH 120 FRAME F2 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
            
         ASSIGN Nro_Ges[2] = 0
                Nro_Cob[2] = 0
                Vlr_Cob[2] = 0
                Vlr_Rec[2] = 0
                Nro_Parcial[2] = 0
                Nro_Cum[2]     = 0.
      END.
  END.
    
  DISPLAY SKIP(1)
          "       ----------- ----------- --------------- ---------- -------------- ---------------" SKIP
          "T.GENERAL"     
           Nro_Ges[3]
           Nro_Cob[3]     FORM ">>>>>9999999"       
           Vlr_Cob[3] 
           Nro_Cum[3]     FORM ">>>9999999"
           Nro_Parcial[3] FORM ">>>>>>>9999999"
           Vlr_Rec[3]         
      WITH DOWN WIDTH 120 FRAME F3 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
            
  ASSIGN Nro_Ges[3] = 0
         Nro_Cob[3] = 0  
         Vlr_Cob[3] = 0         
         Vlr_Rec[3] = 0
         Nro_Cum[3] = 0
         Nro_Parcial[3] = 0.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Resumen_UsuarioFC W-Con_Compromi 
PROCEDURE Resumen_UsuarioFC :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR Nro_Ges     AS INTEG FORM "9999999"       INIT 0 EXTENT 3.
  DEFI VAR Nro_Cob     AS INTEG FORM "9999999"       INIT 0 EXTENT 3.
  DEFI VAR Nro_Cum     AS INTEG FORM "9999999"       INIT 0 EXTENT 3.
  DEFI VAR Vlr_Cob     AS DEC FORM ">>>,>>>,>>>,>>>" INIT 0 EXTENT 3.
  DEFI VAR Vlr_Rec     AS DEC FORM ">>>,>>>,>>>,>>>" INIT 0 EXTENT 3.
  DEFI VAR Nro_Parcial AS INTEG FORM "9999999"       INIT 0 EXTENT 3.

  DISPLAY "                                COOPERATIVA BELEN" SKIP(1)
          "REPORTE   : Resumen Gestion-Cobranza por Usuarios  -   FECHA: " + STRING(TODAY,"99/99/9999") + 
                                                              " - " + STRING(TIME,"hh:mm am") 
                            FORM "X(100)"
           SKIP
          "Desde     : " + STRING(I_FecIni,"99/99/9999") + " Hasta : " + STRING(I_FecFin,"99/99/9999")  +
          "    Fechas de Compromiso"
                            FORM "X(100)"
          SKIP
          "----------------------------------------------------------------------------------------"
      WITH WIDTH 120 NO-LABELS.

  FOR EACH Cobros WHERE Cobros.Fec_Comprom GE I_FecIni
                    AND Cobros.Fec_Comprom LE I_FecFin NO-LOCK 
               BREAK BY Cobros.Usuario BY Cobros.Agencia BY Cobros.Num_Credito:
      IF FIRST-OF(Cobros.Usuario) THEN DO:
         FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Cobros.Usuario NO-LOCK NO-ERROR. 
         DISPLAY SKIP
                 "Usuario : "
                 Cobros.Usuario
                 Usuarios.Nombre WHEN AVAIL(Usuarios)
              WITH WIDTH 120 NO-LABELS.
      END.
        
      ASSIGN Nro_Ges[1] = Nro_Ges[1] + 1
             Nro_Ges[2] = Nro_Ges[2] + 1
             Nro_Ges[3] = Nro_Ges[3] + 1
             Vlr_Cob[1] = Vlr_Cob[1] + Cobros.Val_Compromi
             Vlr_Cob[2] = Vlr_Cob[2] + Cobros.Val_Compromi
             Vlr_Cob[3] = Vlr_Cob[3] + Cobros.Val_Compromi
             Vlr_Rec[1] = Vlr_Rec[1] + Cobros.Val_Cumplido
             Vlr_Rec[2] = Vlr_Rec[2] + Cobros.Val_Cumplido
             Vlr_Rec[3] = Vlr_Rec[3] + Cobros.Val_Cumplido.

      IF Cobros.Val_Compromi GT 0 THEN
         ASSIGN Nro_Cob[1] = Nro_Cob[1] + 1
                Nro_Cob[2] = Nro_Cob[2] + 1
                Nro_Cob[3] = Nro_Cob[3] + 1.

      IF Cobros.Val_Compromi GT 0 AND Cobros.Val_Cumplido GE Cobros.Val_Compromi THEN
         ASSIGN Nro_Cum[1] = Nro_Cum[1] + 1
                Nro_Cum[2] = Nro_Cum[2] + 1
                Nro_Cum[3] = Nro_Cum[3] + 1.
      ELSE IF Cobros.Val_Compromi GT 0 AND Cobros.Val_Cumplido GT 0
      AND     Cobros.Val_Cumplido LT Cobros.Val_Compromi THEN
         ASSIGN Nro_Parcial[1] = Nro_Parcial[1] + 1
                Nro_Parcial[2] = Nro_Parcial[2] + 1
                Nro_Parcial[3] = Nro_Parcial[3] + 1. 

      IF LAST-OF(Cobros.Agencia) THEN DO:
         DISPLAY Cobros.Agencia     LABEL "Age."
                 Nro_Ges[1]         LABEL "No.Gestiones"
                 Nro_Cob[1]         LABEL "#Compromisos"                 
                 Vlr_Cob[1]         LABEL "Vlr-Compromisos"
                 Nro_Cum[1]         LABEL "#Cumplidos"
                 Nro_Parcial[1]     LABEL "#Cumpl.Parcial"
                 Vlr_Rec[1]         LABEL "Vlr-Recaudos"
            WITH DOWN WIDTH 120 FRAME F1 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
            
         ASSIGN Nro_Ges[1] = 0
                Nro_Cob[1] = 0
                Vlr_Cob[1] = 0
                Vlr_Rec[1] = 0
                Nro_Cum[1] = 0
                Nro_Parcial[1] = 0.
      END.
        
      IF LAST-OF(Cobros.Usuario) THEN DO:
         DISPLAY "       ----------- ----------- --------------- ---------- -------------- ---------------" SKIP
                 "TOT." 
                 Cobros.Usuario  
                 Nro_Ges[2]
                 Nro_Cob[2]      FORM ">>>>>9999999"       
                 Vlr_Cob[2] 
                 Nro_Cum[2]      FORM ">>>9999999"
                 Nro_Parcial[2]  FORM ">>>>>>>9999999"
                 Vlr_Rec[2]         
            WITH DOWN WIDTH 120 FRAME F2 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
            
         ASSIGN Nro_Ges[2] = 0
                Nro_Cob[2] = 0
                Vlr_Cob[2] = 0
                Vlr_Rec[2] = 0
                Nro_Cum[2] = 0
                Nro_Parcial[2] = 0.
      END.
  END.
    
  DISPLAY SKIP(1)
          "       ----------- ----------- --------------- ---------- -------------- ---------------" SKIP
          "T.GENERAL"     
           Nro_Ges[3]
           Nro_Cob[3]      FORM ">>>>>9999999"       
           Vlr_Cob[3] 
           Nro_Cum[3]      FORM ">>>9999999"
           Nro_Parcial[3]  FORM ">>>>>>>9999999"
           Vlr_Rec[3]         
      WITH DOWN WIDTH 120 FRAME F3 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
            
  ASSIGN Nro_Ges[3] = 0
         Nro_Cob[3] = 0  
         Vlr_Cob[3] = 0         
         Vlr_Rec[3] = 0
         Nro_Cum[3] = 0
         Nro_Parcial[3] = 0.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tot_Ag W-Con_Compromi 
PROCEDURE Tot_Ag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR Nro_Cre  AS INTEG FORM "9999999"       INIT 0.
  DEFI VAR Vlr_Sdo  LIKE Creditos.Sdo_Capital     INIT 0.
  DEFI VAR Vlr_Vdo  LIKE Creditos.Val_Atraso      INIT 0.
  DEFI VAR Vlr_Cuo  LIKE Creditos.Cuota           INIT 0.
  DEFI VAR Nro_CreH  AS INTEG FORM "9999999"      INIT 0.
  DEFI VAR Vlr_SdoH  LIKE Creditos.Sdo_Capital    INIT 0. 
  DEFI VAR Vlr_VdoH  LIKE Creditos.Val_Atraso     INIT 0. 
  DEFI VAR Vlr_CuoH  LIKE Creditos.Cuota          INIT 0.

  DEFI VAR Nro_CreF  AS INTEG FORM "9999999"       INIT 0.
  DEFI VAR Vlr_SdoF  LIKE Creditos.Sdo_Capital     INIT 0.
  DEFI VAR Vlr_VdoF  LIKE Creditos.Val_Atraso      INIT 0.
  DEFI VAR Vlr_CuoF  LIKE Creditos.Cuota           INIT 0.
  DEFI VAR Nro_CreHF  AS INTEG FORM "9999999"      INIT 0.
  DEFI VAR Vlr_SdoHF  LIKE Creditos.Sdo_Capital    INIT 0. 
  DEFI VAR Vlr_VdoHF  LIKE Creditos.Val_Atraso     INIT 0. 
  DEFI VAR Vlr_CuoHF  LIKE Creditos.Cuota          INIT 0.

  DISPLAY SKIP(2)
          "                        T O T A L   C O O P E R A T I V A" SKIP
          WITH WIDTH 120 NO-LABELS.

  FOR EACH Cred_Ag BREAK BY Cred_Ag.Cat:
      ASSIGN Nro_Cre  = Nro_Cre  + Cred_Ag.Ncre
             Vlr_Sdo  = Vlr_Sdo  + Cred_Ag.Sdo
             Vlr_Cuo  = Vlr_Cuo  + Cred_Ag.Cuo
             Vlr_Vdo  = Vlr_Vdo  + Cred_Ag.Vdo
             Nro_CreH = Nro_CreH + Cred_Ag.NcreH
             Vlr_SdoH = Vlr_SdoH + Cred_Ag.SdoH 
             Vlr_CuoH = Vlr_CuoH + Cred_Ag.CuoH 
             Vlr_VdoH = Vlr_VdoH + Cred_Ag.VdoH.

      ASSIGN Nro_CreF  = Nro_CreF  + Cred_Ag.Ncre  
             Vlr_SdoF  = Vlr_SdoF  + Cred_Ag.Sdo   
             Vlr_CuoF  = Vlr_CuoF  + Cred_Ag.Cuo   
             Vlr_VdoF  = Vlr_VdoF  + Cred_Ag.Vdo   
             Nro_CreHF = Nro_CreHF + Cred_Ag.NcreH 
             Vlr_SdoHF = Vlr_SdoHF + Cred_Ag.SdoH  
             Vlr_CuoHF = Vlr_CuoHF + Cred_Ag.CuoH  
             Vlr_VdoHF = Vlr_VdoHF + Cred_Ag.VdoH. 



      IF LAST-OF(Cred_Ag.Cat) THEN DO:         
         DISPLAY Cred_Ag.Cat
                 " "
                 Nro_Cre    FORM ">999999"
                 " "
                 Vlr_Sdo    FORM "->>>>>>>>,>>>,>>9"
                 "   "
                 Vlr_Vdo  
                 Vlr_Cuo    FORM "->>>>>>>>,>>>,>>9"
                 Nro_CreH   FORM ">>999999"
                 Vlr_SdoH   FORM "->>>>>,>>>,>>>,>>9"
                 "   "
                 Vlr_VdoH 
                 Vlr_CuoH   FORM "->>>>>>>>,>>>,>>9" 
                 Vlr_Vdo - Vlr_VdoH   FORM "->>>>>>>>,>>>,>>9"
                 Nro_Cre - Nro_CreH   FORM "->>>>>9999999"  SKIP(0)      
              WITH DOWN WIDTH 200 FRAME F2 NO-BOX NO-LABELS USE-TEXT STREAM-IO.
                                        
         ASSIGN Nro_Cre  = 0                               
                Vlr_Sdo  = 0                               
                Vlr_Cuo  = 0                               
                Vlr_Vdo  = 0                        
                Nro_CreH = 0                        
                Vlr_SdoH = 0                        
                Vlr_CuoH = 0                        
                Vlr_VdoH = 0.                                                                 
      END.  
  END.

  DISPLAY "    -------- ------------------ ------------------ ----------------- -------- ------------------ ------------------ ----------------- ----------------- -------------" SKIP
             "   "                                                          
             Nro_CreF    FORM ">999999"                                          
             " "                                                                
             Vlr_SdoF    FORM "->>>>>>>>,>>>,>>9"                                
             "   "                                                              
             Vlr_VdoF                                                            
             Vlr_CuoF    FORM "->>>>>>>>,>>>,>>9"                                
             Nro_CreHF   FORM ">>999999"                                         
             Vlr_SdoHF   FORM "->>>>>,>>>,>>>,>>9"                               
             "   "                                                              
             Vlr_VdoHF                                                           
             Vlr_CuoHF   FORM "->>>>>>>>,>>>,>>9"                                
             Vlr_VdoF - Vlr_VdoHF   FORM "->>>>>>>>,>>>,>>9"                      
             Nro_CreF - Nro_CreHF   FORM "->>>>>9999999"  SKIP(1)                 
          WITH DOWN WIDTH 200 FRAME FT2 NO-BOX NO-LABELS USE-TEXT STREAM-IO.     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tramites W-Con_Compromi 
PROCEDURE Tramites :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  OPEN QUERY Br_Tramites FOR EACH Cobros WHERE Cobros.Nit    EQ W_CedNit
                                           AND Cobros.Estado EQ 0 NO-LOCK,
        EACH Creditos WHERE Creditos.Nit         EQ Cobros.Nit
                        AND Creditos.Num_Credito EQ Cobros.Num_Credito
                        /*AND Creditos.Sdo_Capital GT 0*/ NO-LOCK  INDEXED-REPOSITION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Vdos_SinGestion W-Con_Compromi 
PROCEDURE Vdos_SinGestion :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
  DISPLAY "                                COOPERATIVA BELEN" SKIP(1)
          "REPORTE   : Detalle Creditos Sin Gestion-Cobranza    -   FECHA: " + STRING(TODAY,"99/99/9999") + 
                                                              " - " + STRING(TIME,"hh:mm am") 
                            FORM "X(100)"
           SKIP
          "Desde     : " + STRING(Mora_Ini,"999999") + " Hasta : " + STRING(Mora_Fin,"999999") +
          "  Dias de Mora"
                            FORM "X(100)"
             SKIP
          "-------------------------------------------------------------------------------------"
      WITH WIDTH 120 NO-LABELS.


  FOR EACH Creditos WHERE Creditos.Agencia     GE W_OfiIni
                      AND Creditos.Agencia     LE W_OfiFin
                      AND Creditos.Estado      EQ 2 
                      AND Creditos.Dias_Atraso GE Mora_Ini
                      AND Creditos.Dias_Atraso LE Mora_Fin NO-LOCK
                       BY Creditos.Agencia     BY Creditos.Sdo_Capital DESCEND:
      FIND FIRST Cobros WHERE Cobros.Nit       EQ Creditos.Nit
                        AND Cobros.Num_Credito EQ Creditos.Num_Credito
                        AND Cobros.Fec_Acuerdo GE W_Fecha - Creditos.Dias_Atraso NO-LOCK NO-ERROR.
      IF NOT AVAIL(Cobros) THEN DO:
         DISPLAY Creditos.Agencia                         LABEL "Ag."               
                 Creditos.Nit                             LABEL "Ced./Nit"                                                          
                 Creditos.Cod_Credito                     LABEL "Pdto"              
                 Creditos.Num_Credito                     LABEL "#Credito"                                               
                 Creditos.Val_Atraso                      LABEL "Capital Vencido" FORM ">>>>,>>>,>>9"
                 Creditos.Dias_Atraso                     LABEL "Dias-V"         
                 Creditos.Sdo_Capital                     LABEL "Saldo Capital"   FORM ">>>>,>>>,>>9"                                      
                 Creditos.Categoria                       LABEL "Cat"               
                 Creditos.Fec_Pago                        LABEL "F-ProxPago"        
                 Creditos.Fec_UltPago                     LABEL "F-UltPago"         
                 Creditos.Abogado                         LABEL "Abog"                                                      
                 Creditos.Cuota                           LABEL "Valor Cuota"     FORM ">>>>,>>>,>>9"   
                 Creditos.Cuo_Atraso                      LABEL "CuoVdas"          
                 Creditos.Cuo_Pagadas                     LABEL "CuoPagas"         
                 Creditos.Fec_Desembolso                  LABEL "F-Desemb." SKIP (0)
             WITH DOWN WIDTH 200 FRAME DetCSG NO-BOX NO-LABEL USE-TEXT STREAM-IO.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

