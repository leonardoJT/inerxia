&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*----------------------------------------------------------------------------------
Conciliacion Bancaria
W-Conciliacion.w

----------------------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

 {incluido/VARIABLE.I "SHARED"}
 {incluido/VARCON.I "SHARED"}

  DEFINE VARIABLE W_Status       AS LOGICAL.
  DEFINE VARIABLE W_GrabaRgCero  AS LOGICAL INITIAL FALSE.
  DEFINE VARIABLE W_PorValor     AS LOGICAL INITIAL FALSE.
  DEFINE VARIABLE W_SobreExt     AS INTEGER.
  DEFINE VARIABLE Curr-record    AS ROWID.
  DEFINE VARIABLE W-Num1         AS CHARACTER.

  DEFINE VARIABLE W_OfStr        AS CHARACTER FORMAT "X(20)".
  DEFINE VARIABLE W_CheMov     LIKE Mov_Contable.Doc_Referencia.

  DEFINE VARIABLE W_SecExt       AS INTEGER INITIAL 0.
  DEFINE VARIABLE W_SecMov       AS INTEGER INITIAL 1.
  DEFINE VARIABLE W_SecMov1      AS INTEGER INITIAL 0.
  DEFINE VARIABLE W_FCorte       AS DATE INITIAL ?.

  DEFINE VARIABLE RegLinea       AS CHARACTER FORMAT "X(100)".
  DEFINE VARIABLE num_incon      AS INTEGER INITIAL 0 NO-UNDO.
  DEFINE VARIABLE num_proce      AS INTEGER INITIAL 0 NO-UNDO. 

  DEFINE VARIABLE procname        AS CHARACTER FORMAT "X(12)".
  DEFINE VARIABLE I               AS INTEGER.
  DEFINE VARIABLE W-Fec           AS INTEGER.
  DEFINE VARIABLE W_ExDeb       LIKE Mov_CtaConta.Valor         INITIAL 0.
  DEFINE VARIABLE W_ExCre       LIKE Mov_CtaConta.Valor         INITIAL 0.
  DEFINE VARIABLE W_MvDebi      LIKE Mov_CtaConta.Valor         INITIAL 0. 
  DEFINE VARIABLE W_MvCred      LIKE Mov_CtaConta.Valor         INITIAL 0.
  DEFINE VARIABLE W_TotGen      LIKE Mov_CtaConta.Valor         INITIAL 0.
  DEFINE VARIABLE F-CruStr        AS CHARACTER FORMAT "X(13)".
  DEFINE VARIABLE W-CruStr        AS CHARACTER FORMAT "X(13)".
  DEFINE VARIABLE W_RegCtaConta   AS CHARACTER FORMAT "X(13)".
  DEFINE VARIABLE W_Entero        AS INTEGER.
  DEFINE VARIABLE Op_Salida       AS CHARACTER INITIAL "".

  DEFINE VARIABLE W_Proceso      AS CHARACTER FORMAT "X(12)".
  DEFINE VARIABLE W_Ofi        LIKE Agencias.agencia.

  DEFINE VARIABLE W_Titul        AS CHARACTER FORMAT "X(40)" INITIAL "".
  DEFINE VARIABLE W_FecInic      AS DATE. 
  DEFINE VARIABLE W-MesAnt       AS INTEGER FORMAT "99".
  DEFINE VARIABLE W-AnoAnt       AS INTEGER FORMAT "9999". 
  DEFINE VARIABLE W_Dia          AS INTEGER FORMAT "99".
  DEFINE VARIABLE W_MesC         AS INTEGER FORMAT "99".
  DEFINE VARIABLE W_DiaC         AS INTEGER FORMAT "99".
  DEFINE VARIABLE W_Ano          AS INTEGER FORMAT "9999".
  DEFINE VARIABLE W_FormatoExt LIKE Cuentas.Cod_FormatoExt.
  DEFINE VARIABLE W_Cruce        AS CHARACTER FORMAT "X(8)".
  DEFINE VARIABLE W_MvReg      LIKE Mov_CtaConta.Reg_CtaConta.
  DEFINE VARIABLE W_ExReg      LIKE Mov_CtaConta.Reg_CtaConta. 
  DEFINE VARIABLE W_Cont         AS INTEGER.
  DEFINE VARIABLE W_Continua     AS LOGICAL.
  DEFINE VARIABLE W_ConsulCon    AS LOGICAL INITIAL FALSE.
  DEFINE VARIABLE W_SoloConsulta AS LOGICAL INITIAL NO.
  DEFINE VARIABLE W_UltDia       AS INTEGER EXTENT 12 INITIAL
         [31,29,31,30,31,30,31,31,30,31,30,31].
  DEFINE VARIABLE W_PerConcil    AS DATE .
         
 /* Variables para los informes */
 
  DEFINE VARIABLE W_Titul2         AS CHARACTER FORMAT "X(25)".
  DEFINE VARIABLE W_Banco          AS CHARACTER FORMAT "X(20)".

/* Tablas Temporales para la Conciliacion */

  DEFINE TEMP-TABLE Tmp-CtaConta LIKE Mov_CtaConta
         INDEX Inx_Valor Num_Cheque
                         Naturaleza
                         Valor
         INDEX Inx_Naturaleza Naturaleza
                              Valor
         INDEX Inx_Documento Num_Cheque
                             Valor.
        
  DEFINE TEMP-TABLE Tmp-Extracto LIKE Mov_Extracto
         INDEX Inx_Valor Num_Cheque
                         Naturaleza
                         Valor
         INDEX Inx_Naturaleza Naturaleza
                             Valor
         INDEX Inx_Documento Num_Cheque
                             Valor.

  DEFINE TEMP-TABLE Tmp-Concilia
         FIELD Num_Cheque          LIKE Mov_CtaConta.Num_Cheque
         FIELD Valor               LIKE Mov_CtaConta.Valor
         FIELD Puntero               AS ROWID
         INDEX Inx_Documento IS PRIMARY UNIQUE Num_Cheque
                                               Valor
                                               Puntero
         INDEX Inx_Valor             IS UNIQUE Valor
                                               Num_Cheque
                                               Puntero.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Concil
&Scoped-define BROWSE-NAME Brw-Concil

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Conciliacion Mov_CtaConta Mov_Extracto ~
Tmp-CtaConta Tmp-Extracto Formatos

/* Definitions for BROWSE Brw-Concil                                    */
&Scoped-define FIELDS-IN-QUERY-Brw-Concil Conciliacion.Novedad Conciliacion.Fecha_Conciliacion Mov_CtaConta.Num_Cheque Mov_CtaConta.Secuencia Mov_CtaConta.Naturaleza Mov_CtaConta.Valor STRING(SUBSTRING(Conciliacion.Reg_Cruce,1,2) + SUBSTRING(Conciliacion.Reg_Cruce,9,5)) @ W_Cruce Mov_Extracto.Num_Cheque Mov_Extracto.Secuencia Mov_Extracto.Naturaleza Mov_Extracto.Valor   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw-Concil   
&Scoped-define SELF-NAME Brw-Concil
&Scoped-define QUERY-STRING-Brw-Concil FOR EACH Conciliacion WHERE (    Conciliacion.Agencia                      EQ W_Ofi                                                    AND Conciliacion.Cuenta                       EQ W_CtaBco                                                    AND Conciliacion.Per_Conciliacion             LE W_FecCorte)                                                    AND (    (    Conciliacion.Novedad            NE ""                                                              AND Conciliacion.Novedad            NE "CC")                                                          OR (    Conciliacion.Fecha_Conciliacion GE W_FecInic                                                              AND Conciliacion.Fecha_Conciliacion LE W_FecCorte)                                                          OR (    Conciliacion.Per_Conciliacion   GE W_FecInic                                                              AND Conciliacion.Novedad            EQ "CC"))                                             NO-LOCK, ~
                                 EACH Mov_CtaConta WHERE Mov_CtaConta.Agencia       EQ W_Ofi                                               AND Mov_CtaConta.Cuenta        EQ W_CtaBco                                               AND Mov_CtaConta.Reg_CtaConta  EQ Conciliacion.Reg_CtaConta                                             OUTER-JOIN NO-LOCK, ~
                                 EACH Mov_Extracto WHERE Mov_Extracto.Agencia       EQ W_Ofi                                               AND Mov_Extracto.Cuenta        EQ W_CtaBco                                               AND Mov_Extracto.Reg_MovExtrac EQ Conciliacion.Reg_MovExtrac                                             OUTER-JOIN NO-LOCK                                             BY Conciliacion.Agencia                                             BY Conciliacion.Cuenta                                             BY Conciliacion.Novedad                                             BY Conciliacion.Reg_Cruce                                             BY Mov_CtaConta.Num_Cheque                                             BY Mov_Extracto.Num_Cheque. RUN Inicia_TmpCon. RUN Llena_TmpCon
&Scoped-define OPEN-QUERY-Brw-Concil OPEN QUERY Brw-Concil FOR EACH Conciliacion WHERE (    Conciliacion.Agencia                      EQ W_Ofi                                                    AND Conciliacion.Cuenta                       EQ W_CtaBco                                                    AND Conciliacion.Per_Conciliacion             LE W_FecCorte)                                                    AND (    (    Conciliacion.Novedad            NE ""                                                              AND Conciliacion.Novedad            NE "CC")                                                          OR (    Conciliacion.Fecha_Conciliacion GE W_FecInic                                                              AND Conciliacion.Fecha_Conciliacion LE W_FecCorte)                                                          OR (    Conciliacion.Per_Conciliacion   GE W_FecInic                                                              AND Conciliacion.Novedad            EQ "CC"))                                             NO-LOCK, ~
                                 EACH Mov_CtaConta WHERE Mov_CtaConta.Agencia       EQ W_Ofi                                               AND Mov_CtaConta.Cuenta        EQ W_CtaBco                                               AND Mov_CtaConta.Reg_CtaConta  EQ Conciliacion.Reg_CtaConta                                             OUTER-JOIN NO-LOCK, ~
                                 EACH Mov_Extracto WHERE Mov_Extracto.Agencia       EQ W_Ofi                                               AND Mov_Extracto.Cuenta        EQ W_CtaBco                                               AND Mov_Extracto.Reg_MovExtrac EQ Conciliacion.Reg_MovExtrac                                             OUTER-JOIN NO-LOCK                                             BY Conciliacion.Agencia                                             BY Conciliacion.Cuenta                                             BY Conciliacion.Novedad                                             BY Conciliacion.Reg_Cruce                                             BY Mov_CtaConta.Num_Cheque                                             BY Mov_Extracto.Num_Cheque. RUN Inicia_TmpCon. RUN Llena_TmpCon.
&Scoped-define TABLES-IN-QUERY-Brw-Concil Conciliacion Mov_CtaConta ~
Mov_Extracto
&Scoped-define FIRST-TABLE-IN-QUERY-Brw-Concil Conciliacion
&Scoped-define SECOND-TABLE-IN-QUERY-Brw-Concil Mov_CtaConta
&Scoped-define THIRD-TABLE-IN-QUERY-Brw-Concil Mov_Extracto


/* Definitions for BROWSE Brw-MovCtaConta                               */
&Scoped-define FIELDS-IN-QUERY-Brw-MovCtaConta Tmp-CtaConta.Fecha Tmp-CtaConta.Num_Cheque Tmp-CtaConta.Secuencia Tmp-CtaConta.Valor Tmp-CtaConta.Naturaleza Tmp-CtaConta.Descripcion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw-MovCtaConta   
&Scoped-define SELF-NAME Brw-MovCtaConta
&Scoped-define QUERY-STRING-Brw-MovCtaConta FOR EACH Tmp-CtaConta WHERE (Tmp-CtaConta.Agencia EQ W_Ofi                                                    AND Tmp-CtaConta.Cuenta   EQ W_CtaBco                                                    AND Tmp-CtaConta.Fecha    LE W_FecCorte)                                                  NO-LOCK                                                     BY Tmp-CtaConta.Agencia                                                     BY Tmp-CtaConta.Cuenta                                                     BY Tmp-CtaConta.Num_Cheque                                                     BY Tmp-CtaConta.Secuencia
&Scoped-define OPEN-QUERY-Brw-MovCtaConta OPEN QUERY Brw-MovCtaConta FOR EACH Tmp-CtaConta WHERE (Tmp-CtaConta.Agencia EQ W_Ofi                                                    AND Tmp-CtaConta.Cuenta   EQ W_CtaBco                                                    AND Tmp-CtaConta.Fecha    LE W_FecCorte)                                                  NO-LOCK                                                     BY Tmp-CtaConta.Agencia                                                     BY Tmp-CtaConta.Cuenta                                                     BY Tmp-CtaConta.Num_Cheque                                                     BY Tmp-CtaConta.Secuencia.
&Scoped-define TABLES-IN-QUERY-Brw-MovCtaConta Tmp-CtaConta
&Scoped-define FIRST-TABLE-IN-QUERY-Brw-MovCtaConta Tmp-CtaConta


/* Definitions for BROWSE Brw-MovExtrac                                 */
&Scoped-define FIELDS-IN-QUERY-Brw-MovExtrac Tmp-Extracto.Fecha Tmp-Extracto.Num_Cheque Tmp-Extracto.Secuencia Tmp-Extracto.Valor Tmp-Extracto.Naturaleza Tmp-Extracto.Descripcion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw-MovExtrac   
&Scoped-define SELF-NAME Brw-MovExtrac
&Scoped-define QUERY-STRING-Brw-MovExtrac FOR EACH Tmp-Extracto WHERE (    Tmp-Extracto.Agencia      EQ W_Ofi                                                       AND Tmp-Extracto.Cuenta        EQ W_CtaBco                                                       AND Tmp-Extracto.Fecha         LE W_FecCorte)                                                   AND Tmp-Extracto.Num_Cheque    NE "0000000000"                                                NO-LOCK                                                    BY Tmp-Extracto.Agencia                                                    BY Tmp-Extracto.Cuenta                                                    BY Tmp-Extracto.Num_Cheque                                                    BY Tmp-Extracto.Secuencia
&Scoped-define OPEN-QUERY-Brw-MovExtrac OPEN QUERY Brw-MovExtrac FOR EACH Tmp-Extracto WHERE (    Tmp-Extracto.Agencia      EQ W_Ofi                                                       AND Tmp-Extracto.Cuenta        EQ W_CtaBco                                                       AND Tmp-Extracto.Fecha         LE W_FecCorte)                                                   AND Tmp-Extracto.Num_Cheque    NE "0000000000"                                                NO-LOCK                                                    BY Tmp-Extracto.Agencia                                                    BY Tmp-Extracto.Cuenta                                                    BY Tmp-Extracto.Num_Cheque                                                    BY Tmp-Extracto.Secuencia.
&Scoped-define TABLES-IN-QUERY-Brw-MovExtrac Tmp-Extracto
&Scoped-define FIRST-TABLE-IN-QUERY-Brw-MovExtrac Tmp-Extracto


/* Definitions for BROWSE B_Formatos                                    */
&Scoped-define FIELDS-IN-QUERY-B_Formatos Formatos.Nom_Formato Formatos.Nom_Proceso   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Formatos   
&Scoped-define SELF-NAME B_Formatos
&Scoped-define QUERY-STRING-B_Formatos FOR EACH Formatos WHERE Formatos.Id_Formato = "AC"  NO-LOCK
&Scoped-define OPEN-QUERY-B_Formatos OPEN QUERY B_Formatos FOR EACH Formatos WHERE Formatos.Id_Formato = "AC"  NO-LOCK.
&Scoped-define TABLES-IN-QUERY-B_Formatos Formatos
&Scoped-define FIRST-TABLE-IN-QUERY-B_Formatos Formatos


/* Definitions for FRAME F-Concil                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Concil ~
    ~{&OPEN-QUERY-Brw-Concil}

/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-Brw-MovCtaConta}~
    ~{&OPEN-QUERY-Brw-MovExtrac}

/* Definitions for FRAME F_Formato                                      */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Formato ~
    ~{&OPEN-QUERY-B_Formatos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Brw-Concil Btn-DesManual Btn-Desmar ~
Btn-Salir 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Btn-Manual Btn-AutExt 
&Scoped-define List-2 F-Main Rs_ActExt W_CheExt W_ValExt W_DiaExt R_NatExt ~
W_DescExt F_Ext 
&Scoped-define List-4 F-Main Btn_Grabar F_Ext 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Buscar3 
       MENU-ITEM m_Documento3   LABEL "Documento"     
       MENU-ITEM m_Valor3       LABEL "Valor"         
       MENU-ITEM m_Anterior_________F82 LABEL "Anterior          F8"
       MENU-ITEM m_Siguiente________F9 LABEL "Siguiente        F9".

DEFINE MENU POPUP-MENU-Brw-Concil 
       SUB-MENU  m_Buscar3      LABEL "Buscar"        
       RULE
       MENU-ITEM m_Desmarcar_Cruce_Manual LABEL "Seleccionar Cruce Manual"
       MENU-ITEM m_Desmarcar_Seleccin3 LABEL "Desmarcar Selección".

DEFINE SUB-MENU m_Buscar 
       MENU-ITEM m_Documento    LABEL "Documento"     
       MENU-ITEM m_Valor        LABEL "Valor"         
       MENU-ITEM m_Anterior______F8 LABEL "Anterior           F8"
       MENU-ITEM m_Siguiente__________F9 LABEL "Siguiente         F9".

DEFINE MENU POPUP-MENU-Brw-MovCtaConta 
       SUB-MENU  m_Buscar       LABEL "Buscar"        
       RULE
       MENU-ITEM m_Desmarcar_Seleccin2 LABEL "Desmarcar Selección".

DEFINE SUB-MENU m_Buscar2 
       MENU-ITEM m_Documento2   LABEL "Documento"     
       MENU-ITEM m_Valor2       LABEL "Valor"         
       MENU-ITEM m_Anterior_________F8 LABEL "Anterior           F8"
       MENU-ITEM m_Siguiente_________F9 LABEL "Siguiente         F9".

DEFINE MENU POPUP-MENU-Brw-MovExtrac 
       SUB-MENU  m_Buscar2      LABEL "Buscar"        
       RULE
       MENU-ITEM m_Desmarcar_Seleccin LABEL "Desmarcar Selección".


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-DesManual 
     IMAGE-UP FILE "Imagenes\cruceman":U
     LABEL "Btn 85" 
     SIZE 8 BY 1.62 TOOLTIP "Desmarca Número de Cruce (Manual) del Periodo".

DEFINE BUTTON Btn-Desmar 
     LABEL "Desmarcar" 
     SIZE 12 BY 1.62 TOOLTIP "Desmarca Sin Número de Cruce (Automático)"
     FONT 5.

DEFINE BUTTON Btn-Salir 
     IMAGE-UP FILE "Imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 9 BY 1.62 TOOLTIP "Regresa a los Movimientos".

DEFINE BUTTON Btn-AutExt 
     IMAGE-UP FILE "Imagenes\hand06":U
     IMAGE-INSENSITIVE FILE "imagenes/hand06.ico":U
     LABEL "Automática" 
     SIZE 11 BY 1.62 TOOLTIP "Actualización Extracto Desde un Archivo Plano".

DEFINE BUTTON Btn-Borrar 
     IMAGE-UP FILE "Imagenes/borrar.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/borrar.bmp":U
     LABEL "Borrar" 
     SIZE 11 BY 1.62 TOOLTIP "Borra Registros de la Conciliación o Extracto".

DEFINE BUTTON Btn-Cancel DEFAULT 
     LABEL "S&alir" 
     SIZE 11 BY 1.62 TOOLTIP "Sale del Programa"
     BGCOLOR 8 FONT 5.

DEFINE BUTTON Btn-Manual 
     IMAGE-UP FILE "Imagenes\keybrd02":U
     IMAGE-INSENSITIVE FILE "imagenes/keybrd02.ico":U
     LABEL "Man&ual" 
     SIZE 11 BY 1.62 TOOLTIP "Actualización Manual Extracto".

DEFINE BUTTON Btn-Multi 
     IMAGE-UP FILE "Imagenes\delimtxt":U
     IMAGE-INSENSITIVE FILE "imagenes/delimtxt.bmp":U
     LABEL "" 
     SIZE 11 BY 1.62 TOOLTIP "Cruce Manual Entre Movimientos".

DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "Imagenes/interrogacion.bmp":U
     LABEL "Ayuda" 
     SIZE 5 BY 1.23 TOOLTIP "Accesa la Ayuda de la Ventana"
     FONT 4.

DEFINE BUTTON Btn_Concilia 
     IMAGE-UP FILE "Imagenes/proceso.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes\proceso.bmp":U
     LABEL "&Conciliar" 
     SIZE 11 BY 1.62 TOOLTIP "Concilia Automaticamente  los Movientos"
     FONT 4.

DEFINE BUTTON Btn_ConConc 
     IMAGE-UP FILE "Imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Consul&ta" 
     SIZE 11 BY 1.62 TOOLTIP "Consulta la Conciliación"
     FONT 4.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "Imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "I&mprimir" 
     SIZE 11 BY 1.62 TOOLTIP "Imprime Informes de Conciliación"
     FONT 4.

DEFINE BUTTON BUTTON-85 
     IMAGE-UP FILE "Imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "Imagenes/informacion.bmp":U
     LABEL "Button 85" 
     SIZE 11 BY 1.65.

DEFINE VARIABLE Cmb_OfiCon AS CHARACTER FORMAT "X(35)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 25 BY 1 TOOLTIP "Seleccione la Agencia que va a Conciliar"
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-Holgura AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Días Holgura Conciliación" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaBco AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cta de Banco" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .81 TOOLTIP "Ingrese la Cuenta que va a Conciliar"
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE W_FecCorte AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha para Conciliar" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .81
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE W_NomBco AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 51 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-63
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13 BY 10.5.

DEFINE RECTANGLE RECT-65
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13 BY 5.38.

DEFINE RECTANGLE RECT-66
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 3.77.

DEFINE VARIABLE F-Cruce AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Número Cruce" 
     VIEW-AS FILL-IN 
     SIZE 6.72 BY .81
     BGCOLOR 15 FGCOLOR 0 FONT 4 NO-UNDO.

DEFINE VARIABLE W-Num AS INTEGER FORMAT ">>>>>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE W-Valor AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE BUTTON Btn_CancBorrar 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Salir" 
     SIZE 8 BY 1.62
     FONT 5.

DEFINE VARIABLE W_RegBorra AS INTEGER FORMAT ">>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Rs_Borrar AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Solo Conciliación", 1,
"Extracto y Conciliación ", 2
     SIZE 27 BY 3.77
     BGCOLOR 17 FGCOLOR 7 FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.86 BY 1.62.

DEFINE BUTTON Btn_Cancelar 
     LABEL "&Cancelar" 
     SIZE 14 BY 1.62 TOOLTIP "Cancelar".

DEFINE BUTTON Btn_Grabar 
     LABEL "&Grabar" 
     SIZE 14 BY 1.62 TOOLTIP "Guarda el Registro Digitado del Extracto".

DEFINE BUTTON Btn_SalirAct 
     IMAGE-UP FILE "Imagenes/volver.bmp":U
     LABEL "&Salir Manual" 
     SIZE 14 BY 1.62 TOOLTIP "Salir de la Grabación Manual".

DEFINE VARIABLE W_CheExt AS CHARACTER FORMAT "X(10)":U INITIAL "?" 
     LABEL "Número Documento" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DescExt AS CHARACTER FORMAT "X(25)":U 
     LABEL "Descripción" 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DiaExt AS INTEGER FORMAT "99":U INITIAL ? 
     LABEL "Dia" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_SecExt1 AS INTEGER FORMAT ">>>>9":U INITIAL 1 
     LABEL "Secuencia" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_ValExt AS DECIMAL FORMAT "->>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Valor Transacción" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rs_ActExt AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Adicionar", 1,
"Modificar", 2
     SIZE 25 BY 1.35
     FONT 5 NO-UNDO.

DEFINE VARIABLE R_NatExt AS CHARACTER INITIAL "DB" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Consignación", "DB",
"Cobrado", "CR"
     SIZE 17 BY 2.15 NO-UNDO.

DEFINE RECTANGLE RECT-64
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31 BY 1.88.

DEFINE BUTTON Btn_AceFor 
     LABEL "&Aceptar" 
     SIZE 13 BY 1.62 TOOLTIP "Confirma la Selección del Formato".

DEFINE BUTTON Btn_CanFor 
     IMAGE-UP FILE "Imagenes/volver.bmp":U
     LABEL "Ca&ncelar" 
     SIZE 13 BY 1.88 TOOLTIP "Cancela la Selección del Formato".

DEFINE BUTTON BUTTON-83 
     LABEL "&Imprimir" 
     SIZE 15 BY 1.62.

DEFINE BUTTON BUTTON-84 
     IMAGE-UP FILE "Imagenes/volver.bmp":U
     LABEL "Ca&ncelar" 
     SIZE 15 BY 1.62.

DEFINE VARIABLE W_SdoExt AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Saldo del banco" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Tipo AS CHARACTER INITIAL "CC" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Conciliados", "CC",
"No Cobrados", "NC",
"No Girados", "NG",
"Val.Coop diferente a Val.Extracto", "VD",
"No Consignados", "NO",
"Días Holgura", "NF",
"No Registrados", "NR",
"Total", "TO",
"Resumen de Conciliación", "RE"
     SIZE 37 BY 7.27 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Brw-Concil FOR 
      Conciliacion, 
      Mov_CtaConta, 
      Mov_Extracto SCROLLING.

DEFINE QUERY Brw-MovCtaConta FOR 
      Tmp-CtaConta SCROLLING.

DEFINE QUERY Brw-MovExtrac FOR 
      Tmp-Extracto SCROLLING.

DEFINE QUERY B_Formatos FOR 
      Formatos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Brw-Concil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw-Concil W-Win _FREEFORM
  QUERY Brw-Concil NO-LOCK DISPLAY
      Conciliacion.Novedad            COLUMN-LABEL "Nov"
      Conciliacion.Fecha_Conciliacion COLUMN-LABEL "Fecha Concil."
      Mov_CtaConta.Num_Cheque         COLUMN-LABEL "No.Documen"
      Mov_CtaConta.Secuencia          COLUMN-LABEL "Sec"
      Mov_CtaConta.Naturaleza         COLUMN-LABEL "Nat."
      Mov_CtaConta.Valor
      STRING(SUBSTRING(Conciliacion.Reg_Cruce,1,2) + SUBSTRING(Conciliacion.Reg_Cruce,9,5)) @ W_Cruce COLUMN-LABEL "  #Cruce  "
      Mov_Extracto.Num_Cheque         COLUMN-LABEL "No.Documen"
      Mov_Extracto.Secuencia          COLUMN-LABEL "Sec"
      Mov_Extracto.Naturaleza         COLUMN-LABEL "Nat."
      Mov_Extracto.Valor
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS MULTIPLE SIZE 89.72 BY 9.42
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .42.

DEFINE BROWSE Brw-MovCtaConta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw-MovCtaConta W-Win _FREEFORM
  QUERY Brw-MovCtaConta NO-LOCK DISPLAY
      Tmp-CtaConta.Fecha COLUMN-LABEL "       Fecha     " FORMAT "99/99/9999"
      Tmp-CtaConta.Num_Cheque COLUMN-LABEL "No.Documento"
      Tmp-CtaConta.Secuencia
      Tmp-CtaConta.Valor
      Tmp-CtaConta.Naturaleza COLUMN-LABEL "Nat"
      Tmp-CtaConta.Descripcion FORMAT "X(55)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS MULTIPLE SIZE 90 BY 5.38
         BGCOLOR 15 FONT 4
         TITLE BGCOLOR 15 "Movimiento Cooperativa".

DEFINE BROWSE Brw-MovExtrac
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw-MovExtrac W-Win _FREEFORM
  QUERY Brw-MovExtrac NO-LOCK DISPLAY
      Tmp-Extracto.Fecha COLUMN-LABEL "       Fecha     " FORMAT "99/99/9999"
      Tmp-Extracto.Num_Cheque COLUMN-LABEL "No.Documento"
      Tmp-Extracto.Secuencia
      Tmp-Extracto.Valor
      Tmp-Extracto.Naturaleza COLUMN-LABEL "Nat" 
      Tmp-Extracto.Descripcion FORMAT "X(55)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS MULTIPLE SIZE 90 BY 9.15
         BGCOLOR 15 FONT 4
         TITLE BGCOLOR 15 "Movimiento Banco" ROW-HEIGHT-CHARS .58.

DEFINE BROWSE B_Formatos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Formatos W-Win _FREEFORM
  QUERY B_Formatos NO-LOCK DISPLAY
      Formatos.Nom_Formato
      Formatos.Nom_Proceso
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 27 BY 4.5
         BGCOLOR 15 FONT 4 TOOLTIP "Seleccione el Formato para la Conciliación".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-85 AT ROW 1.54 COL 102
     Cmb_OfiCon AT ROW 2.08 COL 18 COLON-ALIGNED HELP
          "Seleccione la Agencia que va a Conciliar"
     W_CtaBco AT ROW 3.15 COL 18 COLON-ALIGNED HELP
          "Ingrese la Cuenta que va a Conciliar" DEBLANK 
     W_NomBco AT ROW 3.15 COL 44 COLON-ALIGNED NO-LABEL
     BUTTON-2 AT ROW 3.15 COL 102 HELP
          "Imprime Informes de Conciliación"
     W_FecCorte AT ROW 4.23 COL 18 COLON-ALIGNED
     F-Holgura AT ROW 4.23 COL 83 COLON-ALIGNED
     Btn_ConConc AT ROW 4.77 COL 102 HELP
          "Consulta la Conciliación"
     Brw-MovCtaConta AT ROW 5.58 COL 7
     Btn-Manual AT ROW 7.73 COL 102 HELP
          "Actualización Manual Extracto"
     Btn-AutExt AT ROW 9.35 COL 102 HELP
          "Actualización Extracto Desde un Archivo Plano"
     Brw-MovExtrac AT ROW 10.96 COL 7
     Btn_Concilia AT ROW 10.96 COL 102 HELP
          "Concilia Automaticamente  los Movientos"
     Btn-Multi AT ROW 12.58 COL 102 HELP
          "Cruce Manual Entre Movimientos"
     Btn-Borrar AT ROW 14.19 COL 102 HELP
          "Borra Registros de la Conciliación o Extracto"
     Btn-Cancel AT ROW 15.81 COL 102 HELP
          "Sale del Programa"
     Btn_Ayuda AT ROW 18.23 COL 105 HELP
          "Accesa la Ayuda de la Ventana"
     "Información Conciliación" VIEW-AS TEXT
          SIZE 23 BY .69 AT ROW 1.27 COL 4
          BGCOLOR 17 FGCOLOR 7 FONT 5
     RECT-63 AT ROW 7.46 COL 101
     RECT-65 AT ROW 1.27 COL 101
     RECT-66 AT ROW 1.54 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 1 ROW 1
         SIZE 114.29 BY 20.96
         BGCOLOR 17 FONT 4.

DEFINE FRAME F_Informe
     W_SdoExt AT ROW 1.27 COL 17 COLON-ALIGNED HELP
          "Ingrese el Saldo en Bancos"
     BUTTON-83 AT ROW 1.27 COL 45 HELP
          "Permite Generar e Imprimir Informes"
     R_Tipo AT ROW 2.62 COL 4 HELP
          "Seleccione el Tipo de informe para la Conciliaci¢n" NO-LABEL
     BUTTON-84 AT ROW 8.27 COL 45 HELP
          "Permite Abandonar el Proceso de Impresión"
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 20 ROW 5.58
         SIZE 61 BY 10.23
         BGCOLOR 17 FONT 5
         TITLE "Información para Generación de Informe".

DEFINE FRAME F_Formato
     B_Formatos AT ROW 1 COL 4 HELP
          "Seleccione el Formato para la Conciliación"
     Btn_AceFor AT ROW 1.54 COL 33 HELP
          "Confirma la Selección del Formato"
     Btn_CanFor AT ROW 3.69 COL 33 HELP
          "Cancela la Selección del Formato"
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 32 ROW 5.58
         SIZE 47 BY 5.65
         BGCOLOR 17 FONT 5
         TITLE "Escoja el Formato de Información".

DEFINE FRAME F_Ext
     Rs_ActExt AT ROW 1.54 COL 14 NO-LABEL
     W_SecExt1 AT ROW 3.42 COL 20 COLON-ALIGNED HELP
          "Secuencia para cheques con igual número"
     W_CheExt AT ROW 4.5 COL 20 COLON-ALIGNED HELP
          "Ingrese el Número de Cheque"
     W_ValExt AT ROW 5.58 COL 20 COLON-ALIGNED HELP
          "Ingrese el Valor de la Transacción"
     W_DiaExt AT ROW 6.65 COL 20 COLON-ALIGNED HELP
          "Ingrese el D¡a de la Transacción"
     R_NatExt AT ROW 4.5 COL 35 HELP
          "Seleccione el Estado en que se encuentra el Cheque" NO-LABEL
     W_DescExt AT ROW 7.73 COL 10.86 HELP
          "Concepto de la Transacción" DEBLANK 
     Btn_Grabar AT ROW 2.35 COL 54 HELP
          "Guarda el Registro Digitado del Extracto"
     Btn_Cancelar AT ROW 4.23 COL 54 HELP
          "Permite Eliminar los cambios realizados en el registro"
     Btn_SalirAct AT ROW 6.38 COL 54 HELP
          "Salir de la Grabación Manual"
     RECT-64 AT ROW 1.27 COL 11
    WITH 1 DOWN OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 19 ROW 9.88
         SIZE 69 BY 8.62
         BGCOLOR 17 FONT 5
         TITLE "Actualización Manual de Extracto".

DEFINE FRAME F_Borrar
     Rs_Borrar AT ROW 1.27 COL 2 NO-LABEL
     W_RegBorra AT ROW 2.35 COL 30 COLON-ALIGNED NO-LABEL
     Btn_CancBorrar AT ROW 3.69 COL 32
     "Registros" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1.27 COL 32
          FGCOLOR 7 FONT 5
     RECT-53 AT ROW 2.08 COL 31
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 28 ROW 11.23
         SIZE 42 BY 5.38
         BGCOLOR 17 FGCOLOR 15 
         TITLE "Eliminar Conciliación".

DEFINE FRAME F-Valor
     W-Valor AT ROW 1.08 COL 3.43 NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         THREE-D 
         AT COL 35.57 ROW 7.73
         SIZE 19.43 BY 2.96
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Valor".

DEFINE FRAME F-Num
     W-Num AT ROW 1.27 COL 2 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 42 ROW 9.35
         SIZE 17 BY 2.15
         BGCOLOR 17 FGCOLOR 0 
         TITLE "Documento".

DEFINE FRAME F-Concil
     Brw-Concil AT ROW 1.54 COL 1
     Btn-DesManual AT ROW 12.31 COL 56 HELP
          "Desmarca Por Número de Cruce (Manual)"
     Btn-Desmar AT ROW 12.31 COL 64 HELP
          "Desmarca Sin Número de Cruce (Automático)"
     Btn-Salir AT ROW 12.31 COL 79 HELP
          "Regresa a los Movimientos"
     "NF. Novedad Días Holgura" VIEW-AS TEXT
          SIZE 25 BY .81 AT ROW 13.12 COL 26
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "VD.  Valores Diferentes" VIEW-AS TEXT
          SIZE 20 BY .54 AT ROW 11.85 COL 3
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "NO.  No Consignado" VIEW-AS TEXT
          SIZE 21 BY .81 AT ROW 13.42 COL 3
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "NR. Registro No Encontrado" VIEW-AS TEXT
          SIZE 26 BY .81 AT ROW 12.04 COL 26
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Conciliado Con...    Movimiento Contable" VIEW-AS TEXT
          SIZE 43 BY .54 AT ROW 1 COL 1
          BGCOLOR 17 FGCOLOR 7 
     "Cruza Con ..." VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 1 COL 44
          BGCOLOR 17 FGCOLOR 7 
     "Movimiento Extracto" VIEW-AS TEXT
          SIZE 36 BY .54 AT ROW 1 COL 54.72
          BGCOLOR 17 FGCOLOR 7 
     "CC.  Conciliado" VIEW-AS TEXT
          SIZE 17 BY .54 AT ROW 11.08 COL 3
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "NC.  No Cobrados" VIEW-AS TEXT
          SIZE 20 BY .54 AT ROW 12.65 COL 3
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "NG.  No Girados" VIEW-AS TEXT
          SIZE 18 BY .54 AT ROW 11.23 COL 26
          BGCOLOR 17 FGCOLOR 7 FONT 5
    WITH 1 DOWN OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 6.92
         SIZE 90 BY 14.27
         BGCOLOR 17 FONT 4
         TITLE "Conciliacion".

DEFINE FRAME F-ManCru
     F-Cruce AT ROW 1.54 COL 4.43
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 35 ROW 4.77
         SIZE 25 BY 2.69
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Seleccionar Cruce Manual".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Conciliacion Bancaria"
         HEIGHT             = 20.96
         WIDTH              = 114.29
         MAX-HEIGHT         = 21.38
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.38
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         FONT               = 1
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F-Concil:FRAME = FRAME F-Main:HANDLE
       FRAME F-ManCru:FRAME = FRAME F-Concil:HANDLE
       FRAME F-Num:FRAME = FRAME F-Main:HANDLE
       FRAME F-Valor:FRAME = FRAME F-Main:HANDLE
       FRAME F_Borrar:FRAME = FRAME F-Main:HANDLE
       FRAME F_Ext:FRAME = FRAME F-Main:HANDLE
       FRAME F_Formato:FRAME = FRAME F-Main:HANDLE
       FRAME F_Informe:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Concil
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB Brw-Concil TEXT-9 F-Concil */
ASSIGN 
       FRAME F-Concil:HIDDEN           = TRUE.

ASSIGN 
       Brw-Concil:POPUP-MENU IN FRAME F-Concil             = MENU POPUP-MENU-Brw-Concil:HANDLE.

/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE UNDERLINE 2 4                                            */
/* BROWSE-TAB Brw-MovCtaConta Btn_ConConc F-Main */
/* BROWSE-TAB Brw-MovExtrac F_Ext F-Main */
/* SETTINGS FOR BROWSE Brw-MovCtaConta IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Brw-MovCtaConta:POPUP-MENU IN FRAME F-Main             = MENU POPUP-MENU-Brw-MovCtaConta:HANDLE
       Brw-MovCtaConta:MAX-DATA-GUESS IN FRAME F-Main         = 10000.

/* SETTINGS FOR BROWSE Brw-MovExtrac IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Brw-MovExtrac:POPUP-MENU IN FRAME F-Main             = MENU POPUP-MENU-Brw-MovExtrac:HANDLE.

/* SETTINGS FOR BUTTON Btn-AutExt IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON Btn-Manual IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR COMBO-BOX Cmb_OfiCon IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomBco IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F-ManCru
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-ManCru:HIDDEN           = TRUE
       FRAME F-ManCru:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN F-Cruce IN FRAME F-ManCru
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME F-Num
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Num:HIDDEN           = TRUE
       FRAME F-Num:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F-Valor
   NOT-VISIBLE UNDERLINE                                                */
ASSIGN 
       FRAME F-Valor:HIDDEN           = TRUE
       FRAME F-Valor:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN W-Valor IN FRAME F-Valor
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME F_Borrar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Borrar:HIDDEN           = TRUE
       FRAME F_Borrar:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Ext
   NOT-VISIBLE 2 4 Custom                                               */
ASSIGN 
       FRAME F_Ext:HIDDEN           = TRUE
       FRAME F_Ext:MOVABLE          = TRUE.

/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME F_Ext
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Grabar IN FRAME F_Ext
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON Btn_SalirAct IN FRAME F_Ext
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Rs_ActExt IN FRAME F_Ext
   2                                                                    */
/* SETTINGS FOR RADIO-SET R_NatExt IN FRAME F_Ext
   2                                                                    */
/* SETTINGS FOR FILL-IN W_CheExt IN FRAME F_Ext
   2                                                                    */
/* SETTINGS FOR FILL-IN W_DescExt IN FRAME F_Ext
   ALIGN-L 2                                                            */
/* SETTINGS FOR FILL-IN W_DiaExt IN FRAME F_Ext
   2                                                                    */
/* SETTINGS FOR FILL-IN W_ValExt IN FRAME F_Ext
   2                                                                    */
/* SETTINGS FOR FRAME F_Formato
   NOT-VISIBLE                                                          */
/* BROWSE-TAB B_Formatos 1 F_Formato */
ASSIGN 
       FRAME F_Formato:HIDDEN           = TRUE
       FRAME F_Formato:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Informe
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Informe:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw-Concil
/* Query rebuild information for BROWSE Brw-Concil
     _START_FREEFORM
OPEN QUERY Brw-Concil FOR EACH Conciliacion WHERE (    Conciliacion.Agencia                      EQ W_Ofi
                                                   AND Conciliacion.Cuenta                       EQ W_CtaBco
                                                   AND Conciliacion.Per_Conciliacion             LE W_FecCorte)
                                                   AND (    (    Conciliacion.Novedad            NE ""
                                                             AND Conciliacion.Novedad            NE "CC")
                                                         OR (    Conciliacion.Fecha_Conciliacion GE W_FecInic
                                                             AND Conciliacion.Fecha_Conciliacion LE W_FecCorte)
                                                         OR (    Conciliacion.Per_Conciliacion   GE W_FecInic
                                                             AND Conciliacion.Novedad            EQ "CC"))
                                            NO-LOCK,
                          EACH Mov_CtaConta WHERE Mov_CtaConta.Agencia       EQ W_Ofi
                                              AND Mov_CtaConta.Cuenta        EQ W_CtaBco
                                              AND Mov_CtaConta.Reg_CtaConta  EQ Conciliacion.Reg_CtaConta
                                            OUTER-JOIN NO-LOCK,
                          EACH Mov_Extracto WHERE Mov_Extracto.Agencia       EQ W_Ofi
                                              AND Mov_Extracto.Cuenta        EQ W_CtaBco
                                              AND Mov_Extracto.Reg_MovExtrac EQ Conciliacion.Reg_MovExtrac
                                            OUTER-JOIN NO-LOCK
                                            BY Conciliacion.Agencia
                                            BY Conciliacion.Cuenta
                                            BY Conciliacion.Novedad
                                            BY Conciliacion.Reg_Cruce
                                            BY Mov_CtaConta.Num_Cheque
                                            BY Mov_Extracto.Num_Cheque.
RUN Inicia_TmpCon.
RUN Llena_TmpCon.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE Brw-Concil */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw-MovCtaConta
/* Query rebuild information for BROWSE Brw-MovCtaConta
     _START_FREEFORM
OPEN QUERY Brw-MovCtaConta FOR EACH Tmp-CtaConta WHERE (Tmp-CtaConta.Agencia EQ W_Ofi
                                                   AND Tmp-CtaConta.Cuenta   EQ W_CtaBco
                                                   AND Tmp-CtaConta.Fecha    LE W_FecCorte)
                                                 NO-LOCK
                                                    BY Tmp-CtaConta.Agencia
                                                    BY Tmp-CtaConta.Cuenta
                                                    BY Tmp-CtaConta.Num_Cheque
                                                    BY Tmp-CtaConta.Secuencia
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE Brw-MovCtaConta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw-MovExtrac
/* Query rebuild information for BROWSE Brw-MovExtrac
     _START_FREEFORM
OPEN QUERY Brw-MovExtrac FOR EACH Tmp-Extracto WHERE (    Tmp-Extracto.Agencia      EQ W_Ofi
                                                      AND Tmp-Extracto.Cuenta        EQ W_CtaBco
                                                      AND Tmp-Extracto.Fecha         LE W_FecCorte)
                                                  AND Tmp-Extracto.Num_Cheque    NE "0000000000"
                                               NO-LOCK
                                                   BY Tmp-Extracto.Agencia
                                                   BY Tmp-Extracto.Cuenta
                                                   BY Tmp-Extracto.Num_Cheque
                                                   BY Tmp-Extracto.Secuencia
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE Brw-MovExtrac */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Formatos
/* Query rebuild information for BROWSE B_Formatos
     _START_FREEFORM
OPEN QUERY B_Formatos FOR EACH Formatos WHERE Formatos.Id_Formato = "AC"  NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE B_Formatos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Valor
/* Query rebuild information for FRAME F-Valor
     _Query            is NOT OPENED
*/  /* FRAME F-Valor */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Conciliacion Bancaria */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  /*IF THIS-PROCEDURE:PERSISTENT THEN */ 
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Conciliacion Bancaria */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to teinate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Brw-Concil
&Scoped-define SELF-NAME Brw-Concil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw-Concil W-Win
ON F8 OF Brw-Concil IN FRAME F-Concil
DO:
  ASSIGN Curr-record = ROWID(Conciliacion). 
  IF NOT W_PorValor THEN
     FIND PREV Tmp-Concilia WHERE Tmp-Concilia.Num_Cheque EQ W-Num1
                            NO-LOCK NO-ERROR.
  ELSE
     FIND PREV Tmp-Concilia WHERE Tmp-Concilia.Valor EQ W-Valor
                            NO-LOCK NO-ERROR.
  RUN Asg_RegCon.
  APPLY "ENTRY":U TO BROWSE Brw-Concil.
  RETURN NO-APPLY.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw-Concil W-Win
ON F9 OF Brw-Concil IN FRAME F-Concil
DO:
  ASSIGN Curr-record = ROWID(Conciliacion). 
  IF NOT W_PorValor THEN
     FIND NEXT Tmp-Concilia WHERE Tmp-Concilia.Num_Cheque EQ W-Num1
                            NO-LOCK NO-ERROR.
  ELSE
     FIND NEXT Tmp-Concilia WHERE Tmp-Concilia.Valor EQ W-Valor
                            NO-LOCK NO-ERROR.
  RUN Asg_RegCon.
  APPLY "ENTRY":U TO BROWSE Brw-Concil.
  RETURN NO-APPLY.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw-Concil W-Win
ON ROW-DISPLAY OF Brw-Concil IN FRAME F-Concil
DO:
   ASSIGN Mov_CtaConta.Naturaleza:FGCOLOR IN BROWSE Brw-Concil = IF Mov_CtaConta.Naturaleza EQ "DB" THEN 1 ELSE 12
          Mov_Extracto.Naturaleza:FGCOLOR = IF Mov_Extracto.Naturaleza EQ "DB" THEN 1 ELSE 12.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Brw-MovCtaConta
&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Brw-MovCtaConta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw-MovCtaConta W-Win
ON F8 OF Brw-MovCtaConta IN FRAME F-Main /* Movimiento Cooperativa */
DO:
  ASSIGN Curr-record = ROWID(Tmp-CtaConta).
  IF NOT W_PorValor THEN
     FIND PREV Tmp-CtaConta WHERE Tmp-CtaConta.Num_Cheque EQ W-Num1
                            NO-LOCK NO-ERROR.
  ELSE
      FIND PREV Tmp-CtaConta WHERE Tmp-CtaConta.Valor EQ W-Valor
                             NO-LOCK NO-ERROR.
  RUN Asg_RegCta.
  APPLY "ENTRY":U TO BROWSE Brw-MovCtaConta.
  RETURN NO-APPLY.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw-MovCtaConta W-Win
ON F9 OF Brw-MovCtaConta IN FRAME F-Main /* Movimiento Cooperativa */
DO:                                         
  ASSIGN Curr-record = ROWID(Tmp-CtaConta). 
  IF NOT W_PorValor THEN
     FIND NEXT Tmp-CtaConta WHERE Tmp-CtaConta.Num_Cheque EQ W-Num1
                            NO-LOCK NO-ERROR.
  ELSE
      FIND NEXT Tmp-CtaConta WHERE Tmp-CtaConta.Valor EQ W-Valor
                             NO-LOCK NO-ERROR.
  RUN Asg_RegCta.
  APPLY "ENTRY":U TO BROWSE Brw-MovCtaConta.
  RETURN NO-APPLY.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw-MovCtaConta W-Win
ON ROW-DISPLAY OF Brw-MovCtaConta IN FRAME F-Main /* Movimiento Cooperativa */
DO:
   ASSIGN Tmp-CtaConta.Naturaleza:FGCOLOR IN BROWSE Brw-MovCtaConta = IF Tmp-CtaConta.Naturaleza EQ "DB" THEN 1 ELSE 12.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Brw-MovExtrac
&Scoped-define SELF-NAME Brw-MovExtrac
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw-MovExtrac W-Win
ON F8 OF Brw-MovExtrac IN FRAME F-Main /* Movimiento Banco */
DO:
  ASSIGN Curr-record = ROWID(Tmp-Extracto).
  IF NOT W_PorValor THEN
     FIND PREV Tmp-Extracto WHERE Tmp-Extracto.Num_Cheque EQ W-Num1
                            NO-LOCK NO-ERROR.
  ELSE
      FIND PREV Tmp-Extracto WHERE Tmp-Extracto.Valor EQ W-Valor
                             NO-LOCK NO-ERROR.
  RUN Asg_RegExt.
  APPLY "ENTRY":U TO BROWSE Brw-MovExtrac.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw-MovExtrac W-Win
ON F9 OF Brw-MovExtrac IN FRAME F-Main /* Movimiento Banco */
DO:
  ASSIGN Curr-record = ROWID(Tmp-Extracto). 
  IF NOT W_PorValor THEN
     FIND NEXT Tmp-Extracto WHERE Tmp-Extracto.Num_Cheque EQ W-Num1
                            NO-LOCK NO-ERROR.
  ELSE
      FIND NEXT Tmp-Extracto WHERE Tmp-Extracto.Valor EQ W-Valor
                             NO-LOCK NO-ERROR.
  RUN Asg_RegExt.
  APPLY "ENTRY":U TO BROWSE Brw-MovExtrac.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw-MovExtrac W-Win
ON MOUSE-SELECT-DBLCLICK OF Brw-MovExtrac IN FRAME F-Main /* Movimiento Banco */
DO:
  IF NOT AVAILABLE Tmp-Extracto THEN
     RETURN NO-APPLY.
  APPLY "CHOOSE":U TO Btn-Manual.
  IF NOT Btn_Grabar:SENSITIVE IN FRAME F_Ext THEN DO:
     APPLY "CHOOSE" TO Btn_SalirAct IN FRAME F_Ext.
     RETURN NO-APPLY.
  END.
  ASSIGN Rs_ActExt = 2
         Rs_ActExt:SCREEN-VALUE IN FRAME F_Ext = STRING(Rs_ActExt).
  FIND Mov_Extracto WHERE Mov_Extracto.Agencia       EQ W_Ofi
                      AND Mov_Extracto.Cuenta        EQ W_CtaBco
                      AND Mov_Extracto.Reg_MovExtrac EQ Tmp-Extracto.Reg_MovExtrac.
  RUN Display-ExtManual.
  APPLY "LEAVE" TO W_CheExt IN FRAME F_Ext.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw-MovExtrac W-Win
ON ROW-DISPLAY OF Brw-MovExtrac IN FRAME F-Main /* Movimiento Banco */
DO:
   ASSIGN Tmp-Extracto.Naturaleza:FGCOLOR IN BROWSE Brw-MovExtrac = IF Tmp-Extracto.Naturaleza EQ "DB" THEN 1 ELSE 12.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-AutExt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-AutExt W-Win
ON CHOOSE OF Btn-AutExt IN FRAME F-Main /* Automática */
DO:  
  ASSIGN FRAME F-Main W_CtaBco.
  IF W_CtaBco EQ "" THEN DO:
       APPLY "ENTRY" TO W_CtaBco.
       RETURN NO-APPLY.
  END.
  IF W_SoloConsulta THEN
     RETURN NO-APPLY.

  FIND Mov_Extracto WHERE Mov_Extracto.Agencia       EQ W_Ofi
                      AND Mov_Extracto.Cuenta        EQ W_CtaBco
                      AND Mov_Extracto.Reg_MovExtrac EQ 
                         ("EX" + STRING(W_Ano,"9999") + STRING(W_MesC,"99") + "00000")
                      NO-LOCK NO-ERROR.
  IF AVAILABLE Mov_Extracto THEN DO:
     MESSAGE  "La Información del Extracto Ya " SKIP
              "Ha Sido Ingresada.             "                 
     VIEW-AS ALERT-BOX ERROR TITLE "Datos del Extracto".
     APPLY "ENTRY" TO Btn-Cancel IN FRAME F-Main.
     RETURN NO-APPLY.
  END.   
  FIND Formatos WHERE Formatos.Agencia     EQ W_Ofi
                  AND Formatos.Cod_Formato EQ W_FormatoExt 
                  AND Formatos.Estado      EQ 1
                NO-LOCK NO-ERROR.
  IF AVAILABLE(Formatos) THEN DO:
     ASSIGN W_Proceso = Formatos.Nom_Proceso.
     RUN Graba_ExtAut. 
  END.
  ELSE DO:
       VIEW FRAME F_Formato.
       APPLY "ENTRY" TO B_Formatos IN FRAME F_Formato.
  END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Borrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Borrar W-Win
ON CHOOSE OF Btn-Borrar IN FRAME F-Main /* Borrar */
DO:
  IF W_CtaBco EQ "" THEN DO:
     APPLY "ENTRY" TO W_CtaBco.
     RETURN NO-APPLY.
  END.
  FIND FIRST Conciliacion WHERE Conciliacion.Agencia           EQ W_Ofi
                            AND Conciliacion.Cuenta            EQ W_CtaBco
                            AND Per_Conciliacion GT W_FecCorte NO-LOCK NO-ERROR.
  IF AVAILABLE Conciliacion THEN DO:
     MESSAGE "No se Permite Borrar esta Conciliacion " SKIP
             "Porque Existe la del Periodo ...." Conciliacion.Per_Conciliacion
        VIEW-AS ALERT-BOX ERROR TITLE "Error Eliminacion de Datos".
     RETURN NO-APPLY.
  END.
  DISABLE ALL WITH FRAME F-Main.
  FRAME F_Borrar:SENSITIVE  = TRUE.
  VIEW FRAME F_Borrar.
  ENABLE Rs_Borrar Btn_CancBorrar WITH FRAME F_Borrar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Cancel W-Win
ON CHOOSE OF Btn-Cancel IN FRAME F-Main /* Salir */
DO: ON RETURN RETURN.
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Concil
&Scoped-define SELF-NAME Btn-DesManual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-DesManual W-Win
ON CHOOSE OF Btn-DesManual IN FRAME F-Concil /* Btn 85 */
DO:
  IF W_SoloConsulta THEN
     RETURN NO-APPLY.
  
  IF Brw-Concil:NUM-SELECTED-ROWS LE 0 THEN
     RETURN NO-APPLY.
  
  ASSIGN W_Status = Brw-Concil:FETCH-SELECTED-ROW(1)
         F-CruStr = Conciliacion.Reg_Cruce. 

  IF F-CruStr EQ "" THEN RETURN NO-APPLY.
         
  DO I = Brw-Concil:NUM-SELECTED-ROWS IN FRAME F-Concil TO 1 By -1:         
     W_Status = Brw-Concil:FETCH-SELECTED-ROW(I).
     IF Conciliacion.Reg_Cruce NE F-CruStr THEN DO: 
        MESSAGE "Partida no se Puede Desmarcar" SKIP
                "Número de Cruce es Diferente." 
           VIEW-AS ALERT-BOX ERROR TITLE "Error Conciliacion".
        Brw-Concil:DESELECT-ROWS().          
        RETURN NO-APPLY.
     END.
     IF Conciliacion.Fecha_Conciliacion LT W_FecInic
     OR Conciliacion.Fecha_Conciliacion GT W_FecCorte THEN DO:
        MESSAGE "Partida no se Puede Desmarcar por el Periodo " Conciliacion.Fecha_Conciliacion 
            VIEW-AS ALERT-BOX ERROR TITLE "Error Conciliacion".
        Brw-Concil:DESELECT-ROWS().          
        RETURN NO-APPLY.
     END.     
  END.       
  RUN Desmarca_Con.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Desmar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Desmar W-Win
ON CHOOSE OF Btn-Desmar IN FRAME F-Concil /* Desmarcar */
DO:
  IF W_SoloConsulta THEN
     RETURN NO-APPLY.
       
  IF Brw-Concil:NUM-SELECTED-ROWS LE 0 THEN
     RETURN NO-APPLY.

  DO I = Brw-Concil:NUM-SELECTED-ROWS IN FRAME F-Concil TO 1 By -1:         
         W_Status = Brw-Concil:FETCH-SELECTED-ROW(I).
         ASSIGN F-CruStr = Conciliacion.Reg_Cruce. 
         IF F-CruStr GT "" THEN DO: 
            MESSAGE "Partida no se Puede Desmarcar" SKIP
                    "Este Cruce Es Manual" 
              VIEW-AS ALERT-BOX ERROR TITLE "Error al Desmarcar".
            Brw-Concil:DESELECT-ROWS().          
            RETURN NO-APPLY.
         END.         
         IF  Conciliacion.Novedad NE "CC" THEN DO:
             MESSAGE "Partida no se Puede Desmarcar , Por El Tipo de Novedad" SKIP
                     Conciliacion.Novedad
                 VIEW-AS ALERT-BOX ERROR TITLE "Error al Desmarcar".
             Brw-Concil:DESELECT-ROWS().          
             RETURN NO-APPLY.
         END.  
         IF Conciliacion.Fecha_Conciliacion LT W_FecInic
         OR Conciliacion.Fecha_Conciliacion GT W_FecCorte THEN DO:
            MESSAGE "Partida no se Puede Desmarcar por el Periodo " Conciliacion.Fecha_Conciliacion 
                VIEW-AS ALERT-BOX ERROR TITLE "Error al Desmarcar".
            Brw-Concil:DESELECT-ROWS().          
            RETURN NO-APPLY.
         END.
         RUN Quita_CC.
  END. 
  {&OPEN-QUERY-Brw-Concil}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn-Manual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Manual W-Win
ON CHOOSE OF Btn-Manual IN FRAME F-Main /* Manual */
DO: 
  IF W_SoloConsulta THEN
     RETURN NO-APPLY.

  FIND Mov_Extracto WHERE Mov_Extracto.Agencia       EQ W_Ofi
                      AND Mov_Extracto.Cuenta        EQ W_CtaBco 
                      AND Mov_Extracto.Reg_MovExtrac EQ 
                         ("EX" + STRING(W_Ano,"9999") + STRING(W_MesC,"99") + "00000")
                     NO-LOCK NO-ERROR.
  IF AVAILABLE Mov_Extracto THEN DO:
     IF TRIM(Mov_Extracto.Descripcion) = "Automatico" THEN DO:
        MESSAGE  "La Información del Extracto Ha Sido " SKIP
                 "Ingresada de Forma Automática, No es" SKIP                 
                 "Posible Modificarla Manualmente"
        VIEW-AS ALERT-BOX WARNING TITLE "Información Extracto".
        APPLY "ENTRY" TO Btn-Cancel IN FRAME F-Main.
        RETURN NO-APPLY.
     END.
     ELSE ASSIGN W_GrabaRgCero = NO.
  END.   
  ELSE
     ASSIGN W_GrabaRgCero = TRUE.

  DISABLE ALL WITH FRAME F-Main.  

  ENABLE Rs_ActExt W_SecExt1 W_CheExt W_ValExt W_DiaExt R_NatExt W_DescExt
         Btn_Grabar Btn_Cancelar Btn_SalirAct WITH FRAME F_Ext.
  ASSIGN Rs_ActExt:SCREEN-VALUE = STRING(1)
         W_CheExt:SCREEN-VALUE  = STRING(0)
         Btn_Grabar:SENSITIVE   = FALSE.
  APPLY "VALUE-CHANGED" TO Rs_ActExt IN FRAME F_Ext.
  APPLY "VALUE-CHANGED" TO R_NatExt IN FRAME F_Ext.
  APPLY "ENTRY" TO Rs_ActExt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Multi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Multi W-Win
ON CHOOSE OF Btn-Multi IN FRAME F-Main
DO:
  IF W_SoloConsulta THEN
     RETURN NO-APPLY.

   IF Brw-MovExtrac:NUM-SELECTED-ROWS GT 0
   AND Brw-MovCtaConta:NUM-SELECTED-ROWS GT 0 THEN 
       RUN EntreMovtos.                       /* Cruces de mvto cuenta y mvto extracto, manuales */
   ELSE    
   IF Brw-MovCtaConta:NUM-SELECTED-ROWS GT 0 THEN
      RUN EntreSiMovCta.                      /* Cruces de mvto cuenta entre si, manuales */
   ELSE 
   IF Brw-MovExtrac:NUM-SELECTED-ROWS GT 0 THEN
      RUN EntreSiExtrac.                      /* Cruces de mvto extracto entre si, manuales */
   ELSE
    RETURN NO-APPLY.

 CLOSE QUERY Brw-MovCtaConta.
 {&OPEN-QUERY-Brw-MovCtaConta}                        
 CLOSE QUERY Brw-MovExtrac.  
 {&OPEN-QUERY-Brw-MovExtrac}  
 ASSIGN W_ConsulCon = TRUE.                         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Concil
&Scoped-define SELF-NAME Btn-Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Salir W-Win
ON CHOOSE OF Btn-Salir IN FRAME F-Concil /* Salir */
DO:
  HIDE FRAME F-Concil.
  ENABLE ALL EXCEPT W_NomBco WITH FRAME F-Main.
  CLOSE QUERY Brw-MovExtrac.
  CLOSE QUERY Brw-MovCtaConta.
 {&OPEN-QUERY-Brw-MovExtrac}
 {&OPEN-QUERY-Brw-MovCtaConta}
 ASSIGN W_ConsulCon = FALSE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Formato
&Scoped-define SELF-NAME Btn_AceFor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AceFor W-Win
ON CHOOSE OF Btn_AceFor IN FRAME F_Formato /* Aceptar */
DO:
   HIDE FRAME F_Formato.
   IF W_Proceso NE "" THEN  
      RUN Graba_ExtAut.
   ELSE
      RUN MostrarMensaje IN W_Manija (INPUT 99, OUTPUT W_Eleccion).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-Win
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main /* Ayuda */
OR HELP OF {&WINDOW-NAME}
DO:
  SYSTEM-HELP "ayudas/tesoreri" CONTEXT 6.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Borrar
&Scoped-define SELF-NAME Btn_CancBorrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CancBorrar W-Win
ON CHOOSE OF Btn_CancBorrar IN FRAME F_Borrar /* Salir */
DO:
  HIDE FRAME F_Borrar.
  ENABLE ALL EXCEPT W_NomBco WITH FRAME F-Main.
  DISABLE Btn-Multi WITH FRAME F-Main.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ext
&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar W-Win
ON CHOOSE OF Btn_Cancelar IN FRAME F_Ext /* Cancelar */
DO:
   ASSIGN W_CheExt:SENSITIVE     = TRUE.
   IF Rs_ActExt EQ 1 THEN
      ASSIGN W_SecExt1 = 1
             W_SecExt1:SCREEN-VALUE = STRING(W_SecExt1).
   ELSE
       ASSIGN W_SecExt1:SENSITIVE  = TRUE.

   RUN Display-ExtManual.
   APPLY "ENTRY" TO W_CheExt IN FRAME F_Ext.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Formato
&Scoped-define SELF-NAME Btn_CanFor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CanFor W-Win
ON CHOOSE OF Btn_CanFor IN FRAME F_Formato /* Cancelar */
DO:
  HIDE FRAME F_Formato.
  ENABLE ALL EXCEPT W_NomBco WITH FRAME F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Concilia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Concilia W-Win
ON CHOOSE OF Btn_Concilia IN FRAME F-Main /* Conciliar */
DO:
   IF W_CtaBco EQ "" THEN DO:
      APPLY "ENTRY" TO W_CtaBco.
      RETURN NO-APPLY.
   END.  
   IF W_SoloConsulta THEN
      RETURN NO-APPLY.
   RUN Conciliar. 
   ENABLE Btn-Multi WITH FRAME F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_ConConc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ConConc W-Win
ON CHOOSE OF Btn_ConConc IN FRAME F-Main /* Consulta */
DO:
  DISABLE ALL WITH FRAME F-Main.
  VIEW FRAME F-Concil. 
  ENABLE Btn-Salir Btn-Desmar WITH FRAME F-Concil.
  IF W_ConsulCon THEN DO:
     CLOSE QUERY Brw-Concil.
    {&OPEN-QUERY-Brw-Concil}
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ext
&Scoped-define SELF-NAME Btn_Grabar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grabar W-Win
ON CHOOSE OF Btn_Grabar IN FRAME F_Ext /* Grabar */
DO:
   ASSIGN W_Valext.
   IF W_Valext EQ 0 THEN DO:
      MESSAGE "No se Permite el Ingreso de Valores en Cero"
         VIEW-AS ALERT-BOX ERROR TITLE "Error de Entrada de Datos".
      RETURN NO-APPLY.
   END.
   SESSION:SET-WAIT-STATE("GENERAL"). 
   RUN Graba_ExtMan.
   RELEASE Mov_Extracto.
   SESSION:SET-WAIT-STATE(""). 
   APPLY "ENTRY" TO W_CheExt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grabar W-Win
ON ENTRY OF Btn_Grabar IN FRAME F_Ext /* Grabar */
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grabar W-Win
ON LEAVE OF Btn_Grabar IN FRAME F_Ext /* Grabar */
DO:
  ON RETURN TAB.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_SalirAct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalirAct W-Win
ON CHOOSE OF Btn_SalirAct IN FRAME F_Ext /* Salir Manual */
DO:
  HIDE FRAME F_Ext.     
  ENABLE ALL EXCEPT W_NomBco WITH FRAME F-Main.
  {&OPEN-QUERY-Brw-MovExtrac}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 W-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Imprimir */
DO:
  VIEW FRAME F_Informe.
  APPLY "ENTRY" TO W_SdoExt IN FRAME F_Informe. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Informe
&Scoped-define SELF-NAME BUTTON-83
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-83 W-Win
ON CHOOSE OF BUTTON-83 IN FRAME F_Informe /* Imprimir */
DO:
  HIDE FRAME F_Informe.
  IF R_Tipo EQ "TO" OR R_Tipo EQ "RE" THEN
     RUN ImprimirTot.
  ELSE 
     IF R_Tipo EQ "CC" OR R_Tipo EQ "VD"
     OR R_Tipo EQ "NF" THEN      
        RUN ImprimirCC.
     ELSE RUN Imprimir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-84
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-84 W-Win
ON CHOOSE OF BUTTON-84 IN FRAME F_Informe /* Cancelar */
DO:
  HIDE FRAME F_Informe.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BUTTON-85
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-85 W-Win
ON CHOOSE OF BUTTON-85 IN FRAME F-Main /* Button 85 */
DO:
  RUN W-infdia.r.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Formatos
&Scoped-define FRAME-NAME F_Formato
&Scoped-define SELF-NAME B_Formatos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Formatos W-Win
ON MOUSE-SELECT-CLICK OF B_Formatos IN FRAME F_Formato
DO:
  ASSIGN W_Proceso = Formatos.Nom_Proceso.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Cmb_OfiCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_OfiCon W-Win
ON VALUE-CHANGED OF Cmb_OfiCon IN FRAME F-Main /* Agencia */
DO:
  ASSIGN W_Ofi = INTEGER(SUBSTRING(Cmb_OfiCon:SCREEN-VALUE IN FRAME F-Main,1,3)). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-ManCru
&Scoped-define SELF-NAME F-Cruce
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Cruce W-Win
ON LEAVE OF F-Cruce IN FRAME F-ManCru /* Número Cruce */
DO:
  ASSIGN F-Cruce
         F-CruStr = "MV" + STRING(W_Ano,"9999") + STRING(W_MesC,"99") + STRING(F-Cruce,"99999").

  HIDE FRAME F-ManCru.
  IF F-Cruce EQ ?
  OR F-Cruce EQ 0 THEN 
     RETURN.
  RUN Seleccion_Cruce.
  APPLY "ENTRY" TO Btn-DesManual IN FRAME F-Concil.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME F-Holgura
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Holgura W-Win
ON LEAVE OF F-Holgura IN FRAME F-Main /* Días Holgura Conciliación */
DO:
  ASSIGN F-Holgura.
  APPLY "ENTRY" TO W_FecCorte.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Anterior______F8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Anterior______F8 W-Win
ON CHOOSE OF MENU-ITEM m_Anterior______F8 /* Anterior           F8 */
DO:
   APPLY "F8" TO BROWSE Brw-MovCtaConta.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Anterior_________F8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Anterior_________F8 W-Win
ON CHOOSE OF MENU-ITEM m_Anterior_________F8 /* Anterior           F8 */
DO:
   APPLY "F8" TO BROWSE Brw-MovExtrac.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Anterior_________F82
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Anterior_________F82 W-Win
ON CHOOSE OF MENU-ITEM m_Anterior_________F82 /* Anterior          F8 */
DO:
  APPLY "F8" TO BROWSE Brw-Concil.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Desmarcar_Cruce_Manual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Desmarcar_Cruce_Manual W-Win
ON CHOOSE OF MENU-ITEM m_Desmarcar_Cruce_Manual /* Seleccionar Cruce Manual */
DO:
  VIEW FRAME F-ManCru.
  APPLY "ENTRY" TO F-Cruce IN FRAME F-ManCru.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Desmarcar_Seleccin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Desmarcar_Seleccin W-Win
ON CHOOSE OF MENU-ITEM m_Desmarcar_Seleccin /* Desmarcar Selección */
DO:
  IF Brw-MovExtrac:NUM-SELECTED-ROWS IN FRAME F-Main > 0 THEN
     Brw-MovExtrac:DESELECT-ROWS().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Desmarcar_Seleccin2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Desmarcar_Seleccin2 W-Win
ON CHOOSE OF MENU-ITEM m_Desmarcar_Seleccin2 /* Desmarcar Selección */
DO:
  IF Brw-MovCtaConta:NUM-SELECTED-ROWS IN FRAME F-Main > 0 THEN
     Brw-MovCtaConta:DESELECT-ROWS().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Desmarcar_Seleccin3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Desmarcar_Seleccin3 W-Win
ON CHOOSE OF MENU-ITEM m_Desmarcar_Seleccin3 /* Desmarcar Selección */
DO:
  IF Brw-Concil:NUM-SELECTED-ROWS IN FRAME F-Concil > 0 THEN
     Brw-Concil:DESELECT-ROWS().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Documento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Documento W-Win
ON CHOOSE OF MENU-ITEM m_Documento /* Documento */
DO:
  ASSIGN W_SobreExt = 1.
  VIEW FRAME F-Num.
  APPLY "ENTRY" TO W-Num IN FRAME F-Num.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Documento2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Documento2 W-Win
ON CHOOSE OF MENU-ITEM m_Documento2 /* Documento */
DO:
  ASSIGN W_SobreExt = 2.
  VIEW FRAME F-Num.
  APPLY "ENTRY" TO W-Num IN FRAME F-Num.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Documento3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Documento3 W-Win
ON CHOOSE OF MENU-ITEM m_Documento3 /* Documento */
DO:
  ASSIGN W_SobreExt = 3.
  ENABLE W-Num WITH FRAME F-Num.
  VIEW FRAME F-Num.
  APPLY "ENTRY" TO W-Num IN FRAME F-Num.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Siguiente________F9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Siguiente________F9 W-Win
ON CHOOSE OF MENU-ITEM m_Siguiente________F9 /* Siguiente        F9 */
DO:
  APPLY "F9" TO BROWSE Brw-Concil.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Siguiente_________F9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Siguiente_________F9 W-Win
ON CHOOSE OF MENU-ITEM m_Siguiente_________F9 /* Siguiente         F9 */
DO:
  APPLY "F9" TO BROWSE Brw-MovExtrac.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Siguiente__________F9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Siguiente__________F9 W-Win
ON CHOOSE OF MENU-ITEM m_Siguiente__________F9 /* Siguiente         F9 */
DO:
   APPLY "F9" TO BROWSE Brw-MovCtaConta.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Valor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Valor W-Win
ON CHOOSE OF MENU-ITEM m_Valor /* Valor */
DO:
  ASSIGN W_SobreExt = 1.
  VIEW FRAME F-Valor.
  APPLY "ENTRY" TO W-Valor IN FRAME F-Valor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Valor2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Valor2 W-Win
ON CHOOSE OF MENU-ITEM m_Valor2 /* Valor */
DO:
  ASSIGN W_SobreExt = 2.
  VIEW FRAME F-Valor.
  APPLY "ENTRY" TO W-Valor IN FRAME F-Valor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Valor3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Valor3 W-Win
ON CHOOSE OF MENU-ITEM m_Valor3 /* Valor */
DO:
  ASSIGN W_SobreExt = 3.
  ENABLE W-Valor WITH FRAME F-Valor.
  VIEW FRAME F-Valor.
  APPLY "ENTRY" TO W-Valor IN FRAME F-Valor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ext
&Scoped-define SELF-NAME Rs_ActExt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_ActExt W-Win
ON LEAVE OF Rs_ActExt IN FRAME F_Ext
DO:
  APPLY "VALUE-CHANGED":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_ActExt W-Win
ON VALUE-CHANGED OF Rs_ActExt IN FRAME F_Ext
DO:
  ASSIGN Rs_ActExt.
  IF Rs_ActExt EQ 1 THEN DO: 
     ASSIGN Btn_Grabar:SENSITIVE IN FRAME F_Ext = TRUE
            W_CheExt:SENSITIVE  = TRUE
            W_SecExt1:SENSITIVE = FALSE.
  END. 
  ELSE DO:
     ASSIGN W_SecExt1:SENSITIVE IN FRAME F_Ext = TRUE
            W_CheExt:SENSITIVE  = TRUE
            Btn_Grabar:SENSITIVE = TRUE.
  END.
  RUN Display-ExtManual.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Borrar
&Scoped-define SELF-NAME Rs_Borrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Borrar W-Win
ON VALUE-CHANGED OF Rs_Borrar IN FRAME F_Borrar
OR MOUSE-SELECT-CLICK OF Rs_Borrar OR RETURN OF Rs_Borrar DO:
  ASSIGN Rs_Borrar = INTEGER(Rs_Borrar:SCREEN-VALUE IN FRAME F_Borrar).
  IF Rs_Borrar EQ 1 THEN 
     MESSAGE "Se Eliminará Toda La Conciliación del Mes..." W_Mesc   SKIP
             "De la Cuenta................................" W_CtaBco SKIP
             "De la Agencia..............................." W_Ofi    SKIP
             "Está Seguro...?" VIEW-AS ALERT-BOX
        QUESTION BUTTONS YES-NO UPDATE W_Status.
  ELSE 
      MESSAGE "Se Eliminará Toda La Conciliación del Mes..." W_Mesc   SKIP
              " Y Todo El Extracto del Mes................." W_Mesc   SKIP
              "De la cuenta................................" W_CtaBco SKIP
              "De la Agencia..............................." W_Ofi    SKIP
              "Está Seguro ..." VIEW-AS ALERT-BOX
          QUESTION BUTTONS YES-NO UPDATE W_Status.

  IF W_Status THEN 
     RUN Borra_ConExt.

  HIDE FRAME F_Borrar.   
  ENABLE ALL EXCEPT W_NomBco WITH FRAME F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ext
&Scoped-define SELF-NAME R_NatExt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_NatExt W-Win
ON VALUE-CHANGED OF R_NatExt IN FRAME F_Ext
DO:
  ASSIGN R_NatExt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Informe
&Scoped-define SELF-NAME R_Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Tipo W-Win
ON LEAVE OF R_Tipo IN FRAME F_Informe
DO:
  APPLY "VALUE-CHANGED" TO R_Tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Tipo W-Win
ON VALUE-CHANGED OF R_Tipo IN FRAME F_Informe
DO:
  ASSIGN R_Tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Num
&Scoped-define SELF-NAME W-Num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Num W-Win
ON LEAVE OF W-Num IN FRAME F-Num
DO:
  ASSIGN W-Num
         W-Num1  = STRING(W-Num, "9999999999")
         W_PorValor = FALSE.

  HIDE FRAME F-Num.

  IF W-Num EQ ?
  OR W-Num EQ 0 THEN
     RETURN.

  IF W_SobreExt EQ 1 THEN DO:
     RUN BusMovCta.
     APPLY "ENTRY":U TO BROWSE Brw-MovCtaConta.
  END.
  ELSE DO:
       IF W_SobreExt EQ 2 THEN DO:
          RUN BusMovExt.
          APPLY "ENTRY":U TO BROWSE Brw-MovExtrac.
       END.
       ELSE DO:
            RUN BusConCta.
            APPLY "ENTRY":U TO BROWSE Brw-Concil.
       END.
  END.
  RETURN NO-APPLY.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Valor
&Scoped-define SELF-NAME W-Valor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Valor W-Win
ON LEAVE OF W-Valor IN FRAME F-Valor
DO:
  ASSIGN W-Valor 
         W_PorValor = TRUE.
  HIDE FRAME F-Valor.
  IF W-Valor EQ ?
  OR W-Valor EQ 0 THEN
     RETURN.

  IF W_SobreExt EQ 1 THEN DO:
     RUN BusMovCta.
     APPLY "ENTRY":U TO BROWSE Brw-MovCtaConta.
  END.
  ELSE DO:
       IF W_SobreExt EQ 2 THEN DO:
          RUN BusMovExt.
          APPLY "ENTRY":U TO BROWSE Brw-MovExtrac.
       END.
       ELSE DO:   
            RUN BusConCta.
            APPLY "ENTRY":U TO BROWSE Brw-Concil.
       END.   
  END.     
  RETURN NO-APPLY.          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ext
&Scoped-define SELF-NAME W_CheExt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CheExt W-Win
ON LEAVE OF W_CheExt IN FRAME F_Ext /* Número Documento */
DO:
  ASSIGN W_CheExt.
  IF W_CheExt EQ ""
  OR W_CheExt EQ ?
  OR Btn_Grabar:SENSITIVE = FALSE THEN
     RETURN.

   ASSIGN FRAME F_Ext W_CheExt
                      W_SecExt1
                      Rs_ActExt.

   RUN Justificar IN W_Manija (INPUT-OUTPUT W_CheExt, INPUT "0", 10, "I").

   IF Rs_ActExt EQ 1 THEN DO:
      ASSIGN W_SecExt1 = 0
             W_ValExt  = 0.
      ASSIGN W_Entero = INTEGER(W_CheExt) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
         IF INTEGER(W_CheExt) EQ 0 THEN
            ASSIGN W_CheExt = "SN" + STRING(W_Dia, "99") + STRING(W_MesC, "99") + STRING(W_Ano, "9999")
                   W_CheExt:SCREEN-VALUE =  W_CheExt.
      
      RUN Sec_ManDoc.
      ASSIGN W_SecExt1:SCREEN-VALUE = STRING(W_SecExt1)
             W_ValExt:SCREEN-VALUE  = STRING(W_ValExt)
             W_CheExt:SENSITIVE = FALSE.
      RETURN.
   END. 

   FIND Mov_Extracto WHERE Mov_Extracto.Agencia    EQ W_Ofi
                       AND Mov_Extracto.Cuenta     EQ W_CtaBco 
                       AND Mov_Extracto.Num_Cheque EQ W_CheExt 
                       AND Mov_Extracto.Secuencia  EQ W_SecExt1 
                       AND Mov_Extracto.Valor      GT 0   
                     NO-ERROR.
   RUN Display-ExtManual.
                     
   IF NOT AVAILABLE Mov_Extracto THEN DO:
      ASSIGN W_CheExt:SENSITIVE  = FALSE
             W_SecExt1:SENSITIVE = FALSE.
      RETURN.
   END.
   
   IF Mov_Extracto.Fecha LT W_FecInic THEN DO:
      MESSAGE "Este Registro No Se Puede Modificar." SKIP
              "Pertenece a Otro Periódo" Mov_Extracto.Fecha
         VIEW-AS ALERT-BOX ERROR TITLE "Error al Modificar".
      ASSIGN Btn_Grabar:SENSITIVE = FALSE.
      RETURN NO-APPLY.
   END.           

   FIND Conciliacion WHERE Conciliacion.Agencia       EQ Mov_Extracto.Agencia
                       AND Conciliacion.Cuenta        EQ Mov_Extracto.Cuenta
                       AND Conciliacion.Reg_MovExtrac EQ Mov_Extracto.Reg_MovExtrac
                     NO-LOCK NO-ERROR.
   IF AVAILABLE Conciliacion THEN DO:   
      IF Conciliacion.Novedad EQ "CC"
      OR Conciliacion.Novedad EQ "NF" THEN DO:
         MESSAGE "Registro ya esta Conciliado"
            VIEW-AS ALERT-BOX.
         ASSIGN Btn_Grabar:SENSITIVE = FALSE.
         RETURN NO-APPLY.
      END.
   END.
   ASSIGN Btn_Grabar:SENSITIVE = TRUE
          W_CheExt:SENSITIVE   = FALSE
          W_SecExt1:SENSITIVE  = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME W_CtaBco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaBco W-Win
ON LEAVE OF W_CtaBco IN FRAME F-Main /* Cta de Banco */
DO:
  ASSIGN W_CtaBco.
  IF W_CtaBco EQ "" THEN    
     RETURN.
  APPLY "TAB" TO SELF.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaBco W-Win
ON TAB OF W_CtaBco IN FRAME F-Main /* Cta de Banco */
OR MOUSE-SELECT-DBLCLICK OF W_CtaBco
  OR RETURN OF W_CtaBco DO:
  DEFI VAR W_Nada AS CHARACTER.
  DEFINE VARIABLE W_Pnombre LIKE cuentas.nombre.
  DEFINE VARIABLE W_naturaleza LIKE cuentas.naturaleza.
  DEFINE VARIABLE w_ctrNat     LIKE cuentas.ctr_naturaleza.
  ASSIGN W_CtaBco.

  FIND Cuentas WHERE Cuentas.Cuenta        EQ W_CtaBco 
                 AND Cuentas.Cod_FlujoEfec EQ "D"
                 AND Cuentas.Car_Efectivo  NE 2 
                 AND Cuentas.Estado        EQ 1
                 AND Cuentas.Tipo          EQ 2  
               NO-LOCK NO-ERROR.  
  IF NOT AVAILABLE(Cuentas) THEN DO:
     ASSIGN W_CtaBco = ""
            W_Nada = " ".
     /*RUN C-Cuentas.r (OUTPUT W_CtaBco,OUTPUT W_Nada).*/
     RUN C-Cuentas.R (OUTPUT W_ctabco, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, OUTPUT W_CtrNat, INPUT "M").
     ASSIGN W_FormatoExt = INTEGER(W_Nada).
     IF W_CtaBco EQ "" THEN
        RETURN NO-APPLY.
     FIND Cuentas WHERE Cuentas.Cuenta EQ W_CtaBco NO-LOCK NO-ERROR.
  END. 
  ELSE  ASSIGN W_FormatoExt = Cuentas.Cod_FormatoExt
               W_CtaBco:SCREEN-VALUE IN FRAME F-Main = W_CtaBco.
 
  DISPLAY Cuentas.Nombre @ W_NomBco 
          W_CtaBco WITH FRAME F-Main.
  APPLY "ENTRY" TO F-Holgura.
  /*RETURN NO-APPLY.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ext
&Scoped-define SELF-NAME W_DescExt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_DescExt W-Win
ON LEAVE OF W_DescExt IN FRAME F_Ext /* Descripción */
DO:
  ASSIGN W_DescExt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_DiaExt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_DiaExt W-Win
ON LEAVE OF W_DiaExt IN FRAME F_Ext /* Dia */
DO:
  ASSIGN W_DiaExt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME W_FecCorte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecCorte W-Win
ON LEAVE OF W_FecCorte IN FRAME F-Main /* Fecha para Conciliar */
DO:
  ASSIGN W_FecCorte NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE  "La Fecha no es Valida" 
        VIEW-AS ALERT-BOX ERROR TITLE "Error de Fecha".
     ASSIGN W_FecCorte = ?
            W_FecCorte:SCREEN-VALUE = STRING(W_FecCorte).
     RETURN NO-APPLY.
  END.     
 
  IF W_FecCorte EQ ? THEN RETURN.

  RUN Valida_Inicio NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     ASSIGN W_FecCorte = ?
            W_FecCorte:SCREEN-VALUE = STRING(W_FecCorte).
     RETURN NO-APPLY.
  END.   
  RUN Graba_MovCta.
  RUN Llena_TmpCta.
  RUN Llena_TmpExt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Informe
&Scoped-define SELF-NAME W_SdoExt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_SdoExt W-Win
ON ENTRY OF W_SdoExt IN FRAME F_Informe /* Saldo del banco */
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_SdoExt W-Win
ON LEAVE OF W_SdoExt IN FRAME F_Informe /* Saldo del banco */
DO:
  ASSIGN W_SdoExt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ext
&Scoped-define SELF-NAME W_SecExt1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_SecExt1 W-Win
ON LEAVE OF W_SecExt1 IN FRAME F_Ext /* Secuencia */
DO:
  ASSIGN W_SecExt1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_ValExt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_ValExt W-Win
ON LEAVE OF W_ValExt IN FRAME F_Ext /* Valor Transacción */
DO:
  ASSIGN W_ValExt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Concil
&Scoped-define BROWSE-NAME Brw-Concil
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asg_RegCon W-Win 
PROCEDURE Asg_RegCon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAILABLE Tmp-Concilia THEN
     ASSIGN Curr-record = Tmp-Concilia.Puntero.
  ELSE
     MESSAGE "El Registro Que esta Buscando" SKIP
             "No Se Encuentra en el Movimiento Contable."   
        VIEW-AS ALERT-BOX INFORMATION TITLE "Buscando Documento".

  IF Curr-record EQ ? THEN RETURN.
  REPOSITION Brw-Concil TO ROWID Curr-record.
  GET NEXT Brw-Concil.
  IF Brw-Concil:SELECT-FOCUSED-ROW() IN FRAME F-Concil THEN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asg_RegCta W-Win 
PROCEDURE Asg_RegCta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAILABLE Tmp-CtaConta THEN
     ASSIGN Curr-record = ROWID(Tmp-CtaConta).
  ELSE
      MESSAGE "El Registro Que esta Buscando" SKIP
              "No Se Encuentra en el Movimiento Contable."   
        VIEW-AS ALERT-BOX INFORMATION TITLE "Buscando Documento".

  IF Curr-record EQ ? THEN RETURN.
  REPOSITION Brw-MovCtaConta TO ROWID Curr-record.
  GET NEXT Brw-MovCtaConta.
  IF Brw-MovCtaConta:SELECT-FOCUSED-ROW() IN FRAME F-Main THEN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asg_RegExt W-Win 
PROCEDURE Asg_RegExt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAILABLE Tmp-Extracto THEN 
     ASSIGN Curr-record = ROWID(Tmp-Extracto).   
  ELSE
     MESSAGE "El Registro Que esta Buscando" SKIP
             "No Se Encuentra en el Movimiento Extracto."   
       VIEW-AS ALERT-BOX INFORMATION TITLE "Buscando Documento".
  IF Curr-record EQ ? THEN RETURN.
  REPOSITION Brw-MovExtrac TO ROWID Curr-record.
  GET NEXT Brw-MovExtrac.
  IF Brw-MovExtrac:SELECT-FOCUSED-ROW() IN FRAME F-Main THEN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aut_CC W-Win 
PROCEDURE Aut_CC :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  Blk-For:
  FOR EACH Tmp-CtaConta WHERE Tmp-CtaConta.Fecha LE W_FecCorte:                         
      FIND FIRST Tmp-Extracto WHERE Tmp-Extracto.Num_Cheque EQ Tmp-CtaConta.Num_Cheque
                                AND Tmp-Extracto.Naturaleza EQ Tmp-CtaConta.Naturaleza
                                AND Tmp-Extracto.Valor      EQ Tmp-CtaConta.Valor
                              NO-LOCK NO-ERROR.
      Blk-Rep:
      REPEAT:
         IF AVAILABLE Tmp-Extracto THEN DO:
            FIND Conciliacion WHERE Conciliacion.Agencia       EQ Tmp-Extracto.Agencia
                                AND Conciliacion.Cuenta        EQ Tmp-Extracto.Cuenta
                                AND Conciliacion.Reg_MovExtrac EQ Tmp-Extracto.Reg_MovExtrac
                              NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Conciliacion THEN LEAVE Blk-Rep.
            FIND NEXT Tmp-Extracto WHERE Tmp-Extracto.Num_Cheque EQ Tmp-CtaConta.Num_Cheque
                                     AND Tmp-Extracto.Naturaleza EQ Tmp-CtaConta.Naturaleza
                                     AND Tmp-Extracto.Valor      EQ Tmp-CtaConta.Valor
                                   NO-LOCK NO-ERROR.
         END.
         ELSE NEXT Blk-For.
      END.
     
      ASSIGN W-Fec = ABS( Tmp-CtaConta.Fecha - Tmp-Extracto.Fecha ) + 1.
 
      RUN Busca_ConDeCta.
      IF  F-Holgura NE 0 
      AND W-Fec     GT F-Holgura THEN
          ASSIGN Conciliacion.Novedad = "NF".
      ELSE DO:   
         ASSIGN Conciliacion.Novedad = "CC"
                Conciliacion.Fecha_Conciliacion = W_FecCorte.
         DELETE Tmp-CtaConta.
         DELETE Tmp-Extracto.          
      END.
  END.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aut_NatValor W-Win 
PROCEDURE Aut_NatValor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  Blk-For:
  FOR EACH Tmp-CtaConta WHERE Tmp-CtaConta.Fecha      LE W_FecCorte
                          AND Tmp-CtaConta.Naturaleza EQ "DB":
      FIND FIRST Tmp-Extracto WHERE Tmp-Extracto.Naturaleza EQ "DB"
                                AND Tmp-Extracto.Valor      EQ Tmp-CtaConta.Valor
                              NO-LOCK NO-ERROR.
      Blk-Rep:
      REPEAT:
         IF AVAILABLE Tmp-Extracto THEN DO:
            FIND Conciliacion WHERE Conciliacion.Agencia       EQ Tmp-Extracto.Agencia
                                AND Conciliacion.Cuenta        EQ Tmp-Extracto.Cuenta
                                AND Conciliacion.Reg_MovExtrac EQ Tmp-Extracto.Reg_MovExtrac
                              NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Conciliacion THEN LEAVE Blk-Rep.
            FIND NEXT Tmp-Extracto WHERE Tmp-Extracto.Naturaleza EQ "DB"
                                     AND Tmp-Extracto.Valor      EQ Tmp-CtaConta.Valor
                                   NO-LOCK NO-ERROR.
         END.
         ELSE NEXT Blk-For.
      END.
     
      ASSIGN W-Fec = ABS( Tmp-CtaConta.Fecha - Tmp-Extracto.Fecha ) + 1
             W_Continua = NO.
      RUN Busca_ConDeCta.               

      IF F-Holgura NE 0 THEN DO:
         IF W-Fec LE 5 THEN DO:
            ASSIGN W_Continua = TRUE.     
            IF W-Fec GT F-Holgura THEN 
               ASSIGN Conciliacion.Novedad = "NF".
            ELSE DO:
               ASSIGN Conciliacion.Novedad            = "CC"
                      Conciliacion.Fecha_Conciliacion = W_FecCorte.                         
               DELETE Tmp-CtaConta.
               DELETE Tmp-Extracto.          
            END.    
         END.
      END.
      ELSE DO:
         IF W-Fec LE 5 THEN DO:
            ASSIGN Conciliacion.Novedad            = "CC"
                   Conciliacion.Fecha_Conciliacion = W_FecCorte
                   W_Continua = TRUE.     
            DELETE Tmp-CtaConta.
            DELETE Tmp-Extracto.          
         END.    
      END.

      IF NOT W_Continua THEN DO:
         IF  Conciliacion.Per_Conciliacion GE W_FecInic
         AND Conciliacion.Per_Conciliacion LE W_FecCorte THEN DO:
             FIND CURRENT Conciliacion.
             DELETE Conciliacion.
         END.
         ELSE
            RUN Busca_Extracto (YES).
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aut_NONC W-Win 
PROCEDURE Aut_NONC :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH Tmp-CtaConta WHERE Tmp-CtaConta.Fecha LE W_FecCorte:                         
      FIND Conciliacion WHERE Conciliacion.Agencia      EQ Tmp-CtaConta.Agencia
                          AND Conciliacion.Cuenta       EQ Tmp-CtaConta.Cuenta
                          AND Conciliacion.Reg_CtaConta EQ Tmp-CtaConta.Reg_CtaConta 
                        NO-ERROR.
      IF AVAILABLE Conciliacion THEN NEXT.
  
      CREATE Conciliacion.
      ASSIGN Conciliacion.Agencia          = Tmp-CtaConta.Agencia
             Conciliacion.Cuenta           = Tmp-CtaConta.Cuenta
             Conciliacion.Reg_CtaConta     = Tmp-CtaConta.Reg_CtaConta
             Conciliacion.Per_Conciliacion = W_FecCorte
             Conciliacion.Usuario          = W_Usuario
             Conciliacion.Reg_Cruce        = ""
             Conciliacion.Reg_MovExtrac    = ""
             Conciliacion.Novedad          = IF Tmp-CtaConta.Naturaleza EQ "DB" THEN "NO" 
                                             ELSE "NC".
  END.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aut_VD W-Win 
PROCEDURE Aut_VD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  Blk-For:
  FOR EACH Tmp-CtaConta WHERE Tmp-CtaConta.Fecha      LE W_FecCorte
                          AND Tmp-CtaConta.Naturaleza NE "DB":
      FIND FIRST Tmp-Extracto WHERE Tmp-Extracto.Num_Cheque EQ Tmp-CtaConta.Num_Cheque
                                AND Tmp-Extracto.Naturaleza EQ Tmp-CtaConta.Naturaleza
                              NO-ERROR.
      Blk-Rep:
      REPEAT:
         IF AVAILABLE Tmp-Extracto THEN DO:
            FIND Conciliacion WHERE Conciliacion.Agencia       EQ Tmp-Extracto.Agencia
                                AND Conciliacion.Cuenta        EQ Tmp-Extracto.Cuenta
                                AND Conciliacion.Reg_MovExtrac EQ Tmp-Extracto.Reg_MovExtrac
                              NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Conciliacion THEN LEAVE Blk-Rep.
            IF Conciliacion.Novedad EQ "NO"
            OR Conciliacion.Novedad EQ "NC"
            OR Conciliacion.Novedad EQ "NR"
            OR Conciliacion.Novedad EQ "NG"  THEN DO:
               FIND CURRENT Conciliacion.
               DELETE Conciliacion.
               LEAVE Blk-Rep.
            END. 
            FIND NEXT Tmp-Extracto WHERE Tmp-Extracto.Num_Cheque EQ Tmp-CtaConta.Num_Cheque
                                     AND Tmp-Extracto.Naturaleza EQ Tmp-CtaConta.Naturaleza
                                   NO-ERROR.
         END.
         ELSE NEXT Blk-For.
      END.
      RUN Busca_ConDeCta. 
      ASSIGN Conciliacion.Novedad = "VD".
  END.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra_Conciliacion W-Win 
PROCEDURE Borra_Conciliacion :
/*------------------------------------------------------------------------------
  Objetivo: Borra la conciliacion del periodo.     
------------------------------------------------------------------------------*/
   FOR EACH Conciliacion WHERE Conciliacion.Agencia          EQ W_Ofi   
                           AND Conciliacion.Cuenta           EQ W_CtaBco
                           AND Conciliacion.Per_Conciliacion GE W_FecInic
                           AND Conciliacion.Per_Conciliacion LE W_FecCorte
                         EXCLUSIVE-LOCK:
       DELETE Conciliacion.              
       ASSIGN W_RegBorra = W_RegBorra + 1.
       DISPLAY W_RegBorra WITH FRAME F_Borrar. 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra_ConExt W-Win 
PROCEDURE Borra_ConExt :
/*------------------------------------------------------------------------------
  Objetivo : Procedimiento que borra la conciliacion o el movimiento del extracto
             para un periodo determinado.
------------------------------------------------------------------------------*/
  ASSIGN W_RegBorra = 0.

  IF Rs_Borrar EQ 1 THEN DO:
     RUN Borra_Conciliacion.
     RUN Desmarca_Ant.
  END.
  ELSE DO:
     RUN Borra_Conciliacion.
     RUN Desmarca_Ant.
     FOR EACH Mov_Extracto WHERE Mov_Extracto.Agencia  EQ W_Ofi
                             AND Mov_Extracto.Cuenta   EQ W_CtaBco
                             AND Mov_Extracto.Fecha    GE W_FecInic
                             AND Mov_Extracto.Fecha    LE W_FecCorte
                           EXCLUSIVE-LOCK:
         DELETE Mov_Extracto.              
         ASSIGN W_RegBorra = W_RegBorra + 1.
         DISPLAY W_RegBorra WITH FRAME F_Borrar. 
     END.     
  END.       
  IF W_RegBorra NE 0 THEN DO: 
     IF Rs_Borrar EQ 1 THEN  
        RUN P-GraLog IN W_Manija              
            (INPUT STRING(W_Usuario) + " " + "Borrado de Conciliación ").
     ELSE
        RUN P-GraLog IN W_Manija
            (INPUT STRING(W_Usuario) + " " + "Borrado de Mov_Extracto.").

     ASSIGN W_ConsulCon = TRUE.
     RUN Inicia-Tmp.
     RUN Llena_TmpCta.
     RUN LLena_TmpExt.
  END.   
  MESSAGE "Nro.de Registros Borrados..." W_RegBorra VIEW-AS ALERT-BOX.
  W_RegBorra:SCREEN-VALUE = "0".  
  ENABLE ALL EXCEPT W_NomBco WITH FRAME F-Main.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Busca_ConDeCta W-Win 
PROCEDURE Busca_ConDeCta :
/*------------------------------------------------------------------------------
  Objetivo:  Busca en conciliacion si un registro de mvto contable ya esta 
             conciliado.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE W_RegExt         LIKE Conciliacion.Reg_MovExtrac.
  DEFINE BUFFER   Tmp-Conciliacion FOR  Conciliacion.  
    
  FIND Conciliacion WHERE Conciliacion.Agencia      EQ Tmp-CtaConta.Agencia
                      AND Conciliacion.Cuenta       EQ Tmp-CtaConta.Cuenta
                      AND Conciliacion.Reg_CtaConta EQ Tmp-CtaConta.Reg_CtaConta 
                    NO-ERROR.
  IF NOT AVAILABLE Conciliacion THEN DO:
     CREATE Conciliacion.
     ASSIGN Conciliacion.Agencia          = Tmp-CtaConta.Agencia
            Conciliacion.Cuenta           = Tmp-CtaConta.Cuenta
            Conciliacion.Reg_CtaConta     = Tmp-CtaConta.Reg_CtaConta
            Conciliacion.Per_Conciliacion = W_FecCorte.
  END.
  IF Conciliacion.Per_Conciliacion EQ ? THEN  
     ASSIGN Conciliacion.Per_Conciliacion = W_FecCorte.
    
  ASSIGN Conciliacion.Usuario       = W_Usuario
         W_RegExt                   = Conciliacion.Reg_MovExtrac 
         W_PerConcil                = Conciliacion.Per_Conciliacion
         Conciliacion.Reg_Cruce     = ""
         Conciliacion.Reg_MovExtrac = IF AVAILABLE Tmp-Extracto THEN Tmp-Extracto.Reg_MovExtrac ELSE "".

  IF W_RegExt NE "" THEN DO:            
     FIND Mov_Extracto WHERE Mov_Extracto.Agencia       EQ W_Ofi
                         AND Mov_Extracto.Cuenta        EQ W_CtaBco 
                         AND Mov_Extracto.Reg_MovExtrac EQ 
                             ("EX" + STRING(SUBSTRING(W_RegExt,3,4), "9999")+ STRING(SUBSTRING(W_RegExt,7,2), "99")
                              + "00000")
                       NO-LOCK NO-ERROR.
     ASSIGN W_PerConcil = Mov_Extracto.Fecha.          
     CREATE Tmp-Conciliacion.     
     ASSIGN Tmp-Conciliacion.Agencia          = Tmp-CtaConta.Agencia
            Tmp-Conciliacion.Cuenta           = Tmp-CtaConta.Cuenta
            Tmp-Conciliacion.Reg_MovExtrac    = W_RegExt
            Tmp-Conciliacion.Per_Conciliacion = W_PerConcil
            Tmp-Conciliacion.Usuario          = W_Usuario
            Tmp-Conciliacion.Novedad          = IF Tmp-CtaConta.Naturaleza EQ "DB" THEN "NR" 
                                                ELSE "NG".
  END.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Busca_Cta W-Win 
PROCEDURE Busca_Cta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER W_Limpia AS LOGICAL.
  FIND Mov_CtaConta WHERE Mov_CtaConta.Agencia      EQ Conciliacion.Agencia
                      AND Mov_CtaConta.Cuenta       EQ Conciliacion.Cuenta
                      AND Mov_CtaConta.Reg_CtaConta EQ Conciliacion.Reg_CtaConta
                    NO-LOCK NO-ERROR.                          

  ASSIGN  Conciliacion.Fecha_Conciliacion = ?
          Conciliacion.Reg_Cruce          = ""
          Conciliacion.Novedad            = IF Mov_CtaConta.Naturaleza EQ "DB" THEN "NO" 
                                            ELSE "NC".
  IF W_Limpia THEN
     ASSIGN Conciliacion.Reg_CtaConta = ""
            Conciliacion.Novedad      = IF Mov_CtaConta.Naturaleza EQ "DB" THEN "NR" 
                                        ELSE "NG".
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Busca_Extracto W-Win 
PROCEDURE Busca_Extracto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER W_Limpia AS LOGICAL.
  
  FIND Mov_Extracto WHERE Mov_Extracto.Agencia       EQ Conciliacion.Agencia
                      AND Mov_Extracto.Cuenta        EQ Conciliacion.Cuenta
                      AND Mov_Extracto.Reg_MovExtrac EQ Conciliacion.Reg_MovExtrac
                    NO-LOCK NO-ERROR.                          

  ASSIGN Conciliacion.Fecha_Conciliacion = ?
         Conciliacion.Reg_Cruce          = ""
         Conciliacion.Novedad            = IF Mov_Extracto.Naturaleza EQ "DB" THEN "NR"
                                           ELSE "NG".
  IF W_Limpia THEN
     ASSIGN Conciliacion.Reg_MovExtrac = ""
            Conciliacion.Novedad       = IF Mov_Extracto.Naturaleza EQ "DB" THEN "NO" 
                                         ELSE "NC".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BusConCta W-Win 
PROCEDURE BusConCta :
/*------------------------------------------------------------------------------
  Objetivo:  Busca Registros por documento o valor, del mvto contable de la cuenta,
             en el browse de Conciliacion.
------------------------------------------------------------------------------*/
  ASSIGN Curr-record = ROWID(Conciliacion). 
  IF NOT W_PorValor THEN
     FIND FIRST Tmp-Concilia WHERE Tmp-Concilia.Num_Cheque EQ W-Num1
                             NO-LOCK NO-ERROR.
  ELSE
     FIND FIRST Tmp-Concilia WHERE Tmp-Concilia.Valor EQ W-Valor
                             NO-LOCK NO-ERROR.
  RUN Asg_RegCon.
  APPLY "ENTRY":U TO BROWSE Brw-Concil.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BusMovCta W-Win 
PROCEDURE BusMovCta :
/*------------------------------------------------------------------------------
  Objetivo:  Busca Registros por documento o valor, del mvto contable de la cuenta.
------------------------------------------------------------------------------*/
  ASSIGN Curr-record = ROWID(Tmp-CtaConta).
  IF NOT W_PorValor THEN
     FIND FIRST Tmp-CtaConta WHERE Tmp-CtaConta.Num_Cheque EQ W-Num1
                             NO-LOCK NO-ERROR.
  ELSE
     FIND FIRST Tmp-CtaConta WHERE Tmp-CtaConta.Valor EQ W-Valor
                             NO-LOCK NO-ERROR.
  RUN Asg_RegCta.
  APPLY "ENTRY":U TO BROWSE Brw-MovCtaConta.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BusMovExt W-Win 
PROCEDURE BusMovExt :
/*------------------------------------------------------------------------------
   Objetivo:  Busca Registros por documento o valor, del mvto contable de la cuenta.
------------------------------------------------------------------------------*/
  ASSIGN Curr-record = ROWID(Tmp-Extracto).

  IF NOT W_PorValor THEN
     FIND FIRST Tmp-Extracto WHERE Tmp-Extracto.Num_Cheque EQ W-Num1
                             NO-LOCK NO-ERROR.
  ELSE
     FIND FIRST Tmp-Extracto WHERE Tmp-Extracto.Valor EQ W-Valor
                             NO-LOCK NO-ERROR.
  RUN Asg_RegExt.
  APPLY "ENTRY":U TO BROWSE Brw-MovExtrac.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConcAut_MovCta W-Win 
PROCEDURE ConcAut_MovCta :
/*------------------------------------------------------------------------------
  Objetivo:   Concilia partiendo de la tabla temporal Tmp-CtaConta.   
  Notes:      CC: Conciliado
              NF: Holgura no permite el "CC"
              VD: Valores Diferentes
              NR: No Registrado 
              NC: No Cobrados 
              NO: No Consignado
              NG: No Girado
------------------------------------------------------------------------------*/ 
  RUN Aut_CC.
  RUN Aut_NatValor.
  RUN Aut_VD. 
  RUN Aut_NONC.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConcAut_MovExt W-Win 
PROCEDURE ConcAut_MovExt :
/*------------------------------------------------------------------------------
  Objetivo:   Concilia partiendo de la tabla temporal Tmp-Extracto.     
  Notes:      NR: No Registrado 
              NG: No Girado
------------------------------------------------------------------------------*/    
  FOR EACH Tmp-Extracto WHERE Tmp-Extracto.Fecha    LE W_FecCorte
                          AND Tmp-Extracto.Valor    NE 0:
                                                                   
      FIND Conciliacion WHERE Conciliacion.Agencia       EQ Tmp-Extracto.Agencia
                          AND Conciliacion.Cuenta        EQ Tmp-Extracto.Cuenta
                          AND Conciliacion.Reg_MovExtrac EQ Tmp-Extracto.Reg_MovExtrac
                        NO-LOCK NO-ERROR.
      IF AVAILABLE Conciliacion THEN NEXT.
     
      CREATE Conciliacion.
      ASSIGN Conciliacion.Agencia          = Tmp-Extracto.Agencia
             Conciliacion.Cuenta           = Tmp-Extracto.Cuenta
             Conciliacion.Reg_MovExtrac    = Tmp-Extracto.Reg_MovExtrac
             Conciliacion.Per_Conciliacion = W_FecCorte                 
             Conciliacion.Usuario          = W_Usuario
             Conciliacion.Novedad          = IF Tmp-Extracto.Naturaleza = "DB" THEN "NR"  
                                             ELSE "NG".
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Conciliar W-Win 
PROCEDURE Conciliar :
/*------------------------------------------------------------------------------
  Objetivo: Inicia el proceso de Conciliacion.
------------------------------------------------------------------------------*/
  RUN P-GraLog IN W_Manija (INPUT "EJECUTAR CONCILIACION " + STRING(TIME,"HH:MM:SS AM") + " " + STRING(W_Fecha) + STRING(W_Usuario)).  
  SESSION:SET-WAIT-STATE("GENERAL"). 
  FOR EACH Conciliacion WHERE Conciliacion.Agencia          EQ W_Ofi
                          AND Conciliacion.Cuenta           EQ W_CtaBco
                          AND Conciliacion.Per_Conciliacion GE W_FecInic
                          AND Conciliacion.Per_Conciliacion LE W_FecCorte
                          AND Conciliacion.Novedad          NE "CC"
                        EXCLUSIVE-LOCK:
      DELETE Conciliacion.
  END. 
  RUN Limpia_VD.                                                
  RUN ConcAut_MovCta. 
  RUN ConcAut_MovExt.
  DISABLE ALL WITH FRAME F-Main.
  CLOSE QUERY Brw-Concil.
  {&OPEN-QUERY-Brw-Concil}
  VIEW FRAME F-Concil.
  ENABLE Btn-Salir Btn-Desmar WITH FRAME F-Concil.
  SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConcMan_MovCta W-Win 
PROCEDURE ConcMan_MovCta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO I = Brw-MovCtaConta:NUM-SELECTED-ROWS IN FRAME F-Main TO 1 By -1:
         ASSIGN W_Status = Brw-MovCtaConta:FETCH-SELECTED-ROW(I).
         FIND Tmp-Extracto WHERE Tmp-Extracto.Agencia       EQ 999
                             AND Tmp-Extracto.Cuenta        EQ "99999999999999"
                             AND Tmp-Extracto.Reg_MovExtrac EQ "9999999999999" 
                           NO-LOCK NO-ERROR.  
         RUN Busca_ConDeCta.
         ASSIGN Conciliacion.Novedad            = "CC"
                Conciliacion.Reg_Cruce          = ("MV" + STRING(W_Ano,"9999") + STRING(W_MesC,"99") + STRING(W_Cont,"99999"))
                Conciliacion.Fecha_Conciliacion = W_FecCorte.
   
         DELETE Tmp-CtaConta.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConcMan_MovExt W-Win 
PROCEDURE ConcMan_MovExt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE W_RegCta         LIKE Conciliacion.Reg_CtaConta.
   DEFINE VARIABLE W_PerReg           AS CHARACTER FORMAT "X(6)".
   DEFINE BUFFER   Tmp-Conciliacion FOR  Conciliacion.  

   DO I = Brw-MovExtrac:NUM-SELECTED-ROWS IN FRAME F-Main TO 1 By -1: 
          ASSIGN W_Status = Brw-MovExtrac:FETCH-SELECTED-ROW(I).    
          FIND Conciliacion WHERE Conciliacion.Agencia       EQ Tmp-Extracto.Agencia
                              AND Conciliacion.Cuenta        EQ Tmp-Extracto.Cuenta
                              AND Conciliacion.Reg_MovExtrac EQ Tmp-Extracto.Reg_MovExtrac 
                            NO-ERROR.
          IF NOT AVAILABLE Conciliacion THEN DO:
             CREATE Conciliacion.
             ASSIGN Conciliacion.Agencia          = Tmp-Extracto.Agencia
                    Conciliacion.Cuenta           = Tmp-Extracto.Cuenta
                    Conciliacion.Reg_MovExtrac    = Tmp-Extracto.Reg_MovExtrac
                    Conciliacion.Per_Conciliacion = W_FecCorte.
          END.
             
          ASSIGN W_PerReg    = SUBSTRING(Conciliacion.Reg_MovExtrac,3,6)
                 W_PerConcil = Conciliacion.Per_Conciliacion.
          FIND Mov_Extracto WHERE Mov_Extracto.Agencia       EQ W_Ofi
                              AND Mov_Extracto.Cuenta        EQ W_CtaBco 
                              AND Mov_Extracto.Reg_MovExtrac EQ 
                                  ("EX" + STRING(W_PerReg) + "00000")
                            NO-LOCK NO-ERROR.
          ASSIGN Conciliacion.Per_Conciliacion = Mov_Extracto.Fecha
                 Conciliacion.Novedad            = "CC"
                 Conciliacion.Fecha_Conciliacion = W_FecCorte
                 Conciliacion.Usuario            = W_Usuario
                 W_RegCta                        = Conciliacion.Reg_CtaConta 
                 Conciliacion.Reg_Cruce          = ("MV" + STRING(W_Ano,"9999") + STRING(W_MesC,"99") + STRING(W_Cont,"99999"))
                 Conciliacion.Reg_CtaConta       = "".
   
          IF  W_RegCta NE ""
          AND W_RegCta NE ? THEN DO:            
             ASSIGN W_PerConcil = DATE(INTEGER(SUBSTRING(W_RegCta,7,2)), 01,
                                       INTEGER(SUBSTRING(W_RegCta,3,4))).
             CREATE Tmp-Conciliacion.     
             ASSIGN Tmp-Conciliacion.Agencia          = Tmp-Extracto.Agencia
                    Tmp-Conciliacion.Cuenta           = Tmp-Extracto.Cuenta
                    Tmp-Conciliacion.Reg_CtaConta     = W_RegCta
                    Tmp-Conciliacion.Per_Conciliacion = W_PerConcil
                    Tmp-Conciliacion.Usuario          = W_Usuario
                    Tmp-Conciliacion.Novedad          = IF Tmp-Extracto.Naturaleza EQ "DB" THEN "N0" 
                                                        ELSE "NC".
          END.                                              
          DELETE Tmp-Extracto.
   END.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea_TmpCon W-Win 
PROCEDURE Crea_TmpCon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER P_Valor      LIKE Mov_CtaConta.Valor.
  DEFINE INPUT PARAMETER P_Num_Cheque LIKE Mov_CtaConta.Num_Cheque.
  
  FIND Tmp-Concilia WHERE Tmp-Concilia.Puntero    EQ ROWID(Conciliacion)
                      AND Tmp-Concilia.Num_Cheque EQ P_Num_Cheque
                      AND Tmp-Concilia.Valor      EQ P_Valor NO-ERROR.
  IF NOT AVAILABLE Tmp-Concilia THEN DO:
     CREATE Tmp-Concilia.
     ASSIGN Tmp-Concilia.Puntero    = ROWID(Conciliacion)
            Tmp-Concilia.Num_Cheque = P_Num_Cheque
            Tmp-Concilia.Valor      = P_Valor.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea_TmpCta W-Win 
PROCEDURE Crea_TmpCta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CREATE Tmp-CtaConta.
  ASSIGN Tmp-CtaConta.Cuenta       = Mov_CtaConta.Cuenta
         Tmp-CtaConta.Num_Cheque   = Mov_CtaConta.Num_Cheque
         Tmp-CtaConta.Secuencia    = Mov_CtaConta.Secuencia
         Tmp-CtaConta.Fecha        = Mov_CtaConta.Fecha
         Tmp-CtaConta.Agencia      = Mov_CtaConta.Agencia
         Tmp-CtaConta.Naturaleza   = Mov_CtaConta.Naturaleza
         Tmp-CtaConta.Descripcion  = Mov_CtaConta.Descripcion 
         Tmp-CtaConta.Valor        = Mov_CtaConta.Valor 
         Tmp-CtaConta.Reg_CtaConta = Mov_CtaConta.Reg_CtaConta.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea_TmpExt W-Win 
PROCEDURE Crea_TmpExt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 CREATE Tmp-Extracto.
 ASSIGN Tmp-Extracto.Cuenta        = Mov_Extracto.Cuenta
        Tmp-Extracto.Num_Cheque    = Mov_Extracto.Num_Cheque
        Tmp-Extracto.Secuencia     = Mov_Extracto.Secuencia
        Tmp-Extracto.Fecha         = Mov_Extracto.Fecha
        Tmp-Extracto.Agencia       = Mov_Extracto.Agencia
        Tmp-Extracto.Naturaleza    = Mov_Extracto.Naturaleza
        Tmp-Extracto.Descripcion   = Mov_Extracto.Descripcion 
        Tmp-Extracto.Valor         = Mov_Extracto.Valor 
        Tmp-Extracto.Reg_MovExtrac = Mov_Extracto.Reg_MovExtrac.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Desmarca_Ant W-Win 
PROCEDURE Desmarca_Ant :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE W_PerReg    AS CHARACTER FORMAT "X(6)".
  DEFINE VARIABLE W_PerReg2   AS CHARACTER FORMAT "X(6)".
  DEFINE VARIABLE W_PerConcil AS CHARACTER FORMAT "X(6)".
  DEFINE VARIABLE W_Realizo   AS LOGICAL INITIAL NO.
  DEFINE VARIABLE W_FecConc   AS DATE.
  DEFINE BUFFER   Tmp-Conciliacion FOR  Conciliacion.  
  ASSIGN W_PerConcil = STRING(W_Ano,"9999") + STRING(W_MesC,"99").
  
  FOR EACH Conciliacion WHERE Conciliacion.Agencia            EQ W_Ofi
                          AND Conciliacion.Cuenta             EQ W_CtaBco
                          AND (   (    Conciliacion.Fecha_Conciliacion GE W_FecInic
                                   AND Conciliacion.Fecha_Conciliacion LE W_FecCorte)
                               OR Conciliacion.Novedad        EQ "NF"
                               OR Conciliacion.Novedad        EQ "VD")
                        EXCLUSIVE-LOCK:
      ASSIGN W_RegBorra = W_RegBorra + 1
             W_PerReg   = SUBSTRING(Conciliacion.Reg_CtaConta,3,6).
             W_Realizo  = NO.
      IF W_PerReg EQ W_PerConcil THEN DO:
         RUN Busca_Cta (YES).
         ASSIGN W_Realizo = YES.
      END.

      ASSIGN W_PerReg = SUBSTRING(Conciliacion.Reg_MovExtrac,3,6).
      IF W_PerReg EQ W_PerConcil THEN DO:
         RUN Busca_Extracto (YES).
         ASSIGN W_Realizo = YES.
      END.
      IF  Conciliacion.Reg_CtaConta EQ ""
      AND Conciliacion.Reg_MovExtrac EQ "" THEN DO:
          DELETE Conciliacion.
          NEXT.
      END.
      ASSIGN W_PerReg = SUBSTRING(Conciliacion.Reg_Cruce,3,6).

      IF W_PerReg EQ W_PerConcil THEN DO:
         IF Conciliacion.Reg_MovExtrac NE "" THEN DO:
            RUN Busca_Extracto (NO).
            ASSIGN W_Realizo = YES.
         END.
         IF Conciliacion.Reg_CtaConta  NE "" THEN DO:
            RUN Busca_Cta (NO).
            ASSIGN W_Realizo = YES.
         END.
      END.
      IF Conciliacion.Novedad EQ "NF"
      OR Conciliacion.Novedad EQ "VD"
      OR W_Realizo THEN NEXT.

      ASSIGN W_PerReg    = SUBSTRING(Conciliacion.Reg_CtaConta,3,6)
             W_PerReg2   = SUBSTRING(Conciliacion.Reg_MovExtrac,3,6).
      IF W_PerReg EQ W_PerReg2 THEN DO:
         FIND Mov_Extracto WHERE Mov_Extracto.Agencia       EQ Conciliacion.Agencia
                             AND Mov_Extracto.Cuenta        EQ Conciliacion.Cuenta
                             AND Mov_Extracto.Reg_MovExtrac EQ Conciliacion.Reg_MovExtrac NO-LOCK.
         CREATE Tmp-Conciliacion.
         ASSIGN Tmp-Conciliacion.Agencia          = Conciliacion.Agencia
                Tmp-Conciliacion.Cuenta           = Conciliacion.Cuenta
                Tmp-Conciliacion.Reg_MovExtrac    = Conciliacion.Reg_MovExtrac
                Tmp-Conciliacion.Per_Conciliacion = Conciliacion.Per_Conciliacion
                Tmp-Conciliacion.Usuario          = W_Usuario
                Tmp-Conciliacion.Novedad          = IF Mov_Extracto.Naturaleza = "DB" THEN "NR"  
                                                    ELSE "NG".
         FIND Mov_CtaConta WHERE Mov_CtaConta.Agencia      EQ Conciliacion.Agencia
                             AND Mov_CtaConta.Cuenta       EQ Conciliacion.Cuenta
                             AND Mov_CtaConta.Reg_CtaConta EQ Conciliacion.Reg_CtaConta
                           NO-LOCK NO-ERROR.                          
         ASSIGN Conciliacion.Reg_MovExtrac      = ""
                Conciliacion.Fecha_Conciliacion = ?
                Conciliacion.Reg_Cruce          = ""
                Conciliacion.Novedad            = IF Mov_CtaConta.Naturaleza EQ "DB" THEN "NO" 
                                                  ELSE "NC".
      END.
      ELSE
         IF W_PerReg NE W_PerReg2 THEN DO:
            ASSIGN W_PerConcil = STRING(YEAR(Conciliacion.Per_Conciliacion), "9999") + 
                                 STRING(MONTH(Conciliacion.Per_Conciliacion), "99").
            IF W_PerReg EQ W_PerConcil THEN DO:
               FIND Mov_Extracto WHERE Mov_Extracto.Agencia       EQ W_Ofi
                                   AND Mov_Extracto.Cuenta        EQ W_CtaBco 
                                   AND Mov_Extracto.Reg_MovExtrac EQ 
                                      ("EX" + STRING(W_PerConcil) + "00000")
                                 NO-LOCK NO-ERROR.
               ASSIGN W_FecConc = Mov_Extracto.Fecha.
               FIND Mov_Extracto WHERE Mov_Extracto.Agencia       EQ Conciliacion.Agencia
                                   AND Mov_Extracto.Cuenta        EQ Conciliacion.Cuenta
                                   AND Mov_Extracto.Reg_MovExtrac EQ Conciliacion.Reg_MovExtrac NO-LOCK.
               CREATE Tmp-Conciliacion.
               ASSIGN Tmp-Conciliacion.Agencia          = Conciliacion.Agencia
                      Tmp-Conciliacion.Cuenta           = Conciliacion.Cuenta
                      Tmp-Conciliacion.Reg_MovExtrac    = Conciliacion.Reg_MovExtrac
                      Tmp-Conciliacion.Per_Conciliacion = W_FecConc
                      Tmp-Conciliacion.Usuario          = W_Usuario
                      Tmp-Conciliacion.Novedad          = IF Mov_Extracto.Naturaleza = "DB" THEN "NR"  
                                                          ELSE "NG".
               FIND Mov_CtaConta WHERE Mov_CtaConta.Agencia      EQ Conciliacion.Agencia
                                   AND Mov_CtaConta.Cuenta       EQ Conciliacion.Cuenta
                                   AND Mov_CtaConta.Reg_CtaConta EQ Conciliacion.Reg_CtaConta
                                 NO-LOCK NO-ERROR.                          
               ASSIGN Conciliacion.Reg_MovExtrac      = ""
                      Conciliacion.Fecha_Conciliacion = ?
                      Conciliacion.Reg_Cruce          = ""
                      Conciliacion.Novedad            = IF Mov_CtaConta.Naturaleza EQ "DB" THEN "NO" 
                                                        ELSE "NC".
            END.
            ELSE DO:
               FIND Mov_CtaConta WHERE Mov_CtaConta.Agencia      EQ Conciliacion.Agencia
                                   AND Mov_CtaConta.Cuenta       EQ Conciliacion.Cuenta
                                   AND Mov_CtaConta.Reg_CtaConta EQ Conciliacion.Reg_CtaConta
                                 NO-LOCK NO-ERROR.                          
               ASSIGN W_FecConc = DATE(INTEGER(SUBSTRING(Conciliacion.Reg_CtaConta,7,2)), 01,
                                       INTEGER(SUBSTRING(Conciliacion.Reg_CtaConta,3,4))).
               CREATE Tmp-Conciliacion.
               ASSIGN Tmp-Conciliacion.Agencia          = Conciliacion.Agencia
                      Tmp-Conciliacion.Cuenta           = Conciliacion.Cuenta
                      Tmp-Conciliacion.Reg_CtaConta     = Conciliacion.Reg_CtaConta
                      Tmp-Conciliacion.Per_Conciliacion = W_FecConc
                      Tmp-Conciliacion.Usuario          = W_Usuario
                      Tmp-Conciliacion.Novedad          = IF Mov_CtaConta.Naturaleza EQ "DB" THEN "NO" 
                                                          ELSE "NC".
               FIND Mov_Extracto WHERE Mov_Extracto.Agencia       EQ Conciliacion.Agencia
                                   AND Mov_Extracto.Cuenta        EQ Conciliacion.Cuenta
                                   AND Mov_Extracto.Reg_MovExtrac EQ Conciliacion.Reg_MovExtrac NO-LOCK.
            
               ASSIGN Conciliacion.Reg_CtaConta       = ""
                      Conciliacion.Fecha_Conciliacion = ?
                      Conciliacion.Reg_Cruce          = ""
                      Conciliacion.Novedad            = IF Mov_Extracto.Naturaleza = "DB" THEN "NR"  
                                                        ELSE "NG".
            END.
         END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Desmarca_Con W-Win 
PROCEDURE Desmarca_Con :
/*------------------------------------------------------------------------------
  Objetivo:  Desmarca registros que han sido marcados manualmente y tienen novedad
             "CC".
------------------------------------------------------------------------------*/
  FIND FIRST Conciliacion WHERE Conciliacion.Agencia   EQ W_Ofi
                            AND Conciliacion.Cuenta    EQ W_CtaBco 
                            AND Conciliacion.Reg_Cruce EQ F-CruStr 
                          NO-ERROR.       
  IF NOT AVAILABLE Conciliacion THEN DO:
     MESSAGE "Cruce no Se Encuentra en Movimiento Conciliado." 
       VIEW-AS ALERT-BOX ERROR TITLE "Error Conciliacion".
     RETURN.  
  END. 
  REPEAT:
     IF NOT AVAILABLE Conciliacion THEN LEAVE.
     RUN Quita_CC.
     FIND NEXT Conciliacion WHERE Conciliacion.Agencia   EQ W_Ofi
                              AND Conciliacion.Cuenta    EQ W_CtaBco 
                              AND Conciliacion.Reg_Cruce EQ F-CruStr 
                            NO-ERROR.                            
  END.
  {&OPEN-QUERY-Brw-Concil}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Display-ExtManual W-Win 
PROCEDURE Display-ExtManual :
/*------------------------------------------------------------------------------
  Purpose:  Iniciar las variables de entrada manual de extracto
------------------------------------------------------------------------------*/
  IF AVAILABLE Mov_Extracto THEN 
     ASSIGN W_CheExt:SCREEN-VALUE IN FRAME F_Ext = Mov_Extracto.Num_Cheque
            W_SecExt1:SCREEN-VALUE = STRING(Mov_Extracto.Secuencia)
            R_NatExt:SCREEN-VALUE  = Mov_Extracto.Naturaleza
            W_ValExt:SCREEN-VALUE  = STRING(Mov_Extracto.Valor)
            W_DescExt:SCREEN-VALUE = Mov_Extracto.Descripcion
            W_DiaExt:SCREEN-VALUE  = STRING(DAY(Mov_Extracto.Fecha)).
  ELSE
     ASSIGN R_NatExt:SCREEN-VALUE  = "DB"
            W_ValExt:SCREEN-VALUE  = STRING(0)
            W_DescExt:SCREEN-VALUE = ""
            W_DiaExt:SCREEN-VALUE  = STRING(DAY(TODAY)).

  ASSIGN W_CheExt
         W_SecExt1
         R_NatExt
         W_ValExt          
         W_DescExt
         W_DiaExt.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY Cmb_OfiCon W_CtaBco W_NomBco W_FecCorte F-Holgura 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-63 RECT-65 RECT-66 BUTTON-85 W_CtaBco BUTTON-2 W_FecCorte 
         F-Holgura Btn_ConConc Btn_Concilia Btn-Multi Btn-Borrar Btn-Cancel 
         Btn_Ayuda 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  DISPLAY F-Cruce 
      WITH FRAME F-ManCru IN WINDOW W-Win.
  ENABLE F-Cruce 
      WITH FRAME F-ManCru IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-ManCru}
  DISPLAY W_SdoExt R_Tipo 
      WITH FRAME F_Informe IN WINDOW W-Win.
  ENABLE W_SdoExt BUTTON-83 R_Tipo BUTTON-84 
      WITH FRAME F_Informe IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Informe}
  ENABLE B_Formatos Btn_AceFor Btn_CanFor 
      WITH FRAME F_Formato IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Formato}
  ENABLE Brw-Concil Btn-DesManual Btn-Desmar Btn-Salir 
      WITH FRAME F-Concil IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Concil}
  DISPLAY W-Valor 
      WITH FRAME F-Valor IN WINDOW W-Win.
  ENABLE W-Valor 
      WITH FRAME F-Valor IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Valor}
  DISPLAY W-Num 
      WITH FRAME F-Num IN WINDOW W-Win.
  ENABLE W-Num 
      WITH FRAME F-Num IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Num}
  DISPLAY Rs_ActExt W_SecExt1 W_CheExt W_ValExt W_DiaExt R_NatExt W_DescExt 
      WITH FRAME F_Ext IN WINDOW W-Win.
  ENABLE Rs_ActExt W_SecExt1 W_CheExt W_ValExt W_DiaExt R_NatExt W_DescExt 
         RECT-64 
      WITH FRAME F_Ext IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Ext}
  DISPLAY Rs_Borrar W_RegBorra 
      WITH FRAME F_Borrar IN WINDOW W-Win.
  ENABLE RECT-53 Rs_Borrar W_RegBorra Btn_CancBorrar 
      WITH FRAME F_Borrar IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Borrar}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EntreMovtos W-Win 
PROCEDURE EntreMovtos :
/*------------------------------------------------------------------------------
  Objetivo:  Cruza los registros seleccionados manualmente del mvto cuenta
             contra el mvto del extracto.
------------------------------------------------------------------------------*/
  IF  Brw-MovCtaConta:NUM-SELECTED-ROWS IN FRAME F-Main EQ 1
  AND Brw-MovExtrac:NUM-SELECTED-ROWS EQ 1 THEN DO:
      ASSIGN W_Status = Brw-MovCtaConta:FETCH-SELECTED-ROW(1)
             W_Status = Brw-MovExtrac:FETCH-SELECTED-ROW(1).
      IF  Tmp-CtaConta.Naturaleza EQ Tmp-Extracto.Naturaleza 
      AND Tmp-CtaConta.Valor      EQ Tmp-Extracto.Valor THEN DO:
          FIND Conciliacion WHERE Conciliacion.Agencia       EQ Tmp-Extracto.Agencia
                              AND Conciliacion.Cuenta        EQ Tmp-Extracto.Cuenta
                              AND Conciliacion.Reg_MovExtrac EQ Tmp-Extracto.Reg_MovExtrac
                            NO-ERROR.
          IF AVAILABLE Conciliacion THEN DO:
             IF Conciliacion.Reg_CtaConta EQ "" THEN
                DELETE Conciliacion.
             ELSE DO:
                ASSIGN Conciliacion.Reg_MovExtrac = ""
                       Conciliacion.Novedad = IF Tmp-CtaConta.Naturaleza EQ "DB" THEN "NO" 
                                              ELSE "NC".
             END.
          END.

          RUN Busca_ConDeCta.
          ASSIGN Conciliacion.Novedad            = "CC"
                 Conciliacion.Fecha_Conciliacion = W_FecCorte.
          DELETE Tmp-CtaConta.
          DELETE Tmp-Extracto.          
          RETURN.
      END.     
      MESSAGE "Valores Diferentes ó Naturaleza Diferente." SKIP
              "Partidas No se Pueden Conciliar" 
         VIEW-AS ALERT-BOX ERROR TITLE "Error Conciliacion".
      RETURN.
  END.      
  RUN Suma_SelCta.
  RUN Suma_SelExt.  

  ASSIGN W_TotGen = ABS( W_MvDebi - W_MvCred ) - ABS( W_ExDeb - W_ExCre ).
  
  IF W_TotGen NE 0 THEN DO:
     MESSAGE "Valores Diferentes, Partidas No se Pueden Conciliar" 
         VIEW-AS ALERT-BOX ERROR TITLE "Error Conciliacion".
     RETURN.
  END.
  RUN Secuencia.  
  RUN ConcMan_MovCta.
  RUN ConcMan_MovExt.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EntreSiExtrac W-Win 
PROCEDURE EntreSiExtrac :
/*------------------------------------------------------------------------------
  Objetivo:  Cruza los registros marcados manualmente entre si, mvto extracto.
------------------------------------------------------------------------------*/
  RUN Suma_SelExt.  

  ASSIGN W_TotGen =  W_ExDeb - W_ExCre.
  
  IF W_TotGen NE 0 THEN DO:
     MESSAGE "Valores Diferentes, Partidas No se Pueden Conciliar" 
         VIEW-AS ALERT-BOX ERROR TITLE "Error Conciliacion".
     RETURN.
  END.
  RUN Secuencia.  
  RUN ConcMan_MovExt.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EntreSiMovCta W-Win 
PROCEDURE EntreSiMovCta :
/*------------------------------------------------------------------------------
 Objetivo: Cruza los registros seleccionados manualmente entre si, Mvto Cuenta.
------------------------------------------------------------------------------*/
  RUN Suma_SelCta.

  ASSIGN W_TotGen = W_MvDebi - W_MvCred.
  
  IF W_TotGen NE 0 THEN DO:
     MESSAGE "Valores Diferentes, Partidas No se Pueden Conciliar" 
         VIEW-AS ALERT-BOX ERROR TITLE "Error Conciliacion".
     RETURN.
  END.
  RUN Secuencia.  
  RUN ConcMan_MovCta.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba_ExtAut W-Win 
PROCEDURE Graba_ExtAut :
/*------------------------------------------------------------------------------
  Objetivo:  Pasa los cheques de Extracto a la tabla de Mov_Extracto  
------------------------------------------------------------------------------*/
   ASSIGN FRAME F-Main W_CtaBco Cmb_OfiCon.    
   SYSTEM-DIALOG GET-FILE procname
     TITLE      "Escoja el archivo ..."
     FILTERS    "Archivos Texto (*.txt)"   "*.txt",
                "Archivos Datos (*.dat)"   "*.dat",
                "Archivos Otros (*.*)"     "*.*"
     MUST-EXIST
     USE-FILENAME
     UPDATE W_Status.
     IF NOT W_Status THEN RETURN.
     
     RUN VALUE(W_Proceso) (INPUT Procname, W_Dia, W_MesC, W_Ano, W_CtaBco, W_Ofi, W_Usuario,
                           W_FecInic, W_FecCorte) NO-ERROR.   
     IF ERROR-STATUS:ERROR THEN RETURN ERROR.  

     CREATE Mov_Extracto.
     ASSIGN Mov_Extracto.Cuenta        = W_CtaBco
            Mov_Extracto.Descripcion   = "Automatico"
            Mov_Extracto.Fecha         = W_FecCorte
            Mov_Extracto.Naturaleza    = ""
            Mov_Extracto.Num_Cheque    = STRING(W_Ano,"9999") + STRING(W_MesC,"99") + "0000"
            Mov_Extracto.Agencia       = W_Ofi
            Mov_Extracto.Reg_MovExtrac = ("EX" + STRING(W_Ano,"9999") + STRING(W_MesC,"99") + "00000")
            Mov_Extracto.Secuencia     = 0 
            Mov_Extracto.Valor         = 0.
     
     RUN Inicia-Tmp.
     RUN Llena_TmpCta.       
     RUN Llena_TmpExt.            
     ENABLE ALL EXCEPT W_NomBco Btn-Multi Btn_ConConc Button-2 Btn-Cancel WITH FRAME F-Main.  
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba_ExtMan W-Win 
PROCEDURE Graba_ExtMan :
/*------------------------------------------------------------------------------
  Objetivo:     Grabar los datos entrados manualmente para el extracto
------------------------------------------------------------------------------*/
  ASSIGN FRAME F_Ext W_CheExt W_ValExt W_DescExt W_DiaExt R_NatExt.
  RUN Justificar IN W_Manija (INPUT-OUTPUT W_CheExt, INPUT "0", 10, "I").
  IF Rs_ActExt EQ 1 THEN DO:
     FIND LAST Mov_Extracto WHERE Mov_Extracto.Agencia       EQ W_Ofi
                              AND Mov_Extracto.Cuenta        EQ W_CtaBco 
                              AND Mov_Extracto.Reg_MovExtrac GT 
                                 ("EX" + STRING(W_Ano,"9999") + STRING(W_MesC,"99") + "00000")
                              AND Mov_Extracto.Reg_MovExtrac LT 
                                 ("EX" + STRING(W_Ano,"9999") + STRING(W_MesC,"99") + "99999")                               
                         NO-LOCK NO-ERROR.
     IF AVAILABLE Mov_Extracto THEN
        ASSIGN W_SecExt = INTEGER(SUBSTRING(Mov_Extracto.Reg_MovExtrac,9,5)).
     ELSE ASSIGN W_SecExt = 0.   
     IF W_CtaBco = "" 
     OR W_CheExt = "" THEN
        RETURN ERROR.

     ASSIGN W_SecExt = W_SecExt + 1.
     CREATE Mov_Extracto.
     ASSIGN Mov_Extracto.Agencia       = W_Ofi 
            Mov_Extracto.Cuenta        = W_CtaBco
            Mov_Extracto.Num_Cheque    = W_CheExt
            Mov_Extracto.Secuencia     = W_SecExt1
            Mov_Extracto.Fecha         = DATE(W_MesC,W_DiaExt,W_Ano)
            Mov_Extracto.Valor         = W_ValExt
            Mov_Extracto.Descripcion   = W_DescExt
            Mov_Extracto.Naturaleza    = R_NatExt
            Mov_Extracto.Reg_MovExtrac = ("EX" + STRING(W_Ano,"9999") + STRING(W_MesC,"99") +  STRING(W_SecExt,"99999")).

     RUN Crea_TmpExt.
     RUN P-GraLog IN W_Manija 
         (INPUT STRING(W_Usuario) + " " + "ING" + " Conciliacion Manual Extracto").        
  END.      
  ELSE DO:
       FIND Mov_Extracto WHERE Mov_Extracto.Agencia      EQ W_Ofi
                           AND Mov_Extracto.Cuenta       EQ W_CtaBco 
                           AND Mov_Extracto.Num_Cheque   EQ W_CheExt 
                           AND Mov_Extracto.Secuencia    EQ W_SecExt1 
                         NO-ERROR.
       IF AVAILABLE(Mov_Extracto) THEN DO:
          ASSIGN Mov_Extracto.Valor       = W_ValExt
                 Mov_Extracto.Fecha       = DATE(W_MesC, W_DiaExt, W_Ano)
                 Mov_Extracto.Descripcion = W_DescExt
                 Mov_Extracto.Naturaleza  = R_NatExt.

          FIND Tmp-Extracto WHERE Mov_Extracto.Agencia       EQ Tmp-Extracto.Agencia
                              AND Mov_Extracto.Cuenta        EQ Tmp-Extracto.Cuenta
                              AND Mov_Extracto.Reg_MovExtrac EQ Tmp-Extracto.Reg_MovExtrac
                            NO-ERROR.
          IF AVAILABLE Tmp-Extracto THEN
             ASSIGN Tmp-Extracto.Valor       = W_ValExt
                    Tmp-Extracto.Fecha       = DATE(W_MesC,W_DiaExt,W_Ano)            
                    Tmp-Extracto.Descripcion = W_DescExt
                    Tmp-Extracto.Naturaleza  = R_NatExt.
       END.        
       RUN P-GraLog IN W_Manija 
           (INPUT STRING(W_Usuario) + " " + "ACT" + " Conciliacion Manual Extracto").           
  END.
  IF W_GrabaRgCero THEN DO:
     CREATE Mov_Extracto.
     ASSIGN Mov_Extracto.Cuenta        = W_CtaBco
            Mov_Extracto.Descripcion   = "Manual"
            Mov_Extracto.Fecha         = W_FecCorte
            Mov_Extracto.Naturaleza    = ""
            Mov_Extracto.Num_Cheque    = STRING(W_Ano,"9999") + STRING(W_MesC,"99") + "0000"
            Mov_Extracto.Agencia       = W_Ofi
            Mov_Extracto.Reg_MovExtrac = ("EX" + STRING(W_Ano,"9999") + STRING(W_MesC,"99") + "00000")
            Mov_Extracto.Secuencia     = 0 
            Mov_Extracto.Valor         = 0
            W_GrabaRgCero              = FALSE.
  END.
  RUN Display-ExtManual.
  APPLY "VALUE-CHANGED" TO Rs_ActExt IN FRAME F_Ext.
  RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba_MovCta W-Win 
PROCEDURE Graba_MovCta :
/*------------------------------------------------------------------------------
  Objetivo : Carga la Tabla de Mov_CtaConta con los Registros de Mov_Contable,
             y Che_Transito.
------------------------------------------------------------------------------*/
/*   DISABLE TRIGGERS FOR LOAD OF Mov_Contable. */
  RUN Inicia-Tmp.

  /* Pasa de Mov_Contable */

  SESSION:SET-WAIT-STATE("GENERAL").
  ASSIGN W_SecMov1 = 0.
  FIND LAST Mov_CtaConta WHERE Mov_CtaConta.Agencia       EQ W_Ofi
                           AND Mov_CtaConta.Cuenta        EQ W_CtaBco 
                           AND Mov_CtaConta.Reg_CtaConta  GT 
                              ("MV" + STRING(W_Ano,"9999") + STRING(W_MesC,"99") + "00000")
                           AND Mov_CtaConta.Reg_CtaConta LT 
                              ("MV" + STRING(W_Ano,"9999") + STRING(W_MesC,"99") + "99999")
                         NO-LOCK NO-ERROR.
  IF AVAILABLE Mov_CtaConta THEN
     ASSIGN W_SecMov1 = INTEGER(SUBSTRING(Mov_CtaConta.Reg_CtaConta,9,5)).

  FOR EACH Mov_Contable FIELDS (Cuenta Doc_Referencia Fec_Contable Agencia 
                               db cr Comprobante Comentario Conciliado)
                        WHERE Mov_Contable.Agencia        EQ W_Ofi                    
                          AND Mov_Contable.Cuenta         EQ W_CtaBco  
                          AND Mov_Contable.Fec_Contable   LE W_FecCorte            
                          AND Mov_Contable.Fec_Contable   GE W_FecInic
                          AND NOT Mov_Contable.Conciliado:
    
      FIND FIRST Comprobantes WHERE Comprobantes.Agencia     EQ W_Ofi
                                AND Comprobantes.Comprobante EQ Mov_Contable.Comprobante 
                              NO-LOCK NO-ERROR.  
      IF NOT AVAILABLE Comprobantes 
      OR Comprobantes.Id_Consecutivo EQ 1 THEN NEXT.   

      ASSIGN W_CheMov  = Mov_Contable.Doc_Referencia          
             W_SecMov  = 1
             W_SecMov1 = W_SecMov1 + 1. 

      RUN Justificar IN W_Manija (INPUT-OUTPUT W_CheMov, INPUT "0", 10, "I").

      FIND FIRST Mov_CtaConta WHERE Mov_CtaConta.Agencia    EQ W_Ofi
                                AND Mov_CtaConta.Cuenta     EQ W_CtaBco
                                AND Mov_CtaConta.Num_Cheque EQ W_CheMov 
                                AND Mov_CtaConta.Secuencia  EQ W_SecMov    
                              NO-ERROR.       
      REPEAT:                              
         IF NOT AVAILABLE(Mov_CtaConta) THEN LEAVE.
         ASSIGN W_SecMov = W_SecMov + 1.   
         FIND NEXT Mov_CtaConta WHERE Mov_CtaConta.Agencia    EQ W_Ofi
                                  AND Mov_CtaConta.Cuenta     EQ W_CtaBco
                                  AND Mov_CtaConta.Num_Cheque EQ W_CheMov                
                                  AND Mov_CtaConta.Secuencia  EQ W_SecMov    
                                NO-ERROR.       
      END.                          
      CREATE Mov_CtaConta.
      ASSIGN Mov_CtaConta.Cuenta       = Mov_Contable.Cuenta
             Mov_CtaConta.Num_Cheque   = W_CheMov
             Mov_CtaConta.Secuencia    = W_SecMov
             Mov_CtaConta.Fecha        = Mov_Contable.Fec_Contable
             Mov_CtaConta.Agencia      = W_Ofi
             Mov_CtaConta.Reg_CtaConta = "MV" + STRING(W_Ano,"9999") + STRING(W_MesC,"99") + STRING(W_SecMov1,"99999")
             Mov_Contable.Conciliado   = TRUE.
      IF Mov_contable.Db <> 0 THEN 
         ASSIGN Mov_CtaConta.Naturaleza   = "DB"
                Mov_CtaConta.valor   = mov_contable.db.
      IF Mov_contable.Cr <> 0 THEN         
         ASSIGN Mov_CtaConta.Naturaleza   = "CR"
                Mov_CtaConta.valor   = mov_contable.Cr.
  END.
 /* Pasa de Che_transito */  
 
 FOR EACH Che_Transito WHERE Che_Transito.Agencia    EQ W_Ofi         
                         AND Che_Transito.Num_Cuenta EQ W_CtaBco      
                         AND Che_Transito.Estado     EQ 4             
                         AND Che_Transito.Fec_Canje  LE W_FecCorte    
                         AND Che_Transito.Fec_Canje  GE W_FecInic    
                         AND NOT Che_Transito.Conciliado:

     ASSIGN W_CheMov  = Che_Transito.Cheque
            W_SecMov  = 1 
            W_SecMov1 = W_SecMov1 + 1. 

     RUN Justificar IN W_Manija (INPUT-OUTPUT W_CheMov, INPUT "0", 10, "I").
     FIND FIRST Mov_CtaConta WHERE Mov_CtaConta.Agencia    EQ W_Ofi
                               AND Mov_CtaConta.Cuenta     EQ W_CtaBco
                               AND Mov_CtaConta.Num_Cheque EQ W_CheMov 
                               AND Mov_CtaConta.Secuencia  EQ W_SecMov    
                             NO-ERROR.       
     REPEAT:                              
        IF NOT AVAILABLE(Mov_CtaConta) THEN LEAVE.
        ASSIGN W_SecMov = W_SecMov + 1.   
        FIND NEXT Mov_CtaConta WHERE Mov_CtaConta.Agencia    EQ W_Ofi
                                 AND Mov_CtaConta.Cuenta     EQ W_CtaBco
                                 AND Mov_CtaConta.Num_Cheque EQ W_CheMov                
                                 AND Mov_CtaConta.Secuencia  EQ W_SecMov    
                               NO-ERROR.       
     END.                                       
     CREATE Mov_CtaConta.
     ASSIGN Mov_CtaConta.Cuenta       = W_CtaBco
            Mov_CtaConta.Agencia      = W_Ofi 
            Mov_CtaConta.Num_Cheque   = W_CheMov
            Mov_CtaConta.Secuencia    = W_SecMov
            Mov_CtaConta.Reg_CtaConta = "MV" + STRING(W_Ano,"9999") + STRING(W_MesC,"99") + STRING(W_SecMov1,"99999")
            Mov_CtaConta.Fecha        = Che_Transito.Fec_Canje
            Mov_CtaConta.Naturaleza   = "CR"
            Mov_CtaConta.Descripcion  = STRING("ChTran " + STRING(Che_Transito.Cod_Producto) + Che_Transito.Tip_Producto + Che_Transito.Num_Cuenta)
            Mov_CtaConta.Valor        = Che_Transito.Valor
            Che_Transito.Conciliado   = TRUE. 
  END.  
  ENABLE ALL EXCEPT W_NomBco WITH FRAME F-Main.   
  SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallarTitul W-Win 
PROCEDURE HallarTitul :
/*------------------------------------------------------------------------------
 Objetivo:    Título para cada informe.
 ------------------------------------------------------------------------------*/ 
  CASE R_Tipo:
     WHEN "TO" THEN
          ASSIGN W_Titul = "General E Inconsistencias ". 
     WHEN "CC" THEN
          ASSIGN W_Titul  = "Información Conciliada "
                 W_Titul2 = "TOTAL MVTO CONCILIADO:".
     WHEN "NC" THEN
          ASSIGN W_Titul = "Pendientes de Cobro "
                 W_Titul2 = "TOTAL NO COBRADOS:". 
     WHEN "NG" THEN
          ASSIGN W_Titul = "Cobrados Banco, Pero No-Girados"
                 W_Titul2 = "TOTAL NO GIRADOS:". 
     WHEN "VD" THEN
          ASSIGN W_Titul = "Valores Diferentes con Extracto"
                 W_Titul2 = "TOTAL VALORES DIFERENTES:". 
     WHEN "IN" THEN
          ASSIGN W_Titul = "Inconsistencias (Dif.Naturaleza)"
                 W_Titul2 = "TOTAL INCONSISTENCIAS:". 
     WHEN "NO" THEN
          ASSIGN W_Titul = "Consignaciones Pdtes.Agencia-Extracto"
                 W_Titul2 = "TOTAL NO CONSIGNADOS:". 
     WHEN "NR" THEN
          ASSIGN W_Titul = "Registros Extracto No Encontrados en Contab."
                 W_Titul2 = "TOTAL REGISTROS NO ENCONTRADOS:". 
     WHEN "NF" THEN
          ASSIGN W_Titul = "Novedad Días de Holgura."
                 W_Titul2 = "TOTAL NOVEDAD DIAS HOLGURA:". 
     WHEN "RE" THEN
          W_Titul = "Resumen y Totales de Conciliación    ". 
  END CASE.          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir W-Win 
PROCEDURE Imprimir :
/*------------------------------------------------------------------------------
  Objetivo:   Genera informe de las novedades NC, NO, NG y NR 
------------------------------------------------------------------------------*/
  DEFINE VARIABLE W_TotalA  AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotalB  AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_ImpReg  AS LOGICAL INITIAL NO.
  DEFINE VARIABLE Procname  AS CHARACTER FORMAT "X(59)" INITIAL "temporal.txt".  
 
  RUN P-Dispos IN W_Manija (INPUT-OUTPUT Procname,INPUT-OUTPUT Op_Salida).      
  IF Op_Salida = "" THEN RETURN.  
  OUTPUT TO VALUE(Procname) PAGE-SIZE 81.
  ASSIGN W_Banco = W_NomBco:SCREEN-VALUE IN FRAME F-Main.                            
  RUN HallarTitul.
  SESSION:SET-WAIT-STATE("GENERAL").  
  DEFINE FRAME F-MovCta
    HEADER
      "PAGINA:"                                       AT 100 PAGE-NUMBER FORMAT ">>>9"
      W_Nom_Entidad                                   AT 47  FORMAT "X(40)"
      W_Titul                                         AT  2
      "INFORME DE CONCILIACION BANCARIA"              AT 47  SKIP (1)
      "BANCO:"                                        AT 02
       W_Banco                                        AT 10  NO-LABELS FORMAT "X(30)"
      "MES:"                                          AT 70
      W_MesC                                          AT 75
      "/"                                             AT 79 
      W_Ano                                           AT 81 SKIP (1)
     "No. Docto.  S/cia   Nv.  Fec.Contabil        Vlr.Contabilidad    Descripción Contabilidad          "  AT 1 
     "----------  -----   --   ------------  ----------------------    ----------------------------------"  AT 1
      WITH WIDTH 150 USE-UNDERLINE USE-TEXT PAGE-TOP FRAME F-MovCta.

  DEFINE FRAME F-Extracto
    HEADER
      "PAGINA:"                                       AT 100 PAGE-NUMBER FORMAT ">>>9"
      W_Nom_Entidad                                   AT 47  FORMAT "X(40)"
      W_Titul                                         AT  2
      "INFORME DE CONCILIACION BANCARIA"              AT 47  SKIP (1)
      "BANCO:"                                        AT 02
       W_Banco                                        AT 10  NO-LABELS FORMAT "X(30)"
      "MES:"                                          AT 70
      W_MesC                                          AT 75
      "/"                                             AT 79 
      W_Ano                                           AT 81 SKIP (1)
     "No. Docto.  S/cia   Nv.  Fec.Extracto          Valor Extracto    Descripción Extracto              "  AT 1 
     "----------  -----   --   ------------  ----------------------    ----------------------------------"  AT 1
   WITH WIDTH 150 USE-UNDERLINE USE-TEXT PAGE-TOP FRAME F-Extracto.

  DEFINE FRAME F-PiePagina
    HEADER 
      "FECHA:"       AT 2 
      TODAY          AT 10 FORMAT "99/99/9999"
      W_Nom_Agencia  AT 40 FONT 9
      "HORA:"        AT 110 STRING(TIME,"HH:MM:SS AM")
      WITH WIDTH 150 FRAME F-Piepagina PAGE-BOTTOM USE-TEXT STREAM-IO. 
  
  
  IF R_Tipo EQ "NG"
  OR R_Tipo EQ "NR" THEN
     VIEW FRAME F-Extracto.
  ELSE DO:  
       IF R_Tipo EQ "NO"
       OR R_Tipo EQ "NC" THEN 
          VIEW FRAME F-MovCta.          
  END.        
  VIEW FRAME F-PiePagina.
  ASSIGN W_ImpReg = NO.
  FOR EACH Conciliacion WHERE Conciliacion.Agencia            EQ W_Ofi
                          AND Conciliacion.Cuenta             EQ W_CtaBco
                          AND Conciliacion.Per_Conciliacion   LE W_FecCorte
                          AND Conciliacion.Novedad            EQ R_Tipo
                        NO-LOCK 
                        BREAK BY Conciliacion.Agencia
                              BY Conciliacion.Cuenta
                              BY Conciliacion.Per_Conciliacion
                              BY Conciliacion.Novedad:   

      FIND Mov_CtaConta WHERE Mov_CtaConta.Agencia       EQ W_Ofi
                          AND Mov_CtaConta.Cuenta        EQ W_CtaBco
                          AND Mov_CtaConta.Reg_CtaConta  EQ Conciliacion.Reg_CtaConta 
                        NO-LOCK NO-ERROR.  

      FIND Mov_Extracto WHERE Mov_Extracto.Agencia       EQ W_Ofi
                          AND Mov_Extracto.Cuenta        EQ W_CtaBco
                          AND Mov_Extracto.Reg_MovExtrac EQ Conciliacion.Reg_MovExtrac 
                        NO-LOCK NO-ERROR.
 
      IF AVAILABLE Mov_CtaConta THEN DO:
         DISPLAY Mov_CtaConta.Num_Cheque  AT  1
                 Mov_CtaConta.Secuencia   AT 13
                 Conciliacion.Novedad     AT 21
                 Mov_CtaConta.Fecha       AT 26
                 Mov_CtaConta.Valor       AT 40 FORMAT "->>>>,>>>,>>>,>>9.99"
                 Mov_CtaConta.Naturaleza  AT 60
                 Mov_CtaConta.Descripcion AT 66
            WITH FRAME F1 WIDTH 150 NO-LABELS NO-BOX STREAM-IO.
         ASSIGN W_TotalA = W_TotalA + Mov_CtaConta.Valor.
      END.     
      ELSE DO:
         IF AVAILABLE Mov_Extracto THEN DO:
            DISPLAY Mov_Extracto.Num_Cheque     AT  1
                    Mov_Extracto.Secuencia      AT 13
                    Conciliacion.Novedad        AT 21
                    Mov_Extracto.Fecha          AT 26
                    Mov_Extracto.Valor          AT 40 FORMAT "->>>>,>>>,>>>,>>9.99"
                    Mov_Extracto.Naturaleza     AT 60
                    Mov_Extracto.Descripcion    AT 66
              WITH FRAME F2 WIDTH 150 NO-LABELS NO-BOX STREAM-IO.
            ASSIGN W_TotalB = W_TotalB + Mov_Extracto.Valor.
         END.
      END.
      IF LAST-OF(Conciliacion.Novedad) THEN DO:
         ASSIGN W_ImpReg = YES.
         IF R_Tipo EQ "NG"
         OR R_Tipo EQ "NR" THEN
            DISPLAY   "--------------------"      AT 40
                      W_Titul2                    AT 1
                      W_TotalB                    AT 40  FORMAT "->>>>,>>>,>>>,>>9.99"
              WITH WIDTH 150 NO-LABELS FRAME CHECONC1 NO-BOX STREAM-IO.
         ELSE DO:  
             IF R_Tipo EQ "NO"
             OR R_Tipo EQ "NC" THEN DO:
                DISPLAY   "--------------------"      AT 40
                          W_Titul2                    AT 1
                          W_TotalA                    AT 40 FORMAT "->>>>,>>>,>>>,>>9.99"
                  WITH WIDTH 150 NO-LABELS FRAME CHECONC NO-BOX STREAM-IO.
             END.   
         END.           
      END.                    
 END. 
 IF NOT W_ImpReg THEN
    DISPLAY "Sin Registros" WITH FRAME W_SinReg NO-LABELS NO-BOX.                   
 PAGE.
 OUTPUT CLOSE.   
 SESSION:SET-WAIT-STATE("").         
 CASE op_salida:
      WHEN "P" THEN
            RUN Pantalla IN W_Manija (INPUT procname). 
      WHEN "I" THEN
            RUN adecomm/_osprint.r (INPUT ?,INPUT procname,INPUT 2,INPUT 1,INPUT 0,
                                   INPUT 0,OUTPUT W_Rpta).
 END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImprimirCC W-Win 
PROCEDURE ImprimirCC :
/*------------------------------------------------------------------------------
  Objetivo: Genera informe de las novedades CC, VD y NF     
------------------------------------------------------------------------------*/
  DEFINE VARIABLE W_TotalCta  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotalExt  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotDb     AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotCr     AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotDbEx   AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotCrEx   AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_ImpReg    AS LOGICAL INITIAL NO.
  DEFINE VARIABLE Procnamenc  AS CHARACTER FORMAT "X(59)" INITIAL "tempor.txt".  
 
  RUN P-Dispos IN W_Manija (INPUT-OUTPUT Procnamenc,INPUT-OUTPUT Op_Salida).      
  IF Op_Salida = "" THEN RETURN.  
  OUTPUT TO VALUE(Procnamenc) PAGE-SIZE 81.
  ASSIGN W_Banco = W_NomBco:SCREEN-VALUE IN FRAME F-Main.                            
  RUN HallarTitul.
  SESSION:SET-WAIT-STATE("GENERAL").  
  DEFINE FRAME F-Encabezado
    HEADER
      "PAGINA:"                                       AT 100 PAGE-NUMBER FORMAT ">>>9"
      W_Nom_Entidad                                   AT 47  FORMAT "X(40)"
      W_Titul                                         AT  2
      "INFORME DE CONCILIACION BANCARIA"              AT 47  SKIP (1)
      "BANCO:"                                        AT 02
       W_Banco                                        AT 10  NO-LABELS FORMAT "X(30)"
      "MES:"                                          AT 70
      W_MesC                                          AT 75
      "/"                                             AT 79 
      W_Ano                                           AT 81 SKIP (1)
     "No. Docto.  Sec. Fec.Contab     Valor.Contabilidad  D.Contabilidad    #Cruce Fec.Extracto No. Docto.  Sec.         Valor Extracto D. Extracto    "  AT 1 
     "---------- ----- ---------- ----------------------  --------------- -------- ------------ ---------- ----- ---------------------- ---------------"  AT 1
    WITH WIDTH 150 USE-UNDERLINE USE-TEXT PAGE-TOP FRAME F-Encabezado.
  DEFINE FRAME F-PiePagina
    HEADER 
      "FECHA:"       AT 2 
      TODAY          AT 10 FORMAT "99/99/9999"
      W_Nom_Agencia  AT 40 FONT 9
      "HORA:"        AT 110 STRING(TIME,"HH:MM:SS AM")
      WITH WIDTH 150 FRAME F-Piepagina PAGE-BOTTOM USE-TEXT STREAM-IO. 
  
  VIEW FRAME F-Encabezado.          
  VIEW FRAME F-PiePagina.
  
  ASSIGN W_ImpReg = NO.
  FOR EACH Conciliacion WHERE Conciliacion.Agencia            EQ W_Ofi
                          AND Conciliacion.Cuenta             EQ W_CtaBco
                          AND ( IF R_Tipo NE "CC" THEN
                                  (    Conciliacion.Per_Conciliacion  LE W_FecCorte
                                   AND Conciliacion.Novedad       EQ R_Tipo)
                                   ELSE
                                  (    Conciliacion.Fecha_Conciliacion GE W_FecInic
                                   AND Conciliacion.Fecha_Conciliacion LE W_FecCorte))
                        NO-LOCK 
                        BREAK BY Conciliacion.Agencia
                              BY Conciliacion.Cuenta
                              BY Conciliacion.Per_Conciliacion
                              BY Conciliacion.Novedad:   

      FIND Mov_CtaConta WHERE Mov_CtaConta.Agencia       EQ W_Ofi
                          AND Mov_CtaConta.Cuenta        EQ W_CtaBco
                          AND Mov_CtaConta.Reg_CtaConta  EQ Conciliacion.Reg_CtaConta 
                        NO-LOCK NO-ERROR.  

      FIND Mov_Extracto WHERE Mov_Extracto.Agencia       EQ W_Ofi
                          AND Mov_Extracto.Cuenta        EQ W_CtaBco
                          AND Mov_Extracto.Reg_MovExtrac EQ Conciliacion.Reg_MovExtrac 
                        NO-LOCK NO-ERROR.

      ASSIGN W_Cruce = STRING(SUBSTRING(Conciliacion.Reg_Cruce,1,2) + SUBSTRING(Conciliacion.Reg_Cruce,9,5)).

      IF  AVAILABLE Mov_CtaConta
      AND AVAILABLE Mov_Extracto THEN DO:
          DISPLAY Mov_CtaConta.Num_Cheque       AT   1
                  Mov_CtaConta.Secuencia        AT  12
                  Mov_CtaConta.Fecha            AT  18
                  Mov_CtaConta.Valor            AT  29 FORMAT "->>>>,>>>,>>>,>>9.99"
                  Mov_CtaConta.Naturaleza       AT  49  
                  Mov_CtaConta.Descripcion      AT  53 FORMAT "X(15)" 
                  Mov_Extracto.Fecha            AT  78
                  Mov_Extracto.Num_Cheque       AT  91
                  Mov_Extracto.Secuencia        AT 102
                  Mov_Extracto.Valor            AT 108 FORMAT "->>>>,>>>,>>>,>>9.99"
                  Mov_Extracto.Naturaleza       AT 128
                  Mov_Extracto.Descripcion      AT 131 FORMAT "X(15)"
             WITH FRAME F1 WIDTH 150 STREAM-IO NO-LABELS NO-BOX.
          IF Mov_CtaConta.Naturaleza EQ "DB" THEN
             ASSIGN W_TotDb   = W_TotDb + Mov_CtaConta.Valor
                    W_TotDbEx = W_TotDbEx + Mov_Extracto.Valor.
          ELSE
             ASSIGN W_TotCr   = W_TotCr + Mov_CtaConta.Valor
                    W_TotCrEx = W_TotCrEx + Mov_Extracto.Valor.
      END.     
      ELSE DO:
         IF AVAILABLE Mov_CtaConta THEN DO:
            DISPLAY Mov_CtaConta.Num_Cheque       AT   1
                    Mov_CtaConta.Secuencia        AT  12
                    Mov_CtaConta.Fecha            AT  18
                    Mov_CtaConta.Valor            AT  29 FORMAT "->>>>,>>>,>>>,>>9.99"
                    Mov_CtaConta.Naturaleza       AT  49  
                    Mov_CtaConta.Descripcion      AT  53 FORMAT "X(15)" 
                    W_Cruce                       AT  69 SKIP
              WITH FRAME F2 WIDTH 150 STREAM-IO NO-LABELS NO-BOX.
            IF Mov_CtaConta.Naturaleza EQ "DB" THEN
               ASSIGN W_TotDb = W_TotDb + Mov_CtaConta.Valor.
            ELSE ASSIGN W_TotCr = W_TotCr + Mov_CtaConta.valor.
         END.
         ELSE DO:
            IF AVAILABLE Mov_Extracto THEN DO:
               DISPLAY W_Cruce                       AT  69
                       Mov_Extracto.Fecha            AT  78
                       Mov_Extracto.Num_Cheque       AT  91
                       Mov_Extracto.Secuencia        AT 102
                       Mov_Extracto.Valor            AT 108 FORMAT "->>>>,>>>,>>>,>>9.99"  
                       Mov_Extracto.Naturaleza       AT 128
                       Mov_Extracto.Descripcion      AT 131 FORMAT "X(15)" SKIP
                 WITH FRAME F3 WIDTH 150 STREAM-IO NO-LABELS NO-BOX.
               IF Mov_Extracto.Naturaleza EQ "DB" THEN    
                  ASSIGN W_TotDbEx = W_TotDbEx + Mov_Extracto.Valor.
               ELSE ASSIGN W_TotCrEx = W_TotCrEx + Mov_Extracto.Valor.
            END.
         END.
      END.    
      IF LAST-OF(Conciliacion.Novedad) THEN DO: 
         ASSIGN W_TotalCta = (W_TotDb - W_TotCr)
                W_TotalExt = (W_TotDbEx - W_TotCrEx)
                W_ImpReg   = YES.                
         DISPLAY "--------------------" AT  29
                 "--------------------" AT 108
                 "TOTAL DEBITOS"        AT   1
                 W_TotDb                AT  29 FORMAT "->>>>,>>>,>>>,>>9.99"
                 W_TotDbEx              AT 108 FORMAT "->>>>,>>>,>>>,>>9.99"
                 "TOTAL CREDITOS"       AT   1
                 W_TotCr                AT  29 FORMAT "->>>>,>>>,>>>,>>9.99"
                 W_TotCrEx              AT 108 FORMAT "->>>>,>>>,>>>,>>9.99"    
                 W_Titul2               AT   1
                 W_TotalCta             AT  29 FORMAT "->>>>,>>>,>>>,>>9.99"
                 W_TotalExt             AT 108 FORMAT "->>>>,>>>,>>>,>>9.99"
         WITH WIDTH 150 NO-LABELS FRAME CHECONC NO-BOX.
      END.          
  END.
  IF NOT W_ImpReg THEN
     DISPLAY "Sin Registros" WITH FRAME W_SinReg NO-LABELS NO-BOX.
  PAGE.
  OUTPUT CLOSE.   
  SESSION:SET-WAIT-STATE("").         
  CASE op_salida:
       WHEN "P" THEN
             RUN Pantalla IN W_Manija (INPUT procnamenc). 
       WHEN "I" THEN
             RUN adecomm/_osprint.r (INPUT ?,INPUT procnamenc,INPUT 2,INPUT 1,INPUT 0,
                                    INPUT 0,OUTPUT W_Rpta).
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImprimirTot W-Win 
PROCEDURE ImprimirTot :
/*------------------------------------------------------------------------------
  Objetivo:  Imprime informe total detallado y Resumido.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE W_SdoCtaDeb LIKE Sal_Cuenta.Sal_inicial .
  DEFINE VARIABLE W_SdoCtaCre LIKE Sal_Cuenta.Sal_Inicial.
  DEFINE VARIABLE W_SdoCta    LIKE Sal_Cuenta.sal_Inicial  .
  DEFINE VARIABLE W_TotalCta    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotalExt    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotDbCtaCC  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotCrCtaCC  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotDbCtaVD  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotCrCtaVD  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotDbCtaNF  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotCrCtaNF  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotDbExtCC  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotCrExtCC  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotDbExtVD  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotCrExtVD  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotDbExtNF  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_TotCrExtNF  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_SubTotCC    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_SubTotExtCC AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_SubTotVD    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_SubTotExtVD AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_SubTotNF    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_SubTotExtNF AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_SubTotNC    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_SubTotNO    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_SubTotNG    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_SubTotNR    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_SdoTotExt   AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_Difer       AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" INITIAL 0.
  DEFINE VARIABLE W_ImpReg      AS LOGICAL INITIAL NO.
  DEFINE VARIABLE Procnamen     AS CHARACTER FORMAT "X(59)" INITIAL "tempo.txt".  
 
  RUN P-Dispos IN W_Manija (INPUT-OUTPUT Procnamen,INPUT-OUTPUT Op_Salida).      
  IF Op_Salida = "" THEN RETURN.  
  OUTPUT TO VALUE(Procnamen) PAGE-SIZE 81.
  RUN HallarSaldo IN W_Manija (INPUT W_Ofi,W_Ofi,1,999,W_CtaBco,W_Ano,W_MesC,
                               OUTPUT W_SdoCtaDeb, OUTPUT W_SdoCtaCre, OUTPUT W_SdoCta).
  ASSIGN W_Banco = W_NomBco:SCREEN-VALUE IN FRAME F-Main.                            
  RUN HallarTitul.
  SESSION:SET-WAIT-STATE("GENERAL").  
  DEFINE FRAME F-Titulo
    HEADER
      "PAGINA:"                                          AT 100 PAGE-NUMBER FORMAT ">>>9"
      W_Nom_Entidad                                      AT 47  FORMAT "X(40)"
      W_Titul   AT 02 "INFORME DE CONCILIACION BANCARIA" AT 47  SKIP (1)
      "BANCO:"  AT 02  W_Banco AT 10  NO-LABELS FORMAT "X(30)" "MES:" AT 70
      W_MesC    AT 75  "/"     AT 79  W_Ano                           AT 81 SKIP (2)
  WITH WIDTH 150 USE-UNDERLINE USE-TEXT PAGE-TOP FRAME F-Titulo.

  DEFINE FRAME F-Encabezado
    HEADER
      "PAGINA:"                                          AT 100 PAGE-NUMBER FORMAT ">>>9"
      W_Nom_Entidad                                      AT 47  FORMAT "X(40)"
      W_Titul  AT  2 "INFORME DE CONCILIACION BANCARIA"  AT 47  SKIP (1)
      "BANCO:" AT 02  W_Banco  AT 10  NO-LABELS FORMAT "X(30)" "MES:" AT 70
      W_MesC   AT 75 "/"       AT 79  W_Ano  AT 81 SKIP (1)
     "No. Docto.  Sec. Nv Fec.Contab     Valor.Contabilidad D.Contabilidad   #Cruce Fec.Extracto No. Docto.  Sec.         Valor Extracto D. Extracto   "  AT 1 
     "---------- ----- -- ---------- ---------------------- -------------- -------- ------------ ---------- ----- ---------------------- --------------"  AT 1
  WITH WIDTH 150 USE-UNDERLINE USE-TEXT PAGE-TOP FRAME F-Encabezado.

  DEFINE FRAME F-PiePagina
    HEADER 
      "FECHA:"       AT 2 TODAY          AT 10 FORMAT "99/99/9999"
      W_Nom_Agencia  AT 40 FONT 9       "HORA:"        AT 110 STRING(TIME,"HH:MM:SS AM")
  WITH WIDTH 150 FRAME F-Piepagina PAGE-BOTTOM USE-TEXT STREAM-IO. 

  IF R_Tipo EQ "RE" THEN 
     VIEW FRAME F-Titulo.
  ELSE VIEW FRAME F-Encabezado.

  VIEW FRAME F-Piepagina.
  ASSIGN W_ImpReg = NO.
  FOR EACH Conciliacion WHERE (    Conciliacion.Agencia                 EQ W_Ofi
                               AND Conciliacion.Cuenta                  EQ W_CtaBco
                               AND Conciliacion.Per_Conciliacion        LE W_FecCorte  )
                          AND (    (    Conciliacion.Novedad            NE ""
                                    AND Conciliacion.Novedad            NE "CC" )
                               OR (     Conciliacion.Fecha_Conciliacion GE W_FecInic
                                    AND Conciliacion.Fecha_Conciliacion LE W_FecCorte )
                               OR (     Conciliacion.Per_Conciliacion   GE W_FecInic
                                    AND Conciliacion.Novedad            EQ "CC" ))  
                        NO-LOCK
                        BREAK BY Conciliacion.Novedad:
      FIND Mov_CtaConta WHERE Mov_CtaConta.Agencia       EQ W_Ofi
                          AND Mov_CtaConta.Cuenta        EQ W_CtaBco
                          AND Mov_CtaConta.Reg_CtaConta  EQ Conciliacion.Reg_CtaConta 
                        NO-LOCK NO-ERROR.  

      FIND Mov_Extracto WHERE Mov_Extracto.Agencia       EQ W_Ofi
                          AND Mov_Extracto.Cuenta        EQ W_CtaBco
                          AND Mov_Extracto.Reg_MovExtrac EQ Conciliacion.Reg_MovExtrac 
                        NO-LOCK NO-ERROR.

      ASSIGN W_Cruce = STRING(SUBSTRING(Conciliacion.Reg_Cruce,1,2) + SUBSTRING(Conciliacion.Reg_Cruce,9,5)).
                        
      IF  AVAILABLE Mov_CtaConta
      AND AVAILABLE Mov_Extracto THEN DO:
          IF R_Tipo NE "RE" THEN DO:
             DISPLAY Mov_CtaConta.Num_Cheque  AT   1 Mov_CtaConta.Secuencia   AT  12  Conciliacion.Novedad          AT  18
                     Mov_CtaConta.Fecha       AT  21 Mov_CtaConta.Valor       AT  32 FORMAT "->>>>,>>>,>>>,>>9.99"
                     Mov_CtaConta.Naturaleza  AT  52 Mov_CtaConta.Descripcion AT  55 FORMAT "X(14)" 
                     Mov_Extracto.Fecha       AT  79 Mov_Extracto.Num_Cheque  AT  92
                     Mov_Extracto.Secuencia   AT 103 Mov_Extracto.Valor       AT 109 FORMAT "->>>>,>>>,>>>,>>9.99"                     Mov_Extracto.Naturaleza       AT 129
                     Mov_Extracto.Naturaleza  AT 129 Mov_Extracto.Descripcion AT 132 FORMAT "X(14)"
               WITH FRAME F1 WIDTH 150 STREAM-IO NO-LABELS NO-BOX.          
          END.   
          IF Mov_CtaConta.Naturaleza EQ "DB" THEN DO:
             IF Conciliacion.Novedad EQ "CC" THEN ASSIGN W_TotDbCtaCC = W_TotDbCtaCC + Mov_CtaConta.Valor
                                                         W_TotDbExtCC = W_TotDbExtCC + Mov_Extracto.Valor.
             IF Conciliacion.Novedad EQ "VD" THEN ASSIGN W_TotDbCtaVD = W_TotDbCtaVD + Mov_CtaConta.Valor
                                                         W_TotDbExtVD = W_TotDbExtVD + Mov_Extracto.Valor.
             IF Conciliacion.Novedad EQ "NF" THEN ASSIGN W_TotDbCtaNF = W_TotDbCtaNF + Mov_CtaConta.Valor
                                                         W_TotDbExtNF = W_TotDbExtNF + Mov_Extracto.Valor.
          END.
          ELSE DO:                                               
               IF Conciliacion.Novedad EQ "CC" THEN ASSIGN W_TotCrCtaCC = W_TotCrCtaCC + Mov_CtaConta.Valor
                                                           W_TotCrExtCC = W_TotCrExtCC + Mov_Extracto.Valor.
               IF Conciliacion.Novedad EQ "VD" THEN ASSIGN W_TotCrCtaVD = W_TotCrCtaVD + Mov_CtaConta.Valor
                                                           W_TotCrExtVD = W_TotCrExtVD + Mov_Extracto.Valor.
               IF Conciliacion.Novedad EQ "NF" THEN ASSIGN W_TotCrCtaNF = W_TotCrCtaNF + Mov_CtaConta.Valor
                                                           W_TotCrExtNF = W_TotCrExtNF + Mov_Extracto.Valor.
          END.                                                 
      END.      
      ELSE DO:
         IF AVAILABLE Mov_CtaConta THEN DO:
            IF R_Tipo NE "RE" THEN DO:
               DISPLAY Mov_CtaConta.Num_Cheque       AT   1 Mov_CtaConta.Secuencia        AT  12
                       Conciliacion.Novedad          AT  18 Mov_CtaConta.Fecha            AT  21
                       Mov_CtaConta.Valor            AT  32 FORMAT "->>>>,>>>,>>>,>>9.99"
                       Mov_CtaConta.Naturaleza       AT  52 Mov_CtaConta.Descripcion      AT  55 FORMAT "X(14)" 
                       W_Cruce                       AT  70 SKIP                      
                 WITH FRAME F2 WIDTH 150 STREAM-IO NO-LABELS NO-BOX.
            END.
            IF Mov_CtaConta.Naturaleza EQ "DB" THEN DO:
               IF Conciliacion.Novedad EQ "CC" THEN ASSIGN W_TotDbCtaCC = W_TotDbCtaCC + Mov_CtaConta.Valor.
               IF Conciliacion.Novedad EQ "VD" THEN ASSIGN W_TotDbCtaVD = W_TotDbCtaVD + Mov_CtaConta.Valor.
               IF Conciliacion.Novedad EQ "NF" THEN ASSIGN W_TotDbCtaNF = W_TotDbCtaNF + Mov_CtaConta.Valor.
            END.
            ELSE DO:                                               
               IF Conciliacion.Novedad EQ "CC" THEN ASSIGN W_TotCrCtaCC = W_TotCrCtaCC + Mov_CtaConta.Valor.
               IF Conciliacion.Novedad EQ "VD" THEN ASSIGN W_TotCrCtaVD = W_TotCrCtaVD + Mov_CtaConta.Valor.
               IF Conciliacion.Novedad EQ "NF" THEN ASSIGN W_TotCrCtaNF = W_TotCrCtaNF + Mov_CtaConta.Valor.
            END.                                                 
            IF Conciliacion.Novedad EQ "NC" THEN ASSIGN W_SubTotNC = W_SubTotNC + Mov_CtaConta.Valor.
            IF Conciliacion.Novedad EQ "NO" THEN ASSIGN W_SubTotNO = W_SubTotNO + Mov_CtaConta.Valor.              
         END.
         ELSE DO:
            IF AVAILABLE Mov_Extracto THEN DO:
               IF R_Tipo NE "RE" THEN DO:
                  DISPLAY Conciliacion.Novedad       AT  18 W_Cruce                    AT  70
                          Mov_Extracto.Fecha         AT  79 Mov_Extracto.Num_Cheque    AT  92
                          Mov_Extracto.Secuencia     AT 103 Mov_Extracto.Valor         AT 109 FORMAT "->>>>,>>>,>>>,>>9.99"
                          Mov_Extracto.Naturaleza    AT 129 Mov_Extracto.Descripcion   AT 132 FORMAT "X(14)" SKIP
                    WITH FRAME F3 WIDTH 150 STREAM-IO NO-LABELS NO-BOX.
               END.   
               IF Mov_Extracto.Naturaleza EQ "DB" THEN DO:
                  IF Conciliacion.Novedad EQ "CC" THEN ASSIGN W_TotDbExtCC = W_TotDbExtCC + Mov_Extracto.Valor.
                  IF Conciliacion.Novedad EQ "VD" THEN ASSIGN W_TotDbExtVD = W_TotDbExtVD + Mov_Extracto.Valor.
                  IF Conciliacion.Novedad EQ "NF" THEN ASSIGN W_TotDbExtNF = W_TotDbExtNF + Mov_Extracto.Valor.
               END.
               ELSE DO:
                  IF Conciliacion.Novedad EQ "CC" THEN ASSIGN W_TotCrExtCC = W_TotCrExtCC + Mov_Extracto.Valor.
                  IF Conciliacion.Novedad EQ "VD" THEN ASSIGN W_TotCrExtVD = W_TotCrExtVD + Mov_Extracto.Valor.
                  IF Conciliacion.Novedad EQ "NF" THEN ASSIGN W_TotCrExtNF = W_TotCrExtNF + Mov_Extracto.Valor.
               END.
               IF Conciliacion.Novedad EQ "NG" THEN ASSIGN W_SubTotNG = W_SubTotNG + Mov_Extracto.Valor.
               IF Conciliacion.Novedad EQ "NR" THEN ASSIGN W_SubTotNR = W_SubTotNR + Mov_Extracto.Valor.              
            END.   
         END.
      END.    
      IF R_Tipo NE "RE" THEN DO:     
         IF LAST-OF(Conciliacion.Novedad) THEN DO: 
            ASSIGN W_SubTotCC    = (W_TotDbCtaCC - W_TotCrCtaCC)
                   W_SubTotExtCC = (W_TotDbExtCC - W_TotCrExtCC)
                   W_SubTotVD    = (W_TotDbCtaVD - W_TotCrCtaVD)
                   W_SubTotExtVD = (W_TotDbExtVD - W_TotCrExtVD)
                   W_SubTotNF    = (W_TotDbCtaNF - W_TotCrCtaNF)
                   W_SubTotExtNF = (W_TotDbExtNF - W_TotCrExtNF).

            IF Conciliacion.Novedad EQ "CC" THEN DO:
               DISPLAY   "--------------------"      AT  32 "--------------------"      AT 109
                         "TOTAL DEBITOS"             AT   1
                         W_TotDbCtaCC                AT  32 FORMAT "->>>>,>>>,>>>,>>9.99"
                         W_TotDbExtCC                AT 109 FORMAT "->>>>,>>>,>>>,>>9.99"
                         "TOTAL CREDITOS"            AT   1
                         W_TotCrCtaCC                AT  32 FORMAT "->>>>,>>>,>>>,>>9.99"
                         W_TotCrExtCC                AT 109 FORMAT "->>>>,>>>,>>>,>>9.99"    
                         "TOTAL CONCILIADOS"         AT   1
                         W_SubTotCC                  AT  32 FORMAT "->>>>,>>>,>>>,>>9.99"
                         W_SubTotExtCC               AT 109 FORMAT "->>>>,>>>,>>>,>>9.99" SKIP (1)    
               WITH WIDTH 150 NO-LABELS FRAME CHECONC NO-BOX.
            END.       
            IF Conciliacion.Novedad EQ "VD" THEN DO:
               DISPLAY   "--------------------"      AT  32 "--------------------"      AT 109
                         "TOTAL DEBITOS"             AT   1
                         W_TotDbCtaVD                AT  32 FORMAT "->>>>,>>>,>>>,>>9.99"
                         W_TotDbExtVD                AT 109 FORMAT "->>>>,>>>,>>>,>>9.99"
                         "TOTAL CREDITOS"            AT   1
                         W_TotCrCtaVD                AT  32 FORMAT "->>>>,>>>,>>>,>>9.99"
                         W_TotCrExtVD                AT 109 FORMAT "->>>>,>>>,>>>,>>9.99"   
                         "TOTAL VALORES DIF.:"       AT   1
                         W_SubTotVD                  AT  32 FORMAT "->>>>,>>>,>>>,>>9.99"
                         W_SubTotExtVD               AT 109 FORMAT "->>>>,>>>,>>>,>>9.99" SKIP (1)  
               WITH WIDTH 150 NO-LABELS FRAME CHECONC1 NO-BOX.
            END. 
            IF Conciliacion.Novedad EQ "NF" THEN DO:
               DISPLAY   "--------------------"      AT  32 "--------------------"      AT 109
                         "TOTAL DEBITOS"             AT   1
                         W_TotDbCtaNF                AT  32 FORMAT "->>>>,>>>,>>>,>>9.99"
                         W_TotDbExtNF                AT 109 FORMAT "->>>>,>>>,>>>,>>9.99"
                         "TOTAL CREDITOS"            AT   1
                         W_TotCrCtaNF                AT  32 FORMAT "->>>>,>>>,>>>,>>9.99"
                         W_TotCrExtNF                AT 109 FORMAT "->>>>,>>>,>>>,>>9.99"   
                         "TOTAL DIAS HOLGURA:"       AT   1
                         W_SubTotNF                  AT  32 FORMAT "->>>>,>>>,>>>,>>9.99"
                         W_SubTotExtNF               AT 109 FORMAT "->>>>,>>>,>>>,>>9.99" SKIP (1)    
               WITH WIDTH 150 NO-LABELS FRAME CHECONC2 NO-BOX.     
            END.
            IF Conciliacion.Novedad EQ "NC" THEN DO:
               DISPLAY   "--------------------"      AT 32 "TOTAL NO COBRADOS:"        AT  1
                         W_SubTotNC                  AT 32 FORMAT "->>>>,>>>,>>>,>>9.99" SKIP (1)
               WITH WIDTH 150 NO-LABELS FRAME CHECONC3 NO-BOX.     
            END.
            IF Conciliacion.Novedad EQ "NO" THEN DO:
               DISPLAY   "--------------------"      AT 32 "TOTAL NO CONSIGNADO:"      AT  1
                         W_SubTotNO                  AT 32 FORMAT "->>>>,>>>,>>>,>>9.99" SKIP (1)
               WITH WIDTH 150 NO-LABELS FRAME CHECONC4 NO-BOX.     
            END.   
            IF Conciliacion.Novedad EQ "NG" THEN DO:
               DISPLAY   "--------------------"      AT 109 "TOTAL NO GIRADO"           AT  79
                         W_SubTotNG                  AT 109 FORMAT "->>>>,>>>,>>>,>>9.99" SKIP (1)
               WITH WIDTH 150 NO-LABELS FRAME CHECONC5 NO-BOX.     
            END.   
            IF Conciliacion.Novedad EQ "NR" THEN DO:
               DISPLAY   "--------------------"      AT 109 "TOTAL NO REGISTRADOS:"     AT  79
                         W_SubTotNR                  AT 109 FORMAT "->>>>,>>>,>>>,>>9.99" SKIP (1)
               WITH WIDTH 150 NO-LABELS FRAME CHECONC6 NO-BOX.     
            END.           
         END.    
      END.    
  END.
  IF R_Tipo = "TO" OR R_Tipo = "RE" THEN DO:
     ASSIGN W_SdoTotExt = W_SdoExt + W_SubTotNO - W_SubTotNC - W_SubTotNR + W_SubTotNG
                          - W_TotDbExtVD + W_TotCrExtVD + W_TotDbCtaVD - W_TotCrCtaVD -
                             W_TotDbExtNF + W_TotCrExtNF + W_TotDbCtaNF - W_TotCrCtaNF
            W_Difer     = (W_SdoTotExt - W_SdoCta).
            W_ImpReg    = YES.
     PAGE.
     DISPLAY 
     "--------------------------------------------------------------------------------------------"  AT 1 SKIP
     "Saldo Según Extracto                     $"   AT   2
      W_SdoExt                                      AT  50  NO-LABELS
     "Mas  : Movimiento no Consignado (NO)     $"   AT   2
      W_SubTotNO                                    AT  50 NO-LABELS
     "Menos: Movimiento no Cobrado    (NC)     $"   AT   2
      W_SubTotNC                                    AT  50  NO-LABELS 
     "Menos: Movimiento no Registrado (NR)     $"   AT   2
      W_SubTotNR                                    AT  50 NO-LABELS
     "Mas  : Movimiento no Girado     (NG)     $"   AT   2
      W_SubTotNG                                    AT  50  NO-LABELS
     "--------------------------------------------------------------------------------------------"  AT 1 SKIP      
     "Menos: Movimiento Extracto (VD) Débito   $"   AT   2
      W_TotDbExtVD                                  AT  50  NO-LABELS
     "Mas  : Movimiento Extracto (VD) Crédito  $"   AT   2
      W_TotCrExtVD                                  AT  50  NO-LABELS
     "Mas  : Movimiento Contable (VD) Débito   $"   AT   2
      W_TotDbCtaVD                                  AT  50  NO-LABELS      
     "Menos: Movimiento Contable (VD) Crédito  $"   AT   2
      W_TotCrCtaVD                                  AT  50  NO-LABELS      
     "Menos: Movimiento Extracto (NF) Débito   $"   AT   2
      W_TotDbExtNF                                  AT  50  NO-LABELS      
     "Mas  : Movimiento Extracto (NF) Crédito  $"   AT   2
      W_TotCrExtNF                                  AT  50  NO-LABELS      
     "Mas  : Movimiento Contable (NF) Débito   $"   AT   2
      W_TotDbCtaNF                                  AT  50  NO-LABELS      
     "Menos: Movimiento Contable (NF) Crédito  $"   AT   2
      W_TotCrCtaNF                                  AT  50  NO-LABELS            
     "--------------------------------------------------------------------------------------------"  AT 1 SKIP
     "                            Saldo        $"   AT 2
      W_SdoTotExt                                   AT 50  NO-LABELS
     "--------------------------------------------------------------------------------------------"  AT 1 
     "Saldo Según Contabilidad                 $"   AT 2
      W_SdoCta                                      AT 50  NO-LABELS
     "--------------------------------------------------------------------------------------------"  AT 1
     "Diferencia entre Extracto y Contabilidad $"   AT 2
      W_Difer                                       AT 50  NO-LABELS
     "--------------------------------------------------------------------------------------------"  AT 1 SKIP
     WITH WIDTH 150. 
  END.          
  IF NOT W_ImpReg THEN
     DISPLAY "Sin Registros" WITH FRAME W_SinReg NO-LABELS NO-BOX.
  PAGE.
  OUTPUT CLOSE.   
  SESSION:SET-WAIT-STATE("").         
  CASE op_salida:
       WHEN "P" THEN
             RUN Pantalla IN W_Manija (INPUT procnamen). 
       WHEN "I" THEN
             RUN adecomm/_osprint.r (INPUT ?,INPUT procnamen,INPUT 2,INPUT 1,INPUT 0,
                                   INPUT 0,OUTPUT W_Rpta).
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicia-Tmp W-Win 
PROCEDURE Inicia-Tmp :
/*------------------------------------------------------------------------------
  Objr¿etivo:  Borra todos los registros de las tablas temporales.
------------------------------------------------------------------------------*/
  FOR EACH Tmp-Extracto:
      DELETE Tmp-Extracto.              
  END.     
  FOR EACH Tmp-CtaConta:
      DELETE Tmp-CtaConta.              
  END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicia_TmpCon W-Win 
PROCEDURE Inicia_TmpCon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH Tmp-Concilia:
      DELETE Tmp-Concilia.
  END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Limpia_VD W-Win 
PROCEDURE Limpia_VD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE W_PerReg    AS CHARACTER FORMAT "X(6)".
  DEFINE VARIABLE W_PerConcil AS CHARACTER FORMAT "X(6)".

  ASSIGN W_PerConcil = STRING(W_Ano,"9999") + STRING(W_MesC,"99").
  
  FOR EACH Conciliacion WHERE Conciliacion.Agencia     EQ W_Ofi
                          AND Conciliacion.Cuenta      EQ W_CtaBco
                          AND (   Conciliacion.Novedad EQ "VD"
                               OR Conciliacion.Novedad EQ "NF")
                        EXCLUSIVE-LOCK:
      RUN Busca_Extracto (YES).                 
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llena_TmpCon W-Win 
PROCEDURE Llena_TmpCon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  GET FIRST Brw-Concil.
  DO WHILE AVAILABLE Conciliacion:
     IF  Conciliacion.Reg_CtaConta  NE ""
     AND Conciliacion.Reg_MovExtrac NE "" THEN DO:
         IF Mov_CtaConta.Num_Cheque EQ Mov_Extracto.Num_Cheque THEN
            IF Mov_CtaConta.Valor EQ Mov_Extracto.Valor THEN
               RUN Crea_TmpCon (Mov_CtaConta.Valor, Mov_CtaConta.Num_Cheque).
            ELSE DO:
               RUN Crea_TmpCon (Mov_CtaConta.Valor, Mov_CtaConta.Num_Cheque).
               RUN Crea_TmpCon (Mov_Extracto.Valor, ?).
            END. 
         ELSE
            IF Mov_CtaConta.Valor EQ Mov_Extracto.Valor THEN DO:
               RUN Crea_TmpCon (Mov_CtaConta.Valor, Mov_CtaConta.Num_Cheque).
               RUN Crea_TmpCon (?, Mov_Extracto.Num_Cheque).
            END.
     END.
     ELSE
        IF Conciliacion.Reg_CtaConta  NE "" THEN
           RUN Crea_TmpCon (Mov_CtaConta.Valor, Mov_CtaConta.Num_Cheque).
        ELSE
           IF Conciliacion.Reg_MovExtrac  NE "" THEN
              RUN Crea_TmpCon (Mov_Extracto.Valor, Mov_Extracto.Num_Cheque).
     GET NEXT Brw-Concil.
  END.
  GET FIRST Brw-Concil.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llena_TmpCta W-Win 
PROCEDURE Llena_TmpCta :
/*------------------------------------------------------------------------------
  Objetivo : Llena tabla temporal Tmp-CtaConta, partiendo de Mov_CtaConta, no carga
             las partidas que estan en Conciliacion con novedad "CC".
------------------------------------------------------------------------------*/
 FOR EACH Mov_CtaConta WHERE Mov_CtaConta.Agencia EQ W_Ofi
                         AND Mov_CtaConta.Cuenta  EQ W_CtaBco
                         AND Mov_CtaConta.Fecha   LE W_FecCorte
                       NO-LOCK:
     FIND Conciliacion WHERE Conciliacion.Agencia      EQ W_Ofi
                         AND Conciliacion.Cuenta       EQ W_CtaBco
                         AND Conciliacion.Reg_CtaConta EQ Mov_CtaConta.Reg_CtaConta
                       NO-LOCK NO-ERROR.                       
     IF AVAILABLE Conciliacion THEN
        IF Conciliacion.Novedad EQ "CC" THEN NEXT.
     RUN Crea_TmpCta.   
 END.
 {&OPEN-QUERY-Brw-MovCtaConta}                                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llena_TmpExt W-Win 
PROCEDURE Llena_TmpExt :
/*------------------------------------------------------------------------------
    Objetivo : Llena tabla temporal Tmp-Extracto, partiendo de Mov_Extracto, no carga
               las partidas que estan en Conciliacion con novedad "CC".
------------------------------------------------------------------------------*/
 FOR EACH Mov_Extracto WHERE Mov_Extracto.Agencia   EQ W_Ofi
                         AND Mov_Extracto.Cuenta    EQ W_CtaBco
                         AND Mov_Extracto.Fecha     LE W_FecCorte
                         AND Mov_Extracto.Secuencia NE 0
                       NO-LOCK:
     FIND Conciliacion WHERE Conciliacion.Agencia       EQ W_Ofi
                         AND Conciliacion.Cuenta        EQ W_CtaBco
                         AND Conciliacion.Reg_MovExtrac EQ Mov_Extracto.Reg_MovExtrac    
                       NO-LOCK NO-ERROR.
     IF AVAILABLE Conciliacion THEN
        IF Conciliacion.Novedad EQ "CC" THEN NEXT.
     RUN Crea_TmpExt.   
 END.  
 {&OPEN-QUERY-Brw-MovExtrac}  
 ASSIGN W_ConsulCon = TRUE.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  DEFINE VARIABLE W_Rpta AS LOGICAL.
  ASSIGN W_Ofi     = W_Agencia
         W_Usuario = W_Usuario.

  FOR EACH Agencias FIELDS (Agencias.Agencia Agencias.Nombre) 
                    WHERE Agencias.Estado EQ 1 NO-LOCK:
    ASSIGN W_OfStr  = STRING(Agencias.Agencia,"999") + "-" + STRING(Agencias.Nombre)
           W_Status = Cmb_OfiCon:ADD-LAST(W_OfStr) IN FRAME F-Main.
    IF Agencias.Agencia = W_Agencia THEN
              Cmb_OfiCon:SCREEN-VALUE IN FRAME F-Main = W_OfStr.
  END.
  Cmb_OfiCon:SENSITIVE IN FRAME F-Main = TRUE.
  RUN SuperUsuario IN W_Manija (INPUT W_Agencia, W_Usuario, OUTPUT W_rpta).
  IF NOT W_Rpta  THEN
     Cmb_OfiCon:SENSITIVE IN FRAME F-Main = FALSE.
  HIDE FRAME F_Ext.
  HIDE FRAME F_Formato.
  HIDE FRAME F_Informe.
  HIDE FRAME F_Borrar.
  HIDE FRAME F-Num.
  HIDE FRAME F-Valor.
  DISABLE ALL EXCEPT W_FecCorte W_CtaBco F-Holgura Cmb_OfiCon Btn-Cancel Btn_Ayuda WITH FRAME F-Main. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Quita_CC W-Win 
PROCEDURE Quita_CC :
/*------------------------------------------------------------------------------
  Objetivo:  Desmarca la Novedad CC y la cambia por la correspondiente :
             NR: No Registrado 
             NC: No Cobrados 
             NO: No Consignado
             NG: No Girado       
------------------------------------------------------------------------------*/

  FIND CURRENT Conciliacion EXCLUSIVE-LOCK.
  ASSIGN W_PerConcil = Conciliacion.Per_Conciliacion
         Conciliacion.Fecha_Conciliacion = ?.

  FIND Mov_CtaConta WHERE Mov_CtaConta.Agencia      EQ Conciliacion.Agencia
                      AND Mov_CtaConta.Cuenta       EQ Conciliacion.Cuenta 
                      AND Mov_CtaConta.Reg_CtaConta EQ Conciliacion.Reg_CtaConta
                    NO-LOCK NO-ERROR.

  FIND Mov_Extracto WHERE Mov_Extracto.Agencia       EQ Conciliacion.Agencia
                      AND Mov_Extracto.Cuenta        EQ Conciliacion.Cuenta
                      AND Mov_Extracto.Reg_MovExtrac EQ Conciliacion.Reg_MovExtrac
                    NO-LOCK NO-ERROR.

  IF  AVAILABLE Mov_CtaConta 
  AND AVAILABLE Mov_Extracto THEN DO:
      RUN Crea_TmpCta.
      RUN Crea_TmpExt.
      ASSIGN Conciliacion.Novedad       = IF Tmp-CtaConta.Naturaleza EQ "DB" THEN "NO" 
                                          ELSE "NC"
             Conciliacion.Reg_MovExtrac = ""
             Conciliacion.Reg_Cruce     = "".
      IF  INTEGER(SUBSTRING(Mov_Extracto.Reg_MovExtrac,3,4)) EQ W_Ano
      AND INTEGER(SUBSTRING(Mov_Extracto.Reg_MovExtrac,7,2)) EQ W_MesC THEN
          ASSIGN W_PerConcil = W_FecCorte.

      CREATE Conciliacion.
      ASSIGN Conciliacion.Agencia          = Mov_Extracto.Agencia
             Conciliacion.Cuenta           = Mov_Extracto.Cuenta
             Conciliacion.Reg_MovExtrac    = Mov_Extracto.Reg_MovExtrac
             Conciliacion.Per_Conciliacion = W_PerConcil
             Conciliacion.Novedad          = IF Mov_Extracto.Naturaleza EQ "DB" THEN "NR" 
                                             ELSE "NG".
  END.
  ELSE DO:
     IF AVAILABLE Mov_CtaConta THEN DO:
        RUN Crea_TmpCta.
        ASSIGN Conciliacion.Novedad       = IF Tmp-CtaConta.Naturaleza EQ "DB" THEN "NO" 
                                            ELSE "NC"
               Conciliacion.Reg_MovExtrac = ""
               Conciliacion.Reg_Cruce     = "".
     END.
     ELSE                                 
        IF AVAILABLE Mov_Extracto THEN DO:
           RUN Crea_TmpExt.
           ASSIGN Conciliacion.Novedad      = IF Mov_Extracto.Naturaleza EQ "DB" THEN "NR" 
                                              ELSE "NG"
                  Conciliacion.Reg_Cruce    = ""
                  Conciliacion.Reg_CtaConta = "".
        END.          
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Secuencia W-Win 
PROCEDURE Secuencia :
/*------------------------------------------------------------------------------
  Objetivo: Halla el último número de cruce manual y aumenta el consecutivo.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE Wk_RegCruce LIKE Conciliacion.Reg_Cruce.
  
  ASSIGN W_Cont = 1
         Wk_RegCruce = "MV" + STRING(W_Ano,"9999") + STRING(W_MesC,"99") + STRING(W_Cont,"99999").
         
  FIND FIRST Conciliacion WHERE Conciliacion.Agencia   EQ W_Ofi
                            AND Conciliacion.Cuenta    EQ W_CtaBco
                            AND Conciliacion.Reg_Cruce EQ Wk_RegCruce                  
                          NO-LOCK NO-ERROR.
  REPEAT:
     IF NOT AVAILABLE Conciliacion THEN LEAVE.
  
     ASSIGN W_Cont = W_Cont + 1
            Wk_RegCruce = "MV" + STRING(W_Ano,"9999") + STRING(W_MesC,"99") + STRING(W_Cont,"99999").

     FIND FIRST Conciliacion WHERE Conciliacion.Agencia   EQ W_Ofi
                               AND Conciliacion.Cuenta    EQ W_CtaBco
                               AND Conciliacion.Reg_Cruce EQ Wk_RegCruce                  
                             NO-LOCK NO-ERROR.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Sec_ManDoc W-Win 
PROCEDURE Sec_ManDoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN W_SecExt1 = 1.
  FIND FIRST Mov_Extracto WHERE Mov_Extracto.Agencia    EQ W_Ofi
                            AND Mov_Extracto.Cuenta     EQ W_CtaBco
                            AND Mov_Extracto.Num_Cheque EQ W_CheExt
                            AND Mov_Extracto.Secuencia  EQ W_SecExt1
                            AND Mov_Extracto.Fecha      GE W_FecInic
                            AND Mov_Extracto.Fecha      LE W_FecCorte
                          NO-LOCK NO-ERROR.
  REPEAT:
     IF NOT AVAILABLE Mov_Extracto THEN LEAVE.
  
     ASSIGN W_SecExt1 = W_SecExt1 + 1.
     FIND FIRST Mov_Extracto WHERE Mov_Extracto.Agencia    EQ W_Ofi
                               AND Mov_Extracto.Cuenta     EQ W_CtaBco
                               AND Mov_Extracto.Num_Cheque EQ W_CheExt
                               AND Mov_Extracto.Secuencia  EQ W_SecExt1
                               AND Mov_Extracto.Fecha      GE W_FecInic
                               AND Mov_Extracto.Fecha      LE W_FecCorte
                             NO-LOCK NO-ERROR.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Seleccion_Cruce W-Win 
PROCEDURE Seleccion_Cruce :
/*------------------------------------------------------------------------------
  Objetivo: Seleccion de registros en conciliacion a traves de un número de Cruce.     
------------------------------------------------------------------------------*/
  FIND FIRST Conciliacion WHERE Conciliacion.Agencia   EQ W_Ofi
                            AND Conciliacion.Cuenta    EQ W_CtaBco 
                            AND Conciliacion.Reg_Cruce EQ F-CruStr 
                          NO-ERROR.       
  IF NOT AVAILABLE Conciliacion THEN DO:
     MESSAGE "Cruce no Se Encuentra en Movimiento Conciliado." 
        VIEW-AS ALERT-BOX ERROR TITLE "Error de Busqueda".
     RETURN.   
  END. 

  REPEAT:
     IF NOT AVAILABLE Conciliacion THEN LEAVE.
     ASSIGN Curr-record = ROWID(Conciliacion).       
     REPOSITION Brw-Concil TO ROWID Curr-record.
     GET NEXT Brw-Concil.
     IF Brw-Concil:SELECT-FOCUSED-ROW() IN FRAME F-Concil THEN.
     FIND NEXT Conciliacion WHERE Conciliacion.Agencia   EQ W_Ofi
                              AND Conciliacion.Cuenta    EQ W_CtaBco 
                              AND Conciliacion.Reg_Cruce EQ F-CruStr 
                            NO-ERROR.         
  END.                                             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "Tmp-Extracto"}
  {src/adm/template/snd-list.i "Tmp-CtaConta"}
  {src/adm/template/snd-list.i "Conciliacion"}
  {src/adm/template/snd-list.i "Mov_CtaConta"}
  {src/adm/template/snd-list.i "Mov_Extracto"}
  {src/adm/template/snd-list.i "Formatos"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Suma_SelCta W-Win 
PROCEDURE Suma_SelCta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN W_MvDebi = 0
         W_MvCred = 0. 

  DO I = Brw-MovCtaConta:NUM-SELECTED-ROWS IN FRAME F-Main TO 1 By -1:
         ASSIGN  W_Status = Brw-MovCtaConta:FETCH-SELECTED-ROW(I).
         IF Tmp-CtaConta.Naturaleza EQ "DB" THEN 
            ASSIGN W_MvDebi = (W_MvDebi + Tmp-CtaConta.Valor).
         ELSE 
            ASSIGN W_MvCred = (W_MvCred + Tmp-CtaConta.Valor).                
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Suma_SelExt W-Win 
PROCEDURE Suma_SelExt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN W_ExDeb  = 0
         W_ExCre  = 0.

  DO I = Brw-MovExtrac:NUM-SELECTED-ROWS IN FRAME F-Main TO 1 By -1:
         ASSIGN W_Status = Brw-MovExtrac:FETCH-SELECTED-ROW(I).
         IF Tmp-Extracto.Naturaleza EQ "DB" THEN 
            ASSIGN W_ExDeb   = (W_ExDeb + Tmp-Extracto.Valor).
         ELSE 
            ASSIGN W_ExCre = (W_ExCre + Tmp-Extracto.Valor).
  END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida_Inicio W-Win 
PROCEDURE Valida_Inicio :
/*------------------------------------------------------------------------------
  Ojetivo: Validacion de las condiciones iniciales para poder continuar con el
           proceso de conciliacion.       
------------------------------------------------------------------------------*/
  ASSIGN W_MesC    = MONTH(W_FecCorte)
         W_Dia     = DAY(W_FecCorte)
         W_Ano     = YEAR(W_FecCorte)
         W_FecInic = DATE(W_MesC,01,W_Ano)
         W-MesAnt  = IF W_MesC <= 1 THEN 12
                     ELSE (W_MesC - 1)
         W-AnoAnt  = IF W_MesC <= 1 THEN (W_Ano - 1)
                     ELSE W_Ano .

  ASSIGN W_UltDia[2] = DAY(DATE(02,29,W_Ano)) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
     ASSIGN W_UltDia[2] = 28.

  IF W_FecCorte GT W_Fecha THEN DO:
     MESSAGE  "La Fecha es Posterior a la del Sistema" 
        VIEW-AS ALERT-BOX ERROR TITLE "Error de Fecha".
     RETURN ERROR.
  END.

  IF W_Dia NE W_UltDia[W_MesC] THEN DO:
     MESSAGE  "El Día de la Fecha es Diferente al último Día del Mes." SKIP
              "La Información solo se Seleccionará hasta este Día.   " SKIP
              "Desea Continuar...?"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO TITLE "Fecha Para Conciliar"
        UPDATE CHOICE AS LOGICAL.
     IF NOT CHOICE THEN 
        RETURN ERROR.
  END.      

  /* VERIFICA CIERRE DE MES */
  FIND Procdia  WHERE Procdia.Agencia            EQ W_Ofi
                      AND Procdia.cod_Proceso        EQ 7
                      AND MONTH(ProcDia.Fecha_Proc) EQ W-MesAnt
                      AND YEAR(ProcDia.Fecha_Proc)  EQ W-AnoAnt
                      AND Procdia.Estado             EQ 2
                    NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Procdia) THEN DO:
       MESSAGE  "El Mes Anterior Esta Abierto." SKIP
                "Cierre el Mes para Poder Conciliar."
       VIEW-AS ALERT-BOX ERROR  TITLE "Estado Mes a Conciliar".
       RETURN ERROR.
  END.     

  ASSIGN W_SoloConsulta = NO.
  FIND FIRST Conciliacion WHERE Conciliacion.Agencia           EQ W_Ofi
                            AND Conciliacion.Cuenta            EQ W_CtaBco
                            AND Conciliacion.Per_Conciliacion  GE W_FecInic
                            AND Conciliacion.Per_Conciliacion  LE W_FecCorte
                          NO-LOCK NO-ERROR.                         
  IF NOT AVAILABLE Conciliacion THEN DO:
     FIND FIRST Conciliacion WHERE Conciliacion.Agencia                 EQ W_Ofi
                               AND Conciliacion.Cuenta                  EQ W_CtaBco
                               AND (    MONTH(Conciliacion.Per_Conciliacion) EQ W-MesAnt
                                    AND YEAR(Conciliacion.Per_Conciliacion ) EQ W-AnoAnt)
                                OR (    MONTH(Conciliacion.Fecha_Conciliacion) EQ W-MesAnt
                                    AND YEAR(Conciliacion.Fecha_Conciliacion ) EQ W-AnoAnt)
                             NO-LOCK NO-ERROR.  
     IF NOT AVAILABLE Conciliacion THEN DO:
        FIND FIRST Conciliacion WHERE Conciliacion.Agencia EQ W_Ofi
                                  AND Conciliacion.Cuenta  EQ W_CtaBco
                                NO-LOCK NO-ERROR.
        IF AVAILABLE Conciliacion THEN DO:
           MESSAGE  "El Ultimo Periódo Conciliado es... " Conciliacion.Per_Conciliacion
             VIEW-AS ALERT-BOX ERROR TITLE "Fecha Para Conciliar".
           RETURN ERROR.
        END.              
     END.
  END.   
  ELSE DO:
     FIND FIRST Conciliacion WHERE Conciliacion.Agencia           EQ W_Ofi
                               AND Conciliacion.Cuenta            EQ W_CtaBco
                               AND Conciliacion.Per_Conciliacion  GT W_FecCorte
                            NO-LOCK NO-ERROR.
     IF AVAILABLE Conciliacion THEN
        ASSIGN W_SoloConsulta = YES.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

