/*


*/

{incluido\iprmt_rpt.i}
/* {incluido\Variable.i "SHARED"} */

DEFINE VARIABLE qbf-count    AS INTEGER NO-UNDO.
DEFINE VARIABLE qbf-governor AS INTEGER NO-UNDO.

DEFINE VARIABLE qbf-govcnt AS INTEGER NO-UNDO.
DEFINE VARIABLE qbf-loop   AS INTEGER NO-UNDO.
DEFINE VARIABLE qbf-time   AS INTEGER NO-UNDO.

DEFINE VARIABLE viCpt1 AS INTEGER     NO-UNDO.
DEFINE VARIABLE viCpt2 AS INTEGER     NO-UNDO.
DEFINE VARIABLE viAge1 AS INTEGER     NO-UNDO.
DEFINE VARIABLE viAge2 AS INTEGER     NO-UNDO.

DEFINE BUFFER Mov_Contable FOR Mov_Contable.

ASSIGN
  qbf-count    = 0
  qbf-governor = 0
  qbf-time     = TIME.

ASSIGN 
    viCpt1 = INTEGER(pc01)
    viCpt2 = INTEGER(pc02)
    viAge1 = INTEGER(pc03)
    viAge2 = INTEGER(pc04).
/********************************/

/* {incluido/Variable.i "SHARED"}. */

  DEFINE VARIABLE W_Usuario   LIKE usuarios.usuario       INITIAL "308". /* 308 - Contabiliza*/
  DEFINE VARIABLE W_Fecha     AS DATE   INITIAL TODAY.
  DEFINE VARIABLE W_PathSpl   AS CHARACTER FORMAT "X(20)" INITIAL "c:\info_juriscoop\".
  DEFINE VARIABLE W_Agencia   LIKE Agencia.Agencia        INITIAL "024". /*"035".*/

    DEFINE {1} VAR W_Estacion    LIKE Estaciones.Estacion.
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
/********************/


/*** Pantalla_Validacion */
DEFINE VAR Listado AS CHARACTER INITIAL "".
DEFINE VARIABLE W_Ok              AS LOGICAL.
DEFINE VARIABLE W_AG1             AS INTEGER FORMAT "999" INITIAL 1.
DEFINE VARIABLE W_AG2             AS INTEGER FORMAT "999" INITIAL 1.
DEFINE VARIABLE W_CB1             AS INTEGER FORMAT "999"  INITIAL 4.
DEFINE VARIABLE W_CB2             AS INTEGER FORMAT "999"  INITIAL 4.
DEFINE VARIABLE W_Fec1            AS DATE    INITIAL "01/05/2007".
DEFINE VARIABLE W_Fec2            AS DATE    INITIAL "01/05/2007".

DEFINE VARIABLE  W_Pantalla         AS WIDGET-HANDLE NO-UNDO.
DEFINE RECTANGLE W_CuadroFechas    EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL SIZE 68 BY 2.
DEFINE RECTANGLE W_CuadroLimite    EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL SIZE 68 BY 6.  
DEFINE RECTANGLE W_CuadroOtros     EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL SIZE 68 BY 4.      
  
DEFINE BUTTON Btn_Informacion IMAGE-UP FILE  "imagenes\informacion.bmp" SIZE 10 BY 1.58 AUTO-GO.
DEFINE BUTTON Btn_Imprimir    IMAGE-UP FILE  "imagenes\impresora2.bmp"  SIZE 10 BY 1.58 AUTO-GO.
DEFINE BUTTON Btn_Salir AUTO-GO LABEL "&Salir" SIZE 10 BY 1.58 FONT 5. 
DEFINE BUTTON Btn_Ayuda IMAGE-UP FILE "imagenes\interrogacion.bmp" SIZE 4 BY 1.08 FONT 4.

/* DEFINE VARIABLE Cmb_Agencia AS CHARACTER FORMAT "X(40)"                                                              */
/*        VIEW-AS COMBO-BOX INNER-LINES 5 FONT 5 BGCOLOR 15 LABEL "Agencia".                                            */
/* DEFINE VARIABLE Cmb_CenCost AS CHARACTER FORMAT "X(40)"                                                              */
/*        VIEW-AS COMBO-BOX INNER-LINES 5 FONT 5 BGCOLOR 15 LABEL "Centro de Costos".                                   */
/* DEFINE VARIABLE Cmb_Comprob AS CHARACTER FORMAT "X(40)"                                                              */
/*        VIEW-AS COMBO-BOX INNER-LINES 5 FONT 5 BGCOLOR 15 LABEL "Comprobantes".                                       */
/* DEFINE VARIABLE Cmb_Nivel AS INTEGER FORMAT "9"                                                                      */
/*        VIEW-AS COMBO-BOX LIST-ITEMS 1, 2, 3, 4, 5, 6, 7, 8 INNER-LINES 5 FONT 5 BGCOLOR 15 LABEL "Nivel" INITIAL 8.  */
  
/* DEFINE VARIABLE W_Usuario1     LIKE Usuarios.Usuario LABEL "Usuario Inicial" BGCOLOR 15.  */
/* DEFINE VARIABLE W_NomUsuario1  AS CHARACTER FORMAT "X(54)" BGCOLOR 18 FGCOLOR 15.         */
/* DEFINE VARIABLE W_Usuario2     LIKE Usuarios.Usuario LABEL "Usuario Final" BGCOLOR 15.    */
/* DEFINE VARIABLE W_NomUsuario2  AS CHARACTER FORMAT "X(54)" BGCOLOR 18 FGCOLOR 15.         */
/* DEFINE VARIABLE W_Fec1         AS DATE FORMAT "99/99/9999" INITIAL TODAY BGCOLOR 15 LABEL "Fecha de Trabajo" */
/*        VIEW-AS FILL-IN SIZE 10 BY .81.                                                                       */
/* DEFINE VARIABLE W_Fec2         AS DATE FORMAT "99/99/9999" INITIAL TODAY BGCOLOR 15 LABEL "Fecha Final"      */
/*        VIEW-AS FILL-IN SIZE 10 BY .81.                                                                       */
/* DEFINE VARIABLE W_Cuenta1      AS CHARACTER FORMAT "X(14)" LABEL "Cuenta Inicial" BGCOLOR 15.                */
/* DEFINE VARIABLE W_NomCuenta1   AS CHARACTER FORMAT "X(40)" BGCOLOR 18 FGCOLOR 15.                            */
/* DEFINE VARIABLE W_Cuenta2      AS CHARACTER FORMAT "X(14)" LABEL "Cuenta Final" BGCOLOR 15.                  */
/* DEFINE VARIABLE W_NomCuenta2   AS CHARACTER FORMAT "X(40)" BGCOLOR 18 FGCOLOR 15.                            */
/* DEFINE VARIABLE W_Nit1         AS CHARACTER FORMAT "X(14)" LABEL "Nit Inicial" BGCOLOR 15.                   */
/* DEFINE VARIABLE W_NomNit1      AS CHARACTER FORMAT "X(40)" BGCOLOR 18 FGCOLOR 15.                            */
/* DEFINE VARIABLE W_Nit2         AS CHARACTER FORMAT "X(14)" LABEL "Nit Final" BGCOLOR 15.                     */
/* DEFINE VARIABLE W_NomNit2      AS CHARACTER FORMAT "X(40)" BGCOLOR 18 FGCOLOR 15.                            */
/* DEFINE VARIABLE W_Base         AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Valor de la Base" BGCOLOR 15  */
/*        VIEW-AS FILL-IN SIZE 13 BY .81.                                                                       */
/* DEFINE VARIABLE W_Porcentaje   AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Porcentaje" BGCOLOR 15        */
/*        VIEW-AS FILL-IN SIZE 13 BY .81.                                                                       */

/*recorre tabla de anexos y hace informe igual a balance prueba pero 
  discriminando por nit*/
DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Informe de Consecutivos de Comprobantes".
/*para archivo de excel*/
DEFINE VAR LisEx AS CHARACTER.
DEFINE VAR Ct AS DECIMAL.
DEFINE VAR Cma AS CHARACTER FORMAT "X" INITIAL ";".
DEFINE TEMP-TABLE IEx
    FIELD NLinea AS INTEGER FORMAT "999999"
    FIELD Linea  AS CHARACTER FORMAT "X(150)".

DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
DEFINE VARIABLE W_CtrNat     LIKE Cuentas.Ctr_Naturaleza.
DEFINE VARIABLE L_CC         AS LOGICAL INITIAL YES.
DEFINE VARIABLE choice AS LOGICAL.

DEFINE VAR W_NitEnc LIKE Clientes.Nit.  
DEFINE VAR TotDeb  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR TotCre  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR TotActIni  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotPasIni  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotPtrIni  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotResIni  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotActFin  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotPasFin  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotPtrFin  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
DEFINE VAR TotResFin  AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".

DEFINE TEMP-TABLE TSDoc
    FIELD TS_Age         LIKE Agencias.Agencia
    FIELD TS_Doc         LIKE Mov_Contable.Num_Documento
    FIELD TS_Com         LIKE Mov_Contable.Comprobante
    FIELD TS_Db          AS DECIMAL /*FORMAT "->>,>>>,>>>,>>9"*/
    FIELD TS_Cr          AS DECIMAL /*FORMAT "->>,>>>,>>>,>>9"*/
    FIELD TS_Fec         AS DATE
    FIELD TS_Hor         AS INTEGER
    FIELD TS_NO          AS LOGICAL INITIAL YES
    INDEX idxTSDoc TS_Age /*TS_Fec*/ TS_Com TS_Doc.

DEFINE TEMP-TABLE tempmov
 FIELD agencia        LIKE mov_contable.agencia
 FIELD comprobante    LIKE mov_contable.comprobante
 FIELD fec_contable   LIKE mov_contable.fec_contable
 FIELD hora           LIKE mov_contable.hora
 FIELD num_documento  LIKE mov_contable.num_documento
 FIELD db             LIKE mov_contable.db
 FIELD cr             LIKE mov_contable.cr
 INDEX idxtodo agencia /*fec_contable*/ comprobante num_documento.

MESSAGE "Desea Sacar las inconsistencias?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice.
EMPTY TEMP-TABLE TSDoc.
EMPTY TEMP-TABLE tempmov.

ASSIGN TotCre = 0 TotDeb = 0 TotActIni = 0 TotActFin = 0 TotPasIni = 0
       TotPasFin = 0 TotPtrIni = 0 TotPtrFin = 0 TotResIni = 0 TotResFin = 0.
RUN Tabla_Temporal.                        
/* ASSIGN Listado = "c:\contabilidad\L-ENTIDA.lst". */
/* {incluido\ImpArch.I "listado"}. */


IF P_NomArchivo EQ "DEFAULT" THEN
    ASSIGN P_NomArchivo = SESSION:TEMP-DIRECTORY + "revcompro.txt".

OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).

{incluido\RepHeader.i}

VIEW FRAME F-Encabezado.

RUN ProcesoImprimir.

PROCEDURE Tabla_Temporal:  /* TT */
  DEFINE VAR W_Consecutivo LIKE Mov_Contable.Num_Documento.
  IF (viCpt1 = 0 AND viCpt2 = 999) AND (viAge1 = 0 AND viAge2 = 999) AND (pdt01 NE ? AND pdt02 NE ?) THEN DO:
     MESSAGE "Entre a Todos."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     FOR EACH Mov_Contable /*field(agencia comprobante fec_contable num_documento db cr)*/ WHERE
         Mov_Contable.Fec_Contable >= pdt01 AND Mov_Contable.Fec_Contable <= pdt02
         NO-LOCK BY mov_contable.agencia BY mov_contable.comprobante BY mov_contable.num_documento:
         CREATE tempmov.
         UPDATE tempmov.agencia       =  mov_contable.agencia      
                tempmov.comprobante   =  mov_contable.comprobante  
                tempmov.fec_contable  =  mov_contable.fec_contable 
                tempmov.Hora          =  Mov_Contable.Hora
                tempmov.num_documento =  mov_contable.num_documento
                tempmov.db            =  mov_contable.db           
                tempmov.cr            =  mov_contable.cr.
     END.
     MESSAGE "Voy a Procesar.."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

     FOR EACH tempmov 
         BREAK BY tempmov.agencia BY tempmov.Comprobante BY tempmov.Num_Documento:
     
         IF FIRST-OF(tempmov.Comprobante) THEN 
            W_Consecutivo = tempmov.Num_Documento.
         ASSIGN TotCre = TotCre + tempmov.CR
                TotDeb = TotDeb + tempmov.DB.
         IF LAST-OF(tempmov.Num_Documento) THEN DO:
            REPEAT WHILE W_Consecutivo LT tempmov.Num_Documento:
              CREATE TSDoc.
              ASSIGN TSDoc.TS_Com = tempmov.Comprobante
                     TSDoc.TS_Age = tempmov.Agencia
                     TSDoc.TS_Doc = W_Consecutivo
                     TSDoc.TS_No  = NO.
              W_Consecutivo = W_Consecutivo + 1.
            END.
            IF W_Consecutivo EQ tempmov.Num_Documento THEN DO:
              CREATE TSDoc.
              ASSIGN TSDoc.TS_Com = tempmov.Comprobante
                     TSDoc.TS_Age = tempmov.Agencia
                     TSDoc.TS_Doc = tempmov.Num_Documento
                     TSDoc.TS_CR  = TotCre
                     TSDoc.TS_DB  = TotDeb
                     TSDoc.TS_Fec = tempmov.Fec_Contable
                     TSDoc.TS_Hor = tempmov.Hora.
              W_Consecutivo = W_Consecutivo + 1.
            END.
            ASSIGN TotCre = 0 TotDeb = 0.
         END.
     END.
  END.
  ELSE
    IF viCpt1 = viCpt2 AND viAge1 = viAge2 AND pdt01 = pdt02 THEN DO:
        MESSAGE "Todas Iguales..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        FOR EACH Mov_Contable /*field(agencia comprobante fec_contable num_documento cuenta db cr)*/
            WHERE (Mov_Contable.Comprobante  = viCpt1 AND Mov_Contable.Comprobante  = viCpt2) AND 
                  (Mov_Contable.Fec_Contable = pdt01  AND Mov_Contable.Fec_Contable = pdt02)  AND 
                  (Mov_Contable.Agencia      = viAge1 AND Mov_Contable.Agencia      = viAge2) NO-LOCK
                  BREAK BY Mov_Contable.agencia  
                        BY Mov_Contable.Comprobante
                        BY Mov_Contable.Num_Documento:
    /*         MESSAGE "Comprobante : " Mov_Contable.Comprobante */
    /*             VIEW-AS ALERT-BOX INFO BUTTONS OK.            */
    /*                                                           */
            IF FIRST-OF(Mov_Contable.Comprobante) THEN 
               W_Consecutivo = Mov_Contable.Num_Documento.
            ASSIGN TotCre = TotCre + Mov_Contable.CR
                   TotDeb = TotDeb + Mov_Contable.DB.
            IF LAST-OF(Mov_Contable.Num_Documento) THEN DO:
               REPEAT WHILE W_Consecutivo LT Mov_Contable.Num_Documento:
                 CREATE TSDoc.
                 ASSIGN TSDoc.TS_Com = Mov_Contable.Comprobante
                        TSDoc.TS_Age = Mov_Contable.Agencia
                        TSDoc.TS_Doc = W_Consecutivo
                        TSDoc.TS_No  = NO.
                 W_Consecutivo = W_Consecutivo + 1.
               END.
               IF W_Consecutivo EQ Mov_Contable.Num_Documento THEN DO:
                 CREATE TSDoc.
                 ASSIGN TSDoc.TS_Com = Mov_Contable.Comprobante
                        TSDoc.TS_Age = Mov_Contable.Agencia
                        TSDoc.TS_Doc = Mov_Contable.Num_Documento
                        TSDoc.TS_CR  = TotCre
                        TSDoc.TS_DB  = TotDeb
                        TSDoc.TS_Fec = Mov_Contable.Fec_Contable
                        TSDoc.TS_Hor = Mov_Contable.Hora.
                 W_Consecutivo = W_Consecutivo + 1.
               END.
               ASSIGN TotCre = 0 TotDeb = 0.
            END.
        END.
    END.
    ELSE 
      IF viCpt1 = viCpt2 AND viAge1 = viAge2 THEN DO:
          MESSAGE "Cpt y Agencia Iguales.."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
            FOR EACH Mov_Contable /*field(agencia comprobante fec_contable num_documento cuenta db cr)*/
                WHERE (Mov_Contable.Comprobante  =  viCpt1 AND Mov_Contable.Comprobante  =  viCpt2) AND 
                      (Mov_Contable.Fec_Contable >= pdt01  AND Mov_Contable.Fec_Contable <= pdt02)  AND 
                      (Mov_Contable.Agencia      =  viAge1 AND Mov_Contable.Agencia      =  viAge2) NO-LOCK
                      BREAK BY Mov_Contable.agencia  
                            BY Mov_Contable.Comprobante
                            BY Mov_Contable.Num_Documento:
        /*         MESSAGE "Comprobante : " Mov_Contable.Comprobante */
        /*             VIEW-AS ALERT-BOX INFO BUTTONS OK.            */
        /*                                                           */
                IF FIRST-OF(Mov_Contable.Comprobante) THEN 
                   W_Consecutivo = Mov_Contable.Num_Documento.
                ASSIGN TotCre = TotCre + Mov_Contable.CR
                       TotDeb = TotDeb + Mov_Contable.DB.
                IF LAST-OF(Mov_Contable.Num_Documento) THEN DO:
                   REPEAT WHILE W_Consecutivo LT Mov_Contable.Num_Documento:
                     CREATE TSDoc.
                     ASSIGN TSDoc.TS_Com = Mov_Contable.Comprobante
                            TSDoc.TS_Age = Mov_Contable.Agencia
                            TSDoc.TS_Doc = W_Consecutivo
                            TSDoc.TS_No  = NO.
                     W_Consecutivo = W_Consecutivo + 1.
                   END.
                   IF W_Consecutivo EQ Mov_Contable.Num_Documento THEN DO:
                     CREATE TSDoc.
                     ASSIGN TSDoc.TS_Com = Mov_Contable.Comprobante
                            TSDoc.TS_Age = Mov_Contable.Agencia
                            TSDoc.TS_Doc = Mov_Contable.Num_Documento
                            TSDoc.TS_CR  = TotCre
                            TSDoc.TS_DB  = TotDeb
                            TSDoc.TS_Fec = Mov_Contable.Fec_Contable
                            TSDoc.TS_Hor = Mov_Contable.Hora.
                     W_Consecutivo = W_Consecutivo + 1.
                   END.
                   ASSIGN TotCre = 0 TotDeb = 0.
                END.
            END.
      END.
      ELSE DO:
        MESSAGE "Cpte ó Agencia Todas.." 
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        IF viAge1 = viAge2 THEN DO:
            MESSAGE "Estoy por la misma agencia.."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            FOR EACH Mov_Contable /*field(agencia comprobante fec_contable num_documento cuenta db cr)*/
                WHERE (Mov_Contable.Comprobante  >= viCpt1 AND Mov_Contable.Comprobante  <= viCpt2) AND 
                      (Mov_Contable.Fec_Contable >= pdt01  AND Mov_Contable.Fec_Contable <= pdt02)  AND 
                      (Mov_Contable.Agencia      =  viAge1 AND Mov_Contable.Agencia      =  viAge2) 
                      NO-LOCK BY mov_contable.agencia BY mov_contable.comprobante BY mov_contable.num_documento:
                 CREATE tempmov.
                 UPDATE tempmov.agencia       =  mov_contable.agencia      
                        tempmov.comprobante   =  mov_contable.comprobante  
                        tempmov.fec_contable  =  mov_contable.fec_contable 
                        tempmov.Hora          =  Mov_Contable.Hora
                        tempmov.num_documento =  mov_contable.num_documento
                        tempmov.db            =  mov_contable.db           
                        tempmov.cr            =  mov_contable.cr.
            END.
            MESSAGE "Voy a Procesar.."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
            FOR EACH tempmov 
                BREAK BY tempmov.agencia BY tempmov.Comprobante BY tempmov.Num_Documento:
            
                IF FIRST-OF(tempmov.Comprobante) THEN 
                   W_Consecutivo = tempmov.Num_Documento.
                ASSIGN TotCre = TotCre + tempmov.CR
                       TotDeb = TotDeb + tempmov.DB.
                IF LAST-OF(tempmov.Num_Documento) THEN DO:
                   REPEAT WHILE W_Consecutivo LT tempmov.Num_Documento:
                     CREATE TSDoc.
                     ASSIGN TSDoc.TS_Com = tempmov.Comprobante
                            TSDoc.TS_Age = tempmov.Agencia
                            TSDoc.TS_Doc = W_Consecutivo
                            TSDoc.TS_No  = NO.
                     W_Consecutivo = W_Consecutivo + 1.
                   END.
                   IF W_Consecutivo EQ tempmov.Num_Documento THEN DO:
                     CREATE TSDoc.
                     ASSIGN TSDoc.TS_Com = tempmov.Comprobante
                            TSDoc.TS_Age = tempmov.Agencia
                            TSDoc.TS_Doc = tempmov.Num_Documento
                            TSDoc.TS_CR  = TotCre
                            TSDoc.TS_DB  = TotDeb
                            TSDoc.TS_Fec = tempmov.Fec_Contable
                            TSDoc.TS_Hor = tempmov.Hora.
                     W_Consecutivo = W_Consecutivo + 1.
                   END.
                   ASSIGN TotCre = 0 TotDeb = 0.
                END.
            END.

/*                       BREAK BY Mov_Contable.agencia                          */
/*                             BY Mov_Contable.Comprobante                      */
/*                             BY Mov_Contable.Num_Documento:                   */
/*         /*         MESSAGE "Comprobante : " Mov_Contable.Comprobante */      */
/*         /*             VIEW-AS ALERT-BOX INFO BUTTONS OK.            */      */
/*         /*                                                           */      */
/*                 IF FIRST-OF(Mov_Contable.Comprobante) THEN                   */
/*                    W_Consecutivo = Mov_Contable.Num_Documento.               */
/*                 ASSIGN TotCre = TotCre + Mov_Contable.CR                     */
/*                        TotDeb = TotDeb + Mov_Contable.DB.                    */
/*                 IF LAST-OF(Mov_Contable.Num_Documento) THEN DO:              */
/*                    REPEAT WHILE W_Consecutivo LT Mov_Contable.Num_Documento: */
/*                      CREATE TSDoc.                                           */
/*                      ASSIGN TSDoc.TS_Com = Mov_Contable.Comprobante          */
/*                             TSDoc.TS_Age = Mov_Contable.Agencia              */
/*                             TSDoc.TS_Doc = W_Consecutivo                     */
/*                             TSDoc.TS_No  = NO.                               */
/*                      W_Consecutivo = W_Consecutivo + 1.                      */
/*                    END.                                                      */
/*                    IF W_Consecutivo EQ Mov_Contable.Num_Documento THEN DO:   */
/*                      CREATE TSDoc.                                           */
/*                      ASSIGN TSDoc.TS_Com = Mov_Contable.Comprobante          */
/*                             TSDoc.TS_Age = Mov_Contable.Agencia              */
/*                             TSDoc.TS_Doc = Mov_Contable.Num_Documento        */
/*                             TSDoc.TS_CR  = TotCre                            */
/*                             TSDoc.TS_DB  = TotDeb                            */
/*                             TSDoc.TS_Fec = Mov_Contable.Fec_Contable         */
/*                             TSDoc.TS_Hor = Mov_Contable.Hora.                */
/*                      W_Consecutivo = W_Consecutivo + 1.                      */
/*                    END.                                                      */
/*                    ASSIGN TotCre = 0 TotDeb = 0.                             */
/*                 END.                                                         */
/*             END.                                                             */
        END.
        ELSE
          IF viCpt1 = viCpt2 THEN
                FOR EACH Mov_Contable /*field(agencia comprobante fec_contable num_documento cuenta db cr)*/
                    WHERE (Mov_Contable.Comprobante  =  viCpt1 AND Mov_Contable.Comprobante   = viCpt2) AND 
                          (Mov_Contable.Fec_Contable >= pdt01  AND Mov_Contable.Fec_Contable <= pdt02)  AND 
                          (Mov_Contable.Agencia      >= viAge1 AND Mov_Contable.Agencia      <= viAge2) NO-LOCK
                          BREAK BY Mov_Contable.agencia  
                                BY Mov_Contable.Comprobante
                                BY Mov_Contable.Num_Documento:
            /*         MESSAGE "Comprobante : " Mov_Contable.Comprobante */
            /*             VIEW-AS ALERT-BOX INFO BUTTONS OK.            */
            /*                                                           */
                    IF FIRST-OF(Mov_Contable.Comprobante) THEN 
                       W_Consecutivo = Mov_Contable.Num_Documento.
                    ASSIGN TotCre = TotCre + Mov_Contable.CR
                           TotDeb = TotDeb + Mov_Contable.DB.
                    IF LAST-OF(Mov_Contable.Num_Documento) THEN DO:
                       REPEAT WHILE W_Consecutivo LT Mov_Contable.Num_Documento:
                         CREATE TSDoc.
                         ASSIGN TSDoc.TS_Com = Mov_Contable.Comprobante
                                TSDoc.TS_Age = Mov_Contable.Agencia
                                TSDoc.TS_Doc = W_Consecutivo
                                TSDoc.TS_No  = NO.
                         W_Consecutivo = W_Consecutivo + 1.
                       END.
                       IF W_Consecutivo EQ Mov_Contable.Num_Documento THEN DO:
                         CREATE TSDoc.
                         ASSIGN TSDoc.TS_Com = Mov_Contable.Comprobante
                                TSDoc.TS_Age = Mov_Contable.Agencia
                                TSDoc.TS_Doc = Mov_Contable.Num_Documento
                                TSDoc.TS_CR  = TotCre
                                TSDoc.TS_DB  = TotDeb
                                TSDoc.TS_Fec = Mov_Contable.Fec_Contable
                                TSDoc.TS_Hor = Mov_Contable.Hora.
                         W_Consecutivo = W_Consecutivo + 1.
                       END.
                       ASSIGN TotCre = 0 TotDeb = 0.
                    END.
                END.
      END.
        MESSAGE "Sali.."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

PROCEDURE ProcesoImprimir:
/*    {Incluido\RepEncabezado.i} */
    DEFINE VARIABLE W_EstadoInf   AS CHARACTER FORMAT "X(8)" INITIAL "".
    DEFINE VARIABLE Nom_Cencosto  AS CHARACTER FORMAT "X(2)" INITIAL "".
    W_Reporte   = "REPORTE   : CONSECUTIVOS ENTRE : " +
                   STRING(pdt01,"99/99/9999") + " y " + STRING(pdt02,"99/99/9999").
                  
                           /*1         2         3         4         5         6         7*/
                 /* 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
/*     W_EncColumna = "               DOC                  DEBITO           CREDITO         FECHA     HORA". */

    DEFINE VAR CtaAnt AS CHARACTER FORMAT "X".
    DEFINE VAR TT_Db  AS DECIMAL   FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99".
    DEFINE VAR TT_Cr  AS DECIMAL   FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99".
    DEFINE VAR TotIni AS DECIMAL   FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99".
    DEFINE VAR TotFin AS DECIMAL   FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99".
    DEFINE VAR Hora   AS CHARACTER FORMAT "X(10)".
    DEFINE VARIABLE vcNom AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE vcDesc AS CHARACTER   NO-UNDO.

/*     VIEW FRAME F-Encabezado. */
    VIEW FRAME f-ftr.
/*     FOR EACH IEx: DELETE IEx. END.                                                 */
/*     CREATE IEx.                                                                    */
/*     ASSIGN Ct = Ct + 1                                                             */
/*            IEx.NLinea = Ct                                                         */
/*            IEx.Linea  = "COMPROBANTE" + Cma + "DOCUMENTO" + Cma +                  */
/*                         "DEBITO" + Cma + "CREDITO" + Cma + "FECHA" + Cma + "HORA". */
    FOR EACH TSDoc 
        BREAK BY TSDoc.TS_Age BY TSDoc.TS_Com BY TSDoc.TS_Doc:

        FORM 
            vcNom   FORMAT "X(12)"
            vcDesc  FORMAT "X(40)"
            WITH FRAME FAge NO-BOX NO-LABEL WIDTH 132.

        FORM
            TSDoc.TS_Doc    COLUMN-LABEL "DOCUMENTO" FORMAT "     9999999"
            TSDoc.TS_Db     COLUMN-LABEL "DÉBITO"    FORMAT  "->>>,>>>,>>>,>>>,>>>,>>9.99"
            TSDoc.TS_Cr     COLUMN-LABEL "CREDITO"   FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99"
            TSDoc.TS_Fec    COLUMN-LABEL "FECHA"     FORMAT "99/99/9999"
            Hora            COLUMN-LABEL "HORA"      FORMAT "X(10)"
            WITH FRAME FRep DOWN COLUMN 1 WIDTH 132
                NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
        
        IF FIRST-OF(TSDoc.TS_Age) THEN DO:
            PAGE.
            DISPLAY "" WITH FRAME FRep.
           FIND Agencias WHERE Agencias.Agencia EQ TSDoc.TS_Age NO-LOCK NO-ERROR.
           IF AVAILABLE Agencias THEN DO:
             DISPLAY ("Age: " + (STRING(Agencias.Agencia,"999"))) @ vcNom
                      Agencias.Nombre @ vcDesc
             WITH FRAME FAge.
             DOWN WITH FRAME FAge.
/*              CREATE IEx.                                                           */
/*              ASSIGN Ct = Ct + 1                                                    */
/*                     IEx.NLinea = Ct                                                */
/*                     IEx.Linea  = STRING(Agencias.Agencia) + Cma + Agencias.Nombre. */
           END.
        END.
        IF FIRST-OF(TSDoc.TS_Com) THEN DO:
           FIND Comprobantes WHERE Comprobantes.Agencia EQ TSDoc.TS_Age AND
                Comprobantes.Comprobante EQ TSDoc.TS_Com NO-LOCK NO-ERROR.
           IF AVAILABLE Comprobantes THEN DO:
             DISPLAY    ("Comp:" + (STRING(Comprobantes.Comprobante,"999")))  @ vcNom
                        Comprobantes.Nombre @ vcDesc 
             WITH FRAME FAge.
             DOWN 1 WITH FRAME FAge.
           END.
        END.
        ASSIGN TT_Db = TT_Db + TSDoc.TS_DB
               TT_CR = TT_CR + TSDoc.TS_CR.
        IF TSDoc.TS_No EQ YES THEN DO:
           Hora = STRING(TSDoc.TS_Hor,"hh:mm:ss").
           DISPLAY 
            TSDoc.TS_Doc     
            TSDoc.TS_Db      
            TSDoc.TS_Cr      
            TSDoc.TS_Fec     
            Hora   
               WITH FRAME FRep.

/*                WITH WIDTH 160 /*132*/ FRAME F_Mov USE-TEXT STREAM-IO NO-LABELS NO-BOX. */
           /*informe excel*/
/*            CREATE IEx.                                                                   */
/*            ASSIGN Ct = Ct + 1                                                            */
/*                   IEx.NLinea = Ct                                                        */
/*                   IEx.Linea  = STRING(TSDoc.TS_Com) + Cma +                              */
/*                                STRING(TSDoc.TS_Doc) + Cma +                              */
/*                                STRING(TSDoc.TS_Db /*,"->>>.>>>.>>>.>>9.99"*/ )  + Cma +  */
/*                                STRING(TSDoc.TS_Cr /*,"->>>.>>>.>>>.>>9.99"*/ )  + Cma +  */
/*                                STRING(TSDoc.TS_Fec) + Cma +                              */
/*                                STRING(Hora).                                             */
        END.
        ELSE DO:
          IF choice THEN DO:
           Hora = "NoExiste".
           DISPLAY 
            TSDoc.TS_Doc      
            Hora              
               WITH FRAME FRep .
/*            WITH WIDTH 160 FRAME F_Mov2 USE-TEXT STREAM-IO NO-LABELS NO-BOX.  */
          END.
        END.
        IF LAST-OF(TSDoc.TS_Com) THEN DO:
            DOWN 1 WITH FRAME FRep.
            DISPLAY "      ---------------------" @ TSDoc.TS_Db
                    "      ---------------------" @ TSDoc.TS_Cr
           WITH FRAME FRep.
           DOWN 1 WITH FRAME FRep.

           DISPLAY ("T.Comp.: " + STRING(TSDoc.TS_Com,"999")) @ TSDoc.TS_Doc 
                   TT_Db @ TSDoc.TS_Db  
                   TT_Cr @ TSDoc.TS_Cr  
           WITH  FRAME FRep.
           DOWN 1 WITH FRAME FRep.
           ASSIGN TT_Db = 0 TT_Cr = 0.
        END.
        DOWN WITH FRAME FRep.
    END.  
    PAGE.
OUTPUT CLOSE.
END PROCEDURE.
RETURN.
