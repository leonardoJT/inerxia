/*
Proposito   : Listar los Comprobantes Contables Descuadrados., 
Date        : Dic/11/2007
By          : Félix Vargas
*/

{incluido\iprmt_rpt.i}
/* {incluido\igetfecha.i}  */
/* {incluido\igetSdo.i}    */

{incluido\Variable.i "SHARED"}
/** Temporales*/
/*   DEFINE VARIABLE W_Usuario   LIKE usuarios.usuario       INITIAL "308". /* 308 - Contabiliza*/ */
/*   DEFINE VARIABLE W_Fecha     AS DATE   INITIAL TODAY.                                          */
/*   DEFINE VARIABLE W_PathSpl   AS CHARACTER FORMAT "X(20)" INITIAL "c:\info_juriscoop\". */
/*   DEFINE VARIABLE W_Agencia   LIKE Agencia.Agencia        INITIAL "035". /*"035".*/             */
/*     DEFINE {1} VAR W_Estacion    LIKE Estaciones.Estacion.                                                   */
/* /*     DEFINE {1} VAR W_Usuario     LIKE Usuarios.Usuario. */                                                */
/*     DEFINE {1} VAR W_Clave       LIKE Usuarios.Clave FORMAT "X(16)".                                         */
/*     DEFINE {1} VAR W_Prioridad   LIKE Usuarios.Prioridad INITIAL "".                                         */
/* /*     DEFINE {1} VAR W_Agencia     LIKE Usuarios.Agencia INITIAL 0.  */                                     */
/*     DEFINE {1} VAR W_Ciudad      LIKE Agencia.Ciudad INITIAL 0.                                              */
/*     DEFINE {1} VAR W_Nom_Agencia   AS CHARACTER FORMAT "X(60)".                                              */
/*     DEFINE {1} VAR W_UbiDatos      AS CHAR INITIAL "D".                                                      */
/*     /*DEFINE {1} VAR W_Ninv        LIKE Inversion.Nro_inversion.*/                                           */
/*     DEFINE {1} VAR W_Nom_Entidad   AS CHARACTER FORMAT "X(60)".                                              */
/*     DEFINE {1} VAR W_Entidad     LIKE Entidad.Entidad.                                                       */
/*     DEFINE {1} VAR W_NitGlobal   LIKE Clientes.Nit INITIAL "".                                               */
/*     DEFINE {1} VAR W_SMLV        LIKE Indicadores.Valor INITIAL 0.                                           */
/*     DEFINE {1} VAR W_Manija        AS HANDLE.                                                                */
/*     DEFINE {1} VAR W_ManFin        AS HANDLE.                                                                */
/*     DEFINE {1} VAR W_ManTaq        AS HANDLE.                                                                */
/*     DEFINE {1} VAR W_Nivel       LIKE Cuentas.Nivel.                                                         */
/*     DEFINE {1} VAR W_CtaMay      LIKE Cuentas.Cuenta.                                                        */
/* /*     DEFINE {1} VAR W_Fecha         AS DATE FORMAT "99/99/9999" INITIAL TODAY.  */                         */
/*     DEFINE {1} VAR W_ficina        AS CHARACTER FORMAT "X(40)" VIEW-AS COMBO-BOX INNER-LINES 4 SIZE 40 BY 1. */
/*     DEFINE {1} VAR W_Path        LIKE Entidad.Dir_Programas.                                                 */
/* /*     DEFINE {1} VAR W_Pathspl     LIKE Entidad.Dir_Spl.  */                                                */
/*     DEFINE {1} VAR W_Eleccion      AS LOGICAL.                                                               */
/*     DEFINE {1} VAR W_CedGral     LIKE Clientes.Nit.                                                          */
/*     DEFINE {1} VAR W_CenCosGral  LIKE Cen_Costos.Cen_Costos.                                                 */
/*     DEFINE {1} VAR W_Cadena        AS CHARACTER FORMAT "X(9)" INITIAL "SIFINCOOP".                           */
/*     /*DEFINE     VAR Agencia_Cnt     AS INTEGER FORMAT "999".*/                                              */
/*     DEFINE {1} VAR P-Valida        AS LOGICAL.                                                               */
/*     DEFINE {1} VAR W_VCodPcto    LIKE Ahorros.Cod_Ahorro.                                                    */
/*     DEFINE {1} VAR W_VCueAhorro  LIKE Ahorros.Cue_Ahorros.                                                   */
/*     DEFINE {1} VAR W_Solicitud   LIKE Solicitud.Num_Solicitud.                                               */
/*     DEFINE {1} VAR W_PagareS     LIKE Creditos.Pagare.                                                       */
/*     DEFINE {1} VAR P_SdoTot      LIKE Creditos.Sdo_Capital.                                                  */
/*     DEFINE {1} VAR W_OfiCierre   LIKE Agencias.Agencia.                                                      */
/********************/

DEFINE VARIABLE qbf-count    AS INTEGER NO-UNDO.
DEFINE VARIABLE qbf-governor AS INTEGER NO-UNDO.
 
DEFINE VARIABLE qbf-govcnt AS INTEGER NO-UNDO.
DEFINE VARIABLE qbf-loop   AS INTEGER NO-UNDO.
DEFINE VARIABLE qbf-time   AS INTEGER NO-UNDO.

DEFINE VARIABLE viCpt1 AS INTEGER     NO-UNDO.
DEFINE VARIABLE viCpt2 AS INTEGER     NO-UNDO.
DEFINE VARIABLE viAge1 AS INTEGER     NO-UNDO.
DEFINE VARIABLE viAge2 AS INTEGER     NO-UNDO.
DEFINE VARIABLE vccuenta AS CHARACTER NO-UNDO.

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

DEFINE VARIABLE vdSaldo   AS DECIMAL  INITIAL 0 NO-UNDO.
DEFINE VARIABLE vdDebito  AS DECIMAL  INITIAL 0 NO-UNDO.
DEFINE VARIABLE vdCredito AS DECIMAL  INITIAL 0 NO-UNDO.
DEFINE VARIABLE vcNomCta  LIKE Cuentas.Nombre INITIAL "".
DEFINE VARIABLE vcNatura  LIKE Cuentas.Naturaleza INITIAL "".

DEFI TEMP-TABLE TempCtas
     FIELD Agen    LIKE mov_contable.Agencia
     FIELD Fecha   LIKE mov_contable.fec_contable
     FIELD Cpte    LIKE mov_contable.Comprobante
     FIELD Dcto    LIKE mov_contable.num_documento
     FIELD Debito  AS DECIMAL
     FIELD Credito AS DECIMAL
     FIELD Saldo   AS DECIMAL
     INDEX x3 agen fecha Cpte Dcto.

DEFINE TEMP-TABLE tempmov
 FIELD agencia        LIKE mov_contable.agencia
 FIELD comprobante    LIKE mov_contable.comprobante
 FIELD fec_contable   LIKE mov_contable.fec_contable
 FIELD num_documento  LIKE mov_contable.num_documento
 FIELD db             LIKE mov_contable.db
 FIELD cr             LIKE mov_contable.cr
 INDEX idxtodo agencia fec_contable comprobante num_documento.

EMPTY TEMP-TABLE TempCtas.
EMPTY TEMP-TABLE tempmov.

/*DO FOR Mov_Contable:*/
/*     {incluido\RepHeader.i}                                                                   */
/*     VIEW FRAME F-Encabezado.                                                                 */
/*     W_Reporte   = "REPORTE   : Cpte.Descuadrados: " + P_Titulo + " (prCptesDescuadrados.p) " */
/*                   + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").          */
/*     main-loop:  */
    IF (viCpt1 = 0 AND viCpt2 = 999) AND (viAge1 = 0 AND viAge2 = 999) AND (pdt01 NE ? AND pdt02 NE ?) THEN DO:
        FOR EACH Mov_Contable /*field(agencia comprobante fec_contable num_documento db cr)*/ WHERE
            Mov_Contable.Fec_Contable >= pdt01 AND Mov_Contable.Fec_Contable <= pdt02
            NO-LOCK BY mov_contable.agencia:
            CREATE tempmov.
            UPDATE tempmov.agencia       =  mov_contable.agencia      
                   tempmov.comprobante   =  mov_contable.comprobante  
                   tempmov.fec_contable  =  mov_contable.fec_contable 
                   tempmov.num_documento =  mov_contable.num_documento
                   tempmov.db            =  mov_contable.db           
                   tempmov.cr            =  mov_contable.cr.
        END.
        FOR EACH tempmov BREAK BY tempmov.agencia  BY tempmov.fec_contable
                         BY tempmov.Comprobante    BY tempmov.Num_Documento:
        
            IF FIRST-OF(tempmov.Agencia) OR FIRST-OF(tempmov.fec_contable) OR 
               FIRST-OF(tempmov.Comprobante) OR FIRST-OF(tempmov.num_documento) THEN
               ASSIGN vdSaldo   = 0
                      vdDebito  = 0
                      vdCredito = 0.
            
            ASSIGN vdDebito  = vdDebito  + tempmov.Db
                   vdCredito = vdCredito + tempmov.Cr.
            
            IF LAST-OF(tempmov.Agencia) OR LAST-OF(tempmov.fec_contable) OR
               LAST-OF(tempmov.Comprobante) OR LAST-OF(tempmov.num_documento) THEN DO:
               ASSIGN vdSaldo = vdCredito - vdDebito.
               IF vdSaldo NE 0 THEN DO:
                  CREATE TempCtas.
                  UPDATE TempCtas.Agen    = tempmov.agencia
                         TempCtas.Fecha   = tempmov.fec_contable
                         TempCtas.Cpte    = tempmov.Comprobante
                         TempCtas.Dcto    = tempmov.num_documento
                         TempCtas.Debito  = vdDebito
                         TempCtas.Credito = vdCredito
                         TempCtas.Saldo   = vdSaldo.
               END.
            END.
        END.
    END.
    ELSE
      IF viCpt1 = viCpt2 AND viAge1 = viAge2 AND pdt01 = pdt02 THEN DO:
         FOR EACH Mov_Contable /*field(agencia comprobante fec_contable num_documento cuenta db cr)*/
             WHERE Mov_Contable.Comprobante = viCpt1 AND Mov_Contable.Fec_Contable = pdt01 AND Mov_Contable.Agencia = viAge1 NO-LOCK
                   BREAK BY Mov_Contable.agencia  
                         BY Mov_Contable.fec_contable
                         BY Mov_Contable.Comprobante
                         BY Mov_Contable.Num_Documento:

             IF FIRST-OF(Mov_Contable.Agencia) OR FIRST-OF(Mov_Contable.fec_contable) OR 
                FIRST-OF(Mov_Contable.Comprobante) OR FIRST-OF(Mov_Contable.num_documento) THEN
                ASSIGN vdSaldo   = 0
                       vdDebito  = 0
                       vdCredito = 0.
            
             ASSIGN vdDebito  = vdDebito  + Mov_Contable.Db
                    vdCredito = vdCredito + Mov_Contable.Cr.
            
             IF LAST-OF(Mov_Contable.Agencia) OR LAST-OF(Mov_Contable.fec_contable) OR
                LAST-OF(Mov_Contable.Comprobante) OR LAST-OF(Mov_Contable.num_documento) THEN DO:
                ASSIGN vdSaldo = vdCredito - vdDebito.
                IF vdSaldo NE 0 THEN DO:
                   CREATE TempCtas.
                   UPDATE TempCtas.Agen    = mov_contable.agencia
                          TempCtas.Fecha   = mov_contable.fec_contable
                          TempCtas.Cpte    = mov_contable.Comprobante
                          TempCtas.Dcto    = mov_contable.num_documento
                          TempCtas.Debito  = vdDebito
                          TempCtas.Credito = vdCredito
                          TempCtas.Saldo   = abs(vdSaldo).
                END.
             END.
         END.
      END.
      ELSE 
        IF viCpt1 = viCpt2 AND viAge1 = viAge2 THEN DO:
           FOR EACH Mov_Contable /*field(agencia comprobante fec_contable num_documento cuenta db cr)*/
               WHERE Mov_Contable.Comprobante  =  viCpt1 AND 
                     (Mov_Contable.Fec_Contable >= pdt01 AND Mov_Contable.Fec_Contable <= pdt02)  AND 
                     Mov_Contable.Agencia      =  viAge1 NO-LOCK
                     BREAK BY Mov_Contable.agencia  
                           BY Mov_Contable.fec_contable
                           BY Mov_Contable.Comprobante
                           BY Mov_Contable.Num_Documento:
    
               IF FIRST-OF(Mov_Contable.Agencia) OR FIRST-OF(Mov_Contable.fec_contable) OR 
                  FIRST-OF(Mov_Contable.Comprobante) OR FIRST-OF(Mov_Contable.num_documento) THEN
                  ASSIGN vdSaldo   = 0
                         vdDebito  = 0
                         vdCredito = 0.
                
               ASSIGN vdDebito  = vdDebito  + Mov_Contable.Db
                      vdCredito = vdCredito + Mov_Contable.Cr.
                
               IF LAST-OF(Mov_Contable.Agencia) OR LAST-OF(Mov_Contable.fec_contable) OR
                  LAST-OF(Mov_Contable.Comprobante) OR LAST-OF(Mov_Contable.num_documento) THEN DO:
                  ASSIGN vdSaldo = vdCredito - vdDebito.
                  IF vdSaldo NE 0 THEN DO:
                     CREATE TempCtas.
                       UPDATE TempCtas.Agen    = mov_contable.agencia
                              TempCtas.Fecha   = mov_contable.fec_contable
                              TempCtas.Cpte    = mov_contable.Comprobante
                              TempCtas.Dcto    = mov_contable.num_documento
                              TempCtas.Debito  = vdDebito
                              TempCtas.Credito = vdCredito
                              TempCtas.Saldo   = abs(vdSaldo).
                  END.
               END.
           END.
        END.
        ELSE
          IF viAge1 = viAge2 THEN DO:
             FOR EACH Mov_Contable /*field(agencia comprobante fec_contable num_documento cuenta db cr)*/
                 WHERE (Mov_Contable.Comprobante  >= viCpt1 AND Mov_Contable.Comprobante  <= viCpt2) AND 
                       (Mov_Contable.Fec_Contable >= pdt01  AND Mov_Contable.Fec_Contable <= pdt02)  AND 
                       Mov_Contable.Agencia       =  viAge1 
                       BREAK BY Mov_Contable.agencia  
                             BY Mov_Contable.fec_contable
                             BY Mov_Contable.Comprobante
                             BY Mov_Contable.Num_Documento:
        
                 IF FIRST-OF(Mov_Contable.Agencia) OR FIRST-OF(Mov_Contable.fec_contable) OR 
                    FIRST-OF(Mov_Contable.Comprobante) OR FIRST-OF(Mov_Contable.num_documento) THEN
                    ASSIGN vdSaldo   = 0
                           vdDebito  = 0
                           vdCredito = 0.
                  
                 ASSIGN vdDebito  = vdDebito  + Mov_Contable.Db
                        vdCredito = vdCredito + Mov_Contable.Cr.
                  
                 IF LAST-OF(Mov_Contable.Agencia) OR LAST-OF(Mov_Contable.fec_contable) OR
                    LAST-OF(Mov_Contable.Comprobante) OR LAST-OF(Mov_Contable.num_documento) THEN DO:
                    ASSIGN vdSaldo = vdCredito - vdDebito.
                    IF vdSaldo NE 0 THEN DO:
                       CREATE TempCtas.
                         UPDATE TempCtas.Agen    = mov_contable.agencia
                                TempCtas.Fecha   = mov_contable.fec_contable
                                TempCtas.Cpte    = mov_contable.Comprobante
                                TempCtas.Dcto    = mov_contable.num_documento
                                TempCtas.Debito  = vdDebito
                                TempCtas.Credito = vdCredito
                                TempCtas.Saldo   = abs(vdSaldo).
                    END.
                 END.
             END.
          END.
          ELSE
            IF viCpt1 = viCpt2 THEN
               FOR EACH Mov_Contable /*field(agencia comprobante fec_contable num_documento cuenta db cr)*/
                   WHERE Mov_Contable.Comprobante  =  viCpt1  AND 
                         (Mov_Contable.Fec_Contable >= pdt01  AND Mov_Contable.Fec_Contable <= pdt02)  AND 
                         (Mov_Contable.Agencia      >= viAge1 AND Mov_Contable.Agencia      <= viAge2) NO-LOCK
                         BREAK BY Mov_Contable.agencia  
                               BY Mov_Contable.fec_contable
                               BY Mov_Contable.Comprobante
                               BY Mov_Contable.Num_Documento:
            
                   IF FIRST-OF(Mov_Contable.Agencia) OR FIRST-OF(Mov_Contable.fec_contable) OR 
                      FIRST-OF(Mov_Contable.Comprobante) OR FIRST-OF(Mov_Contable.num_documento) THEN
                      ASSIGN vdSaldo   = 0
                             vdDebito  = 0
                             vdCredito = 0.
                    
                   ASSIGN vdDebito  = vdDebito  + Mov_Contable.Db
                          vdCredito = vdCredito + Mov_Contable.Cr.
                    
                   IF LAST-OF(Mov_Contable.Agencia) OR LAST-OF(Mov_Contable.fec_contable) OR
                      LAST-OF(Mov_Contable.Comprobante) OR LAST-OF(Mov_Contable.num_documento) THEN DO:
                      ASSIGN vdSaldo = vdCredito - vdDebito.
                      IF vdSaldo NE 0 THEN DO:
                         CREATE TempCtas.
                           UPDATE TempCtas.Agen    = mov_contable.agencia
                                  TempCtas.Fecha   = mov_contable.fec_contable
                                  TempCtas.Cpte    = mov_contable.Comprobante
                                  TempCtas.Dcto    = mov_contable.num_documento
                                  TempCtas.Debito  = vdDebito
                                  TempCtas.Credito = vdCredito
                                  TempCtas.Saldo   = abs(vdSaldo).
                      END.
                   END.
               END.

      RUN PValida_Salida NO-ERROR.

PROCEDURE PValida_Salida:
IF P_NomArchivo EQ "DEFAULT" THEN
    ASSIGN P_NomArchivo = W_PathSpl + "\CptesDescuadrados.txt".
OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).

FOR EACH TempCtas: 
    FORM
        TempCtas.Agen    COLUMN-LABEL "Age"         FORMAT "999"
        TempCtas.Fecha   COLUMN-LABEL "Fecha"       FORMAT "99/99/9999"
        TempCtas.Cpte    COLUMN-LABEL "Comprobante" FORMAT "999"
        TempCtas.Dcto    COLUMN-LABEL "Documento"   FORMAT "999999999"
        TempCtas.Debito  COLUMN-LABEL "Debito"      FORMAT "->>>,>>>,>>>,>>9.99"
        TempCtas.Credito COLUMN-LABEL "Credito"     FORMAT "->>>,>>>,>>>,>>9.99"
        TempCtas.Saldo   COLUMN-LABEL "Diferencia"  FORMAT "->>>,>>>,>>>,>>9.99"
        WITH FRAME FCptDesc DOWN COLUMN 1 WIDTH 160
            NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
    DISPLAY 
        TempCtas.Agen   
        TempCtas.Fecha 
        TempCtas.Cpte
        TempCtas.Dcto  
        TempCtas.Debito 
        TempCtas.Credito
        TempCtas.Saldo  
      WITH FRAME FCptDesc.
    DOWN WITH FRAME FCptDesc.
END.
VIEW FRAME F-Ftr.
OUTPUT CLOSE.
END PROCEDURE.
RETURN.
