/*
    Reporte de Asesoría Credito PreScoring
Creado  : Giocam
Fecha   : Nov 18 / 2007
*/


{incluido\iprmt_rpt.i}
{incluido\Variable.i "SHARED"}

    DEFINE SHARED TEMP-TABLE TTAsesoria    
        FIELD   nit         LIKE    clientes.nit    FORMAT "X(14)"
        FIELD   cliente     AS CHARACTER            FORMAT "X(40)"
        FIELD   agencia     LIKE    clientes.Agencia
        FIELD   monto       AS INTEGER
        FIELD   plazo       AS INTEGER
        FIELD   numPers     AS  INTEGER
        FIELD   tasa        AS DECIMAL
        FIELD   porGtoSost  AS DECIMAL       
        FIELD   ingBruto    AS INTEGER
        FIELD   ingOtros    AS INTEGER
        FIELD   ingHonor    AS INTEGER
        FIELD   ingTotal    AS INTEGER
        FIELD   egrNomina   AS INTEGER
        FIELD   cap50       AS INTEGER
        FIELD   capSMLV     AS INTEGER
        FIELD   gtosSost    AS INTEGER
        FIELD   gtosVivi    AS INTEGER
        FIELD   gtosCIFIN   AS INTEGER
        FIELD   ingNeto50   AS INTEGER
        FIELD   egreMes50   AS INTEGER
        FIELD   ctaMax50     AS INTEGER 
        FIELD   CtaValSol50  AS INTEGER 
        FIELD   IngNetoSMLV  AS INTEGER 
        FIELD   egreMesSMLV  AS INTEGER 
        FIELD   ctaMaxSMLV   AS INTEGER 
        
        FIELD   plazo1      AS INTEGER
        FIELD   Monto1A      AS INTEGER
        FIELD   Monto1B      AS INTEGER
        FIELD   plazo2      AS INTEGER
        FIELD   Monto2A      AS INTEGER
        FIELD   Monto2B      AS INTEGER
        FIELD   plazo3      AS INTEGER
        FIELD   Monto3A      AS INTEGER
        FIELD   Monto3B      AS INTEGER
        FIELD   plazo4      AS INTEGER
        FIELD   Monto4A      AS INTEGER
        FIELD   Monto4B      AS INTEGER
        FIELD   plazo5      AS INTEGER
        FIELD   Monto5A      AS INTEGER
        FIELD   Monto5B      AS INTEGER.

DEFINE VARIABLE vcAgencia AS CHARACTER INITIAL "" NO-UNDO.
DEFINE VARIABLE vcCiudad AS CHARACTER   NO-UNDO.
DEFINE VARIABLE viSMLV AS INTEGER     NO-UNDO. /* Valor salario mínimo*/
DEFINE VARIABLE viNUMSMLV AS DECIMAL     NO-UNDO.


IF P_NomArchivo EQ "DEFAULT" THEN
    ASSIGN P_NomArchivo = SESSION:TEMP-DIRECTORY + "Asesoria.txt".

OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).

FIND FIRST TTAsesoria NO-LOCK NO-ERROR.
IF NOT AVAILABLE TTAsesoria THEN 
    RETURN.
FIND FIRST Agencias WHERE Agencias.agencia EQ TTAsesoria.Agencia NO-LOCK NO-ERROR.
IF AVAILABLE(Agencias) THEN DO:
    ASSIGN vcAgencia = STRING(Agencias.Agencia) + " - " + Agencias.nombre.
    FIND FIRST ubicacion WHERE ubicacion.ubicacion EQ agencia.ciudad NO-LOCK NO-ERROR.
    IF AVAILABLE(ubicacion) THEN 
        ASSIGN vcCiudad = Ubicacion.nombre.
END.    
FIND FIRST indicadores WHERE Indicadores.indicador = 21 NO-LOCK NO-ERROR.
ASSIGN viSMLV = Indicadores.Valor
    viNUMSMLV = monto / Indicadores.Valor.



    {incluido\RepHeader.i}
    
    VIEW FRAME F-Encabezado.
    W_Reporte   = "REPORTE   : PreScoring: " + P_Titulo + " (prAsesoriaPreScoring.p) " 
                  + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").

IF TTAsesoria.ctaMax50 > 0 AND TTAsesoria.ctaMaxSMLV > 0 THEN DO:
        RUN impNom50.
        RUN impNomSMLV.
END.
ELSE DO:
    IF TTAsesoria.Monto1A > 0 THEN DO:
        RUN impNomCajaOpcA.
    END.
    ELSE DO:
        RUN impNomCajaOpcB.
    END.
END.
    
OUTPUT CLOSE.

PROCEDURE impNom50:
    FORM
        "                FORMATO PRESCORING                      "                  AT ROW 3   COLUMN 10
        "--------------------------------------------------------"                  AT ROW 5   COLUMN 10
        TTAsesoria.nit          LABEL "Ident.      " FORMAT "X(12)"                 AT ROW 6    COLUMN 10
        TTAsesoria.cliente      LABEL "Cliente     " FORMAT "X(40)"                 AT ROW 7    COLUMN 10
        vcAgencia               LABEL "Age. Cliente" FORMAT "X(40)"                 AT ROW 8    COLUMN 10
        vcCiudad                LABEL "Ciudad      " FORMAT "X(40)"                 AT ROW 9    COLUMN 10
        "-------------------------------------------------------"                   AT ROW 10   COLUMN 10
        "          ASIGNACION DEL PRESTAMO SOLICITADO           "                   AT ROW 11   COLUMN 10     
        "-------------------------------------------------------"                   AT ROW 12   COLUMN 10
        TTAsesoria.monto        LABEL "Monto       "    FORMAT "$>,>>>,>>>,>>9"     AT ROW 13    COLUMN 10
        viNUMSMLV               LABEL "SMMLV a Prestar" FORMAT ">>9.99"             AT ROW 13    COLUMN 42
        TTAsesoria.plazo        LABEL "Plazo(Meses)"    FORMAT "99"                 AT ROW 14    COLUMN 10
        TTAsesoria.tasa         LABEL "Tasa NMV    "    FORMAT "99.99%"             AT ROW 15    COLUMN 10
        "-------------------------------------------------------"                   AT ROW 16   COLUMN 10
        "           INFORMACION FINANCIERA DE INGRESOS          "                   AT ROW 17   COLUMN 10     
        "-------------------------------------------------------"                   AT ROW 18   COLUMN 10
        TTAsesoria.ingBruto     LABEL "Ing. Brutos "    FORMAT "$>,>>>,>>>,>>9"     AT ROW 19    COLUMN 10 
        TTAsesoria.ingOtros     LABEL "Otros Ingr. "    FORMAT "$>,>>>,>>>,>>9"     AT ROW 20    COLUMN 10
        TTAsesoria.ingHonor     LABEL "Honorarios  "    FORMAT "$>,>>>,>>>,>>9"     AT ROW 21    COLUMN 10
        "_______________"                                                           AT ROW 22    COLUMN 24
        TTAsesoria.ingTotal     LABEL "Ing. Totales"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 23    COLUMN 10
        "-------------------------------------------------------"                   AT ROW 24   COLUMN 10
        " INFORMACION FINANCIERA DE EGRESOS CAPACIDAD AL 50%    "                   AT ROW 25   COLUMN 10     
        "-------------------------------------------------------"                   AT ROW 26   COLUMN 10
        TTAsesoria.egrNomina    LABEL "Desc. Nómina"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 27    COLUMN 10 
        TTAsesoria.cap50        LABEL "Cap.  al 50%"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 28    COLUMN 10 
        "_______________"                                                           AT ROW 29    COLUMN 24
        TTAsesoria.egreMes50    LABEL "Tot. Egresos"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 30    COLUMN 10 
        TTAsesoria.ingNeto50    LABEL "Ingreso Neto"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 31    COLUMN 10 
        "-------------------------------------------------------"                   AT ROW 32   COLUMN 10
        "         RESULTADOS PRESCORING CAPACIDAD AL 50%        "                   AT ROW 33   COLUMN 10     
        "-------------------------------------------------------"                   AT ROW 34   COLUMN 10
        TTAsesoria.ctaMax50     LABEL "Cuota Max.  "    FORMAT "$>,>>>,>>>,>>9"     AT ROW 35    COLUMN 10 
        TTAsesoria.CtaValSol50  LABEL "Cuta Val.Sol"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 36    COLUMN 10 
        "-------------------------------------------------------"                   AT ROW 37   COLUMN 10
        "  OPCIONES EN PLAZO(Meses) Y MONTO CAPACIDAD AL 50%    "                   AT ROW 39   COLUMN 10     
        "-------------------------------------------------------"                   AT ROW 40   COLUMN 10
        "  MESES                 MONTO                          "                   AT ROW 41   COLUMN 10     
        "-------------------------------------------------------"                   AT ROW 42   COLUMN 10
        TTAsesoria.plazo1                       NO-LABEL FORMAT "99"                AT ROW 43    COLUMN 14 
        TTAsesoria.Monto1A                      NO-LABEL FORMAT "$>>,>>>,>>>,>>9"   AT ROW 43    COLUMN 30
        TTAsesoria.plazo2                       NO-LABEL FORMAT "99"                AT ROW 44    COLUMN 14 
        TTAsesoria.Monto2A                      NO-LABEL FORMAT "$>>,>>>,>>>,>>9"   AT ROW 44    COLUMN 30
        TTAsesoria.plazo3                       NO-LABEL FORMAT "99"                AT ROW 45    COLUMN 14 
        TTAsesoria.Monto3A                      NO-LABEL FORMAT "$>>,>>>,>>>,>>9"   AT ROW 45    COLUMN 30
        TTAsesoria.plazo4                       NO-LABEL FORMAT "99"                AT ROW 46    COLUMN 14 
        TTAsesoria.Monto4A                      NO-LABEL FORMAT "$>>,>>>,>>>,>>9"   AT ROW 46    COLUMN 30
        TTAsesoria.plazo5                       NO-LABEL FORMAT "99"                AT ROW 47    COLUMN 14 
        TTAsesoria.Monto5A                      NO-LABEL FORMAT "$>>,>>>,>>>,>>9"   AT ROW 47    COLUMN 30
        "--------" AT ROW 55 COLUMN 10
    
    WITH FRAME FAsesoriaNom50
    SIDE-LABELS OVERLAY FONT 0 .
        
    /* {incluido\RepHeader.i} */
        
    DISPLAY 
        TTAsesoria.nit         
        TTAsesoria.cliente     
        vcAgencia
        vcCiudad
        TTAsesoria.monto
        viNUMSMLV
        TTAsesoria.plazo
        TTAsesoria.tasa
        TTAsesoria.ingBruto
        TTAsesoria.ingOtros
        TTAsesoria.ingHonor
        TTAsesoria.ingTotal
        TTAsesoria.egrNomina
        TTAsesoria.cap50
/*         TTAsesoria.gtosSost   */
/*         TTAsesoria.gtosVivi   */
/*         TTAsesoria.gtosCIFIN  */
        TTAsesoria.ingNeto50
        TTAsesoria.egreMes50
        TTAsesoria.ctaMax50
        TTAsesoria.CtaValSol50
        TTAsesoria.plazo1
        TTAsesoria.Monto1A
        TTAsesoria.plazo2
        TTAsesoria.Monto2A
        TTAsesoria.plazo3
        TTAsesoria.Monto3A
        TTAsesoria.plazo4
        TTAsesoria.Monto4A
        TTAsesoria.plazo5
        TTAsesoria.Monto5A
    
    WITH FRAME FAsesoriaNom50.

END PROCEDURE.


PROCEDURE impNomSMLV:
    FORM
        "                FORMATO PRESCORING                      "                  AT ROW 3   COLUMN 10
        "--------------------------------------------------------"                  AT ROW 5   COLUMN 10
        TTAsesoria.nit          LABEL "Ident.      " FORMAT "X(12)"                 AT ROW 6    COLUMN 10
        TTAsesoria.cliente      LABEL "Cliente     " FORMAT "X(40)"                 AT ROW 7    COLUMN 10
        vcAgencia               LABEL "Age. Cliente" FORMAT "X(40)"                 AT ROW 8    COLUMN 10
        vcCiudad                LABEL "Ciudad      " FORMAT "X(40)"                 AT ROW 9    COLUMN 10
        "-------------------------------------------------------"                   AT ROW 10   COLUMN 10
        "          ASIGNACION DEL PRESTAMO SOLICITADO            "                  AT ROW 11   COLUMN 10     
        "--------------------------------------------------------"                  AT ROW 12   COLUMN 10
        TTAsesoria.monto        LABEL "Monto       "    FORMAT "$>,>>>,>>>,>>9"     AT ROW 13    COLUMN 10
        viNUMSMLV               LABEL "SMMLV a Prestar" FORMAT ">>9.99"             AT ROW 13    COLUMN 42
        TTAsesoria.plazo        LABEL "Plazo(Meses)"    FORMAT "99"                 AT ROW 14    COLUMN 10
        TTAsesoria.tasa         LABEL "Tasa NMV    "    FORMAT "99.99%"             AT ROW 15    COLUMN 10
        "--------------------------------------------------------"                  AT ROW 16   COLUMN 10
        "          INFORMACION FINANCIERA DE INGRESOS            "                  AT ROW 17   COLUMN 10     
        "--------------------------------------------------------"                  AT ROW 18   COLUMN 10
        TTAsesoria.ingBruto     LABEL "Ing. Brutos "    FORMAT "$>,>>>,>>>,>>9"     AT ROW 19    COLUMN 10 
        TTAsesoria.ingOtros     LABEL "Otros Ingr. "    FORMAT "$>,>>>,>>>,>>9"     AT ROW 20    COLUMN 10
        TTAsesoria.ingHonor     LABEL "Honorarios  "    FORMAT "$>,>>>,>>>,>>9"     AT ROW 21    COLUMN 10
        "_______________"                                                           AT ROW 22    COLUMN 24
        TTAsesoria.ingTotal     LABEL "Ing. Totales"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 23    COLUMN 10
        "--------------------------------------------------------"                  AT ROW 24   COLUMN 10
        "  INFORMACION FINANCIERA DE EGRESOS CAPACIDAD AL SMMLV  "                  AT ROW 25   COLUMN 10     
        "--------------------------------------------------------"                  AT ROW 26   COLUMN 10
        TTAsesoria.egrNomina    LABEL "Desc. Nómina"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 27    COLUMN 10 
        TTAsesoria.capSMLV      LABEL "Cap. A SMMLV"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 28    COLUMN 10 
        "_______________"                                                           AT ROW 29    COLUMN 24
        TTAsesoria.egreMesSMLV  LABEL "Tot. Egresos"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 30    COLUMN 10 
        TTAsesoria.ingNetoSMLV  LABEL "Ingreso Neto"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 31    COLUMN 10 
        "--------------------------------------------------------"                  AT ROW 32   COLUMN 10
        "      RESULTADOS PRESCORING CAPACIDAD AL SMMLV          "                  AT ROW 33   COLUMN 10     
        "--------------------------------------------------------"                  AT ROW 34   COLUMN 10
        TTAsesoria.ctaMaxSMLV   LABEL "Cuota Max.  "    FORMAT "$>,>>>,>>>,>>9"     AT ROW 35    COLUMN 10 
        TTAsesoria.CtaValSol50  LABEL "Cuta Val.Sol"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 36    COLUMN 10 
        "--------------------------------------------------------"                  AT ROW 37   COLUMN 10
        "  OPCIONES EN PLAZO(Meses) Y MONTO CAPACIDAD AL SMMLV   "                  AT ROW 39   COLUMN 10     
        "--------------------------------------------------------"                  AT ROW 40   COLUMN 10
        "  MESES                 MONTO                           "                  AT ROW 41   COLUMN 10     
        "--------------------------------------------------------"                  AT ROW 42   COLUMN 10
        TTAsesoria.plazo1                       NO-LABEL FORMAT "99"                AT ROW 43    COLUMN 14 
        TTAsesoria.Monto1B                      NO-LABEL FORMAT "$>>,>>>,>>>,>>9"   AT ROW 43    COLUMN 30
        TTAsesoria.plazo2                       NO-LABEL FORMAT "99"                AT ROW 44    COLUMN 14 
        TTAsesoria.Monto2B                      NO-LABEL FORMAT "$>>,>>>,>>>,>>9"   AT ROW 44    COLUMN 30
        TTAsesoria.plazo3                       NO-LABEL FORMAT "99"                AT ROW 45    COLUMN 14 
        TTAsesoria.Monto3B                      NO-LABEL FORMAT "$>>,>>>,>>>,>>9"   AT ROW 45    COLUMN 30
        TTAsesoria.plazo4                       NO-LABEL FORMAT "99"                AT ROW 46    COLUMN 14 
        TTAsesoria.Monto4B                      NO-LABEL FORMAT "$>>,>>>,>>>,>>9"   AT ROW 46    COLUMN 30
        TTAsesoria.plazo5                       NO-LABEL FORMAT "99"                AT ROW 47    COLUMN 14 
        TTAsesoria.Monto5B                      NO-LABEL FORMAT "$>>,>>>,>>>,>>9"   AT ROW 47    COLUMN 30
    
        "--------" AT ROW 55 COLUMN 10
    WITH FRAME FAsesoriaNomSMLV
    SIDE-LABELS OVERLAY FONT 0 .
        
    /* {incluido\RepHeader.i} */
        
    DISPLAY 
        TTAsesoria.nit         
        TTAsesoria.cliente     
        vcAgencia
        vcCiudad
        TTAsesoria.monto
        viNUMSMLV
        TTAsesoria.plazo
        TTAsesoria.tasa
        TTAsesoria.ingBruto
        TTAsesoria.ingOtros
        TTAsesoria.ingHonor
        TTAsesoria.ingTotal
        TTAsesoria.egrNomina
        TTAsesoria.capSMLV
        TTAsesoria.CtaValSol50
        TTAsesoria.IngNetoSMLV
        TTAsesoria.egreMesSMLV
        TTAsesoria.ctaMaxSMLV
        TTAsesoria.plazo1
        TTAsesoria.Monto1B
        TTAsesoria.plazo2
        TTAsesoria.Monto2B
        TTAsesoria.plazo3
        TTAsesoria.Monto3B
        TTAsesoria.plazo4
        TTAsesoria.Monto4B
        TTAsesoria.plazo5
        TTAsesoria.Monto5B
    
    WITH FRAME FAsesoriaNomSMLV.

END PROCEDURE.

PROCEDURE impNomCajaOpcA:
    FORM
        "                FORMATO PRESCORING                      "                  AT ROW 3   COLUMN 10
        "--------------------------------------------------------"                  AT ROW 5   COLUMN 10
        TTAsesoria.nit          LABEL "Ident.      " FORMAT "X(12)"                 AT ROW 6    COLUMN 10
        TTAsesoria.cliente      LABEL "Cliente     " FORMAT "X(40)"                 AT ROW 7    COLUMN 10
        vcAgencia               LABEL "Age. Cliente" FORMAT "X(40)"                 AT ROW 8    COLUMN 10
        vcCiudad                LABEL "Ciudad      " FORMAT "X(40)"                 AT ROW 9    COLUMN 10
        "-------------------------------------------------------"                   AT ROW 10   COLUMN 10
        "          ASIGNACION DEL PRESTAMO SOLICITADO           "                   AT ROW 11   COLUMN 10     
        "-------------------------------------------------------"                   AT ROW 12   COLUMN 10
        TTAsesoria.monto        LABEL "Monto       "    FORMAT "$>,>>>,>>>,>>9"     AT ROW 13   COLUMN 10
        viNUMSMLV               LABEL "SMMLV a Prestar" FORMAT ">>9.99"             AT ROW 13   COLUMN 42
        TTAsesoria.plazo        LABEL "Plazo(Meses)"    FORMAT "99"                 AT ROW 14   COLUMN 10
        TTAsesoria.numpers      LABEL "Pers. a Cargo  "   FORMAT "99"               AT ROW 14   COLUMN 42
        TTAsesoria.tasa         LABEL "Tasa NMV    "    FORMAT "99.99%"             AT ROW 15   COLUMN 10
        TTAsesoria.porGtoSost   LABEL "Gasto Sostenim."   FORMAT "99.99%"           AT ROW 15   COLUMN 42
        "-------------------------------------------------------"                   AT ROW 16   COLUMN 10
        "           INFORMACION FINANCIERA DE INGRESOS          "                   AT ROW 17   COLUMN 10     
        "-------------------------------------------------------"                   AT ROW 18   COLUMN 10
        TTAsesoria.ingBruto     LABEL "Ing. Brutos "    FORMAT "$>,>>>,>>>,>>9"     AT ROW 19   COLUMN 10 
        TTAsesoria.ingOtros     LABEL "Otros Ingr. "    FORMAT "$>,>>>,>>>,>>9"     AT ROW 20   COLUMN 10
        TTAsesoria.ingHonor     LABEL "Honorarios  "    FORMAT "$>,>>>,>>>,>>9"     AT ROW 21   COLUMN 10
        "_______________"                                                           AT ROW 22   COLUMN 24
        TTAsesoria.ingTotal     LABEL "Ing. Totales"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 23   COLUMN 10
        "-------------------------------------------------------"                   AT ROW 24   COLUMN 10
        "         INFORMACION FINANCIERA DE EGRESOS             "                   AT ROW 25   COLUMN 10     
        "-------------------------------------------------------"                   AT ROW 26   COLUMN 10
        TTAsesoria.egrNomina    LABEL "Desc. Nómina"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 27   COLUMN 10 
        TTAsesoria.gtosSost     LABEL "Gtos. Sosten"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 28   COLUMN 10 
        TTAsesoria.gtosVivi     LABEL "Viv/Arriendo"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 29   COLUMN 10 
        TTAsesoria.gtosCIFIN    LABEL "Ctral. Riesg"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 30   COLUMN 10 
        "_______________"                                                           AT ROW 31   COLUMN 24
        TTAsesoria.egreMes50    LABEL "Tot. Egresos"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 32   COLUMN 10 
        TTAsesoria.ingNeto50    LABEL "Ingreso Neto"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 33   COLUMN 10 
        "-------------------------------------------------------"                   AT ROW 34   COLUMN 10
        "             RESULTADOS PRESCORING                     "                   AT ROW 35   COLUMN 10     
        "-------------------------------------------------------"                   AT ROW 36   COLUMN 10
        TTAsesoria.ctaMax50     LABEL "Cuota Max.  "    FORMAT "$>,>>>,>>>,>>9"     AT ROW 37   COLUMN 10 
        TTAsesoria.CtaValSol50  LABEL "Cuta Val.Sol"    FORMAT "$>,>>>,>>>,>>9"     AT ROW 38   COLUMN 10 
        "-------------------------------------------------------"                   AT ROW 39   COLUMN 10
        "        OPCIONES EN PLAZO(Meses) Y MONTO               "                   AT ROW 41   COLUMN 10     
        "-------------------------------------------------------"                   AT ROW 42   COLUMN 10
        "  MESES                 MONTO                          "                   AT ROW 43   COLUMN 10     
        "-------------------------------------------------------"                   AT ROW 44   COLUMN 10
        TTAsesoria.plazo1                       NO-LABEL FORMAT "99"                AT ROW 45   COLUMN 14 
        TTAsesoria.Monto1A                      NO-LABEL FORMAT "$>>,>>>,>>>,>>9"   AT ROW 45   COLUMN 30
        TTAsesoria.plazo2                       NO-LABEL FORMAT "99"                AT ROW 46   COLUMN 14 
        TTAsesoria.Monto2A                      NO-LABEL FORMAT "$>>,>>>,>>>,>>9"   AT ROW 46   COLUMN 30
        TTAsesoria.plazo3                       NO-LABEL FORMAT "99"                AT ROW 47   COLUMN 14 
        TTAsesoria.Monto3A                      NO-LABEL FORMAT "$>>,>>>,>>>,>>9"   AT ROW 47   COLUMN 30
        TTAsesoria.plazo4                       NO-LABEL FORMAT "99"                AT ROW 48   COLUMN 14 
        TTAsesoria.Monto4A                      NO-LABEL FORMAT "$>>,>>>,>>>,>>9"   AT ROW 48   COLUMN 30
        TTAsesoria.plazo5                       NO-LABEL FORMAT "99"                AT ROW 49   COLUMN 14 
        TTAsesoria.Monto5A                      NO-LABEL FORMAT "$>>,>>>,>>>,>>9"   AT ROW 49   COLUMN 30
        "--------" AT ROW 55 COLUMN 10
    
    WITH FRAME FAsesoriaCajaA
    SIDE-LABELS OVERLAY FONT 0 .
        
    /* {incluido\RepHeader.i} */
        
    DISPLAY 
        TTAsesoria.nit         
        TTAsesoria.cliente     
        vcAgencia
        vcCiudad
        TTAsesoria.monto
        viNUMSMLV
        TTAsesoria.plazo
        TTAsesoria.numpers
        TTAsesoria.tasa
        TTAsesoria.porGtoSost
        TTAsesoria.ingBruto
        TTAsesoria.ingOtros
        TTAsesoria.ingHonor
        TTAsesoria.ingTotal
        TTAsesoria.egrNomina
        TTAsesoria.gtosSost  
        TTAsesoria.gtosVivi  
        TTAsesoria.gtosCIFIN 

        TTAsesoria.ingNeto50
        TTAsesoria.egreMes50
        TTAsesoria.ctaMax50
        TTAsesoria.CtaValSol50
        TTAsesoria.plazo1
        TTAsesoria.Monto1A
        TTAsesoria.plazo2
        TTAsesoria.Monto2A
        TTAsesoria.plazo3
        TTAsesoria.Monto3A
        TTAsesoria.plazo4
        TTAsesoria.Monto4A
        TTAsesoria.plazo5
        TTAsesoria.Monto5A
    
    WITH FRAME FAsesoriaCajaA.

END PROCEDURE.

