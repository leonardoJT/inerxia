
/* Par�metros */

/* Variables */
DEFINE VAR pAgencia AS INTEGER INITIAL 2.
DEFINE VAR pCenCostos AS INTEGER INITIAL 22.
DEFINE VAR pNit AS CHARACTER INITIAL "75147376".
DEFINE VAR pNumFactura AS CHARACTER INITIAL "2345".
DEFINE VAR pValor AS DECIMAL FORMAT "$>>>,>>>,>>9.99" INITIAL 1000000.
DEFINE VAR pIVA AS DECIMAL FORMAT "$>>>,>>>,>>9.99" INITIAL 160000.
DEFINE VAR pUsuario AS CHARACTER INITIAL "desarrollo".

/* Variables de procedimiento */
{Incluido\pdf_inc.i "NOT SUPER"}

/* Create stream for new PDF file */
RUN pdf_new IN h_PDFinc ("Spdf","C:\Info_Fodun\Reportes\FacturaAlojamiento-" + ".pdf").

pdf_PageFooter ("Spdf", THIS-PROCEDURE:HANDLE, "PageFooter").
pdf_PageHeader ("Spdf", THIS-PROCEDURE:HANDLE, "PageHeader").

/* Load FODUN Logo File */
RUN pdf_load_image IN h_PDFinc ("Spdf","Logo","imagenes\logo-fodun.jpg").

/* Margen inferior */
RUN pdf_set_BottomMargin IN h_PDFinc ("Spdf",20).

/* Instantiate a New Page */
RUN pdf_new_page IN h_PDFinc ("Spdf").

/* Loop through appropriate record set */

/*RUN end_of_report.*/

RUN pdf_close IN h_PDFinc ("Spdf").

/* -------------------- INTERNAL PROCEDURES -------------------------- */

PROCEDURE end_of_report:
  /* Display Footer UnderLine and End of Report Tag (Centered) */
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_stroke_fill IN h_PDFinc ("Spdf",1.0,1.0,1.0).
  RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", "End of Report", 250, pdf_TextY("Spdf") - 20, pdf_Text_Width("Spdf","End of Report"), 16, "Left",1).
END.

PROCEDURE PageFooter:
    RUN pdf_skip IN h_PDFinc ("Spdf").
    RUN pdf_set_dash IN h_PDFinc ("Spdf",1,0).
    RUN pdf_line IN h_PDFinc ("Spdf", 0, pdf_TextY("Spdf") - 5, pdf_PageWidth("Spdf") - 20 , pdf_TextY("Spdf") - 5, 1).
    RUN pdf_skip IN h_PDFinc ("Spdf").
    RUN pdf_skip IN h_PDFinc ("Spdf").
    RUN pdf_text_to IN h_PDFinc ("Spdf", "P�gina: " + STRING(pdf_page("Spdf")) + " de " + pdf_TotalPages("Spdf"), 97).
END.

PROCEDURE PageHeader:
    /* Place Logo but only on first page of Report */
    IF pdf_Page("Spdf") = 1 THEN DO:
        RUN pdf_place_image IN h_PDFinc ("Spdf",
                                         "Logo",
                                         pdf_LeftMargin("Spdf") + 20,
                                         pdf_TopMargin("Spdf") + 40,
                                         145,
                                         55).

        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",12).
        RUN pdf_text_color IN h_PDFinc ("Spdf",.50,0.0,0.0).

        RUN pdf_text_xy IN h_PDFinc ("Spdf", "FONDO DE EMPLEADOS DOCENTES",209,750).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "DE LA UNIVERSIDAD NACIONAL DE COLOMBIA",170,736).

        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",9).
        
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "NIT. 800.112.808-7",260,720).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "Resoluci�n No.320000993214 - Fecha: 2013/03/05",180,710).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "Facturaci�n autorizada del B-8001 al B-10000",188,700).

        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",12).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.5,0,0).
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf",0.5,0,0).
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 500, /* Eje Y */ 750, /* Ancho */ 120, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","FACTURA DE VENTA",505,745).


        
        FIND FIRST clientes WHERE clientes.nit = pNit NO-LOCK NO-ERROR.
        
        /*RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Oblique",12).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2,320,740).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "Direcci�n: " + clientes.dir_residencia, 320, 730).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "Tel�fono: " + clientes.tel_residencia, 320, 720).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "Celular: " + clientes.celular, 320, 710).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "Email: " + clientes.email, 320, 700).
        
        /* Set Header Font Size and Colour */
        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",17.5).
        RUN pdf_text_color IN h_PDFinc ("Spdf",1.0,1.0,1.0).

        /* Put a Rectangle around the Header */
        RUN pdf_stroke_color IN h_PDFinc ("Spdf", .50,.0,.0).
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf", .50,.0,.0).
        RUN pdf_rect IN h_PDFinc ("Spdf",
                                  pdf_LeftMargin("Spdf") + 30, /* Distancia del margen izquierdo */
                                  pdf_TextY("Spdf") - 75, /* Margen superior */
                                  pdf_PageWidth("Spdf") - 71, /* Ancho */
                                  25, /* Altura */
                                  0.5 /* Grosor de l�nea */).

        RUN pdf_place_image IN h_PDFinc ("Spdf",
                                         "Tarjeta",
                                         pdf_LeftMargin("Spdf") + 513,
                                         pdf_TopMargin("Spdf") + 72,
                                         55,18).

        RUN pdf_text_xy IN h_PDFinc ("Spdf","ESTADO DE CUENTA CUPO ROTATIVO",51,674).

        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",11).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.5,0,0).
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf",0.5,0,0).

        FIND FIRST creditos WHERE creditos.nit = clientes.nit AND creditos.cod_credito = 123 AND creditos.estado = 2 NO-LOCK NO-ERROR.

        /* T�tulos 1 */

        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",11).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.5,0,0).
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf",0.5,0,0).
        
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 641, /* Ancho */ 90, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","# DE CR�DITO",43,645).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 130, /* Eje Y */ 641, /* Ancho */ 90, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","PAGO M�NIMO",137,645).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 220, /* Eje Y */ 641, /* Ancho */ 110, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","TASA LIQUIDACI�N",222,645).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 330, /* Eje Y */ 641, /* Ancho */ 60, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","PLAZO",342,645).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 390, /* Eje Y */ 641, /* Ancho */ 90, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","CUPO APROBADO",392,645).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 480, /* Eje Y */ 641, /* Ancho */ 100, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","FECHA �LT PAGO",483,645).

        /* Detalles 1 */

        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",10).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 625, /* Ancho */ 90, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", STRING(creditos.num_credito,"999999999999"),48,629).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 130, /* Eje Y */ 625, /* Ancho */ 90, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.cuota,"$>>>,>>>,>>9"),137,629).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 220, /* Eje Y */ 625, /* Ancho */ 110, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.tasa / 12,">>9.99%"),250,629).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 330, /* Eje Y */ 625, /* Ancho */ 60, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.plazo,">> meses"),336,629).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 390, /* Eje Y */ 625, /* Ancho */ 90, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.monto,"$>>>,>>>,>>9"),401,629).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 480, /* Eje Y */ 625, /* Ancho */ 100, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.fec_ultPago,"99/99/9999"),500,629).


        /* T�tulos 2 */

        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",11).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.5,0,0).
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf",0.5,0,0).
        
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 599, /* Ancho */ 110, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","SALDO DE CAPITAL",42,603).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 150, /* Eje Y */ 599, /* Ancho */ 80, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","INTER�S",166,603).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 230, /* Eje Y */ 599, /* Ancho */ 95, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","FECHA DE CORTE",232,603).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 325, /* Eje Y */ 599, /* Ancho */ 80, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","D�AS ATRASO",329,603).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 405, /* Eje Y */ 599, /* Ancho */ 90, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","SALDO EN MORA",407,603).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 495, /* Eje Y */ 599, /* Ancho */ 85, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","INTERES MORA",496,603).

        /* Detalles 2 */

        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",10).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 583, /* Ancho */ 110, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", STRING(creditos.sdo_capital,"$>>>,>>>,>>9"),65,587).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 150, /* Eje Y */ 583, /* Ancho */ 80, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.int_corriente + creditos.int_difCobro,"$>>>,>>>,>>9"),151,587).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 230, /* Eje Y */ 583, /* Ancho */ 95, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(fec_corte,"99/99/9999"),247,587).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 325, /* Eje Y */ 583, /* Ancho */ 80, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.dias_atraso,">>9"),352,587).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 405, /* Eje Y */ 583, /* Ancho */ 90, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.val_atraso,"$>>>,>>>,>>9"),407,587).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 495, /* Eje Y */ 583, /* Ancho */ 85, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.INT_mora + creditos.INT_MoraDifCob,"$>>>,>>>,>>9"),497,587).


        /* T�tulos 3 */

        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",11).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.5,0,0).
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf",0.5,0,0).
        
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 557, /* Ancho */ 120, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","CUOTAS PENDIENTES",44,561).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 160, /* Eje Y */ 557, /* Ancho */ 100, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","CUOTAS EN MORA",164,561).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 260, /* Eje Y */ 557, /* Ancho */ 80, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","SALDO TOTAL",263,561).

        /* Detalles 3 */

        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier",10).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 541, /* Ancho */ 120, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", STRING(creditos.plazo - creditos.cuo_pagadas,">9"),100,545).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 160, /* Eje Y */ 541, /* Ancho */ 100, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.cuo_atraso,">9"),202,545).

        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 260, /* Eje Y */ 541, /* Ancho */ 80, /* Alto */ 16, /* Justificaci�n */ "Center", /* Ancho de l�nea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.sdo_capital + creditos.int_corriente + creditos.int_difCobro + creditos.int_morCobrar + creditos.INT_moraDifCob,"$>>>,>>>,>>9"),261,545).

        RUN pdf_set_font IN h_PDFinc ("Spdf","Courier-Bold",15).

        IF creditos.fec_pago <= fec_corte THEN
            pFecPago = "INMEDIATO".
        ELSE
            pFecPago = STRING(creditos.fec_pago,"99/99/9999").

        RUN pdf_text_xy IN h_PDFinc ("Spdf","PAGAR ANTES DE: " + pFecPago ,348,552).


        /* Display Header UnderLine */
        RUN pdf_set_dash IN h_PDFinc ("Spdf",1,0).
        /*RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin("Spdf"), pdf_TextY("Spdf") + 5, pdf_PageWidth("Spdf") - 20 , pdf_TextY("Spdf") + 5, 1).*/
        RUN pdf_skip IN h_PDFinc ("Spdf").*/
    END.
        /* Set Detail Font Colour */
    RUN pdf_text_color IN h_PDFinc ("Spdf",.0,.0,.0).
END. /* PageHeader */

IF VALID-HANDLE(h_PDFinc) THEN
  DELETE PROCEDURE h_PDFinc.

