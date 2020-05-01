/* Parámetros */
DEFINE INPUT PARAMETER pNit AS CHARACTER.
DEFINE INPUT PARAMETER pNumcredito AS INTEGER.
DEFINE INPUT PARAMETER pFecCorte AS DATE.
DEFINE INPUT PARAMETER pTasa AS DECIMAL. 

/* Variables */
/*DEFINE VAR pNit AS CHARACTER INITIAL "10099010".
DEFINE VAR pNumCredito AS INTEGER INITIAL 10053.
DEFINE VAR pFecCorte AS DATE INITIAL 04/16/2018.
DEFINE VAR pNit AS CHARACTER INIT "19117326".
DEFINE VAR pNumcredito AS INTEGER INIT 3932.
DEFINE VAR pFecCorte AS DATE INIT 04/17/2020.
DEFINE VAR pTasa AS DECIMAL INIT 0.8.*/


/* Variables de procedimiento */
DEFINE VAR pFecPago AS CHARACTER.
DEFINE VAR posY AS INTEGER INITIAL 514.
DEFINE VAR interlineado AS INTEGER INIT 14.
DEFINE VAR anteriorSombra AS INTEGER INIT 1.

{Incluido\pdf_inc.i "NOT SUPER"}

/* Create stream for new PDF file */
/*RUN pdf_new IN h_PDFinc ("Spdf","Reportes\Prueba" + pNit + ".pdf").*/
RUN pdf_new IN h_PDFinc ("Spdf","Reportes\FacturasCupos\" + pNit + ".pdf").

pdf_PageFooter ("Spdf", THIS-PROCEDURE:HANDLE, "PageFooter").
pdf_PageHeader ("Spdf", THIS-PROCEDURE:HANDLE, "PageHeader").

/* Load FODUN Logo File */
RUN pdf_load_image IN h_PDFinc ("Spdf","Logo","imagenes\logo-fodun2018.jpg").
RUN pdf_load_image IN h_PDFinc ("Spdf","Tarjeta","imagenes\TarjetaFodun.jpg").
RUN pdf_load_font IN h_PDFinc ("Spdf","Futura","fonts\FuturaBookfont.ttf", "fonts\FuturaBookfont.afm","").
RUN pdf_load_font IN h_PDFinc ("Spdf","Futura-bold","fonts\futura heavy font.ttf", "fonts\futura heavy font.afm","").
RUN pdf_load_font IN h_PDFinc ("Spdf","Futura-medium","fonts\futura medium bt.ttf", "fonts\futura medium bt.afm","").
RUN pdf_load_font IN h_PDFinc ("Spdf","Futura-light","fonts\FuturaLightBT.ttf", "fonts\FuturaLightBT.afm","").

/* Margen inferior */
RUN pdf_set_BottomMargin IN h_PDFinc ("Spdf",30).

/* Instantiate a New Page */
RUN pdf_new_page IN h_PDFinc ("Spdf").

FIND FIRST clientes WHERE clientes.nit = pNit NO-LOCK NO-ERROR.
FIND FIRST creditos WHERE creditos.nit = clientes.nit
                          AND creditos.num_credito = pNumCredito
                          AND creditos.cod_credito = 123
                          AND creditos.estado = 2 NO-LOCK NO-ERROR.

RUN TituloUtilizaciones.

FOR EACH utilizacionesRotativo WHERE utilizacionesRotativo.cliente_id = pNit
                AND utilizacionesRotativo.credito_id = creditos.num_credito
                AND utilizacionesRotativo.estado = 1
                AND utilizacionesRotativo.fec_utilizacion > ADD-INTERVAL(pFecCorte,-1,"months") + 1
                AND utilizacionesRotativo.fec_utilizacion <= pFecCorte NO-LOCK BY utilizacionesRotativo.fec_utilizacion
                                                                    BY utilizacionesRotativo.hora_utilizacion:
    IF posY < 40 THEN DO:
        RUN pdf_line  IN h_PDFinc ("Spdf",40,posY + interlineado - 3,510,posY + interlineado - 3,0.5).
        RUN pdf_new_page IN h_PDFinc ("Spdf").
        posY = 645.
        RUN TituloUtilizaciones.
    END.
    
    IF anteriorSombra MODULO 2 = 0 THEN DO:
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf", .84, .84, .84).
        RUN pdf_stroke_color IN h_PDFinc ("Spdf",1.0,1.0,1.0).
        RUN pdf_rect IN h_PDFinc ("Spdf", 40, posY - 3, 470, interlineado, 0).
    END.
    RUN pdf_stroke_color IN h_PDFinc ("Spdf",0,0,0).
    RUN pdf_line  IN h_PDFinc ("Spdf",40,posY - 3,40,posY + interlineado,0.5).
    RUN pdf_line  IN h_PDFinc ("Spdf",140,posY - 3,140,posY + interlineado,0.5).
    RUN pdf_line  IN h_PDFinc ("Spdf",290,posY - 3,290,posY + interlineado,0.5).
    RUN pdf_line  IN h_PDFinc ("Spdf",370,posY - 3,370,posY + interlineado,0.5).
    RUN pdf_line  IN h_PDFinc ("Spdf",429,posY - 3,429,posY + interlineado,0.5).
    RUN pdf_line  IN h_PDFinc ("Spdf",510,posY - 3,510,posY + interlineado,0.5).

    RUN pdf_text_xy IN h_PDFinc ("Spdf", string(utilizacionesRotativo.fec_utilizacion) + " - " + string(utilizacionesRotativo.hora_utilizacion),46,posY).
    RUN pdf_text_xy IN h_PDFinc ("Spdf",utilizacionesRotativo.descripcion,150,posY).
    RUN pdf_text_align IN h_PDFinc ("Spdf",string(round(utilizacionesRotativo.monto,0),"$>>>,>>>,>>9"),"RIGHT",360,posY).
    RUN pdf_text_align IN h_PDFinc ("Spdf",STRING(utilizacionesRotativo.tasa,">>9.99%"),"RIGHT",410,posY).
    RUN pdf_text_align IN h_PDFinc ("Spdf",string(round(utilizacionesRotativo.saldo,0),"$>>>,>>>,>>9"),"RIGHT",494,posY).
    posY = posY - interlineado.
    anteriorSombra = anteriorSombra + 1.
END.
RUN pdf_line  IN h_PDFinc ("Spdf",40,posY + interlineado - 3,510,posY + interlineado - 3,0.5).

posY = posY - 11.
IF posY < 70 THEN DO:
    RUN pdf_new_page IN h_PDFinc ("Spdf").
    posY = 645.
END.

RUN TituloMovimientos.

FOR EACH mov_creditos WHERE mov_creditos.nit = pNit
                        AND mov_creditos.num_credito = creditos.num_credito
                        AND mov_creditos.fecha > ADD-INTERVAL(pFecCorte,-1,"months") + 1
                        AND mov_Creditos.fecha <= pFecCorte
                        AND mov_creditos.cod_operacion <> 999999999 NO-LOCK BY mov_creditos.fecha
                                                                            BY mov_creditos.hora:
    IF posY < 40 THEN DO:
        RUN pdf_line  IN h_PDFinc ("Spdf",40,posY + interlineado - 3,510,posY + interlineado - 3,0.5).
        RUN pdf_new_page IN h_PDFinc ("Spdf").
        posY = 645.
        RUN TituloMovimientos.
    END.

    IF anteriorSombra MODULO 2 = 0 THEN DO:
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf", .84, .84, .84).
        RUN pdf_stroke_color IN h_PDFinc ("Spdf",1.0,1.0,1.0).
        RUN pdf_rect IN h_PDFinc ("Spdf", 40, posY - 3, 470, interlineado, 0).
    END.

    RUN pdf_stroke_color IN h_PDFinc ("Spdf",0,0,0).
    RUN pdf_line  IN h_PDFinc ("Spdf",40,posY - 3,40,posY + interlineado,0.5).
    RUN pdf_line  IN h_PDFinc ("Spdf",100,posY - 3,100,posY + interlineado,0.5).
    RUN pdf_line  IN h_PDFinc ("Spdf",159,posY - 3,159,posY + interlineado,0.5).
    RUN pdf_line  IN h_PDFinc ("Spdf",330,posY - 3,330,posY + interlineado,0.5).
    RUN pdf_line  IN h_PDFinc ("Spdf",415,posY - 3,415,posY + interlineado,0.5).
    RUN pdf_line  IN h_PDFinc ("Spdf",510,posY - 3,510,posY + interlineado,0.5).
    
    RUN pdf_text_xy IN h_PDFinc ("Spdf",mov_creditos.fecha,48,posY).

    FIND FIRST operacion WHERE operacion.cod_operacion = mov_creditos.cod_operacion NO-LOCK NO-ERROR.
    IF AVAILABLE operacion THEN DO:
        IF operacion.tipo_operacion = 1 THEN
            RUN pdf_text_xy IN h_PDFinc ("Spdf","Pago",118,posY).
        ELSE
            RUN pdf_text_xy IN h_PDFinc ("Spdf","Avance",118,posY).
    END.

    RUN pdf_text_xy IN h_PDFinc ("Spdf",mov_creditos.descrip,165,posY).
    RUN pdf_text_align IN h_PDFinc ("Spdf",string(round(mov_creditos.val_efectivo + mov_creditos.val_cheque,0),"$>>>,>>>,>>9"),"RIGHT", 400,posY).
    RUN pdf_text_align IN h_PDFinc ("Spdf",string(round(mov_creditos.sdo_capital,0),"$>>>,>>>,>>9"),"RIGHT", 494,posY).

    posY = posY - interlineado.
    anteriorSombra = anteriorSombra + 1.
END.

RUN pdf_line  IN h_PDFinc ("Spdf",40,posY + interlineado - 3,510,posY + interlineado - 3,0.5).

/* Display Header UnderLine */
RUN pdf_set_dash IN h_PDFinc ("Spdf",1,0).
/*RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin("Spdf"), pdf_TextY("Spdf") + 5, pdf_PageWidth("Spdf") - 20 , pdf_TextY("Spdf") + 5, 1).*/
RUN pdf_skip IN h_PDFinc ("Spdf").

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
    RUN pdf_line IN h_PDFinc ("Spdf", 40, pdf_TextY("Spdf") - 5, pdf_PageWidth("Spdf") - 35 , pdf_TextY("Spdf") - 5, 1).
    RUN pdf_skip IN h_PDFinc ("Spdf").
    RUN pdf_skip IN h_PDFinc ("Spdf").
    RUN pdf_text_to IN h_PDFinc ("Spdf", "Página: " + STRING(pdf_page("Spdf")) + " de " + pdf_TotalPages("Spdf"), 120).
END.

PROCEDURE PageHeader:
    /* Marca de agua 
    RUN pdf_watermark IN h_PDFinc ("Spdf","FODUN","Futura-light",120,.87,.87,.87,85,330).*/

    RUN pdf_place_image IN h_PDFinc ("Spdf",
                                     "Logo",
                                     pdf_LeftMargin("Spdf") + 30,
                                     pdf_TopMargin("Spdf") + 43,
                                     80,
                                     61).

    RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).
    
    FIND FIRST clientes WHERE clientes.nit = pNit NO-LOCK NO-ERROR.
    
    RUN pdf_set_font IN h_PDFinc ("Spdf","Futura", 10).
    RUN pdf_text_xy IN h_PDFinc ("Spdf", clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2,320,740).
    RUN pdf_text_xy IN h_PDFinc ("Spdf", "Dirección: " + clientes.dir_residencia, 320, 730).
    RUN pdf_text_xy IN h_PDFinc ("Spdf", "Teléfono: " + clientes.tel_residencia, 320, 720).
    RUN pdf_text_xy IN h_PDFinc ("Spdf", "Celular: " + clientes.celular, 320, 710).
    RUN pdf_text_xy IN h_PDFinc ("Spdf", "Email: " + clientes.email, 320, 700).
    
    /* Set Header Font Size and Colour */
    RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold",17.5).
    RUN pdf_text_color IN h_PDFinc ("Spdf",1.0,1.0,1.0).

    /* Put a Rectangle around the Header */
    RUN pdf_stroke_color IN h_PDFinc ("Spdf", .50,.0,.0).
    RUN pdf_stroke_fill IN h_PDFinc ("Spdf", .50,.0,.0).
    RUN pdf_rect IN h_PDFinc ("Spdf",
                              pdf_LeftMargin("Spdf") + 30, /* Distancia del margen izquierdo */
                              pdf_TextY("Spdf") - 75, /* Margen superior */
                              pdf_PageWidth("Spdf") - 140, /* Ancho */
                              25, /* Altura */
                              0.5 /* Grosor de línea */).

    RUN pdf_place_image IN h_PDFinc ("Spdf",
                                     "Tarjeta",
                                     pdf_LeftMargin("Spdf") + 430,
                                     pdf_TopMargin("Spdf") + 72,
                                     55,18).

    RUN pdf_text_xy IN h_PDFinc ("Spdf","Estado de cuenta cupo rotativo",51,674).

    RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-Bold",11).
    RUN pdf_text_color IN h_PDFinc ("Spdf",0.5,0,0).
    RUN pdf_stroke_fill IN h_PDFinc ("Spdf",0.5,0,0).

    FIND FIRST creditos WHERE creditos.nit = clientes.nit
                          AND creditos.num_credito = pNumCredito
                          AND creditos.cod_credito = 123
                          AND creditos.estado = 2 NO-LOCK NO-ERROR.
    
    IF pdf_Page("Spdf") = 1 THEN DO:
        /* Títulos 1 */
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-Bold",11).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.5,0,0).
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf",0.5,0,0).
        
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 641, /* Ancho */ 90, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Núm de crédito",43,645).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 130, /* Eje Y */ 641, /* Ancho */ 80, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Pago mínimo",136,645).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 210, /* Eje Y */ 641, /* Ancho */ 65, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Tasa actual",212,645).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 275, /* Eje Y */ 641, /* Ancho */ 48, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Plazo",283,645).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 323, /* Eje Y */ 641, /* Ancho */ 90, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Cupo aprobado",326,645).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 413, /* Eje Y */ 641, /* Ancho */ 97, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Fecha últ.pago",420,645).
    
        /* Detalles 1 */
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura",10).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 625, /* Ancho */ 90, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", STRING(creditos.num_credito,"999999999999"),48,629).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 130, /* Eje Y */ 625, /* Ancho */ 80, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.cuota,"$>>>,>>>,>>9"),137,629).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 210, /* Eje Y */ 625, /* Ancho */ 65, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(pTasa,">>9.99%"),223,629).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 275, /* Eje Y */ 625, /* Ancho */ 48, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.plazo,">> meses"),279,629).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 323, /* Eje Y */ 625, /* Ancho */ 90, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.monto,"$>>>,>>>,>>9"),340,629).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 413, /* Eje Y */ 625, /* Ancho */ 97, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.fec_ultPago,"99/99/9999"),435,629).
    
    
        /* Títulos 2 */
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-Bold",11).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.5,0,0).
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf",0.5,0,0).
        
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 599, /* Ancho */ 94, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Saldo de capital",43,603).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 134, /* Eje Y */ 599, /* Ancho */ 65, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Interés",150,603).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 199, /* Eje Y */ 599, /* Ancho */ 80, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Fecha de corte",201,603).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 279, /* Eje Y */ 599, /* Ancho */ 69, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Días atraso",285,603).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 348, /* Eje Y */ 599, /* Ancho */ 83, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Saldo en mora",351,603).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 431, /* Eje Y */ 599, /* Ancho */ 79, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Interés mora",434,603).
    
        /* Detalles 2 */
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura",10).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 583, /* Ancho */ 94, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", STRING(creditos.sdo_capital,"$>>>,>>>,>>9"),55,587).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 134, /* Eje Y */ 583, /* Ancho */ 65, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.int_corriente + creditos.int_difCobro,"$>>>,>>>,>>9"),134,587).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 199, /* Eje Y */ 583, /* Ancho */ 80, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(pFecCorte,"99/99/9999"),208,587).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 279, /* Eje Y */ 583, /* Ancho */ 69, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.dias_atraso,">>9"),302,587).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 348, /* Eje Y */ 583, /* Ancho */ 83, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.val_atraso,"$>>>,>>>,>>9"),353,587).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 431, /* Eje Y */ 583, /* Ancho */ 79, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.INT_mora + creditos.INT_MoraDifCob,"$>>>,>>>,>>9"),430,587).
    
    
        /* Títulos 3 */
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-Bold",11).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.5,0,0).
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf",0.5,0,0).
        
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 557, /* Ancho */ 108, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Cuotas pendientes",44,561).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 148, /* Eje Y */ 557, /* Ancho */ 92, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Cuotas en mora",151,561).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 240, /* Eje Y */ 557, /* Ancho */ 80, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Disponible",252,561).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 320, /* Eje Y */ 557, /* Ancho */ 80, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Saldo total",333,561).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 400, /* Eje Y */ 557, /* Ancho */ 110, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Pagar antes de",410,561).
    
    
    
        /* Detalles 3 */
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura",10).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 40, /* Eje Y */ 541, /* Ancho */ 108, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", STRING(creditos.plazo - creditos.cuo_pagadas,">9"),90,545).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 148, /* Eje Y */ 541, /* Ancho */ 92, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.cuo_atraso,">9"),190,545).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 240, /* Eje Y */ 541, /* Ancho */ 80, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.monto - creditos.sdo_capital,"$>>>,>>>,>>9"),247,545).
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 320, /* Eje Y */ 541, /* Ancho */ 80, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf",STRING(creditos.sdo_capital + creditos.int_corriente + creditos.int_difCobro + creditos.int_morCobrar + creditos.INT_moraDifCob,"$>>>,>>>,>>9"),330,545).
    
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-Bold",12).
    
        IF creditos.fec_pago <= pFecCorte THEN
            pFecPago = "Inmediato".
        ELSE
            pFecPago = STRING(creditos.fec_pago,"99/99/9999").
    
        RUN pdf_text_boxed_xy IN h_PDFinc ("Spdf", /* Texto */ "", /* Eje X */ 400, /* Eje Y */ 541, /* Ancho */ 110, /* Alto */ 16, /* Justificación */ "Center", /* Ancho de línea */ 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", pFecPago,420,545).
    END.
    /* Set Detail Font Colour */
    RUN pdf_text_color IN h_PDFinc ("Spdf",.0,.0,.0).
END. /* PageHeader */

PROCEDURE TituloUtilizaciones:
    /* Encabezados Detalle */
    RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-Bold",12).
    RUN pdf_text_color IN h_PDFinc ("Spdf",0.5,0,0).
    RUN pdf_stroke_fill IN h_PDFinc ("Spdf",0.5,0,0).
    
    RUN pdf_text_xy IN h_PDFinc ("Spdf","Detalle de utilizaciones",210,posY).
    RUN pdf_rect2 IN h_PDFinc ("Spdf", 40, posY - 3, 470, 16, 1).
    posY = posY - 16.
    
    RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-Bold",11).
    RUN pdf_text_xy IN h_PDFinc ("Spdf","Fecha",52,posY).
    RUN pdf_text_xy IN h_PDFinc ("Spdf","Descripción",190,posY).
    RUN pdf_text_xy IN h_PDFinc ("Spdf","Valor",320,posY).
    RUN pdf_text_xy IN h_PDFinc ("Spdf","Tasa",385,posY).
    RUN pdf_text_xy IN h_PDFinc ("Spdf","Saldo",445,posY).    
    RUN pdf_rect2 IN h_PDFinc ("Spdf", 40, posY - 3, 100, 16, 1).
    RUN pdf_rect2 IN h_PDFinc ("Spdf", 40, posY - 3, 250, 16, 1).
    RUN pdf_rect2 IN h_PDFinc ("Spdf", 40, posY - 3, 330, 16, 1).
    RUN pdf_rect2 IN h_PDFinc ("Spdf", 40, posY - 3, 389, 16, 1).
    RUN pdf_rect2 IN h_PDFinc ("Spdf", 40, posY - 3, 470, 16, 1).

    RUN pdf_set_font IN h_PDFinc ("Spdf","Futura",10).
    RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).

    posY = posY - interlineado.

END.

PROCEDURE TituloMovimientos:
    RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-Bold",12).
    RUN pdf_text_color IN h_PDFinc ("Spdf",0.5,0,0).
    RUN pdf_stroke_fill IN h_PDFinc ("Spdf",0.5,0,0).
    
    RUN pdf_text_xy IN h_PDFinc ("Spdf","Detalle de movimientos",210,posY).
    RUN pdf_rect2 IN h_PDFinc ("Spdf", 40, posY - 3, 470, 16, 1).
    
    posY = posY - 16.
    
    RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-Bold",11).
    RUN pdf_text_xy IN h_PDFinc ("Spdf","Fecha",52,posY).
    RUN pdf_text_xy IN h_PDFinc ("Spdf","Tipo",122,posY).
    RUN pdf_text_xy IN h_PDFinc ("Spdf","Descripción",187,posY).
    RUN pdf_text_xy IN h_PDFinc ("Spdf","Valor",360,posY).
    RUN pdf_text_xy IN h_PDFinc ("Spdf","Saldo capital",435,posY).
    RUN pdf_rect2 IN h_PDFinc ("Spdf", 40, posY - 3, 60, 16, 1).
    RUN pdf_rect2 IN h_PDFinc ("Spdf", 40, posY - 3, 119, 16, 1).
    RUN pdf_rect2 IN h_PDFinc ("Spdf", 40, posY - 3, 290, 16, 1).
    RUN pdf_rect2 IN h_PDFinc ("Spdf", 40, posY - 3, 375, 16, 1).
    RUN pdf_rect2 IN h_PDFinc ("Spdf", 40, posY - 3, 470, 16, 1).

    RUN pdf_set_font IN h_PDFinc ("Spdf","Futura",10).
    RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).

    posY = posY - interlineado.
END.


IF VALID-HANDLE(h_PDFinc) THEN
  DELETE PROCEDURE h_PDFinc.
