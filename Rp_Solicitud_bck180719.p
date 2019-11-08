/* Parámetros */
DEFINE INPUT PARAMETER pNit AS CHARACTER.
DEFINE INPUT PARAMETER pNumSolicitud AS INTEGER.

/* Variables de procedimiento */
DEFINE VAR iHeight AS INTEGER.
DEFINE VAR iWidth AS INTEGER.
DEFINE VAR linea AS INTEGER.
DEFINE VAR linea2 AS INTEGER.
DEFINE VAR interlineado AS INTEGER.
DEFINE VAR facultad AS char.
DEFINE VAR Bfacultad AS char.
DEFINE VAR margen AS INTEGER.
DEFINE BUFFER Bclientes FOR Clientes.
DEFINE BUFFER Bfacultades FOR Facultades.
DEFINE BUFFER BAnexos_clientes FOR Anexos_Clientes.
DEFINE VAR i AS INTEGER INITIAL 1.
linea = 18.
linea2 = 7.
interlineado = 12.

{Incluido\pdf_inc.i "NOT SUPER"}

/* Create stream for new PDF file */
/*RUN pdf_new IN h_PDFinc ("Spdf","\\192.168.1.100\Aplicacion\Obj\Reportes\Solicitudes\" + pNit + "_" + CHAR(pNumSolicitud) + ".pdf").*/
RUN pdf_new IN h_PDFinc ("Spdf","Solicitudes\" + pNit + "_" + STRING(pNumSolicitud) + ".pdf").
/*RUN pdf_new IN h_PDFinc ("Spdf","Solicitudes\Solicitud.pdf").*/

pdf_PageFooter ("Spdf", THIS-PROCEDURE:HANDLE, "PageFooter").
pdf_PageHeader ("Spdf", THIS-PROCEDURE:HANDLE, "PageHeader").

/* Load FODUN Logo File */
RUN pdf_load_image IN h_PDFinc ("Spdf","Logo","imagenes\logo-fodun2018.jpg").
RUN pdf_load_image IN h_PDFinc ("Spdf","Tarjeta","imagenes\TarjetaFodun.jpg").
RUN pdf_load_font IN h_PDFinc ("Spdf","Futura","fonts\FuturaBookfont.ttf", "fonts\FuturaBookfont.afm","").
RUN pdf_load_font IN h_PDFinc ("Spdf","Futura-bold","fonts\futura heavy font.ttf", "fonts\futura heavy font.afm","").
RUN pdf_load_font IN h_PDFinc ("Spdf","Futura-medium","fonts\futura medium bt.ttf", "fonts\futura medium bt.afm","").
RUN pdf_load_font IN h_PDFinc ("Spdf","Futura-light","fonts\FuturaLightBT.ttf", "fonts\FuturaLightBT.afm","").
margen = pdf_LeftMargin("Spdf").

/* Margen inferior */
RUN pdf_set_BottomMargin IN h_PDFinc ("Spdf",30).

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
    RUN pdf_text_xy IN h_PDFinc ("Spdf", "Página: " + STRING(pdf_page("Spdf")) + " de " + pdf_TotalPages("Spdf"), 420, 30).
END.

PROCEDURE PageHeader:
    iHeight = pdf_PageHeight("Spdf") + 20.
    iWidth = pdf_PageWidth("Spdf").
    RUN pdf_watermark IN h_PDFinc ("Spdf","FODUN","Futura-light",120,.87,.87,.87,85,330).
    RUN pdf_place_image IN h_PDFinc ("Spdf",
                                         "Logo",
                                         pdf_LeftMargin("Spdf") + 30,
                                         pdf_TopMargin("Spdf") + 36,
                                         95,
                                         73).
    RUN pdf_text_color IN h_PDFinc ("Spdf",.50,0.0,0.0).   

    RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold", 17.5).
    RUN pdf_text_xy IN h_PDFinc ("Spdf","SOLICITUD DE CRÉDITO", 190, 725).
    RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold",9).
    RUN pdf_text_xy IN h_PDFinc ("Spdf","No. de solicitud", 420, 735).
    RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold",11).
    RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).
    RUN pdf_text_xy IN h_PDFinc ("Spdf", pNumSolicitud, 490, 735).
    
    /* Place Logo but only on first page of Report */
    IF pdf_Page("Spdf") = 1 THEN DO:
        FIND FIRST Solicitud WHERE Solicitud.Num_Solicitud EQ pNumSolicitud AND Solicitud.Nit EQ pNit EXCLUSIVE-LOCK NO-ERROR.
        
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold",11).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Fecha de solicitud: ", pdf_LeftMargin("Spdf") + 30, pdf_PageHeight("Spdf") - 743 + linea * 36 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Crédito solicitado: ", pdf_LeftMargin("Spdf") + 210, pdf_PageHeight("Spdf") - 743 + linea * 36 - interlineado).
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf", .84, .84, .84).
        RUN pdf_rect IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 33, (iWidth - 2 * 30), linea, 1).
        RUN pdf_text_center IN h_PDFinc ("Spdf", "Datos del asociado", (iWidth - 2 * 30) / 2, (iHeight - 743) + (linea * 34) - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Cédula de ciudadanía", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 33 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Nombre completo", pdf_LeftMargin("Spdf") + 270, iHeight - 743 + linea * 33 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Facultad", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 31 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Escuela, Área Curricular o Departamento", pdf_LeftMargin("Spdf") + 230, iHeight - 743 + linea * 31 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Dedicación y Categoría", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 29 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Dirección de residencia", pdf_LeftMargin("Spdf") + 270, iHeight - 743 + linea * 29 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Tel. celular", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 27 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Tel. residencia", pdf_LeftMargin("Spdf") + 120, iHeight - 743 + linea * 27 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Tel. oficina", pdf_LeftMargin("Spdf") + 235, iHeight - 743 + linea * 27 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","E-mail", pdf_LeftMargin("Spdf") + 370, iHeight - 743 + linea * 27 - interlineado).
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf", .84, .84, .84).
        RUN pdf_rect IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 24, (iWidth - 2 * 30), linea, 1).
        RUN pdf_text_center IN h_PDFinc ("Spdf", "Crédito solicitado", (iWidth - 2 * 30) / 2, (iHeight - 743) + (linea * 25) - interlineado).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura",11).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Monto", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 24 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "Plazo", pdf_LeftMargin("Spdf") + 220, iHeight - 743 + linea * 24 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "Destinación", pdf_LeftMargin("Spdf") + 330, iHeight - 743 + linea * 24 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Saldo créd. anterior: _________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 22 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Otros saldos pendientes: _________________", pdf_LeftMargin("Spdf") + 320, iHeight - 743 + linea * 22 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Observaciones: __________________________________________________________________________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 21 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Garantía __________________ Valor $________________ Fecha vencimiento: _____________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 20 - interlineado).
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf", .84, .84, .84).
        RUN pdf_rect IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 18, (iWidth - 2 * 30), linea, 1).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold",11).
        RUN pdf_text_center IN h_PDFinc ("Spdf", "Para estudio de capacidad de pago por caja diligenciar los dos siguientes recuadros", iWidth / 2, (iHeight - 743) + (linea * 19) - interlineado).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura",11).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Ingresos mensuales ________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 18 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Hipoteca valor comercial del inmueble: $___________", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 + linea * 18 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Egresos mensuales: ________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 17 - interlineado).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura",10).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Nota: 10 días hábiles para la entrega de certificación de tradi-", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 + linea * 17 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Nota: Adjuntar certificaciones de los ingresos", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 16 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","ción y libertad donde conste la anotación de la hipoteca", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 + linea * 16 - interlineado).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura",11).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","DATOS DEL CODEUDOR No. 1:", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 15 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","DATOS DEL CODEUDOR No. 2:", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 + linea * 15 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Nombre: ______________________________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 14 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Nombre: ______________________________________", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 + linea * 14 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","CC: ______________ Tel. residencial: ____________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 13 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","CC: ______________ Tel. residencial: ____________", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 + linea * 13 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Celular: _____________ Tel. oficina: _____________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 12 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Celular: ____________ Tel. oficina: ______________", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 + linea * 12 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Facultad: __________________________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 11 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Facultad: __________________________________", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 + linea * 11 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Departamento: ______________________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 10 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Departamento: ______________________________", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 + linea * 10 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Dirección: _________________________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 9 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Dirección: _________________________________", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 + linea * 9 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Firma: __________________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 8 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Firma: __________________________", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 + linea * 8 - interlineado).
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf", .84, .84, .84).
        RUN pdf_rect IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 6, (iWidth - 2 * 30), linea, 1).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold",11).
        RUN pdf_text_center IN h_PDFinc ("Spdf", "Datos para el desembolso", iWidth / 2, (iHeight - 743) + linea * 7 - interlineado).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura",11).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Autorizo a FODUN para transferir a:", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 6 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Información de terceros", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 + linea * 6 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Titular: __________________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 5 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Dirección: __________________________", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 + linea * 5 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","CC/NIT: __________________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 4 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","E-mail: __________________________", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 + linea * 4 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Cuenta: Corriente (  ) De ahorros (  )", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 3 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Tel. celular: __________________________", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 + linea * 3 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","No: __________________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 2 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Tel. residencia: __________________________", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 + linea * 2 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Banco: __________________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 1 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Tel. oficina: ____________________", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 + linea * 1 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","¿El tercero es asociado? SI (  ) NO (  )", (iWidth - 2 * 30) / 2 + 30, iHeight - 743 - interlineado).

        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 7, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 8, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 16, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, (iWidth - 2 * 30) / 2 - 5, linea * 7, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 7, (iWidth - 2 * 30) / 2 - 5, linea * 11, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 19, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 20, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 21, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 23, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 26, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 27, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 28, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 29, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 30, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 31, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 32, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 33, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 34, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 35, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 22, 180, linea * 2, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 22, 300, linea * 2, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 25, 90, linea * 2, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 25, 200, linea * 2, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 25, 340, linea * 2, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 27, 220, linea * 2, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 29, 180, linea * 2, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 31, 170, linea * 2, 1).

        
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura",11).

        RUN pdf_text_xy IN h_PDFinc ("Spdf", Solicitud.Fec_Solicitud, pdf_LeftMargin("Spdf") + 135, pdf_PageHeight("Spdf") - 743 + linea * 36 - interlineado).
        FIND FIRST Pro_Creditos WHERE Solicitud.Cod_Credito EQ Pro_Creditos.Cod_Credito NO-LOCK NO-ERROR.
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold",11).
        RUN pdf_text_color IN h_PDFinc ("Spdf",.50,0.0,0.0). 
        RUN pdf_text_xy IN h_PDFinc ("Spdf", caps(Pro_Creditos.Nom_Producto), pdf_LeftMargin("Spdf") + 310, pdf_PageHeight("Spdf") - 743 + linea * 36 - interlineado).

        FIND FIRST clientes WHERE clientes.nit = pNit NO-LOCK NO-ERROR.
        FIND FIRST Agencias WHERE Agencias.Agencia = Clientes.Agencia NO-LOCK NO-ERROR.
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold",9).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Agencia", 420, 722).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold",11).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", Agencias.Nombre, 460, 722).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura",11).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", pNit, pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 32 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2, pdf_LeftMargin("Spdf") + 270, iHeight - 743 + linea * 32 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", clientes.dir_residencia, pdf_LeftMargin("Spdf") + 270, iHeight - 743 + linea * 28 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", clientes.celular, pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 26 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", clientes.tel_residencia, pdf_LeftMargin("Spdf") + 120, iHeight - 743 + linea * 26 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", clientes.tel_comercial, pdf_LeftMargin("Spdf") + 235, iHeight - 743 + linea * 26 - interlineado).
        
        FIND FIRST Anexos_Clientes WHERE Anexos_Clientes.Nit = pNit NO-LOCK NO-ERROR.
        IF AVAILABLE(Anexos_Clientes) THEN DO:
            IF INT(Anexos_Clientes.Extencion_Comercial) NE 0 THEN
                RUN pdf_text_xy IN h_PDFinc ("Spdf", "Ext. " + string(Anexos_Clientes.Extencion_Comercial), pdf_LeftMargin("Spdf") + 295, iHeight - 743 + linea * 26 - interlineado).
        END.
        RUN pdf_text_xy IN h_PDFinc ("Spdf", clientes.email, pdf_LeftMargin("Spdf") + 370, iHeight - 743 + linea * 26 - interlineado).
        
        FIND FIRST facultades WHERE INT(facultades.codigo) = INT(clientes.facultad) AND INT(facultades.agencia) = INT(clientes.agencia) AND Facultades.tipo = "F" NO-LOCK NO-ERROR.
        IF AVAILABLE(facultades) THEN DO:
            RUN pdf_text_xy IN h_PDFinc ("Spdf", facultades.nombre, pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 30 - interlineado).
            facultad = facultades.codigo.
        END.
        
        FIND FIRST facultades WHERE INT(SUBSTRING(facultades.codigo,3,3))=INT(clientes.departamento) AND SUBSTRING(facultades.codigo,1,2)=facultad AND INT(facultades.agencia) = INT(clientes.agencia) AND Facultades.tipo = "D" NO-LOCK NO-ERROR. 
        IF AVAILABLE(facultades) THEN
            RUN pdf_text_xy IN h_PDFinc ("Spdf", facultades.nombre, pdf_LeftMargin("Spdf") + 230, iHeight - 743 + linea * 30 - interlineado).
        
        RUN pdf_text_xy IN h_PDFinc ("Spdf", string(round(Solicitud.Monto,0),"$>>>,>>>,>>9"), pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 23 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", Solicitud.Plazo / 30, pdf_LeftMargin("Spdf") + 220, iHeight - 743 + linea * 23 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "meses", pdf_LeftMargin("Spdf") + 250, iHeight - 743 + linea * 23 - interlineado).
        FIND FIRST Varios WHERE Varios.Tipo = 38
                        AND Varios.Codigo = INT(Solicitud.Destinof) NO-LOCK NO-ERROR.
        IF AVAILABLE(Varios) THEN
            RUN pdf_text_xy IN h_PDFinc ("Spdf", Varios.Descripcion, pdf_LeftMargin("Spdf") + 330, iHeight - 743 + linea * 23 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", string(round(cliente.Ing_Honorarios + cliente.Salario + cliente.Ing_Otros,0),"$>>>,>>>,>>9"), pdf_LeftMargin("Spdf") + 140, iHeight - 743 + linea * 18 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", string(round(cliente.Sdo_Obligaciones + cliente.Gto_Obligacion + cliente.Gto_Familiar + cliente.Gto_Arriendo,0),"$>>>,>>>,>>9"), pdf_LeftMargin("Spdf") + 140, iHeight - 743 + linea * 17 - interlineado).
        
        FOR EACH Relaciones WHERE int(Relaciones.Cuenta) = int(Solicitud.Num_Solicitud) AND Relaciones.Estado=1 NO-LOCK:
            FIND FIRST Bclientes WHERE Bclientes.nit = Relaciones.Nit_relacion NO-LOCK NO-ERROR.
            IF i > 1 THEN margen = margen - 42 + iWidth / 2.
            IF i > 2 THEN LEAVE.
            IF AVAILABLE(Bclientes) THEN DO:
                RUN pdf_text_xy IN h_PDFinc ("Spdf", Bclientes.nombre + " " + Bclientes.apellido1 + " " + Bclientes.apellido2, margen + 80, iHeight - 743 + linea * 14 - interlineado).
                RUN pdf_text_xy IN h_PDFinc ("Spdf", Relaciones.Nit_relacion, margen + 55, iHeight - 743 + linea * 13 - interlineado).
                RUN pdf_text_xy IN h_PDFinc ("Spdf", Bclientes.tel_residencia, margen + 214, iHeight - 743 + linea * 13 - interlineado).
                RUN pdf_text_xy IN h_PDFinc ("Spdf", Bclientes.tel_comercial, margen + 206, iHeight - 743 + linea * 12 - interlineado).
                FIND FIRST BAnexos_Clientes WHERE BAnexos_Clientes.Nit = Bclientes.nit NO-LOCK NO-ERROR.
                IF AVAILABLE(BAnexos_Clientes) THEN DO:
                    IF INT(BAnexos_Clientes.Extencion_Comercial) NE 0 THEN
                        RUN pdf_text_xy IN h_PDFinc ("Spdf", string(BAnexos_Clientes.Extencion_Comercial), pdf_LeftMargin("Spdf") + 236, iHeight - 743 + linea * 12 - interlineado).
                END.
                RUN pdf_text_xy IN h_PDFinc ("Spdf", Bclientes.celular, margen + 75, iHeight - 743 + linea * 12 - interlineado).
                FIND FIRST Bfacultades WHERE INT(Bfacultades.codigo) = INT(Bclientes.facultad) AND INT(Bfacultades.agencia) = INT(Bclientes.agencia) AND Bfacultades.tipo = "F" NO-LOCK NO-ERROR.
                IF AVAILABLE(Bfacultades) THEN DO:
                    RUN pdf_text_xy IN h_PDFinc ("Spdf", Bfacultades.nombre, margen + 80, iHeight - 743 + linea * 11 - interlineado).
                    Bfacultad = Bfacultades.codigo.
                END.
                FIND FIRST Bfacultades WHERE INT(SUBSTRING(Bfacultades.codigo,3,3))=INT(Bclientes.departamento) AND SUBSTRING(Bfacultades.codigo,1,2)=Bfacultad AND INT(Bfacultades.agencia) = INT(Bclientes.agencia) AND Bfacultades.tipo = "D" NO-LOCK NO-ERROR. 
                IF AVAILABLE(Bfacultades) THEN
                    RUN pdf_text_xy IN h_PDFinc ("Spdf", Bfacultades.nombre, margen + 110, iHeight - 743 + linea * 10 - interlineado).
                RUN pdf_text_xy IN h_PDFinc ("Spdf", Bclientes.dir_residencia, margen + 82, iHeight - 743 + linea * 9 - interlineado).
                i = i + 1.
            END.
        END.


        RUN pdf_set_dash IN h_PDFinc ("Spdf",1,0).
        /*RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin("Spdf"), pdf_TextY("Spdf") + 5, pdf_PageWidth("Spdf") - 20 , pdf_TextY("Spdf") + 5, 1).*/
        RUN pdf_skip IN h_PDFinc ("Spdf").
        RUN pdf_new_page IN h_PDFinc ("Spdf").
        iHeight = pdf_PageHeight("Spdf").
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743, iWidth - 2 * 30, linea * 36, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743, iWidth - 2 * 30, linea * 27, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743, iWidth - 2 * 30, linea * 26, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743, iWidth - 2 * 30, linea * 18, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743, iWidth - 2 * 30, linea * 14, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743, iWidth - 2 * 30, linea * 11, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 14, (iWidth - 2 * 30) / 2 - 5, linea * 12, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 120, iHeight - 743 + linea * 4, 450, linea * 3.5, 1).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold",9).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.5,0.0,0.0).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Agencia", 420, 722).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold",11).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", Agencias.Nombre, 460, 722).
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf", .84, .84, .84).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura", 6).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "Por la presente, autorizo al Fondo de Empleados Docentes de la Universidad Nacional de Colombia – FODUN o a quien represente sus derechos u ostente en el futuro la calidad de acreedor, a reportar,", pdf_LeftMargin("Spdf") + 23, iHeight - 743 + linea2 * 91).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "procesar, solicitar y divulgar a las centrales de riesgo o a cualquier otra entidad que maneje o administre bases de datos con los mismos fines, toda la información referente a mi comportamiento comercial", pdf_LeftMargin("Spdf") + 23, iHeight - 743 + linea2 * 90).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "con el FODUN. Lo anterior implica que el cumplimiento o incumplimiento de mis obligaciones se reflejará en las mencionadas bases de datos, en donde se consignan de manera completa todos los datos", pdf_LeftMargin("Spdf") + 23, iHeight - 743 + linea2 * 89).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "referentes a mi actual y pasado comportamiento frente al sector financiero y, en general, frente al cumplimiento de mis obligaciones.", pdf_LeftMargin("Spdf") + 23, iHeight - 743 + linea2 * 88).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "El asociado que supere los 70 años y que no haya tenido continuidad en los créditos, el seguro vida deudores no lo podrá cubrir.", pdf_LeftMargin("Spdf") + 23, iHeight - 743 + linea2 * 87).
        RUN pdf_text_center IN h_PDFinc ("Spdf", "PROTECCIÓN DE DATOS", (iWidth - 2 * 30) / 2, iHeight - 743 + linea2 * 86).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "De acuerdo con la Ley Estatutaria 1581 de 2.012 de Protección de Datos y con el Decreto 1377 de 2.013, doy mi consentimiento, como Titular de los datos, para que éstos sean incorporados en una base", pdf_LeftMargin("Spdf") + 23, iHeight - 743 + linea2 * 85).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "de datos responsabilidad de FONDO DE EMPLEADOS DOCENTES DE LA UNIVERSIDAD NACIONAL DE COLOMBIA, para que sean tratados con arreglo a los siguientes criterios:", pdf_LeftMargin("Spdf") + 23, iHeight - 743 + linea2 * 84).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", " • La finalidad del tratamiento será la que se defina en cada caso concreto, respetando en todo momento con los principios básicos que marca la Ley. • La posibilidad de ejercitar los derechos de acceso,", pdf_LeftMargin("Spdf") + 23, iHeight - 743 + linea2 * 83).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "corrección, supresión, revocación o reclamo por infracción sobre mis datos, con un escrito dirigido a FONDO DE EMPLEADOS DOCENTES DE LA UNIVERSIDAD NACIONAL DE COLOMBIA a la dirección", pdf_LeftMargin("Spdf") + 23, iHeight - 743 + linea2 * 82).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "de correo electrónico protecciondedatos@fodun.com.co, indicando en el asunto el derecho que desea ejercitar, o mediante correo ordinario remitido a CL 44 # 45-67 Unidad Camilo Torres Bl C8 Ps5 en", pdf_LeftMargin("Spdf") + 23, iHeight - 743 + linea2 * 81).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "Bogotá, Colombia. • La política de tratamiento a la que se encuentran sujetos los datos personales se podrá consultar o acceder por Correo electrónico y pagina Web.", pdf_LeftMargin("Spdf") + 23, iHeight - 743 + linea2 * 80).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "Asi mismo, FODUN cuenta con su propio esquema de SARLAFT, por el cual todos las operaciones del Fondo serán revisadas para disminuir el riesgo de LAFT, por lo que entiendo que la información", pdf_LeftMargin("Spdf") + 23, iHeight - 743 + linea2 * 79).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "suministrada será verificada según las instrucciones impartidas en la CE 004 de 2017 y las políticas establecidas en el manual SARLAFT del Fondo.", pdf_LeftMargin("Spdf") + 23, iHeight - 743 + linea2 * 78).
        
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura",11).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).

        RUN pdf_text_xy IN h_PDFinc ("Spdf","_________________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 30 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Firma del asociado", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 29 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","C.C." + clientes.nit, pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 28 - interlineado).
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf", .84, .84, .84).
        RUN pdf_rect IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 26, (iWidth - 2 * 30), linea, 1).
        RUN pdf_text_center IN h_PDFinc ("Spdf", "Para estudio financiero, uso exclusivo de FODUN", (iWidth - 2 * 30) / 2, (iHeight - 743) + (linea * 27) - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Análisis comité de créditos", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 26 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Documentos radicados:", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 25 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Calificación de riesgo:", pdf_LeftMargin("Spdf") + 300, iHeight - 743 + linea * 25 - interlineado).
        RUN pdf_set_TextY IN h_PDFinc ("Spdf", iHeight - 743 + linea * 24 - interlineado).
        RUN pdf_text_to IN h_PDFinc ("Spdf","Avalúo (SI) (NO)", 42).
        RUN pdf_text_center IN h_PDFinc ("Spdf","Central de riesgo", pdf_LeftMargin("Spdf") + (iWidth - 2 * 30) / 2 + (iWidth - 2 * 30) / 4, iHeight - 743 + linea * 24 - interlineado).
        RUN pdf_text_center IN h_PDFinc ("Spdf","Estado normal (  )  Moroso (  )", pdf_LeftMargin("Spdf") + (iWidth - 2 * 30) / 2 + (iWidth - 2 * 30) / 4, iHeight - 743 + linea * 23 - interlineado).
        RUN pdf_set_TextY IN h_PDFinc ("Spdf", iHeight - 743 + linea * 23 - interlineado).
        RUN pdf_text_to IN h_PDFinc ("Spdf","Cert. de entidad financiera (SI) (NO)", 42).
        RUN pdf_text_center IN h_PDFinc ("Spdf","FODUN", pdf_LeftMargin("Spdf") + (iWidth - 2 * 30) / 2 + (iWidth - 2 * 30) / 4, iHeight - 743 + linea * 22 - interlineado).
        RUN pdf_text_center IN h_PDFinc ("Spdf","Estado normal (  )  Moroso (  )", pdf_LeftMargin("Spdf") + (iWidth - 2 * 30) / 2 + (iWidth - 2 * 30) / 4, iHeight - 743 + linea * 21 - interlineado).
        RUN pdf_set_TextY IN h_PDFinc ("Spdf", iHeight - 743 + linea * 22 - interlineado).
        RUN pdf_text_to IN h_PDFinc ("Spdf","Garantía real (SI) (NO)", 42).
        RUN pdf_set_TextY IN h_PDFinc ("Spdf", iHeight - 743 + linea * 21 - interlineado).
        RUN pdf_text_to IN h_PDFinc ("Spdf","Póliza incendio (SI) (NO)", 42).
        RUN pdf_set_TextY IN h_PDFinc ("Spdf", iHeight - 743 + linea * 20 - interlineado).
        RUN pdf_text_to IN h_PDFinc ("Spdf","Notaría No. __________", 42).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Garantía exigida:", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 18 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Sistema de pago", pdf_LeftMargin("Spdf") + 300, iHeight - 743 + linea * 18 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Fiador ___________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 17 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Libranza __________", pdf_LeftMargin("Spdf") + 300, iHeight - 743 + linea * 17 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Cuota mes $__________", pdf_LeftMargin("Spdf") + 415, iHeight - 743 + linea * 17 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Real (hipoteca, pignoración) __________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 16 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Fecha vencimiento de la garantía: __________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 15 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Caja __________", pdf_LeftMargin("Spdf") + 300, iHeight - 743 + linea * 15 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Cuota mes $__________", pdf_LeftMargin("Spdf") + 415, iHeight - 743 + linea * 15 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","SARLAF: El oficial de cumplimiento certifica que consultó al tercero(s) en listas restrictivas :Si (  ) Fecha:________", pdf_LeftMargin("Spdf") + 28, iHeight - 743 + linea * 14 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","La valoración del O.C es: (  ) Favorable, se puede continuar con la operación.", pdf_LeftMargin("Spdf") + 60, iHeight - 743 + linea * 13 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","(  ) Desfavorable, se recomienda no continuar con la Operación.", pdf_LeftMargin("Spdf") + 200, iHeight - 743 + linea * 12 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Valoración final:", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 11 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Aprobado (  )   Rechazado (  )   Monto aprobado: $__________________", pdf_LeftMargin("Spdf") + 80, iHeight - 743 + linea * 10 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Fecha del pagaré: ___________  Resolución No.: ______  Interés de: _____%", pdf_LeftMargin("Spdf") + 80, iHeight - 743 + linea * 9 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Observaciones:", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 8 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Firma del comité de crédito:", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 4 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_____________________                          __________________________                       _____________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 2 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Firma miembro comité (1)                          Firma miembro comité (2)                      Firma miembro comité (3)", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea - interlineado).
    END.
    
    /* Set Detail Font Colour */
    RUN pdf_text_color IN h_PDFinc ("Spdf",.0,.0,.0).
END. /* PageHeader */

IF VALID-HANDLE(h_PDFinc) THEN
  DELETE PROCEDURE h_PDFinc.
