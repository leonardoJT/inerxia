/* Parámetros */
DEFINE INPUT PARAMETER pNit AS CHARACTER.
DEFINE INPUT PARAMETER pCuenta AS INTEGER.
DEFINE INPUT PARAMETER pMonto AS DECIMAL.

/* Variables de procedimiento */
DEFINE VAR iHeight AS INTEGER.
DEFINE VAR iWidth AS INTEGER.
DEFINE VAR linea AS INTEGER.
DEFINE VAR linea2 AS INTEGER.
DEFINE VAR interlineado AS INTEGER.
DEFINE VAR facultad AS char.
DEFINE VAR Bfacultad AS char.
DEFINE VAR margen AS INTEGER.
DEFINE VAR i AS INTEGER INITIAL 1.
linea = 18.
linea2 = 7.
interlineado = 12.

{Incluido\pdf_inc.i "NOT SUPER"}

RUN pdf_new IN h_PDFinc ("Spdf","FormatosEfectivoSARLAFT\" + pNit + "_" + string(DAY(NOW),"99") + string(MONTH(NOW),"99") + STRING(YEAR(NOW),"9999") + "_" + STRING(TIME) + ".pdf").

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
END.

PROCEDURE PageHeader:
    iHeight = pdf_PageHeight("Spdf").
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
    RUN pdf_text_xy IN h_PDFinc ("Spdf","Reporte de transacciones en efectivo iguales", 170, 745).
    RUN pdf_text_xy IN h_PDFinc ("Spdf","o superiores a 10 millones de pesos", 200, 725).
    
    /* Place Logo but only on first page of Report */
    IF pdf_Page("Spdf") = 1 THEN DO:
        FIND FIRST clientes WHERE clientes.nit = pNit NO-LOCK NO-ERROR.
        FIND FIRST Agencias WHERE Agencias.Agencia = Clientes.Agencia NO-LOCK NO-ERROR.
        
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 37, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 34, 1).

        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold",11).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Agencia", 300, 705).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold",11).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", Agencias.Nombre, 345, 705).
        
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold",11).
        RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,0.0,0.0).

        RUN pdf_stroke_fill IN h_PDFinc ("Spdf", .84, .84, .84).
        RUN pdf_rect IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 35, (iWidth - 2 * 30), linea, 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Fecha de transacción:             Número de cuenta afectada:              Valor de la operación:", pdf_LeftMargin("Spdf") + 30, pdf_PageHeight("Spdf") - 743 + linea * 36 - interlineado).
        
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 34, 140, linea * 2, 1).
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 34, 345, linea * 2, 1).

        RUN pdf_stroke_fill IN h_PDFinc ("Spdf", .84, .84, .84).
        RUN pdf_rect IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 33, iWidth - 2 * 30, linea, 1).
        RUN pdf_text_center IN h_PDFinc ("Spdf", "Datos del asociado", (iWidth - 2 * 30) / 2, (iHeight - 743) + linea * 34 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","CC/CE: ___________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 33 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Nombre:  _________________________________________", (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 33 - interlineado).
		RUN pdf_text_xy IN h_PDFinc ("Spdf","Tel/Cel: __________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 32 - interlineado).
		RUN pdf_text_xy IN h_PDFinc ("Spdf","Domicilio: _______________________________________", (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 32 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Facultad: _______________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 31 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Firma: ___________________", (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 31 - interlineado).

        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura",11).
        
        RUN pdf_text_xy IN h_PDFinc ("Spdf", today, pdf_LeftMargin("Spdf") + 30, pdf_PageHeight("Spdf") - 743 + linea * 35 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", STRING(TIME,"HH:MM:SS"), pdf_LeftMargin("Spdf") + 90, pdf_PageHeight("Spdf") - 743 + linea * 35 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", pCuenta, pdf_LeftMargin("Spdf") + 195, pdf_PageHeight("Spdf") - 743 + linea * 35 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", "$" + string(pMonto), pdf_LeftMargin("Spdf") + 400, pdf_PageHeight("Spdf") - 743 + linea * 35 - interlineado).

		RUN pdf_text_xy IN h_PDFinc ("Spdf", pNit, pdf_LeftMargin("Spdf") + 90, iHeight - 743 + linea * 33 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf", clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2, ((iWidth - 2 * 30) / 2) + 65, iHeight - 743 + linea * 33 - interlineado).
        
		IF clientes.celular <> "" THEN
			RUN pdf_text_xy IN h_PDFinc ("Spdf", clientes.celular, pdf_LeftMargin("Spdf") + 90, iHeight - 743 + linea * 32 - interlineado).
		ELSE IF clientes.tel_residencia <> "" THEN
			RUN pdf_text_xy IN h_PDFinc ("Spdf", clientes.tel_residencia, pdf_LeftMargin("Spdf") + 90, iHeight - 743 + linea * 32 - interlineado).
		ELSE DO:
            RUN pdf_text_xy IN h_PDFinc ("Spdf", clientes.tel_comercial, pdf_LeftMargin("Spdf") + 90, iHeight - 743 + linea * 32 - interlineado).
			FIND FIRST Anexos_Clientes WHERE Anexos_Clientes.Nit = pNit NO-LOCK NO-ERROR.
			IF AVAILABLE(Anexos_Clientes) THEN DO:
				IF INT(Anexos_Clientes.Extencion_Comercial) NE 0 THEN
					RUN pdf_text_xy IN h_PDFinc ("Spdf", "Ext. " + string(Anexos_Clientes.Extencion_Comercial), pdf_LeftMargin("Spdf") + 295, iHeight - 743 + linea * 26 - interlineado).
			END.
        END.
		
		RUN pdf_text_xy IN h_PDFinc ("Spdf", clientes.dir_residencia, ((iWidth - 2 * 30) / 2) + 65, iHeight - 743 + linea * 32 - interlineado).
        FIND FIRST facultades WHERE INT(facultades.codigo) = INT(clientes.facultad) AND INT(facultades.agencia) = INT(clientes.agencia) AND Facultades.tipo = "F" NO-LOCK NO-ERROR.
        IF AVAILABLE(facultades) THEN DO:
            RUN pdf_text_xy IN h_PDFinc ("Spdf", facultades.nombre, pdf_LeftMargin("Spdf") + 90, iHeight - 743 + linea * 31 - interlineado).
            facultad = facultades.codigo.
        END.
          
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold",11).
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf", .84, .84, .84).
        RUN pdf_rect IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 29, (iWidth - 2 * 30), linea, 1).
        RUN pdf_text_center IN h_PDFinc ("Spdf", "Datos de quien físicamente realiza la operación", (iWidth - 2 * 30) / 2, (iHeight - 743) + linea * 30 - interlineado).
        
        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura",11).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Es el mismo asociado quien realiza la operación físicamente en la oficina: SI (  ) NO (  )", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 29 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","* Si respondió NO por favor diligencie los datos a continuación:", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 28 - interlineado).

        RUN pdf_set_font IN h_PDFinc ("Spdf","Futura-bold",11).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","CC/CE: _____________________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 27 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Nombre: ____________________________", (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 27 - interlineado).
		RUN pdf_text_xy IN h_PDFinc ("Spdf","Tel/Cel: _____________________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 26 - interlineado).
		RUN pdf_text_xy IN h_PDFinc ("Spdf","Domicilio: ___________________________", (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 26 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Facultad: ____________________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 25 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Firma: _______________________________", (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 25 - interlineado).

        RUN pdf_stroke_fill IN h_PDFinc ("Spdf", .84, .84, .84).
        RUN pdf_rect IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 23, (iWidth - 2 * 30), linea, 1).
        RUN pdf_text_center IN h_PDFinc ("Spdf", "Operaciones", (iWidth - 2 * 30) / 2, (iHeight - 743) + (linea * 24) - interlineado).
        
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Constitución de aportes:", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 23 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf"," $", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 22 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Pagos cartera:", 30 + (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 23 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf"," $", 30 + (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 22 - interlineado).

        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 22, 1).

        RUN pdf_text_xy IN h_PDFinc ("Spdf","Detalle de la operación:", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 21 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Especifique el origen o procedencia de los recursos:", 20 + (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 21 - interlineado).
        
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 20 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 19 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 18 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 17 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 16 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 15 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 14 - interlineado).
        
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", 30 + (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 20 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", 30 + (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 19 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", 30 + (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 18 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", 30 + (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 17 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", 30 + (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 16 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", 30 + (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 15 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", 30 + (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 14 - interlineado).
        
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 12, (iWidth - 2 * 30) / 2 - 15, linea * 11, 1).

        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 13, 1).

        RUN pdf_text_xy IN h_PDFinc ("Spdf","Firman:", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 12 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Empleado FODUN:", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 11 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Gerente Regional:", (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 11 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_____________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 10 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_____________________", (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 10 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","C.C.", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 9 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","C.C.", (iWidth - 2 * 30) / 2, iHeight - 743 + linea * 9 - interlineado).

        RUN pdf_stroke_fill IN h_PDFinc ("Spdf", .84, .84, .84).
        RUN pdf_rect IN h_PDFinc ("Spdf", 30, iHeight - 743 + linea * 7, (iWidth - 2 * 30), linea, 1).
        RUN pdf_text_center IN h_PDFinc ("Spdf", "Observaciones", (iWidth - 2 * 30) / 2, (iHeight - 743) + (linea * 8) - interlineado).
        
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 7 - interlineado * 2).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 6 - interlineado * 2).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 5 - interlineado * 2).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 4 - interlineado * 2).
        
        RUN pdf_rect2 IN h_PDFinc ("Spdf", 30, iHeight - 743 - linea, iWidth - 2 * 30, linea * 3, 1).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Firma:", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 2 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","Oficial de cumplimiento: ________________________________________", pdf_LeftMargin("Spdf") + 30, iHeight - 743 + linea * 1 - interlineado).
        RUN pdf_text_xy IN h_PDFinc ("Spdf","C.C.", pdf_LeftMargin("Spdf") + 160, iHeight - 743 - interlineado).
        RUN pdf_set_dash IN h_PDFinc ("Spdf",1,0).
        
        RUN pdf_skip IN h_PDFinc ("Spdf").
        
    END.
    
    /* Set Detail Font Colour */
    RUN pdf_text_color IN h_PDFinc ("Spdf",.0,.0,.0).
END. /* PageHeader */

IF VALID-HANDLE(h_PDFinc) THEN
  DELETE PROCEDURE h_PDFinc.
