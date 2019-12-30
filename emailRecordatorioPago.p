DEFINE INPUT PARAMETER pNombre AS CHARACTER.
DEFINE INPUT PARAMETER pEmail AS CHARACTER.
DEFINE INPUT PARAMETER pDiasGeneraCarta AS INTEGER.

/* oakley */

DEF INPUT PARAMETER linea   AS CHAR NO-UNDO.
DEF INPUT PARAMETER credito   AS INT NO-UNDO.
DEF INPUT PARAMETER saldo     AS INT NO-UNDO.
DEF INPUT PARAMETER cuota     AS INT NO-UNDO.
DEF INPUT PARAMETER vencimiento    AS DATE NO-UNDO.
DEF INPUT PARAMETER emailcartera   AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER enviado AS LOGICAL NO-UNDO.
DEF OUTPUT PARAMETER respuesta AS CHAR NO-UNDO.

DEF VAR body AS CHARACTER.
DEF VAR mora AS CHARACTER.
DEF VAR encabezado AS CHARACTER.
DEF VAR tablaCred AS CHARACTER.
DEF VAR art60 AS CHARACTER.
DEF VAR art61 AS CHARACTER.
DEF VAR mensaje AS CHARACTER.
DEF VAR piedecorreo AS CHARACTER.

IF pDiasGeneraCarta <= 0 THEN
    mora = "vencimiento".
ELSE DO:
    IF pDiasGeneraCarta = 1 THEN
        mora = "MORA " + STRING(pDiasGeneraCarta) + " DIA".
    ELSE
        mora = "MORA " + STRING(pDiasGeneraCarta) + " DIAS".
END.

encabezado = "<img width=""200px"" src=""https://www.fodun.com.co/Files/Logo/logo-fodun-1.png"" align=""right""/>
        <p>Bogot&aacute;, D.C., " + STRING(TODAY,"99/99/9999") + "</p>
        <p><strong>&nbsp;</strong></p>
        <p><strong>REF. Recordatorio de pago (" + mora + ").</strong></p>
        <p><strong>Profesor(a):</strong></p>
        <p><strong>" + pNombre + "</strong></p>
        <p><strong>Asociado(a) de Fodun</strong></p>
        <p>&nbsp;</p>
        <p>Respetado(a) Profesor(a):</p>
        <p>&nbsp;</p>
        <p>Reciba un cordial saludo en nombre de <strong>FODUN</strong>.</p>".

tablaCred = "<table style=""border: 1px solid #ddd;text-align: center;border-collapse: collapse;"">
        <tbody>
            <tr style=""border: 1px solid #ddd;text-align: center;"">
                <td style=""padding: 5px;border: 1px solid #ddd;""><p><strong>Cr&eacute;dito</strong></p></td>
                <td style=""padding: 5px;border: 1px solid #ddd;""><p><strong>Saldo de capital</strong></p></td>
                <td style=""padding: 5px;border: 1px solid #ddd;""><p><strong>Valor pendiente</strong></p></td>
                <td style=""padding: 5px;border: 1px solid #ddd;""><p><strong>Vencimiento</strong></p></td>
                <td style=""padding: 5px;border: 1px solid #ddd;""><p><strong>D&iacute;as Mora</strong></p></td>
            </tr>
            <tr style=""border: 1px solid #ddd;text-align: center;"">
                <td style=""padding: 5px;border: 1px solid #ddd;""><p>" + STRING(linea) + "<br>" + STRING(credito) + "</p></td>
                <td style=""padding: 5px;border: 1px solid #ddd;""><p>$ " + STRING(saldo,"$>>>,>>>,>>9") + "</p></td>
                <td style=""padding: 5px;border: 1px solid #ddd;""><p>$ " + STRING(cuota,"$>>>,>>>,>>9") + "</p></td>
                <td style=""padding: 5px;border: 1px solid #ddd;""><p>" + STRING(vencimiento,"99/99/9999") + "</p></td>
                <td style=""padding: 5px;border: 1px solid #ddd;""><p>" + STRING(pDiasGeneraCarta) + "</p></td>
            </tr>
        </tbody>
        </table>".

art61 = "<p><strong>Art&iacute;culo 61&ordm;. Exclusi&oacute;n</strong>. Los asociados de FODUN perder&aacute;n su car&aacute;cter de tales mediante exclusi&oacute;n como sanci&oacute;n cuando incurran en una o varias de las siguientes causales:</p>
        <ul>
            <li><strong>a)</strong> Por reiterado incumplimiento de las obligaciones pecuniarias contra&iacute;das con FODUN.</li>
            <li><strong>b)</strong> Por servirse indebidamente de FODUN en provecho propio o de terceros.</li>
            <li><strong>c)</strong> Por entregar a FODUN bienes de procedencia il&iacute;cita.</li>
            <li><strong>d)<strong> Por mora mayor de noventa (90) d&iacute;as, no justificable, en el cumplimiento de las obligaciones pecuniarias con FODUN.</li>
            <li><strong>e)</strong> Por efectuar operaciones ficticias o dolosas en perjuicio de FODUN.</li>
            <li><strong>f)</strong> Por incapacidad legal declarada por autoridad competente en sentencia judicial en firme.</li>
            <li><strong>g)</strong> Por incurrir en delitos econ&oacute;micos contra FODUN con sentencia en firme.</li>
        </ul>".

art60 = "<p><strong>Art&iacute;culo 60&ordm;. Suspensi&oacute;n temporal de derechos y/o servicios.</strong> La Junta Directiva podr&aacute; declarar suspendido el uso de servicios o, en su caso, la suspensi&oacute;n temporal de derechos y servicios a los asociados por alguna o algunas de las siguientes causas:</p>
        <ul>
            <li><strong>a)</strong> Por mora mayor de treinta (30) d&iacute;as calendario y menor de noventa y un (91) d&iacute;as calendario en el cumplimiento de sus obligaciones pecuniarias con FODUN.</li>
            <li><strong>b)</strong> Por negligencia o descuido en el desempe&ntilde;o de las funciones que se le conf&iacute;en dentro de FODUN.</li>
            <li><strong>c)</strong> Por extender a favor de terceros no contemplados en los reglamentos, los servicios y recursos que proporciona FODUN a sus asociados.</li>
            <li><strong>d)</strong> Por incumplimiento de los deberes consagrados en los presentes estatutos.</li>
        </ul>".

piedecorreo = "<p>Recuerde que puede acercarse a nuestras oficinas para cancelar el valor antes mencionado;&nbsp; Si lo prefiere, puede realizar su pago a trav&eacute;s del bot&oacute;n &ldquo;<a href=""https://sucursal.fodun.com.co/"">pagos en l&iacute;nea</a>&rdquo; de nuestra p&aacute;gina web, mediante transferencia o consignaci&oacute;n a las cuentas de FODUN en su respectiva regional, indicadas a continuaci&oacute;n.</p>
                <table style=""text-align: left;"">
                    <tbody>
                        <tr>
                            <td style=""padding: 5px;""><p>Bogot&aacute;</p></td>
                            <td style=""padding: 5px;""><p>Banco Davivienda</p></td>
                            <td style=""padding: 5px;""><p>Cuenta Corriente</p></td>
                            <td style=""padding: 5px;""><p>No.001669996439</p></td>
                            <td style=""padding: 5px;""><p>mail: pagosbog@fodun.com.co</p></td>
                        </tr>
                        <tr>
                            <td style=""padding: 5px;""><p>Medell&iacute;n</p></td>
                            <td style=""padding: 5px;""><p>Bancolombia</p></td>
                            <td style=""padding: 5px;""><p>Cuenta Ahorros</p></td>
                            <td style=""padding: 5px;""><p>No.31979142121</p></td>
                            <td style=""padding: 5px;""><p>mail: pagosmed@fodun.com.co</p></td>
                        </tr>
                        <tr>
                            <td style=""padding: 5px;""><p>Manizales</p></td>
                            <td style=""padding: 5px;""><p>Banco BBVA</p></td>
                            <td style=""padding: 5px;""><p>Cuenta Corriente</p></td>
                            <td style=""padding: 5px;""><p>No. 63801072-8</p></td>
                            <td style=""padding: 5px;""><p>mail: manizales@fodun.com.co</p></td>
                        </tr>
                        <tr>
                            <td style=""padding: 5px;""><p>Palmira</p></td>
                            <td style=""padding: 5px;""><p>Banco Caja Social</p></td>
                            <td style=""padding: 5px;""><p>Cuenta Corriente</p></td>
                            <td style=""padding: 5px;""><p>No. 21002620096</p></td>
                            <td style=""padding: 5px;""><p>mail: palmira@fodun.com.co</p></td>
                        </tr>
                    </tbody>
                </table>
                <p>&nbsp;</p>
                <p>Una vez realizada la transacci&oacute;n, le solicitamos enviar el comprobante al email correspondiente.</p>
                <p>&nbsp;</p>
                <p><em>Si usted ya realiz&oacute; este pago, por favor haga caso omiso este mensaje.</em></p>
                <p>Cordial Saludo,</p>
                <p>Cartera FODUN</p>
                <table style=""text-align: center;"">
                <tbody>
                    <tr>
                        <td style=""padding: 5px;"">
                            <p><strong><a href=""https://www.fodun.com.co/oficina/regional-bogota"">Regional Bogot&aacute;</a></strong></p>
                            <p>(1) 2211461</p>
                            <p>&nbsp;</p>
                        </td>
                        <td style=""padding: 5px;"">
                            <p><strong><a href=""https://www.fodun.com.co/oficina/regional-medellin"">Regional Medell&iacute;n</a></strong></p>
                            <p>(4) 4 44 30 60</p>
                            <p>&nbsp;</p>
                        </td>
                        <td style=""padding: 5px;"">
                            <p><strong><a href=""https://www.fodun.com.co/oficina/regional-manizales"">Regional Manizales</a></strong></p>
                            <p>(6) 874 2895</p>
                            <p>&nbsp;</p>
                        </td>
                        <td style=""padding: 5px;"">
                            <p><strong><a href=""https://www.fodun.com.co/oficina/regional-palmira"">Regional Palmira</a></strong></p>
                            <p>(2) 2624025</p>
                            <p>&nbsp;</p>
                        </td>
                    </tr>
                </tbody>
                </table>
                <p style=""text-align: center;"">www.fodun.com.co</p>".


            IF pDiasGeneraCarta < 1 THEN
                    mensaje = "<p>Nos permitimos recordarle que el <strong>" + string(vencimiento,"99/99/9999") + "</strong>, vencer&aacute; la cuota correspondiente a su " + linea + " Nro.:" + string(credito) + ", por un monto de " + string(cuota,"$>>>,>>>,>>9") + ".</p>".
            ELSE DO:
                IF pDiasGeneraCarta > 0 AND pDiasGeneraCarta <= 30 THEN DO:
                    mensaje = "<p>Al d&iacute;a de hoy " + STRING(TODAY,"99/99/9999") + ", usted presenta valores pendientes en el pago de sus obligaciones con Fodun.</p>" + tablaCred + "<p>Le sugerimos cancelar los valores que se encuentran pendientes de
                    pago antes de cumplir 30 d&iacute;as de mora y as&iacute; evitar la suspensi&oacute;n de los servicios de Fodun.</p><p>&nbsp;</p>" + art60.
                    IF pDiasGeneraCarta < 8 THEN
                        mensaje = mensaje + "<p><strong>De acuerdo con la Ley estatutaria 1266 del 31 de diciembre de 2008, nos permitimos informar que si transcurridos veinte (20) d&iacute;as calendario a la fecha de este correo no cancela sus obligaciones con FODUN, ser&aacute; reportado en centrales de riesgo.</strong><p>".
                END.
                ELSE DO:
                    IF pDiasGeneraCarta > 30 AND pDiasGeneraCarta < 85 THEN
                        mensaje = "<p>Por tener una mora en el pago de sus obligaciones superior a 30 d&iacute;as, usted se encuentra con una suspensi&oacute;n de los servicios de Fodun. Le sugerimos cancelar a la mayor brevedad los valores que se encuentran
                        pendientes de pago.</p><p>A continuaci&oacute;n relacionamos su estado de cuenta al " + STRING(TODAY,"99/99/9999") + " en el cual encontrar&aacute; valores pendientes de pago de sus obligaciones con Fodun, as&iacute;:</p>" + tablaCred + art60.
                    ELSE
                        mensaje = "<p>A continuaci&oacute;n relacionamos su estado de cuenta al " + STRING(TODAY,"99/99/9999") + " en el cual encontrar&aacute; valores pendientes de pago de sus obligaciones con Fodun, as&iacute;:</p>" + tablaCred + 
                        "<p>Le sugerimos cancelar los valores que se encuentran en mora de pago antes de cumplir 90 d&iacute;as de mora y as&iacute; evitar un proceso de investigaci&oacute;n.</p>" + art61 + art60.
                END.
            END.
                    
body = encabezado + mensaje + piedecorreo.

RUN mail.r(INPUT pEmail,
           INPUT pNombre,
           INPUT emailcartera,
           INPUT "Gest. de Cartera FODUN",
           INPUT "",
           INPUT emailcartera + " , gestioncobro@fodun.com.co", 
           INPUT "Recordatorio de pago (" + mora + ")",
           INPUT body,
           INPUT "",
           OUTPUT enviado,
           OUTPUT respuesta).

/*RUN mail.p("lcmartinez@fodun.com.co", "Ing. Luis Carlos",
           "bogota@fodun.com.co", "Gest. Cartera FODUN",
           "luiscmart@gmail.com", TRUE, 
           "Mensaje de prueba con procedimiento mail y archivo adjunto",
           "<img src=""https://www.fodun.com.co/Files/Logo/logo-fodun-1.png""/><br>Este es un mensaje de prueba. Con una tilde en Asociaci&oacute;n<br>Con un archivo adjunto.<br>Cordial saludo",
           "D:\SFG\Desarrollo\Prog\Solicitudes\6402643.pdf", OUTPUT enviado, OUTPUT respuesta).
MESSAGE "El mensaje " + string(enviado) + " fue enviado. " + respuesta VIEW-AS ALERT-BOX ERROR.*/
