module AsHtml =

    let htmlHeader = @"<!DOCTYPE html>
<html>
    <head>
        <style>
            body { background-color: #f0f0a0 }
            .calendar { border-collapse: collapse; margin: auto }
            .calendar td { border: 1px solid lightgray; text-align: right; background-color: white; padding: 0.2em }
            .calendar .mh { background-color: #0C93ED; color: white; text-align: center; border-radius: 22px 0 0; box-sizing: border-box; border: 0 }
            .calendar .wd { background-color: olive; color: white; font-size: 0.75em }
            .calendar .ec { background-color: #f0f0f0 }
            .calendar .s { background-color: #f0f0a0; border: 0; width: 2em; height: 2em; line-height: 0.5em; font-size: 0.5em }
            .calendar tr .dc:nth-child(8n+7) { background-color: lightgreen }
        </style>
    </head>
    <body>
        <table class=calendar>
"

    let htmlFooter = @"
        </table>
    </body>
</html>
"
    let renderMonthRules cell =
        match cell with
        | VSeparator -> "<td class=s>&nbsp;</td>"
        | HSeparator -> "<td class=s>&nbsp;</td>"
        | MonthHeader d -> String.Format("<td class=mh colspan=7>{0:MMM - yy}</td>", d)
        | WeekDayHeader d -> String.Format("<td class=wd>{0:ddd}</td>", d)
        | JustDay d -> String.Format("<td class=dc>{0: d}</td>", d)
        | EmptyDay -> "<td class=ec>&nbsp;</td>"

    let renderCalendarAsHtml cols rows year month weeday =
        renderCalendar
            (fun ss -> String.Join(String.Empty, ss))
            (fun cs -> "<tr>" + String.Join(String.Empty, Seq.map renderMonthRules cs) + "</tr>")
            cols rows year month weeday

    let renderHtmlCalendarToFile fpath cols rows year month weeday =
        File.WriteAllText(fpath, htmlHeader + renderCalendarAsHtml cols rows year month weeday + htmlFooter)

// e.g. `AsHtml.renderHtmlCalendarToFile @"calendar.html" 2 2 2014 1 DayOfWeek.Monday`
