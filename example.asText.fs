module AsText =

    let renderMonthRules cell =
        match cell with
        | VSeparator -> " | "
        | HSeparator -> "---"
        | MonthHeader d -> String.Format(@"{0,21}", d.ToString(@"MMM - yy"))
        | WeekDayHeader d -> String.Format(@"{0,3}", d.ToString(@"ddd"))
        | JustDay d -> String.Format(@"{0,3}", d.ToString(@" d"))
        | EmptyDay -> "   "

    let renderCalendarAsText cols rows year month weeday =
        renderCalendar
            (fun ioSeq -> ioSeq |> Seq.toList |> List.iter (fun f -> f()))
            (fun cs -> (fun () -> Console.WriteLine(String.Join(String.Empty, Seq.map renderMonthRules cs |> Seq.concat))))
            cols rows year month weeday

// e.g. `AsText.renderCalendarAsText 2 2 2014 1 DayOfWeek.Monday`
