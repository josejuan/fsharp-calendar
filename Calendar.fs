namespace Calendar

module Render =

    open System

    type Cell = VSeparator
                | HSeparator
                | MonthHeader   of DateTime
                | WeekDayHeader of DateTime
                | JustDay       of DateTime
                | EmptyDay

    let makeMonthCalendar (d : DateTime) (weekDay : DayOfWeek): Cell seq seq =
        let rec monthDayB (q : DateTime) =
            if q.Month = d.Month
                then seq { yield JustDay q; yield! monthDayB (q.AddDays(1.)) }
                else seq { while true do yield EmptyDay }
        let rec monthDayA (wd : DayOfWeek) =
            if wd = d.DayOfWeek
                then monthDayB d
                else seq { yield EmptyDay; yield! monthDayA (enum<DayOfWeek> ((1 + int wd) % 7)) }
        seq {
            yield seq { yield MonthHeader d }
            yield Seq.unfold (fun (f : DateTime) -> Some((f, f.AddDays(1.)))) d |>
                    Seq.skipWhile (fun f -> f.DayOfWeek <> weekDay) |>
                    Seq.take 7 |>
                    Seq.map WeekDayHeader
            yield! monthDayA weekDay |> Seq.take 42 |> chunks 7 |> Seq.map Array.toSeq
                  
        }

    let makeCalendar (cols : int) (rows : int) (idate : DateTime) (weekDay : DayOfWeek) : Cell seq seq =
        let vsep = seq { while true do yield seq { yield VSeparator } }
        let hsep = seq { while true do yield HSeparator } |> Seq.take (8 * cols - 1)
        chunks cols (seq { for i in 0 .. (cols * rows - 1) do yield makeMonthCalendar (idate.AddMonths(i)) weekDay }) |>
        Seq.map Array.toSeq |>
        Seq.map (Seq.reduce (fun a b -> Seq.zip3 a vsep b |> Seq.map (fun (r, s, t) -> seq { yield! r; yield! s; yield! t }))) |>
        Seq.reduce (fun a b -> seq { yield! a; yield hsep; yield! b })

    let renderCalendar : (('b seq -> 't) -> (Cell seq -> 'b) -> int -> int -> int -> int -> DayOfWeek -> 't) =
        fun joinRows renderCell cols rows year month weekDay ->
            makeCalendar cols rows (new DateTime(year, month, 1)) weekDay |>
            Seq.map renderCell |>
            joinRows

