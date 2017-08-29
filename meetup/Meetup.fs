module Meetup
open System

type Schedule = First | Second | Third | Fourth | Last | Teenth

let meetupDay (dayOfWeek: DayOfWeek) schedule year month =
    let d = if schedule = Teenth then DateTime(year, month, 13)
            elif schedule = Last then DateTime(year, month, DateTime.DaysInMonth(year, month) - 6)
            else DateTime(year, month, 1)
    let first = int d.DayOfWeek
    let dayNum = int dayOfWeek
    let firstGivenDay = if first < dayNum then d.AddDays (float dayNum - float first)
                        elif first > dayNum then d.AddDays (7. - float first + float dayOfWeek)
                        else d
    match schedule with
    | First | Teenth | Last -> firstGivenDay
    | Second -> firstGivenDay.AddDays 7.
    | Third -> firstGivenDay.AddDays 14.
    | Fourth -> firstGivenDay.AddDays 21.