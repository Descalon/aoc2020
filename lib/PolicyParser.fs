namespace Lib

open FParsec

module PolicyParser =
    let str s = pstring s
    let n = pint32 .>> str "-" >>. pint32
    let r = run n "1-3"