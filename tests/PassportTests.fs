module PassportTests

open Xunit

let passports = [
    ["ecl:gry;pid:860033327;eyr:2020;hcl:#fffffd;";
    "byr:1937;iyr:2017;cid:147;hgt:183cm;"];

    ["iyr:2013;ecl:amb;cid:350;eyr:2023;pid:028048884;";
    "hcl:#cfa07d;byr:1929;"];

    ["hcl:#ae17e1;iyr:2013;";
    "eyr:2024;";
    "ecl:brn;pid:760753108;byr:1931;";
    "hgt:179cm;"];

    ["hcl:#cfa07d;eyr:2025;pid:166559648;";
    "iyr:2011;ecl:brn;hgt:59in;"];
]

let assertEqual (expected:int)(actual:int) = Assert.Equal(expected, actual)
let assertTrue (b:bool)  = Assert.True(b)
let assertFalse (b:bool) = Assert.False(b)

[<Fact>]
let ``Test with example`` () =
    passports
    |> List.map Lib.PassportProcessing.checkPassport
    |> List.sumBy (fun x -> if x then 1 else 0)
    |> assertEqual 2

[<Fact>]
let ``Single Line test`` () =
    Lib.PassportProcessing.checkPassport ["hgt:172in;pid:170cm;hcl:17106b;iyr:2012;ecl:gry;"]
    |> assertFalse