module CsvParser

let parseLineWithRegex =
    let regex = new System.Text.RegularExpressions.Regex("(^|,)((\"(?<escaped>((\"\"|[^\"])*))\")|(?<unescaped>([^\",]*)))(?=($|,))")
    fun line ->
        [|
            for m in regex.Matches(line) do
            if m.Groups.["unescaped"].Success then
                yield m.Groups.["unescaped"].Value
            else
                yield m.Groups.["escaped"].Value.Replace("\"\"", "\"")
        |]
