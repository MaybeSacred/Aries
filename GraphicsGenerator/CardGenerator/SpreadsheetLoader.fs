module SpreadsheetLoader

open System
open System.Xml.Linq
open FSharp.Data
open FSharpPlus
open FSharpPlus.Data
open FsToolkit.ErrorHandling
open System.IO
open System.IO.Compression

open Types

type SpreadsheetXml = XmlProvider<"""../../content.xml""">

[<Literal>]
let NameCol = "A"
[<Literal>]
let FactionCol = "B"
[<Literal>]
let KindCol = "C"
[<Literal>]
let TextCol = "D"
[<Literal>]
let TradeCostCol = "E"
[<Literal>]
let StrengthCostCol = "F"
[<Literal>]
let FavorCol = "G"
[<Literal>]
let AnimaCol = "H"
[<Literal>]
let TradeGainCol = "I"
[<Literal>]
let StrengthGainCol = "J"
[<Literal>]
let AnimaGainCol = "K"
[<Literal>]
let FavorGainCol = "L"
[<Literal>]
let ShieldHealthCol = "N"
[<Literal>]
let UpgradeCostCol = "U"
[<Literal>]
let TrashCol = "Y"
[<Literal>]
let AllyCol = "Z"
[<Literal>]
let FlavorTextCol = "AF"
[<Literal>]
let CardCountCol = "AH"
[<Literal>]
let ShowCardCountCol = "AI"
// if adding a col, update the code at the bottom

type ParsedRow = string option list

type PartialCard = {
    Name: string
    Faction: FactionData
    Kind: string
    Text: string
    CardCount: uint option
    ShowCardCount: bool
    TradeCost: string option
    StrengthCost: string option
    Favor: string option
    AnimaCost: string option
    TradeGain: string option
    StrengthGain: string option
    AnimaGain: string option
    FavorGain: string option
    ShieldHealth: string option
    UpgradeCost: uint option
    FlavorText: string option
}

type RowKind = 
    | MainRow of PartialCard
    | AllyRow of PartialCard
    | TrashRow of PartialCard
    | UpgradeMain of PartialCard
    | UpgradeAlly of PartialCard

let rowToList (row: SpreadsheetXml.TableRow) = 
    row.TableCells
    |> List.ofArray
    |> List.collect (fun c -> 
        match c.NumberColumnsRepeated with
        | Some r -> c.P |> Option.bind (fun p -> p.String) |> konst |> List.init r
        | None -> c.P |> Option.bind (fun p -> p.String) |> List.singleton)

let rowToList2 (row: SpreadsheetXml.TableRow2) = 
    row.TableCells
    |> List.ofArray
    |> List.collect (fun c -> 
        match c.NumberColumnsRepeated with
        | Some r -> c.P |> Option.bind (fun p -> p.String) |> konst |> List.init r
        | None -> c.P |> Option.bind (fun p -> p.String) |> List.singleton)

let codeToFaction s =
    match s with
    | "U" | "P" | "R" | "SM" | "M" -> Some Types.unaligned
    | "BB" -> Some Types.botBrigade
    | "I" -> Some Types.imperium
    | "S" -> Some Types.stellarion
    | "RA" -> Some Types.rogueAlliance
    | _ -> None

let createPartial kind upgrade ally trash row =
    match kind, upgrade, ally, trash with
    | "Ally", None, Some _, _ -> AllyRow row
    | "Trash", None, _, Some _ -> TrashRow row
    | s, None, Some _, _ when s.Contains "Upgrade:" -> { row with Name = String.replace "Upgrade:" "" row.Name |> String.trimWhiteSpaces } |> UpgradeAlly 
    | s, Some _, None, _ when s.Contains "Upgrade:" -> { row with Name = String.replace "Upgrade:" "" row.Name |> String.trimWhiteSpaces } |> UpgradeMain 
    | _ -> MainRow row

let colToInt (c: string) =
    let charToInt (ch: string) pos = int (Char.ConvertToUtf32 (ch, pos)) - 64
    match c with
    | c when c.Length = 1 -> charToInt c 0
    | c when c.Length = 2 -> (charToInt c 0) * 26 + charToInt c 1
    |> fun i -> i - 1

let toDebugRow (r: ParsedRow) =
    List.init r.Length (fun i -> if i >= 26 then $"A{Char.ConvertFromUtf32 (i - 26 + 65)}" else Char.ConvertFromUtf32 (i + 65))
    |> flip List.zip r

let itemAt col (r: ParsedRow) =
    List.tryItem (colToInt col) r
    |> Option.bind id
    |> Option.map String.trimWhiteSpaces

let tryReadRow (r: ParsedRow) =
    let hasValue = tryParse<uint> >> Option.filter (fun s -> s > 0u)
    result {
        let! nameOrRowKind = itemAt NameCol r |> Result.requireSome $"No name provided for row %A{toDebugRow r}"
        let! faction = itemAt FactionCol r >>= codeToFaction |> Result.requireSome $"No faction provided for row %A{toDebugRow r}"
        let! kind = itemAt KindCol r |> Result.requireSome $"No kind provided for row %A{toDebugRow r}"
        let! text = itemAt TextCol r |> Result.requireSome $"No text provided for row %A{toDebugRow r}"
        let hasCardCount = 
            itemAt ShowCardCountCol r 
            >>= hasValue
            |> Option.map (fun _ -> true) 
            |> Option.defaultValue false
        let cardCount = itemAt CardCountCol r >>= tryParse<uint> 
        let tradeCost = itemAt TradeCostCol r 
        let strengthCost = itemAt StrengthCostCol r 
        let favor = itemAt FavorCol r 
        let animaCost = itemAt AnimaCol r 
        let tradeGain = itemAt TradeGainCol r 
        let strengthGain = itemAt StrengthGainCol r 
        let animaGain = itemAt AnimaGainCol r 
        let favorGain = itemAt FavorGainCol r 
        let shieldHealth = itemAt ShieldHealthCol r 
        let upgradeCost = itemAt UpgradeCostCol r >>= hasValue
        let flavorText = itemAt FlavorTextCol r 
        let ally = itemAt AllyCol r >>= hasValue
        let trash = itemAt TrashCol r >>= hasValue
        let row = {
            Name = nameOrRowKind
            Faction = faction
            Kind = kind
            Text = text
            CardCount = cardCount
            ShowCardCount = hasCardCount
            TradeCost = tradeCost
            StrengthCost = strengthCost
            Favor = favor
            AnimaCost = animaCost
            TradeGain = tradeGain
            StrengthGain = strengthGain
            AnimaGain = animaGain
            FavorGain = favorGain
            ShieldHealth = shieldHealth
            UpgradeCost = upgradeCost
            FlavorText = flavorText
        }
        return createPartial nameOrRowKind upgradeCost ally trash row
    }

let tryParseToMeasure<[<Measure>] 'a> = Option.bind tryParse<uint> >> Option.map LanguagePrimitives.UInt32WithMeasure<'a>

let parseCost kind c s =
    match kind, tryParseToMeasure<trade> c, tryParseToMeasure<strength> s with
    | _, Some c, None -> c |> TradeOnly |> Some
    | _, None, Some s -> s |> StrengthOnly |> Some
    // hack: only Nomad can be either/or right now
    | "Nomad", Some c, Some s -> TradeOrStrength (c, s) |> Some
    | _, Some c, Some s -> TradeAndStrength (c, s) |> Some
    | _ -> None

let rowToAbility (row: RowKind) = 
    let metadata = 
        match row with
        | MainRow s
        | AllyRow s
        | TrashRow s
        | UpgradeMain s
        | UpgradeAlly s -> 
            { TradeGain = tryParseToMeasure s.TradeGain
              StrengthGain = tryParseToMeasure s.StrengthGain
              AnimaCost = tryParseToMeasure s.AnimaCost
              AnimaGain = tryParseToMeasure s.AnimaGain
              FavorGain = tryParseToMeasure s.FavorGain }
    match row with
    | MainRow s -> 
        Main { MainAbility.Text = s.Text; Metadata = metadata; Cost = metadata.AnimaCost }
    | AllyRow s -> 
        Ally { Text = s.Text; Metadata = metadata; Faction = s.Faction }

let tryCreateCard main ally trash =
    result {
        let! cardCount = main.CardCount |> Result.requireSome $"Cards must have a count %A{main} %A{ally} %A{trash}"
        let core = {
            Name = main.Name
            MainAbility = rowToAbility main
            Cost = parseCost main.Kind main.TradeCost main.StrengthCost
            Favor = main.Favor >>= tryParse<uint>
            Count = cardCount
            ShowCount = main.ShowCardCount
            Faction = main.Faction
            FlavorText = main.FlavorText
        }
        let upgraded = main.UpgradeCost |> Option.isSome
        match main.Kind with
        | "Human" -> 
            return! Human {
                Core = core
                SecondaryAbility = ally
                Upgraded = upgraded
            } |> Ok
        | "Building" ->
            return! Building {
                Core = core
                SecondaryAbility = ally
                Upgraded = upgraded
            } |> Ok
        | "Shield" ->
            let! health = tryParseToMeasure main.ShieldHealth |> Result.requireSome "Health must be provided for shield cards"
            return! Shield {
                Core = core
                Health = health
                Upgraded = upgraded
            } |> Ok
        | "Nomad" ->
            return! Nomad {
                Core = core
            } |> Ok
        | "Monster" ->
            return! Monster {
                Core = core
            } |> Ok
        | "Relic" ->
            return! Relic {
                Core = core
            } |> Ok
        | "Settlement"
        | _ -> return! Error $"Unsupported row {main.Name}: %A{main} %A{ally} %A{trash}"
    }

let partialRowsToCard (rows: ParsedRow list) =
    let tryChoose chooser r = r |> List.choose chooser |> List.tryHead
    validation {
        let! parsed = rows |> List.traverseResultA tryReadRow
        let! main = parsed 
                    |> List.choose (function MainRow s -> Some s | _ -> None) 
                    |> List.tryExactlyOne 
                    |> Result.requireSome $"Could not read rows %A{parsed}: %A{rows}"
        let ally = parsed |> tryChoose (function AllyRow _ as s -> Some s | _ -> None) |>> rowToAbility
        let trash = parsed |> tryChoose (function TrashRow _ as s -> Some s | _ -> None) |>> rowToAbility
        do! Option.zip ally trash
            |> Result.requireNone $"Cards cannot have both an ally and trash ability {main.Name}: %A{main} %A{ally} %A{trash}"
        let upgrade = parsed |> tryChoose (function UpgradeMain _ as s -> Some s | _ -> None)
        let upgradeAlly = parsed |> tryChoose (function UpgradeAlly _ as s -> Some s | _ -> None) |>> rowToAbility
        let! maybeUpgrade = upgrade |> Option.map (fun s -> tryCreateCard s upgradeAlly None) |> Option.sequenceResult
        let! main = tryCreateCard main ally
        return main, maybeUpgrade
    }

let readOdt path =
    use zipToOpen = new FileStream(path, FileMode.Open, FileAccess.Read)
    use archive = new ZipArchive(zipToOpen, ZipArchiveMode.Read)
    let readmeEntry = archive.GetEntry "content.xml"
    use reader = new StreamReader(readmeEntry.Open())
    reader.ReadToEnd ()

let load (path: string) =
    let opened = if String.endsWith ".xml" path then SpreadsheetXml.Load path else readOdt path |> SpreadsheetXml.Parse
    let cards, errors = 
        opened.Body.Spreadsheet.Tables[1].TableRows
        |> List.ofArray
        |> List.map (rowToList >> List.singleton)
        |> List.append 
            (opened.Body.Spreadsheet.Tables[1].TableRowGroups
            |> List.ofArray
            |> List.map (fun s -> s.TableRows |> List.ofArray |> List.map rowToList2))
        |> List.map (List.map (List.take (colToInt ShowCardCountCol + 1)) >> partialRowsToCard)
        |> Result.partition
    let cards, maybeCards = cards |> List.unzip
    maybeCards |> List.choose id |> List.append cards, errors |> List.collect id