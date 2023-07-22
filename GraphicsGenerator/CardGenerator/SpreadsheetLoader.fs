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
let FortificationHealthCol = "N"
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
[<Literal>]
let GenerateCardCol = "AJ"
[<Literal>]
let DebugRowNumberCol = "AK"
// if adding a col, update the code at the bottom

type ParsedRow = string option list

type PartialCard = {
    DebugNumber: uint
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
    FortificationHealth: string option
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
    | "SU" -> Some Types.sumerian
    | "NA" -> Some Types.nativeAmerican
    | "EG" -> Some Types.egyptian
    | "DR" -> Some Types.druidic
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
    let parseNumeric = tryParse<uint> >> Option.filter (fun s -> s > 0u)
    result {
        let! debug = itemAt DebugRowNumberCol r >>= parseNumeric |> Result.requireSome $"Debug number must be present %A{toDebugRow r}" 
        let! nameOrRowKind = itemAt NameCol r |> Result.requireSome $"No name provided for row %A{toDebugRow r}"
        let! faction = itemAt FactionCol r >>= codeToFaction |> Result.requireSome $"No faction provided for row %A{toDebugRow r}"
        let! kind = itemAt KindCol r |> Result.requireSome $"No kind provided for row %A{toDebugRow r}"
        let! text = itemAt TextCol r |> Result.requireSome $"No text provided for row %A{toDebugRow r}"
        let hasCardCount = 
            itemAt ShowCardCountCol r 
            >>= parseNumeric
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
        let fortificationHealth = itemAt FortificationHealthCol r 
        let upgradeCost = itemAt UpgradeCostCol r >>= parseNumeric
        let flavorText = itemAt FlavorTextCol r 
        let ally = itemAt AllyCol r >>= parseNumeric
        let trash = itemAt TrashCol r >>= parseNumeric
        let row = {
            DebugNumber = debug
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
            FortificationHealth = fortificationHealth
            UpgradeCost = upgradeCost
            FlavorText = flavorText
        }
        return createPartial nameOrRowKind upgradeCost ally trash row
    }

let tryParseToMeasure<[<Measure>] 'a> = Option.bind tryParse<uint> >> Option.map LanguagePrimitives.UInt32WithMeasure<'a>

let parseCost c s a =
    { Trade = tryParseToMeasure<trade> c; Strength = tryParseToMeasure<strength> s; Anima = tryParseToMeasure<anima> a }

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
    match row, metadata with
    | UpgradeMain s, { AnimaCost = None }
    | MainRow s, { AnimaCost = None } -> 
        Plain { PlainOrTrashAbility.Text = s.Text; Metadata = metadata }
    | UpgradeAlly s, { AnimaCost = None } 
    | AllyRow s, { AnimaCost = None } -> 
        Ally { Text = s.Text; Metadata = metadata; Faction = s.Faction }
    | UpgradeMain s, { AnimaCost = Some c }
    | MainRow s, { AnimaCost = Some c }
    | UpgradeAlly s, { AnimaCost = Some c } 
    | AllyRow s, { AnimaCost = Some c } -> 
        Anima { Text = s.Text; Metadata = metadata; Cost = c }
    | TrashRow s, _ -> 
        Trash { Text = s.Text; Metadata = metadata }


let tryCreateCard main ally =
    result {
        let! cardCount = main.CardCount |> Result.requireSome $"Cards must have a count %A{main} %A{ally}"
        let core = {
            Name = main.Name
            MainAbility = MainRow main |> rowToAbility 
            Cost = parseCost main.TradeCost main.StrengthCost main.AnimaCost
            Favor = main.Favor >>= tryParse<uint>
            Image = femaleHumanImage
            Count = cardCount
            ShowCount = main.ShowCardCount
            FlavorText = main.FlavorText
        }
        let upgraded = main.UpgradeCost |> Option.isSome
        match main.Kind with
        | "Human" -> 
            return Human {
                Core = core
                Faction = main.Faction
                SecondaryAbility = ally
                Upgraded = upgraded
            }
        | "Building" ->
            return Building {
                Core = core
                Faction = main.Faction
                RightSlot = None
                Upgraded = upgraded
            }
        | "Fortification" ->
            let! health = tryParseToMeasure main.FortificationHealth |> Result.requireSome "Health must be provided for fortification cards"
            return Fortification {
                Core = core
                Faction = main.Faction
                Health = health
                LeftSlot = None
                Upgraded = upgraded
            }
        | "Nomad" ->
            return Nomad {
                Core = core
            }
        | "Creature" ->
            return Creature {
                Core = core
            }
        | "Relic" ->
            return Relic {
                Core = core
            }
        | "Settlement" ->
            let! health = tryParseToMeasure main.FortificationHealth |> Result.requireSome "Health must be provided for settlement cards"
            return Settlement {
                Core = core
                Faction = main.Faction
                Health = health
                SecondaryAbility = None
                TertiaryAbility = None
                LeftSlot = None
                TopSlot = None
                RightSlot = None
                GarrisonSlot = []
            }
        | "God" ->
            return God {
                Core = core
                Faction = main.Faction
            }
        | _ -> return! Error $"Unsupported row {main.Name}: %A{main} %A{ally}"
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
        let upgradeAlly = parsed |> tryChoose (function UpgradeAlly _ as s -> Some s | _ -> None) |>> rowToAbility
        let! maybeUpgrade =  
            parsed 
            |> tryChoose (function UpgradeMain s -> Some s | _ -> None)
            |> Option.map (fun s -> tryCreateCard s upgradeAlly) |> Option.sequenceResult
        let! main = tryCreateCard main ally
        return main, maybeUpgrade
    }

let readOdt path =
    use zipToOpen = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
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
        |> List.map (List.map (List.take 52) >> partialRowsToCard)
        |> Result.partition
    let cards, maybeCards = cards |> List.unzip
    maybeCards |> List.choose id |> List.append cards, errors |> List.collect id