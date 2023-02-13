module SpreadsheetLoader

open System
open System.Xml.Linq
open FSharp.Data
open FSharpPlus
open FSharpPlus.Data
open FsToolkit.ErrorHandling
open Types
open System.IO
open System.IO.Compression

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
let CardCountCol = "E"
[<Literal>]
let CreditCostCol = "F"
[<Literal>]
let StrengthCostCol = "G"
[<Literal>]
let RewardCol = "H"
[<Literal>]
let CreditGainCol = "J"
[<Literal>]
let StrengthGainCol = "K"
[<Literal>]
let EnergyGainCol = "L"
[<Literal>]
let RewardGainCol = "M"
[<Literal>]
let ShieldHealthCol = "O"
[<Literal>]
let UpgradeCostCol = "V"
[<Literal>]
let TrashCol = "Z"
[<Literal>]
let AllyCol = "AA"
[<Literal>]
let FlavorTextCol = "AF"

type PartialCard = {
    Name: string
    Faction: FactionData
    Kind: string
    Text: string
    CardCount: string option
    CreditCost: string option
    StrengthCost: string option
    Reward: string option
    CreditGain: string option
    StrengthGain: string option
    EnergyGain: string option
    RewardGain: string option
    ShieldHealth: string option
    UpgradeCost: string option
    FlavorText: string option
}

type RowKind = 
    | Main of PartialCard
    | Ally of PartialCard
    | Trash of PartialCard
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

let createPartial kind ally trash row =
    match kind, ally, trash with
    | "Ally", Some _, _ -> Ally row
    | "Trash", _, Some _ -> Trash row
    | s, Some _, _ when s.Contains "Upgrade:" -> { row with Name = String.replace "Upgrade:" "" row.Name|> String.trimWhiteSpaces } |> UpgradeAlly 
    | s, None, _ when s.Contains "Upgrade:" -> { row with Name = String.replace "Upgrade:" "" row.Name |> String.trimWhiteSpaces } |> UpgradeMain 
    | _ -> Main row

let colToInt (c: string) =
    let charToInt ch pos = int (Char.ConvertToUtf32 (c, pos)) - 65
    match c with
    | c when c.Length = 1 -> charToInt c 0
    | c when c.Length = 2 -> (charToInt c 0) * 26 + charToInt c 1

let itemAt col (r: string option list) =
    List.tryItem (colToInt col) r
    |> Option.bind id
    |> Option.map String.trimWhiteSpaces

let tryReadRow (r: string option list) =
    result {
        let! nameOrRowKind = itemAt NameCol r |> Result.requireSome $"No name provided for row {r}"
        let! faction = itemAt FactionCol r >>= codeToFaction |> Result.requireSome $"No faction provided for row {r}"
        let! kind = itemAt KindCol r |> Result.requireSome $"No kind provided for row {r}"
        let! text = itemAt TextCol r |> Result.requireSome $"No text provided for row {r}"
        let cardCount = itemAt CardCountCol r 
        let creditCost = itemAt CreditCostCol r 
        let strengthCost = itemAt StrengthCostCol r 
        let reward = itemAt RewardCol r 
        let creditGain = itemAt CreditGainCol r 
        let strengthGain = itemAt StrengthGainCol r 
        let energyGain = itemAt EnergyGainCol r 
        let rewardGain = itemAt RewardGainCol r 
        let shieldHealth = itemAt ShieldHealthCol r 
        let upgradeCost = itemAt UpgradeCostCol r 
        let flavorText = itemAt FlavorTextCol r 
        let ally = itemAt AllyCol r 
        let trash = itemAt TrashCol r 
        let row = {
            Name = nameOrRowKind
            Faction = faction
            Kind = kind
            Text = text
            CardCount = cardCount
            CreditCost = creditCost
            StrengthCost = strengthCost
            Reward = reward
            CreditGain = creditGain
            StrengthGain = strengthGain
            EnergyGain = energyGain
            RewardGain = rewardGain
            ShieldHealth = shieldHealth
            UpgradeCost = upgradeCost
            FlavorText = flavorText
        }
        return createPartial nameOrRowKind ally trash row
    }

let tryParseToMeasure<[<Measure>] 'a> = Option.bind tryParse<uint> >> Option.map LanguagePrimitives.UInt32WithMeasure<'a>

let parseCost kind c s =
    match kind, tryParseToMeasure<credit> c, tryParseToMeasure<strength> s with
    | _, Some c, None -> c |> CreditOnly |> Some
    | _, None, Some s -> s |> StrengthOnly |> Some
    // hack: only Mercenary can be either/or right now
    | "Mercenary", Some c, Some s -> CreditOrStrength (c, s) |> Some
    | _, Some c, Some s -> CreditAndStrength (c, s) |> Some
    | _ -> None

let rowToAbility (row: PartialCard) = 
    { Text = row.Text
      IsInfinite = false
      Metadata = {
        CreditGain = tryParseToMeasure row.CreditGain
        StrengthGain = tryParseToMeasure row.StrengthGain
        EnergyGain = tryParseToMeasure row.EnergyGain
        RewardGain = tryParseToMeasure row.RewardGain } }

let tryCreateCard main ally trash =
    result {
        let core = {
            Name = main.Name
            MainAbility = rowToAbility main
            Cost = parseCost main.Kind main.CreditCost main.StrengthCost
            Reward = main.Reward >>= tryParse<uint>
            Count = main.CardCount >>= tryParse<uint>
            Faction = main.Faction
        }
        let upgraded = main.UpgradeCost >>= tryParse<uint> |> Option.isSome
        match main.Kind with
        | "Ship" -> 
            return! Ship {
                Core = core
                AllyAbility = ally
                TrashAbility = trash
                Upgraded = upgraded
            } |> Ok
        | "Fleet" ->
            return! Fleet {
                Core = core
                AllyAbility = ally
                TrashAbility = trash
                Upgraded = upgraded
            } |> Ok
        | "Shield" ->
            let! health = tryParseToMeasure main.ShieldHealth |> Result.requireSome "Health must be provided for shield cards"
            return! Shield {
                Core = core
                Health = health
                Upgraded = upgraded
            } |> Ok
        | "Mercenary" ->
            return! Mercenary {
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
        | "Planet"
        | _ -> return! Error $"No kind provided for {main.Name}: {main} {ally} {trash}"
    }

let partialRowsToCard (rows: string option list list) =
    let tryChoose chooser r = r |> List.choose chooser |> List.tryHead
    validation {
        let! parsed = rows |> List.traverseResultA tryReadRow
        let! main = parsed 
                    |> List.choose (function Main s -> Some s | _ -> None) 
                    |> List.tryExactlyOne 
                    |> Result.requireSome $"Could not read rows {rows}"
        let ally = parsed |> tryChoose (function Ally s -> Some s | _ -> None) |>> rowToAbility
        let trash = parsed |> tryChoose (function Trash s -> Some s | _ -> None) |>> rowToAbility
        let upgrade = parsed |> tryChoose (function UpgradeMain s -> Some s | _ -> None)
        let upgradeAlly = parsed |> tryChoose (function UpgradeAlly s -> Some s | _ -> None) |>> rowToAbility
        let! main = tryCreateCard main ally trash
        let! maybeUpgrade = upgrade |> Option.map (fun s -> tryCreateCard s upgradeAlly None) |> Option.sequenceResult
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
        |> List.map (rowToList >> List.singleton >> partialRowsToCard)
        |> List.append 
            (opened.Body.Spreadsheet.Tables[1].TableRowGroups
            |> List.ofArray
            |> List.map (fun s -> s.TableRows |> List.ofArray |> List.map rowToList2 |> partialRowsToCard))
        |> Result.partition
    let cards, maybeCards = cards |> List.unzip
    maybeCards |> List.choose id |> List.append cards, errors |> List.collect id