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
let FlavorTextCol = "AG"

type ParsedRow = string option list

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
    UpgradeCost: uint option
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

let createPartial kind upgrade ally trash row =
    match kind, upgrade, ally, trash with
    | "Ally", None, Some _, _ -> Ally row
    | "Trash", None, _, Some _ -> Trash row
    | s, None, Some _, _ when s.Contains "Upgrade:" -> { row with Name = String.replace "Upgrade:" "" row.Name |> String.trimWhiteSpaces } |> UpgradeAlly 
    | s, Some _, None, _ when s.Contains "Upgrade:" -> { row with Name = String.replace "Upgrade:" "" row.Name |> String.trimWhiteSpaces } |> UpgradeMain 
    | _ -> Main row

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
        let cardCount = itemAt CardCountCol r 
        let creditCost = itemAt CreditCostCol r 
        let strengthCost = itemAt StrengthCostCol r 
        let reward = itemAt RewardCol r 
        let creditGain = itemAt CreditGainCol r 
        let strengthGain = itemAt StrengthGainCol r 
        let energyGain = itemAt EnergyGainCol r 
        let rewardGain = itemAt RewardGainCol r 
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
        return createPartial nameOrRowKind upgradeCost ally trash row
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
        let upgraded = main.UpgradeCost |> Option.isSome
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
        | _ -> return! Error $"Unsupported row {main.Name}: %A{main} %A{ally} %A{trash}"
    }

let partialRowsToCard (rows: ParsedRow list) =
    let tryChoose chooser r = r |> List.choose chooser |> List.tryHead
    validation {
        let! parsed = rows |> List.traverseResultA tryReadRow
        let! main = parsed 
                    |> List.choose (function Main s -> Some s | _ -> None) 
                    |> List.tryExactlyOne 
                    |> Result.requireSome $"Could not read rows %A{parsed}: %A{rows}"
        let ally = parsed |> tryChoose (function Ally s -> Some s | _ -> None) |>> rowToAbility
        let trash = parsed |> tryChoose (function Trash s -> Some s | _ -> None) |>> rowToAbility
        let upgrade = parsed |> tryChoose (function UpgradeMain s -> Some s | _ -> None)
        let upgradeAlly = parsed |> tryChoose (function UpgradeAlly s -> Some s | _ -> None) |>> rowToAbility
        let! maybeUpgrade = upgrade |> Option.map (fun s -> tryCreateCard s upgradeAlly None) |> Option.sequenceResult
        let! main = tryCreateCard main ally trash
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
        |> List.map (List.map (List.take (colToInt FlavorTextCol + 1)) >> partialRowsToCard)
        |> Result.partition
    let cards, maybeCards = cards |> List.unzip
    maybeCards |> List.choose id |> List.append cards, errors |> List.collect id