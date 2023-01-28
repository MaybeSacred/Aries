module Types

open ImageMagick

[<Measure>]
type dot

[<Measure>]
type inch

[<Measure>]
type credit

[<Measure>]
type strength

[<Measure>]
type hp = strength

[<Measure>]
type energy

[<Measure>]
type clout

[<Literal>]
let ImagesPath = @"Images"

[<Literal>]
let CardPath = @"Cards"

type Icon = {
    Path: string 
    ScaleCorrection: float 
}

type FactionData = {
    Primary: MagickColor
    Secondary: MagickColor
    Icon: Icon option
    Name: string
}

type AbilityMetadata = { 
    CreditGain: uint<credit> option
    StrengthGain: uint<strength> option
    EnergyGain: uint<energy> option
    CloutGain: uint<clout> option
}

let defaultMetadata = { 
    CreditGain = None
    StrengthGain = None
    EnergyGain = None
    CloutGain = None
}

type MainAbility = {
    Text: string 
    Metadata: AbilityMetadata  
}

type AllyAbility = {
    Text: string 
    Metadata: AbilityMetadata
    IsInfinite: bool
}

type ScrapAbility = {
    Text: string 
    Metadata: AbilityMetadata
}

type CardCost =
    | CreditOnly of uint<credit>
    | StrengthOnly of uint<strength>
    | CreditAndStrength of credit: uint<credit> * strength: uint<strength>

// it's card-core parkour
type CardCore = {
    Name: string
    MainAbility: MainAbility
    Cost: CardCost option
    Count: uint option
    Clout: uint option
}

type FleetOrShip = {
    Core: CardCore
    Faction: FactionData
    AllyAbility: AllyAbility option
    ScrapAbility: ScrapAbility option
}

type Shield = {
    Core: CardCore
    Faction: FactionData
    Health: uint<hp>
}

type CloutSchedule = { First: uint<clout>; Second: uint<clout>; Third: uint<clout> }

type Planet = {
    Core: CardCore
    Health: uint<hp>
    CloutSchedule: CloutSchedule
}

type Card = 
    | Shield of Shield 
    | Ship of FleetOrShip
    | Fleet of FleetOrShip
    | Planet of Planet

let cardKind =
    function  
    | Shield _ -> "Shield"
    | Ship _ -> "Ship"
    | Fleet _ -> "Fleet"
    | Planet _ -> "Planet"

let name =
    function  
    | Shield { Core = { Name = name } } 
    | Ship { Core = { Name = name } } 
    | Fleet { Core = { Name = name } } 
    | Planet { Core = { Name = name } } -> name

type ImageState = {
    Image: MagickImage
    Drawables: IDrawables<byte>
    Width: float<dot>
    Height: float<dot>
}

let shipIcon = {
    Path = @"Ship.webp"
    ScaleCorrection = 0.98
}

let fleetIcon = {
    Path = @"FleetLogo.png"
    ScaleCorrection = 0.98
}

let shieldIcon = {
    Path = @"ShieldOutlineLogo.png"
    ScaleCorrection = 0.98
}

let trashIcon = {
    Path = @"icons8-trash-can-50.png"
    ScaleCorrection = 0.85
}

let unaligned = {
    Primary = new MagickColor(0xAAuy, 0x27uy, 0x04uy)
    Secondary = new MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = None
    Name = "Unaligned"
}

let imperium = {
    Primary = new MagickColor(0xAAuy, 0x27uy, 0x04uy)
    Secondary = new MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = Some {
        Path = @"RustRedImperiumLogoUpdated.png"
        ScaleCorrection = 1.0
    }
    Name = "Imperium"
}

let stellarion = {
    Primary = new MagickColor(0x0Auy, 0xA0uy, 0xDEuy)
    Secondary = new MagickColor(0x00uy, 0x1Euy, 0x6Cuy)
    Icon = Some {
        Path = @"StellarionLogo.png"
        ScaleCorrection = 1.0
    }
    Name = "Stellarion"
}

let botBrigade = {
    Primary = new MagickColor(0xdeuy, 0xaeuy, 0x01uy)
    Secondary = new MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = Some {
        Path = @"BattleBotLogoUpdated.png"
        ScaleCorrection = 0.85
    }
    Name = "Bot Brigade"
}

let rogueAlliance = {
    Primary = MagickColor(0x05uy, 0x45uy, 0x16uy)
    Secondary = new MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = Some {
        Path = @"RogueAllianceLogo.webp"
        ScaleCorrection = 1.0
    }
    Name = "Rogue Alliance"
}

let sparky = Ship {
    Core = {
        Name = "Metallic Hydrogen Supplier"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata }
        Cost = Some <| CreditOnly 88u<credit>
        Clout = Some 88u
        Count = Some 3u
    }
    Faction = rogueAlliance
    AllyAbility = Some { Text = "Draw 1 card. Some other really long text to see what happens"; Metadata = defaultMetadata; IsInfinite = true }
    ScrapAbility = None
}

let imperialFighter = Ship {
    Core = {
        Name = "Imperial Fighter"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata }
        Cost = Some <| CreditOnly 88u<credit>
        Clout = Some 88u
        Count = Some 3u
    }
    Faction = imperium
    AllyAbility = None
    ScrapAbility = Some { Text = "Scrap this card. Gain 1 Strength"; Metadata = defaultMetadata }
}

let ``343rd Batallion`` = Fleet {
    Core = {
        Name = "343rd Batallion"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata }
        Cost = Some <| StrengthOnly 88u<strength>
        Clout = Some 88u
        Count = Some 3u
    }
    Faction = botBrigade
    AllyAbility = Some { Text = "Draw 1 card. Some other really long text to see what happens"; Metadata = defaultMetadata; IsInfinite = true }
    ScrapAbility = Some { Text = "Scrap this card. Gain 1 Strength"; Metadata = defaultMetadata }
}

let refractiveShield = Shield {
    Core = {
        Name = "Refractive Shield"
        MainAbility = { 
            Text = "Draw 1 card. Some really long text to see what happens"; 
            Metadata = { 
                CreditGain = Some 1u<credit>
                StrengthGain = Some 1u<strength>
                EnergyGain = Some 1u<energy>
                CloutGain = Some 1u<clout>
        } }
        Cost = Some <| CreditAndStrength (88u<credit>, 88u<strength>)
        Clout = Some 88u
        Count = Some 3u
    }
    Faction = stellarion
    Health = 9u<hp>
}

let planet = Planet {
    Core = {
        Name = "Vega"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata }
        Cost = Some <| CreditOnly 88u<credit>
        Clout = Some 88u
        Count = Some 1u
    }
    Health = 3u<hp>
    CloutSchedule = { First = 3u<clout>; Second = 2u<clout>; Third = 1u<clout> }
}

let sampleCards = 
    [sparky; imperialFighter; ``343rd Batallion``; refractiveShield]