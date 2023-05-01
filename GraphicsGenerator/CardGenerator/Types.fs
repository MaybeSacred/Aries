module Types

open ImageMagick

[<Measure>]
type trade

[<Measure>]
type strength

[<Measure>]
type hp = strength

[<Measure>]
type anima

[<Measure>]
type favor

type ImageData = {
    Path: string 
    ScaleCorrection: float 
    Opacity: float
}

type FactionData = {
    Primary: MagickColor
    Secondary: MagickColor
    Icon: ImageData option
    Name: string
}

type AbilityMetadata = { 
    TradeGain: uint<trade> option
    StrengthGain: uint<strength> option
    AnimaGain: uint<anima> option
    AnimaCost: uint<anima> option
    FavorGain: uint<favor> option
}

let defaultMetadata = { 
    TradeGain = None
    StrengthGain = None
    AnimaGain = None
    AnimaCost = None
    FavorGain = None
}

type MainAbility = {
    Text: string 
    Metadata: AbilityMetadata
    Cost: uint<anima> option
}

type AllyAbility = {
    Text: string 
    Metadata: AbilityMetadata
    Faction: FactionData
}

type TrashAbility = {
    Text: string 
    Metadata: AbilityMetadata
}

type AnimaAbility = {
    Text: string 
    Metadata: AbilityMetadata
    Cost: uint<anima>
}

type Ability = 
    | Main of MainAbility 
    | Ally of AllyAbility 
    | Anima of AnimaAbility 
    | Trash of TrashAbility

    member x.Text =
        match x with 
        | Main s -> s.Text
        | Ally s -> s.Text
        | Anima s -> s.Text
        | Trash s -> s.Text

type CardCost =
    | TradeOnly of uint<trade>
    | StrengthOnly of uint<strength>
    | TradeOrStrength of trade: uint<trade> * strength: uint<strength>
    | TradeAndStrength of trade: uint<trade> * strength: uint<strength>

// it's card-core parkour
type CardCore = {
    Name: string
    MainAbility: MainAbility
    Cost: CardCost option
    Count: uint
    ShowCount: bool
    Favor: uint option
    Faction: FactionData
    FlavorText: string option
}

type BuildingOrHuman = {
    Core: CardCore
    SecondaryAbility: Ability option
    Upgraded: bool
}

type Nomad = {
    Core: CardCore
}

type Monster = {
    Core: CardCore
}

type God = {
    Core: CardCore
}

type Relic = {
    Core: CardCore
}
// aegis? battlement? fortification?
type Shield = {
    Core: CardCore
    Health: uint<hp>
    Upgraded: bool
}

//type FavorSchedule = { First: uint<favor>; Second: uint<favor>; Third: uint<favor> }

type Settlement = {
    Core: CardCore
    Health: uint<hp>
    //FavorSchedule: FavorSchedule
}

type Card = 
    | Shield of Shield 
    | Human of BuildingOrHuman
    | Building of BuildingOrHuman
    | Settlement of Settlement
    | God of God
    | Nomad of Nomad
    | Monster of Monster
    | Relic of Relic

let cardKind =
    function  
    | Shield _ -> "Shield"
    | Human _ -> "Human"
    | Building _ -> "Building"
    | Settlement _ -> "Settlement"
    | God _ -> "God"
    | Nomad _ -> "Nomad"
    | Monster _ -> "Monster"
    | Relic _ -> "Relic"

let core =
    function
    | Shield { Core = c } 
    | Human { Core = c } 
    | Building { Core = c } 
    | Settlement { Core = c }
    | God { Core = c }
    | Nomad { Core = c }
    | Monster { Core = c }
    | Relic { Core = c }
        -> c

let name c = core c |> fun s -> s.Name

// data

let humanImage = {
    Path = @"Human.webp"
    ScaleCorrection = 0.98
    Opacity = 1.
}

let buildingImage = {
    Path = @"BuildingLogo.png"
    ScaleCorrection = 0.98
    Opacity = 1.
}

let shieldImage = {
    Path = @"ShieldOutlineLogoUpdated.png"
    ScaleCorrection = 0.98
    Opacity = 0.8
}

let trashImage = {
    Path = @"icons8-trash-can-50.png"
    ScaleCorrection = 0.85
    Opacity = 1.
}

let settlementImage = {
    Path = @"SettlementGrayIcon.png"
    ScaleCorrection = 0.98
    Opacity = 1.
}

let relicImage = {
    Path = @"RelicImage.png"
    ScaleCorrection = 0.98
    Opacity = 0.8
}

let spaceNomadImage = {
    Path = @"SpaceNomadUpdated.webp"
    ScaleCorrection = 0.98
    Opacity = 0.8
}

let spaceMonster1Image = {
    Path = @"SpaceMonster1.webp"
    ScaleCorrection = 0.98
    Opacity = 0.8
}

let spaceMonster2Image = {
    Path = @"SpaceMonster2.webp"
    ScaleCorrection = 0.98
    Opacity = 0.8
}

let spaceMonster3Image = {
    Path = @"SpaceMonster3.webp"
    ScaleCorrection = 0.98
    Opacity = 0.8
}

let spaceMonster4Image = {
    Path = @"SpaceMonster4.webp"
    ScaleCorrection = 0.98
    Opacity = 0.8
}

let spaceMonster5Image = {
    Path = @"SpaceMonster5.webp"
    ScaleCorrection = 0.98
    Opacity = 0.8
}

let spaceMonsterIcons = [
    spaceMonster1Image
    spaceMonster2Image
    spaceMonster3Image
    spaceMonster4Image
    spaceMonster5Image
]

let unaligned = {
    Primary = MagickColor(0xAFuy, 0xAFuy, 0xAFuy)
    Secondary = MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = None
    Name = "Unaligned"
}

let all = {
    Primary = MagickColor(0xAFuy, 0xAFuy, 0xAFuy)
    Secondary = MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = Some {
        Path = @"NomadLogoUpdated.png"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "All"
}

let nomad = {
    Primary = MagickColor(0x5Fuy, 0x68uy, 0x7Auy)
    Secondary = MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = Some {
        Path = @"NomadLogoUpdated.png"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Nomad"
}

let monster = {
    Primary = MagickColor(0x6Fuy, 0x6Fuy, 0x7Auy)
    Secondary = MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = Some {
        Path = @"MonsterLogoUpdated.webp"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Monster"
}

let relic = {
    Primary = MagickColors.Purple
    Secondary = MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = Some {
        Path = @"RelicLogo.webp"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Relic"
}

let imperium = {
    Primary = MagickColor(0xAAuy, 0x27uy, 0x04uy)
    Secondary = MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = Some {
        Path = @"RustRedImperiumLogoUpdated.png"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Imperium"
}

let stellarion = {
    Primary = MagickColor(0x0Auy, 0xA0uy, 0xDEuy)
    Secondary = MagickColor(0x00uy, 0x1Euy, 0x6Cuy)
    Icon = Some {
        Path = @"StellarionLogo.png"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Stellarion"
}

let botBrigade = {
    Primary = MagickColor(0xdeuy, 0xaeuy, 0x01uy)
    Secondary = MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = Some {
        Path = @"BattleBotLogoUpdated.png"
        ScaleCorrection = 0.85
        Opacity = 1.
    }
    Name = "Bot Brigade"
}

let rogueAlliance = {
    Primary = MagickColor(0x0Buy, 0x5Fuy, 0x1Cuy)
    Secondary = MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = Some {
        Path = @"RogueAllianceLogo.webp"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Rogue Alliance"
}

let metallicHydrogenSupplier = Human {
    Core = {
        Name = "Metallic Hydrogen Supplier"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; Cost = None }
        Cost = Some <| TradeOnly 88u<trade>
        Favor = Some 88u
        Count = 3u
        ShowCount = true
        Faction = rogueAlliance
        FlavorText = Some "Flavor text"
    }
    SecondaryAbility = Some <| Ally { Text = "Draw 1 card. Some other really long text to see what happens"; Metadata = defaultMetadata; Faction = rogueAlliance }
    Upgraded = true
}

let imperialFighter = Human {
    Core = {
        Name = "Imperial Fighter"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; Cost = None }
        Cost = Some <| TradeOnly 88u<trade>
        Favor = Some 88u
        Count = 3u
        ShowCount = true
        Faction = imperium
        FlavorText = Some "Flavor text"
    }
    SecondaryAbility = Some <| Trash { Text = "Scrap this card. Gain 1 Strength"; Metadata = defaultMetadata }
    Upgraded = false
}

let ``343rd Batallion`` = Building {
    Core = {
        Name = "343rd Batallion"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; Cost = None }
        Cost = Some <| StrengthOnly 88u<strength>
        Favor = Some 88u
        Count = 3u
        ShowCount = true
        Faction = botBrigade
        FlavorText = Some "Flavor text"
    }
    SecondaryAbility = Some <| Trash { Text = "Scrap this card. Gain 1 Strength"; Metadata = defaultMetadata}
    Upgraded = true
}

let bigTrade = Human {
    Core = {
        Name = "Big Trade"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; Cost = None }
        Cost = Some <| TradeOnly 22u<trade>
        Favor = Some 1u
        Count = 1u
        ShowCount = false
        Faction = unaligned
        FlavorText = Some "Flavor text"
    }
    SecondaryAbility = None
    Upgraded = false
}

let bigLaser = Human {
    Core = {
        Name = "Big Laser"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; Cost = None }
        Cost = Some <| TradeOrStrength (88u<trade>, 88u<strength>)
        Favor = Some 1u
        Count = 1u
        ShowCount = false
        Faction = unaligned
        FlavorText = Some "Flavor text"
    }
    SecondaryAbility = None
    Upgraded = false
}

let refractiveShield = Shield {
    Core = {
        Name = "Refractive Shield"
        MainAbility = { 
            Text = "Draw 1 card. Some really long text to see what happens"
            Cost = None
            Metadata = { 
                TradeGain = Some 8u<trade>
                StrengthGain = Some 8u<strength>
                AnimaGain = Some 8u<anima>
                AnimaCost = Some 8u<anima>
                FavorGain = Some 8u<favor>
        } }
        Cost = Some <| TradeAndStrength (88u<trade>, 88u<strength>)
        Favor = Some 88u
        Count = 3u
        ShowCount = true
        Faction = stellarion
        FlavorText = Some "Flavor text"
    }
    Health = 9u<hp>
    Upgraded = true
}

let settlement = Settlement {
    Core = {
        Name = "Vega"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; Cost = None }
        Cost = Some <| TradeOnly 88u<trade>
        Favor = Some 88u
        Count = 1u
        ShowCount = true
        Faction = unaligned
        FlavorText = Some "Flavor text"
    }
    Health = 8u<hp>
    //FavorSchedule = { First = 3u<favor>; Second = 2u<favor>; Third = 1u<favor> }
}

let sampleCards = 
    [settlement; metallicHydrogenSupplier; bigTrade; bigLaser; imperialFighter; ``343rd Batallion``; refractiveShield]