module Types

open ImageMagick

[<Measure>]
type credit

[<Measure>]
type strength

[<Measure>]
type hp = strength

[<Measure>]
type energy

[<Measure>]
type reward

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
    CreditGain: uint<credit> option
    StrengthGain: uint<strength> option
    EnergyGain: uint<energy> option
    RewardGain: uint<reward> option
}

let defaultMetadata = { 
    CreditGain = None
    StrengthGain = None
    EnergyGain = None
    RewardGain = None
}

type Ability = {
    Text: string 
    Metadata: AbilityMetadata
    IsInfinite: bool
}

type CardCost =
    | CreditOnly of uint<credit>
    | StrengthOnly of uint<strength>
    | CreditOrStrength of credit: uint<credit> * strength: uint<strength>
    | CreditAndStrength of credit: uint<credit> * strength: uint<strength>

// it's card-core parkour
type CardCore = {
    Name: string
    MainAbility: Ability
    Cost: CardCost option
    Count: uint
    ShowCount: bool
    Reward: uint option
    Faction: FactionData
    FlavorText: string option
}

type FleetOrShip = {
    Core: CardCore
    AllyAbility: Ability option
    TrashAbility: Ability option
    Upgraded: bool
}

type Mercenary = {
    Core: CardCore
}

type Monster = {
    Core: CardCore
}

type Relic = {
    Core: CardCore
}

type Shield = {
    Core: CardCore
    Health: uint<hp>
    Upgraded: bool
}

type RewardSchedule = { First: uint<reward>; Second: uint<reward>; Third: uint<reward> }

type Planet = {
    Core: CardCore
    Health: uint<hp>
    RewardSchedule: RewardSchedule
}

type Card = 
    | Shield of Shield 
    | Ship of FleetOrShip
    | Fleet of FleetOrShip
    | Planet of Planet
    | Mercenary of Mercenary
    | Monster of Monster
    | Relic of Relic

let cardKind =
    function  
    | Shield _ -> "Shield"
    | Ship _ -> "Ship"
    | Fleet _ -> "Fleet"
    | Planet _ -> "Planet"
    | Mercenary _ -> "Mercenary"
    | Monster _ -> "Monster"
    | Relic _ -> "Relic"

let core =
    function
    | Shield { Core = c } 
    | Ship { Core = c } 
    | Fleet { Core = c } 
    | Planet { Core = c }
    | Mercenary { Core = c }
    | Monster { Core = c }
    | Relic { Core = c }
        -> c

let name c = core c |> fun s -> s.Name

// data

let shipImage = {
    Path = @"Ship.webp"
    ScaleCorrection = 0.98
    Opacity = 1.
}

let fleetImage = {
    Path = @"FleetLogo.png"
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

let planetImage = {
    Path = @"PlanetGrayIcon.png"
    ScaleCorrection = 0.98
    Opacity = 1.
}

let relicImage = {
    Path = @"RelicImage.png"
    ScaleCorrection = 0.98
    Opacity = 0.8
}

let spaceMercenaryImage = {
    Path = @"SpaceMercenaryUpdated.webp"
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

let mercenary = {
    Primary = MagickColor(0x5Fuy, 0x68uy, 0x7Auy)
    Secondary = MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = Some {
        Path = @"MercenaryLogoUpdated.png"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Mercenary"
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
    Primary = MagickColor(0x05uy, 0x45uy, 0x16uy)
    Secondary = MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = Some {
        Path = @"RogueAllianceLogo.webp"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Rogue Alliance"
}

let metallicHydrogenSupplier = Ship {
    Core = {
        Name = "Metallic Hydrogen Supplier"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; IsInfinite = false }
        Cost = Some <| CreditOnly 88u<credit>
        Reward = Some 88u
        Count = 3u
        ShowCount = true
        Faction = rogueAlliance
        FlavorText = Some "Flavor text"
    }
    AllyAbility = Some { Text = "Draw 1 card. Some other really long text to see what happens"; Metadata = defaultMetadata; IsInfinite = true }
    TrashAbility = None
    Upgraded = true
}

let imperialFighter = Ship {
    Core = {
        Name = "Imperial Fighter"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; IsInfinite = false }
        Cost = Some <| CreditOnly 88u<credit>
        Reward = Some 88u
        Count = 3u
        ShowCount = true
        Faction = imperium
        FlavorText = Some "Flavor text"
    }
    AllyAbility = None
    TrashAbility = Some { Text = "Scrap this card. Gain 1 Strength"; Metadata = defaultMetadata; IsInfinite = false }
    Upgraded = false
}

let ``343rd Batallion`` = Fleet {
    Core = {
        Name = "343rd Batallion"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; IsInfinite = false }
        Cost = Some <| StrengthOnly 88u<strength>
        Reward = Some 88u
        Count = 3u
        ShowCount = true
        Faction = botBrigade
        FlavorText = Some "Flavor text"
    }
    AllyAbility = Some { Text = "Draw 1 card. Some other really long text to see what happens"; Metadata = defaultMetadata; IsInfinite = true }
    TrashAbility = Some { Text = "Scrap this card. Gain 1 Strength"; Metadata = defaultMetadata; IsInfinite = false }
    Upgraded = true
}

let bigCredit = Ship {
    Core = {
        Name = "Big Credit"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; IsInfinite = false }
        Cost = Some <| CreditOnly 22u<credit>
        Reward = Some 1u
        Count = 1u
        ShowCount = false
        Faction = unaligned
        FlavorText = Some "Flavor text"
    }
    AllyAbility = None
    TrashAbility = None
    Upgraded = false
}

let bigLaser = Ship {
    Core = {
        Name = "Big Laser"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; IsInfinite = false }
        Cost = Some <| CreditOrStrength (88u<credit>, 88u<strength>)
        Reward = Some 1u
        Count = 1u
        ShowCount = false
        Faction = unaligned
        FlavorText = Some "Flavor text"
    }
    AllyAbility = None
    TrashAbility = None
    Upgraded = false
}

let refractiveShield = Shield {
    Core = {
        Name = "Refractive Shield"
        MainAbility = { 
            Text = "Draw 1 card. Some really long text to see what happens";
            IsInfinite = false
            Metadata = { 
                CreditGain = Some 8u<credit>
                StrengthGain = Some 8u<strength>
                EnergyGain = Some 8u<energy>
                RewardGain = Some 8u<reward>
        } }
        Cost = Some <| CreditAndStrength (88u<credit>, 88u<strength>)
        Reward = Some 88u
        Count = 3u
        ShowCount = true
        Faction = stellarion
        FlavorText = Some "Flavor text"
    }
    Health = 9u<hp>
    Upgraded = true
}

let planet = Planet {
    Core = {
        Name = "Vega"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; IsInfinite = false }
        Cost = Some <| CreditOnly 88u<credit>
        Reward = Some 88u
        Count = 1u
        ShowCount = true
        Faction = unaligned
        FlavorText = Some "Flavor text"
    }
    Health = 8u<hp>
    RewardSchedule = { First = 3u<reward>; Second = 2u<reward>; Third = 1u<reward> }
}

let sampleCards = 
    [planet; metallicHydrogenSupplier; bigCredit; bigLaser; imperialFighter; ``343rd Batallion``; refractiveShield]