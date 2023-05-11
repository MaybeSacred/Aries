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

    member x.Metadata =
        match x with 
        | Main s -> s.Metadata
        | Ally s -> s.Metadata
        | Anima s -> s.Metadata
        | Trash s -> s.Metadata

type CardCost = {
    Trade: uint<trade> option
    Strength: uint<strength> option
    Anima: uint<anima> option
}

type Slot =
    | GodSlot
    | FortificationSlot
    | BuildingSlot
    | Garrison

// it's card-core parkour
// TODO: add card image
type CardCore = {
    Name: string
    MainAbility: MainAbility
    Cost: CardCost
    Image: ImageData
    Count: uint
    ShowCount: bool
    Favor: uint option
    Faction: FactionData
    FlavorText: string option
}

type Human = {
    Core: CardCore
    SecondaryAbility: Ability option
    Upgraded: bool
}

type Building = {
    Core: CardCore
    RightSlot: Slot option
    Upgraded: bool
}

type Nomad = {
    Core: CardCore
}

type Monster = {
    Core: CardCore
}

type Relic = {
    Core: CardCore
}

type God = {
    Core: CardCore
}
// aegis? battlement? fortification?
type Fortification = {
    Core: CardCore
    Health: uint<hp>
    LeftSlot: Slot option
    Upgraded: bool
}

//type FavorSchedule = { First: uint<favor>; Second: uint<favor>; Third: uint<favor> }

type Settlement = {
    Core: CardCore
    Health: uint<hp>
    SecondaryAbility: Ability option
    TertiaryAbility: Ability option
    LeftSlot: Slot option
    RightSlot: Slot option
    // top slot can only have a God
    TopSlot: Slot option
    GarrisonSlot: Slot list
    //FavorSchedule: FavorSchedule
}

type Card = 
    | Fortification of Fortification 
    | Human of Human
    | Building of Building
    | Settlement of Settlement
    | God of God
    | Nomad of Nomad
    | Monster of Monster
    | Relic of Relic

let cardKind =
    function  
    | Fortification _ -> "Fortification"
    | Human _ -> "Human"
    | Building _ -> "Building"
    | Settlement _ -> "Settlement"
    | God _ -> "God"
    | Nomad _ -> "Nomad"
    | Monster _ -> "Monster"
    | Relic _ -> "Relic"

let core =
    function
    | Fortification { Core = c } 
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
    Path = @"female-hero.webp"
    ScaleCorrection = 0.98
    Opacity = 1.
}

let buildingImage = {
    Path = @"temple.webp"
    ScaleCorrection = 0.98
    Opacity = 1.
}

let fortificationImage = {
    Path = @"fort.webp"
    ScaleCorrection = 0.98
    Opacity = 0.8
}

let trashImage = {
    Path = @"icons8-trash-can-50.png"
    ScaleCorrection = 0.85
    Opacity = 1.
}

let settlementImage = {
    Path = @"settlement.webp"
    ScaleCorrection = 0.98
    Opacity = 1.
}

let relicImage = {
    Path = @"Relic.webp"
    ScaleCorrection = 0.98
    Opacity = 0.8
}

let nomadImage = {
    Path = @"cow-sumer-god.webp"
    ScaleCorrection = 0.98
    Opacity = 0.8
}

let godImage = {
    Path = @"cow-sumer-god.webp"
    ScaleCorrection = 0.98
    Opacity = 0.8
}

let monster1Image = {
    Path = @"mist-dragon.webp"
    ScaleCorrection = 0.98
    Opacity = 0.8
}

let monster2Image = {
    Path = @"monster-flying.webp"
    ScaleCorrection = 0.98
    Opacity = 0.8
}

let spaceMonsterIcons = [
    monster1Image
    monster2Image
]

let unaligned = {
    Primary = MagickColors.Gray
    Icon = None
    Name = "Unaligned"
}

let all = {
    Primary = MagickColors.AntiqueWhite
    Icon = Some {
        Path = @"star-icon.webp"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "All"
}

let nomad = {
    Primary = MagickColors.SandyBrown
    Icon = Some {
        Path = @"BattleBotLogoUpdated.png"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Nomad"
}

let monster = {
    Primary = MagickColors.DarkRed
    Icon = Some {
        Path = @"MonsterLogoUpdated.webp"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Monster"
}

let relic = {
    Primary = MagickColors.Purple
    Icon = Some {
        Path = @"RelicLogo.webp"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Relic"
}

let ancient = {
    // dark gray
    Primary = MagickColor(0x0Auy, 0x0Auy, 0x1Fuy)
    Icon = Some {
        Path = @"indian-logo-3.webp"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Ancient"//Velur
}

let indian = {
    Primary = MagickColors.Violet
    Icon = Some {
        Path = @"indian-logo-3.webp"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Velurian"//Velur
}
// todo: color purple
let nativeAmerican = {
    Primary = MagickColors.PaleVioletRed
    Icon = Some {
        Path = @"feather-logo-3.webp"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Hopi"
}

let egyptian = {
    Primary = MagickColors.AliceBlue
    Icon = Some {
        Path = @"eye-of-horus.webp"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Setumi"//Setum: Set + Atum
}

let sumerian = {
    Primary = MagickColors.DarkOrange
    Icon = Some {
        Path = @"sumerian-logo.webp"
        ScaleCorrection = 0.85
        Opacity = 1.
    }
    Name = "At-Hurian"//At-Hur
}

let druidic = {
    Primary = MagickColors.ForestGreen
    Icon = Some {
        Path = @"druid-icon.webp"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Artonian"//Artonia
}

let hero = {
    Core = {
        Name = "Hero"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; Cost = None }
        Cost = { Trade = Some 88u<trade>; Strength = None; Anima = None }
        Favor = Some 88u
        Image = humanImage
        Count = 3u
        ShowCount = true
        Faction = druidic
        FlavorText = Some "Flavor text"
    }
    SecondaryAbility = Some <| Ally { Text = "Draw 1 card. Some other really long text to see what happens"; Metadata = defaultMetadata; Faction = druidic }
    Upgraded = true
}

let bookOfTheDead = Relic {
    Core = {
        Name = "Book of the Dead"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; Cost = None }
        Cost = { Trade = None; Strength = None; Anima = Some 88u<anima> } 
        Favor = Some 88u
        Image = relicImage
        Count = 3u
        ShowCount = true
        Faction = relic
        FlavorText = Some "Flavor text"
    }
}

let building = Building {
    Core = {
        Name = "Temple of Babylon"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; Cost = None }
        Cost = { Trade = Some 88u<trade>; Strength = Some 88u<strength>; Anima = None } 
        Favor = Some 88u
        Image = buildingImage
        Count = 3u
        ShowCount = true
        Faction = sumerian
        FlavorText = Some "Flavor text"
    }
    Upgraded = false
    RightSlot = Some GodSlot
}

let ogre = Monster {
    Core = {
        Name = "Ogre"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; Cost = None }
        Cost = { Trade = None; Strength = Some 88u<strength>; Anima = None } 
        Favor = Some 1u
        Image = monster1Image
        Count = 1u
        ShowCount = false
        Faction = monster
        FlavorText = Some "Flavor text"
    }
}

let camelArcher = Nomad {
    Core = {
        Name = "Camel Archer"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; Cost = None }
        Cost = { Trade = Some 88u<trade>; Strength = Some 88u<strength>; Anima = None } 
        Favor = Some 1u
        Image = nomadImage
        Count = 1u
        ShowCount = false
        Faction = nomad
        FlavorText = Some "Flavor text"
    }
}

let fort = Fortification {
    Core = {
        Name = "Egyptian Fort"
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
        Cost = { Trade = Some 88u<trade>; Strength = Some 88u<strength>; Anima = None } 
        Favor = Some 88u
        Image = fortificationImage
        Count = 3u
        ShowCount = true
        Faction = indian
        FlavorText = Some "Flavor text"
    }
    Health = 9u<hp>
    Upgraded = true
    LeftSlot = Some FortificationSlot
}

let zeus = God {
    Core = {
        Name = "Zeus"
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
        Cost = { Trade = Some 88u<trade>; Strength = Some 88u<strength>; Anima = Some 88u<anima> } 
        Favor = Some 88u
        Image = godImage
        Count = 3u
        ShowCount = true
        Faction = ancient
        FlavorText = Some "Flavor text"
    }
}

let settlement = Settlement {
    Core = {
        Name = "Vegas"
        MainAbility = { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; Cost = None }
        Cost = { Trade = Some 88u<trade>; Strength = None; Anima = Some 88u<anima> } 
        Favor = Some 88u
        Image = settlementImage
        Count = 1u
        ShowCount = true
        Faction = unaligned
        FlavorText = Some "Flavor text"
    }
    Health = 8u<hp>
    SecondaryAbility = Some <| Anima { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; Cost = 88u<anima> }
    TertiaryAbility = Some <| Anima { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; Cost = 2u<anima> }
    LeftSlot = Some FortificationSlot
    TopSlot = Some GodSlot
    RightSlot = Some BuildingSlot
    GarrisonSlot = [FortificationSlot; Garrison; BuildingSlot]
    //FavorSchedule = { First = 3u<favor>; Second = 2u<favor>; Third = 1u<favor> }
}

let sampleCards = 
    [settlement; 
     Human hero; 
     Human { hero with Core = { hero.Core with Faction = nativeAmerican } }; 
     Human { hero with Core = { hero.Core with Faction = unaligned } }; 
     Human { hero with Core = { hero.Core with Faction = ancient } }; 
     Human { hero with Core = { hero.Core with Faction = egyptian } }; 
     Human { hero with Core = { hero.Core with Faction = sumerian } }; 
     Human { hero with Core = { hero.Core with Faction = indian } }; 
     ogre; 
     zeus; 
     fort;
     bookOfTheDead; 
     building; 
     camelArcher]