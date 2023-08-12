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
    AnimaLoss: uint<anima> option
    FavorGain: uint<favor> option
}

let defaultMetadata = { 
    TradeGain = None
    StrengthGain = None
    AnimaGain = None
    AnimaLoss = None
    FavorGain = None
}

type PlainOrTrashAbility = {
    Text: string 
    Metadata: AbilityMetadata
}

type AllyAbility = {
    Text: string 
    Metadata: AbilityMetadata
    Faction: FactionData
}

type AnimaAbility = {
    Text: string 
    Metadata: AbilityMetadata
    Cost: uint<anima>
}

type Ability = 
    | Plain of PlainOrTrashAbility 
    | Ally of AllyAbility 
    | Anima of AnimaAbility 
    | Trash of PlainOrTrashAbility

    member x.Text =
        match x with 
        | Plain s -> s.Text
        | Ally s -> s.Text
        | Anima s -> s.Text
        | Trash s -> s.Text

    member x.Metadata =
        match x with 
        | Plain s -> s.Metadata
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
    MainAbility: Ability
    Cost: CardCost
    Image: ImageData
    Count: uint
    ShowCount: bool
    Favor: uint option
    FlavorText: string option
    SubKind: string option
}

type Human = {
    Core: CardCore
    Faction: FactionData
    SecondaryAbility: Ability option
    Upgraded: bool
}

type Building = {
    Core: CardCore
    Faction: FactionData
    RightSlot: Slot option
    Upgraded: bool
}

type Nomad = {
    Core: CardCore
}

type Creature = {
    Core: CardCore
}

type Relic = {
    Core: CardCore
}

type God = {
    Core: CardCore
    Faction: FactionData
}
// aegis? battlement? fortification?
type Fortification = {
    Core: CardCore
    Faction: FactionData
    Health: uint<hp>
    LeftSlot: Slot option
    Upgraded: bool
}

//type FavorSchedule = { First: uint<favor>; Second: uint<favor>; Third: uint<favor> }

type Settlement = {
    Core: CardCore
    Faction: FactionData
    Health: uint<hp>
    SecondaryAbility: Ability option
    TertiaryAbility: Ability option
    LeftSlot: Slot option
    RightSlot: Slot option
    // top slot can only have a God
    TopSlot: Slot option
    /// allowed slot types
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
    | Creature of Creature
    | Relic of Relic


let faction =
    function  
    | Fortification { Faction = f } 
    | Human { Faction = f } 
    | Building { Faction = f } 
    | Settlement { Faction = f } 
    | God { Faction = f }
        -> Some f
    | Nomad _ 
    | Creature _ 
    | Relic _ -> None

let core =
    function
    | Fortification { Core = c } 
    | Human { Core = c } 
    | Building { Core = c } 
    | Settlement { Core = c }
    | God { Core = c }
    | Nomad { Core = c }
    | Creature { Core = c }
    | Relic { Core = c }
        -> c

let cardKind c =
    let subKind = (core c).SubKind |> Option.map (fun s -> " • " + s) |> Option.defaultValue ""
    match c with
    | Fortification f -> "Fortification"
    | Human h -> "Human"
    | Building b -> "Building"
    | Settlement s -> "Settlement"
    | God g -> "God"
    | Nomad n -> "Nomad"
    | Creature c -> "Creature"
    | Relic r -> "Relic"
    |> fun s -> s + subKind

let name c = (core c).Name

// data

let femaleHumanImage = {
    Path = @"female-hero.webp"
    ScaleCorrection = 0.98
    Opacity = 1.
}

let maleHumanImage = {
    Path = @"male-champion.webp"
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

let animaImage = {
    Path = @"star-icon.webp"
    ScaleCorrection = 1.5
    Opacity = 1.
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

let creature1Image = {
    Path = @"mist-dragon.webp"
    ScaleCorrection = 0.98
    Opacity = 0.8
}

let creature2Image = {
    Path = @"monster-flying.webp"
    ScaleCorrection = 0.98
    Opacity = 0.8
}

let spaceCreatureIcons = [
    creature1Image
    creature2Image
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

let creature = {
    Primary = MagickColors.DarkRed
    Icon = Some {
        Path = @"MonsterLogoUpdated.webp"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Creature"
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
        Path = @"hand.webp"
        ScaleCorrection = 1.1
        Opacity = 1.
    }
    Name = "Ancient"
}

let indian = {
    Primary = MagickColors.RebeccaPurple
    Icon = Some {
        Path = @"indian-logo-3.webp"
        ScaleCorrection = 1.3
        Opacity = 1.
    }
    Name = "Velurian"//Velur
}
// todo: color purple
let nativeAmerican = {
    Primary = MagickColors.ForestGreen
    Icon = Some {
        Path = @"feather-logo-3.webp"
        ScaleCorrection = 1.0
        Opacity = 1.
    }
    Name = "Raviraw"//Ravirawan
}

let egyptian = {
    Primary = MagickColors.CadetBlue
    Icon = Some {
        Path = @"eye-of-horus.webp"
        ScaleCorrection = 1.25
        Opacity = 1.
    }
    Name = "Setumi"//Setum: Set + Atum
}

let sumerian = {
    Primary = MagickColors.OrangeRed
    Icon = Some {
        Path = @"sumerian-logo.webp"
        ScaleCorrection = 1.2
        Opacity = 1.
    }
    Name = "Hitturian"//Hitturia
}

let druidic = {
    Primary = MagickColors.SaddleBrown
    Icon = Some {
        Path = @"druid-icon.webp"
        ScaleCorrection = 1.9
        Opacity = 1.
    }
    Name = "Artonian"//Artonia
}

let iconography =
    function  
    | Fortification { Faction = f } 
    | Human { Faction = f } 
    | Building { Faction = f } 
    | Settlement { Faction = f } 
    | God { Faction = f } -> f
    | Nomad _ -> nomad
    | Creature _  -> creature
    | Relic _ -> relic

let hero = {
    Core = {
        Name = "Hero"
        MainAbility = Plain { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata }
        Cost = { Trade = Some 88u<trade>; Strength = None; Anima = None }
        Favor = Some 88u
        Image = femaleHumanImage
        Count = 3u
        ShowCount = true
        FlavorText = Some "Flavor text"
        SubKind = Some "Soldier"
    }
    Faction = druidic
    SecondaryAbility = Some <| Ally { Text = "Draw 1 card. Some other really long text to see what happens"; Metadata = defaultMetadata; Faction = druidic }
    Upgraded = true
}

let bookOfTheDead = Relic {
    Core = {
        Name = "Book of the Dead"
        MainAbility = Trash { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata }
        Cost = { Trade = None; Strength = None; Anima = Some 88u<anima> } 
        Favor = Some 88u
        Image = relicImage
        Count = 3u
        ShowCount = true
        FlavorText = Some "Flavor text"
        SubKind = None
    }
}

let building = Building {
    Core = {
        Name = "Temple of Babylon"
        MainAbility = Anima { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; Cost = 88u<anima> }
        Cost = { Trade = Some 88u<trade>; Strength = None; Anima = None } 
        Favor = Some 88u
        Image = buildingImage
        Count = 3u
        ShowCount = true
        FlavorText = Some "Flavor text"
        SubKind = None
    }
    Faction = sumerian
    Upgraded = false
    RightSlot = Some GodSlot
}

let ogre = Creature {
    Core = {
        Name = "Ogre"
        MainAbility = Plain { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata }
        Cost = { Trade = None; Strength = Some 88u<strength>; Anima = None } 
        Favor = Some 1u
        Image = creature1Image
        Count = 1u
        ShowCount = false
        FlavorText = Some "Flavor text"
        SubKind = None
    }
}

let camelArcher = Nomad {
    Core = {
        Name = "Camel Archer"
        MainAbility = Plain { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata }
        Cost = { Trade = Some 88u<trade>; Strength = None; Anima = None } 
        Favor = Some 1u
        Image = nomadImage
        Count = 1u
        ShowCount = false
        FlavorText = Some "Flavor text"
        SubKind = None
    }
}

let fort = Fortification {
    Core = {
        Name = "Egyptian Fort"
        MainAbility = Plain { 
            Text = "Draw 1 card. Some really long text to see what happens"
            Metadata = { 
                TradeGain = Some 8u<trade>
                StrengthGain = Some 8u<strength>
                AnimaGain = Some 8u<anima>
                AnimaLoss = Some 8u<anima>
                FavorGain = Some 8u<favor>
        } }
        Cost = { Trade = Some 88u<trade>; Strength = None; Anima = None } 
        Favor = Some 88u
        Image = fortificationImage
        Count = 3u
        ShowCount = true
        FlavorText = Some "Flavor text"
        SubKind = None
    }
    Faction = indian
    Health = 9u<hp>
    Upgraded = true
    LeftSlot = Some FortificationSlot
}

let zeus = God {
    Core = {
        Name = "Zeus"
        MainAbility = Anima { 
            Text = "Draw 1 card. Some really long text to see what happens"
            Cost = 88u<anima>
            Metadata = { 
                TradeGain = Some 8u<trade>
                StrengthGain = Some 8u<strength>
                AnimaGain = Some 8u<anima>
                AnimaLoss = Some 8u<anima>
                FavorGain = Some 8u<favor>
        } }
        Cost = { Trade = Some 88u<trade>; Strength = Some 88u<strength>; Anima = Some 88u<anima> } 
        Favor = Some 88u
        Image = godImage
        Count = 3u
        ShowCount = true
        FlavorText = Some "Flavor text"
        SubKind = Some "Thunder"
    }
    Faction = ancient
}

let settlement = Settlement {
    Core = {
        Name = "Vegas"
        MainAbility = Plain { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata }
        Cost = { Trade = Some 88u<trade>; Strength = None; Anima = Some 88u<anima> } 
        Favor = Some 88u
        Image = settlementImage
        Count = 1u
        ShowCount = true
        FlavorText = Some "Flavor text"
        SubKind = None
    }
    Faction = unaligned
    Health = 8u<hp>
    SecondaryAbility = Some <| Anima { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; Cost = 88u<anima> }
    TertiaryAbility = Some <| Anima { Text = "Draw 1 card. Some really long text to see what happens"; Metadata = defaultMetadata; Cost = 2u<anima> }
    LeftSlot = Some FortificationSlot
    TopSlot = Some GodSlot
    RightSlot = Some BuildingSlot
    GarrisonSlot = [FortificationSlot; Garrison; BuildingSlot]
}

let sampleCards = 
    [settlement; 
     Human hero; 
     Human { hero with Core = { hero.Core with Name = "Hopi Archer" }; Faction = nativeAmerican }; 
     Human { hero with Core = { hero.Core with Name = "Basic" }; Faction = unaligned }; 
     Human { hero with Core = { hero.Core with Name = "Ancient Man" }; Faction = ancient }; 
     Human { hero with 
                Core = { hero.Core with Name = "Ancient Woman" }; Faction = ancient; 
                SecondaryAbility = Some <| Anima { 
                    Text = "Draw 1 card. Some really long text to see what happens"
                    Cost = 88u<anima>
                    Metadata = defaultMetadata } }; 
     Human { hero with 
                Core = { hero.Core with Name = "Ancient Person" }; Faction = ancient; 
                SecondaryAbility = Some <| Trash { 
                    Text = "Scrap this card"; 
                    Metadata = defaultMetadata } }; 
     Human { hero with 
                Core = { hero.Core with Name = "Big Daddy" }; Faction = ancient; 
                SecondaryAbility = Some <| Ally { 
                    Text = "Scrap this card"
                    Faction = indian
                    Metadata = defaultMetadata; } }; 
     Human { hero with Core = { hero.Core with Name = "Tutankamen" }; Faction = egyptian }; 
     Human { hero with Core = { hero.Core with Name = "Sumer Queen" }; Faction = sumerian }; 
     Human { hero with Core = { hero.Core with Name = "Vishatriya" }; Faction = indian }; 
     ogre; 
     zeus; 
     fort;
     bookOfTheDead; 
     building; 
     camelArcher]