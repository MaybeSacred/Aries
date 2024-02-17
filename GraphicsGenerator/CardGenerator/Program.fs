open ImageMagick
open ImageMagick.Configuration
open System
open System.IO
open Microsoft.FSharp.Collections
open FSharp.Collections.ParallelSeq
open FSharpPlus
open System.Collections.Concurrent

open Types
open DrawingPrimitives

let topTextBottom = inset + ``3/8`` + (fontToDot medSize) + 2. * padding

let tradeStrengthDualCostOffset = ``1/16`` + 2.<dot>
// card
let cardMidpoint = 2.<inch> * dpi
let cardAbilityBottomPoint = cardBoundaries.HeightInInches * dpi - 2. * (``1/8`` + inset)

// god
let godVerticalMidpoint = 2.05<inch> * dpi
let godAbilityBottomPoint = godBoundaries.HeightInInches * dpi - 2. * (``1/8`` + inset)

// settlement
let settlementVerticalMidpoint = (settlementBoundaries.WidthInInches - cardBoundaries.WidthInInches) * dpi - ``1/4``
let settlementAbilityBottomPoint = settlementBoundaries.HeightInInches * dpi - 2. * (``1/8`` + inset)
let settlementAbilityTopPoint = 2.5<inch> * dpi

let iconCostRadius = ``3/16``
let iconCostDiameter = 2. * iconCostRadius
let iconCostOffset = iconCostRadius + inset + padding + quanta
let iconCostTextXOffset = iconCostRadius + inset + padding
let iconCostTextYOffset = iconCostRadius + inset - quanta
let favorRadius = ``1/8`` - quanta
let favorDiameter = 2. * favorRadius

// TODO: 
// draw some, scrap
// draw 1, gain anima
// wizards tower
// event cards that deal damage to settlements, discard garrisoned units, discard
// TODO: pick better icons for health, costs, anima gain

let drawFortificationAbilities (core: CardCore, health) textSize top radius (i: ImageState) =
    let availableHeight = cardMidpoint - top - textPadding
    [ Some <| float health, healthGreen
      Option.map float core.MainAbility.Metadata.TradeGain, tradeGold
      Option.map float core.MainAbility.Metadata.StrengthGain, strengthRed
      Option.map float core.MainAbility.Metadata.AnimaGain, animaBlue ]
    |> List.mapi (fun i (v, color) -> 
        match v with
        | Some value ->
            filledCircle color darkGray (radius + inset + abilityIconPadding) (top + textPadding + availableHeight * (float i / 4.) + radius) radius
            >> (text textSize TextAlignment.Center Center (radius + inset + abilityIconPadding + quanta) (top + textPadding + availableHeight * (float i / 4.) + (radius - padding)) <| $"{int value}")
        | None -> id)
    |> List.iter (fun s -> s i |> ignore)
    i

// TODO: remove inset to startX and width here, and add them to parameters
let drawAbilities (startX: float<dot>) (top: float<dot>) (width: float<dot>) (bottom: float<dot>) card (i: ImageState) =
    let drawTextAtHeight abilityTop height text =
        line darkGray lineworkWidth startX (top + abilityTop) (startX + width) (top + abilityTop)
        >> captionText medSize (startX + textPadding) (top + abilityTop + padding) (width - 2. * (textPadding)) (height - 2. * padding) text
    let drawAbility abilityTop height icon text =
        line darkGray lineworkWidth startX (top + abilityTop) (startX + width) (top + abilityTop)
        >> match icon with 
           | Some i -> 
                outlinedCircle (startX + ``5/32`` + abilityIconPadding) (top + abilityTop + height / 2.) ``5/32``
                >> overlayImageAsCircle (startX + abilityIconPadding) (top + abilityTop + (height / 2. - ``5/32``)) ``5/16`` i
                >> captionText medSize (startX + 2. * ``5/32`` + textPadding) (top + abilityTop + padding) (width - 2. * (textPadding + ``5/32``)) (height - 2. * padding) text
           | None -> 
                captionText medSize (startX + textPadding) (top + abilityTop + padding) (width - 2. * textPadding) (height - 2. * padding) text
    let iconForAbility =
        function 
        | Plain _ -> None
        | Ally a -> a.Faction.Icon
        | Trash _ -> Some trashImage
        | Anima _ -> Some animaImage
    let cardAbilityHalfPoint = (bottom - top) / 2.
    let cardAbilityThirdPoint = (bottom - top) / 3.
    let cardAbilityTwoThirdPoint = (bottom - top) * 2. / 3.
    let { MainAbility = main; FlavorText = flavor }, secondary = 
        match card with
        | Human { Core = core; SecondaryAbility = ally; GarrisonDamage = garriDmg } ->
            { core with 
                MainAbility = 
                    core.MainAbility 
                    |> updateAbilityText (fun s -> 
                        match core.SubKind |>> String.trimWhiteSpaces , garriDmg with
                        | Some "Soldier", Some u -> $"{s}. Deal {u} Damage if garrisoned against an Animist" 
                        | Some "Guard", Some u -> $"{s}. Deal {u} Damage if garrisoned against a Soldier" 
                        | Some "Animist", Some u -> $"{s}. Deal {u} Damage if garrisoned against a Guard" 
                        | Some "Soldier", None 
                        | Some "Guard", None 
                        | Some "Animist", None -> 
                            raise <| invalidOp $"Garrison card without damage %A{card}"
                        | _ -> s
                    ) }, ally
        | God { Core = core } ->
            { core with 
                MainAbility = 
                    core.MainAbility 
                    |> updateAbilityText (fun s -> s + " (including this one)") }, None
        | Creature { Core = core } ->
            let prependFavor text = String.concat Environment.NewLine [$"Gain {core.Favor.Value} Favor"; text]
            { core with 
                MainAbility = 
                    core.MainAbility 
                    |> updateAbilityText prependFavor }, None
        | Settlement { Core = core; SecondaryAbility = secondary } ->
            printfn "secondary %A" secondary
            core, secondary
        | Building { Core = core }
        | Fortification { Core = core }
        | Nomad { Core = core }
        | Relic { Core = core } ->
            core, None
    i 
    |> match secondary, flavor with
       | Some at, Some flavor -> 
           drawAbility 0.<dot> cardAbilityThirdPoint (iconForAbility main) main.Text
           >> drawAbility cardAbilityThirdPoint cardAbilityThirdPoint (iconForAbility at) at.Text
           >> drawTextAtHeight cardAbilityTwoThirdPoint cardAbilityThirdPoint flavor
       | Some at, None -> 
           drawAbility 0.<dot> cardAbilityHalfPoint (iconForAbility main) main.Text
           >> drawAbility cardAbilityHalfPoint cardAbilityHalfPoint (iconForAbility at) at.Text
       | None, Some flavor -> 
           drawAbility 0.<dot> cardAbilityHalfPoint (iconForAbility main) main.Text
           >> drawTextAtHeight cardAbilityHalfPoint cardAbilityHalfPoint flavor
       | None, None -> 
           drawAbility 0.<dot> (bottom - top) (iconForAbility main) main.Text
    |> line darkGray lineworkWidth startX bottom (startX + width) bottom

let drawCostAt right (cost: CardCost) =
    let centerX = right - iconCostOffset
    let circle fill from to' x = filledArc fill darkGray from to' x iconCostOffset iconCostRadius
    let strengthXCenterOffset = iconCostDiameter + 2. * padding
    let createIcon color val' iconCostTextXOffset =
        circle color 0 360 iconCostTextXOffset
        >> text extraExtraLargeSize TextAlignment.Center Center iconCostTextXOffset iconCostTextYOffset (string val')
    // inverse order
    [cost.Anima |>> createIcon animaBlue; cost.Strength |>> createIcon strengthRed;  cost.Trade |>> createIcon tradeGold]
    |> List.choose id
    |> List.fold (fun (s, xOffset) t -> s >> t xOffset, xOffset - strengthXCenterOffset) (id, centerX)
    |> fst

let drawLogo card =
    let drawCardLogo i =
        let w = (cardMidpoint - (inset + 2. * padding + ``1/2``))
        overlayImage 
            (cardBoundaries.Width / 2. - w / 2.)
            (inset + 2. * padding + ``1/2``) 
            w w i
    match card with
    | Building { Core = c } 
    | Nomad { Core = c } 
    | Relic { Core = c } 
    | Creature { Core = c } -> 
        drawCardLogo c.Image
    | Human { Core = c } -> 
        if c.Name.GetHashCode() % 2 = 1 then maleHumanImage else femaleHumanImage
        |> drawCardLogo
    | Fortification s -> 
        drawCardLogo s.Core.Image
        >> drawFortificationAbilities (s.Core, s.Health) extraLargeSize topTextBottom ``5/32``
    | God { Core = c } ->
        let w = (godVerticalMidpoint - 2. * (inset + padding))
        overlayImage 
            (godVerticalMidpoint / 2. - w / 2. + (inset + padding) / 2.)
            (inset + 2. * padding + ``1/2``) 
            w w c.Image
    | Settlement { Core = c; Health = h } -> 
        let w = (settlementAbilityTopPoint - ``1/2`` - 2. * (inset + padding))
        overlayImage 
            (settlementVerticalMidpoint / 2. - w / 2.)
            (inset + 2. * padding + ``1/2``) 
            w w c.Image
        >> drawFortificationAbilities (c, h) extraExtraLargeSize inset ``3/16``

let drawFavor boundaries favor =
    match favor with
    | Some i -> 
        outlinedCircle (favorRadius + inset + 2. * padding) (boundaries.Height - favorRadius - inset - 2. * padding) favorRadius
        >> (string i |> text largeSize TextAlignment.Center Center (favorRadius + inset + 2. * padding + 1.<dot>) (boundaries.Height - favorRadius - inset - 3. * padding - 1.<dot>))
    | None -> id

let calculateCaptionText upgraded card =
    $"""{(if upgraded then "Upgraded " else "")}{(match faction card with Some f -> f.Name + " " | None -> "")}{cardKind card}"""

type DrawingOptions = {
    Rows: int
    Columns: int
}
let groupSize d = d.Rows * d.Columns

let ``2 x 2`` = { Rows = 2; Columns = 2 }
let ``3 x 3`` = { Rows = 3; Columns = 3 }
let ttsOptions = { Rows = 1; Columns = 1 }
//let ttsOptions = { Rows = 10; Columns = 7 }

type PaperPrintout = {
    Samples: bool 
    DrawAllCards: bool
    CompositeCards: bool
    MainOptions: DrawingOptions
    GodOptions: DrawingOptions 
    SettlementOptions: DrawingOptions
}

type DrawingMode = 
    | TTS of DrawingOptions
    | PaperPrintout of PaperPrintout

let drawCardCore boundaries (card: Card) (i: ImageState) =
    let data, upgraded = 
        match card with
        | Human { Core = core; Upgraded = upgraded }
        | Building { Core = core; Upgraded = upgraded } ->
            core, upgraded
        | Fortification { Core = core; Upgraded = upgraded } ->
            core, upgraded
        | Nomad { Core = core }
        | Creature { Core = core }
        | Relic { Core = core } ->
            core, false
        | Settlement _ | God _ -> invalidOp "This function does not support drawing settlements"

    i 
    |> rectangle (iconography card).Primary inset (inset / 2.) (inset / 2.) (boundaries.Width - (inset / 2.)) (boundaries.Height - (inset / 2.))
    |> rectangle darkGray lineworkWidth inset inset (boundaries.Width - inset) (boundaries.Height - inset)
    // icon
    |> match (iconography card).Icon with
       | Some p ->
           outlinedCircle iconCostOffset iconCostOffset iconCostRadius
           >> overlayImageAsCircle (inset + padding + quanta) (inset + padding + quanta) iconCostDiameter p
       | None -> id
    // logo
    |> drawLogo card
    // cost
    |> drawCostAt boundaries.Width data.Cost
    // favor
    |> drawFavor boundaries data.Favor 
    // ability area
    |> drawAbilities inset cardMidpoint (boundaries.Width - inset * 2.) cardAbilityBottomPoint card
    // name
    |> captionText largeSize (iconCostDiameter + inset + padding) inset (boundaries.Width - 2. * (iconCostDiameter + inset + padding)) ``3/8`` data.Name
    // faction-kind banner
    |> captionText kindBannerSize (iconCostDiameter + inset + padding) (inset + ``3/8``) (boundaries.Width - 2. * (iconCostDiameter + inset + padding)) (fontToDot kindBannerSize + 2. * padding) (calculateCaptionText upgraded card)
    // version
    |> text smallSize TextAlignment.Right Bottom (boundaries.Width - inset - padding) (boundaries.Height - inset - textPadding) version
    // count
    |> (if data.ShowCount then List.init (int data.Count) id else []
        |> List.fold (fun s i -> s >> filledCircle black darkGray (boundaries.Width - inset - 0.35<inch> * dpi - (float i) * (fontToDot smallSize + padding)) (boundaries.Height - inset - 2. * padding - (fontToDot smallSize / 2.)) (favorCircleSize / 2.)) id)

let drawGod boundaries (card: God) (i: ImageState) =
    let nameBottom = fontToDot largeSize + inset + 2. * textPadding
    //let leftRectangle = { boundaries with Width }
    i 
    |> rectangle card.Faction.Primary inset (inset / 2.) (inset / 2.) (boundaries.Width - (inset / 2.)) (boundaries.Height - (inset / 2.))
    |> rectangle darkGray lineworkWidth inset inset (boundaries.Width - inset) (boundaries.Height - inset)
    |> line darkGray lineworkWidth godVerticalMidpoint inset godVerticalMidpoint (boundaries.Height - inset)
    |> match (iconography <| God card).Icon with
       | Some p ->
           outlinedCircle iconCostOffset iconCostOffset iconCostRadius
           >> overlayImageAsCircle (inset + padding + quanta) (inset + padding + quanta) iconCostDiameter p
       | None -> id
    // logo
    |> drawLogo (God card)
    // cost
    |> drawCostAt boundaries.Width card.Core.Cost
    // favor
    |> drawFavor boundaries card.Core.Favor 
    // health bar, favor schedule, regular favor placement, cost icons, flavor text
    // ability area
    |> drawAbilities godVerticalMidpoint (inset + ``3/8`` + fontToDot smallSize + 2. * padding) (godVerticalMidpoint - inset) godAbilityBottomPoint (God card)
    // name
    |> captionText largeSize (``3/8`` + inset) inset (godVerticalMidpoint - 2. * (``3/8`` + inset)) ``3/8`` card.Core.Name
    // kind banner
    |> captionText kindBannerSize (``3/8`` + inset) (inset + ``3/8``) (godVerticalMidpoint - 2. * (``3/8`` + inset)) (fontToDot kindBannerSize + 2. * padding) (calculateCaptionText false <| God card)
    // version
    |> text smallSize TextAlignment.Right Bottom (boundaries.Width - inset - padding) (boundaries.Height - inset - padding) version

let drawSettlement boundaries (card: Settlement) (i: ImageState) =
    let nameBottom = fontToDot largeSize + inset + 2. * textPadding
    //let leftRectangle = { boundaries with Width }
    i 
    |> rectangle card.Faction.Primary inset (inset / 2.) (inset / 2.) (boundaries.Width - (inset / 2.)) (boundaries.Height - (inset / 2.))
    |> rectangle darkGray lineworkWidth inset inset (boundaries.Width - inset) (boundaries.Height - inset)
    |> line darkGray lineworkWidth settlementVerticalMidpoint inset settlementVerticalMidpoint (boundaries.Height - inset)
    |> match (iconography <| Settlement card).Icon with
       | Some p ->
           outlinedCircle iconCostOffset iconCostOffset iconCostRadius
           >> overlayImageAsCircle (inset + padding + quanta) (inset + padding + quanta) iconCostDiameter p
       | None -> id
    // logo
    |> drawLogo (Settlement card)
    // cost
    |> drawCostAt (settlementVerticalMidpoint + inset) card.Core.Cost
    // favor
    |> drawFavor boundaries card.Core.Favor
    // TODO: allow for three settlement abilities, with icons
    // health bar, favor schedule, regular favor placement, cost icons, flavor text
    // ability area
    |> drawAbilities inset settlementAbilityTopPoint (settlementVerticalMidpoint - inset) settlementAbilityBottomPoint (Settlement card)
    // name
    |> captionText largeSize (``3/8`` + inset) inset (settlementVerticalMidpoint - 2. * (``3/8`` + inset)) ``3/8`` card.Core.Name
    // kind banner
    |> captionText kindBannerSize (``3/8`` + inset) (inset + ``3/8``) (settlementVerticalMidpoint - 2. * (``3/8`` + inset)) (fontToDot kindBannerSize + 2. * padding) (calculateCaptionText (not <| String.startsWith "Starter. " card.Core.MainAbility.Text) <| Settlement card)
    // garrison
    |> captionText medSize (settlementVerticalMidpoint + ``3/8`` + inset) ((settlementBoundaries.Height - ``3/8``) / 2.) (settlementBoundaries.Width - settlementVerticalMidpoint - 2. * (``3/8`` + inset)) ``3/8`` "You may Garrison a card here"
    // version
    |> text smallSize TextAlignment.Right Bottom (boundaries.Width - inset - padding) (boundaries.Height - inset - padding) version

let cardCache = ConcurrentDictionary<Card, MagickImage>()

let drawAt card boundaries startX startY (masterImage: MagickImage) =
    let image = 
        cardCache.GetOrAdd (card, (fun c ->
            let image = new MagickImage(MagickColors.White, int boundaries.XPixelCount, int boundaries.YPixelCount)
            image.Density <- Density(float dpi, DensityUnit.PixelsPerInch)
            let drawable = Drawables()
            { Image = image; Drawables = drawable }
            |> match card with
               | Settlement p -> drawSettlement boundaries p
               | God p -> drawGod boundaries p
               | s -> drawCardCore boundaries s
            |> ignore
            drawable.Draw image
            image))
    
    printfn $"Drawing card {card} at position {(startX, startY)}"
    masterImage.Composite(image, startX, startY, CompositeOperator.Over)

let drawToFile (path: string) options (cards: Card list) =
    let boundaries, drawingOptions, compositeCards, margin, subPath = 
        match cards[0], options with 
        | Settlement _, PaperPrintout { SettlementOptions = s; CompositeCards = c } -> settlementBoundaries, s, c, 2, SettlementFolder
        | God _, PaperPrintout { GodOptions = s; CompositeCards = c }  -> godBoundaries, s, c, 2, GodFolder
        | _, PaperPrintout { MainOptions = s; CompositeCards = c } -> cardBoundaries, s, c, 2, MainFolder
        | Settlement a, TTS s -> settlementBoundaries, s, true, 0, string a.Core.Deck 
        | God a, TTS s -> godBoundaries, s, true, 0, string a.Core.Deck 
        | a, TTS s -> cardBoundaries, s, true, 0, string (core a).Deck 
    use image = 
        new MagickImage(MagickColors.White,
            (if compositeCards then drawingOptions.Rows * int boundaries.XPixelCount + (drawingOptions.Rows * margin - margin) else int boundaries.XPixelCount),
             if compositeCards then drawingOptions.Columns * int boundaries.YPixelCount + (drawingOptions.Columns * margin - margin) else int boundaries.YPixelCount)
    image.Density <- Density(float dpi, DensityUnit.PixelsPerInch)
    for (x, y, c) in 
        cards 
        |> List.mapi (fun i c -> 
            (i / drawingOptions.Rows * (int boundaries.XPixelCount) + margin * (i / drawingOptions.Rows), 
             i % drawingOptions.Columns * (int boundaries.YPixelCount) + margin * (i % drawingOptions.Columns), c)) do 
        drawAt c boundaries x y image
    let name = 
        match options with
        | TTS _ ->
            let c = List.head cards |> core
            $"%02i{c.Count}x {c.Name}"
        | PaperPrintout { CompositeCards = true } ->
            Guid.NewGuid().ToString() 
        | _ -> 
            List.head cards |> name
    image.Write $"{path}\{subPath}\{name}.png"

//let drawingMode = TTS ttsOptions
let drawingMode = PaperPrintout {
    Samples = false
    DrawAllCards = true
    CompositeCards = true
    MainOptions = ``3 x 3``
    GodOptions = ``2 x 2``
    SettlementOptions = ``2 x 2``
}

let outputDirectory = Path.Combine(basePath, GeneratedFolder)
if Directory.Exists outputDirectory then
    Directory.Delete(outputDirectory, true)
Directory.CreateDirectory outputDirectory |> ignore

try
    match drawingMode with
    | PaperPrintout { Samples = true } ->
        sampleCards
    | _ ->
        let cards, errors = SpreadsheetLoader.load (basePath + @"Cards.ods")
        File.WriteAllLines(Path.Combine(basePath, GeneratedFolder, "errors.txt"), errors)
        match drawingMode with
        | PaperPrintout _ ->
            Path.Combine(outputDirectory, MainFolder) |> Directory.CreateDirectory |> ignore
            Path.Combine(outputDirectory, SettlementFolder) |> Directory.CreateDirectory |> ignore
            Path.Combine(outputDirectory, GodFolder) |> Directory.CreateDirectory |> ignore
        | TTS _ ->
            cards 
            |> Seq.groupBy (fun s -> (core s).Deck)
            |> Seq.iter (fun (f, _) -> Path.Combine(outputDirectory, string f) |> Directory.CreateDirectory |> ignore)
        cards
    //|> Seq.filter (
    //    function 
    //    | Settlement _ -> true
    //    | _ -> false
    //)
    |> Seq.groupBy (
        fun c ->
            match c, drawingMode with
            | Settlement _, TTS a -> "s", groupSize a
            | God _, TTS a -> "g", groupSize a
            | _, TTS a -> "o", groupSize a
            | Settlement _, PaperPrintout { SettlementOptions = s } -> "s", groupSize s
            | God _, PaperPrintout { GodOptions = g } -> "g", groupSize g
            | _, PaperPrintout { MainOptions = m } -> "o", groupSize m
    )
    |> Seq.collect (fun ((_, perPage),c) -> 
        c
        |> Seq.collect (fun c -> 
            konst c 
            |> List.init (match drawingMode with | PaperPrintout { CompositeCards = true } -> int (core c).Count | _ -> 1)
        )
        |> Seq.sortBy name
        |> Seq.chunkBySize (match drawingMode with | PaperPrintout { CompositeCards = true } -> perPage | _ -> 1)
        |> Seq.map List.ofArray
    )
    |> PSeq.iter (fun card -> drawToFile outputDirectory drawingMode card)
    // TODO: have settlement abilities work correctly
finally
    for i in cardCache do
        i.Value.Dispose()