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
let settlementAbilityTopPoint = 1.5<inch> * dpi

let iconCostRadius = ``3/16``
let iconCostDiameter = 2. * iconCostRadius
let iconCostOffset = iconCostRadius + inset + padding + quanta
let iconCostTextXOffset = iconCostRadius + inset + padding
let iconCostTextYOffset = iconCostRadius + inset - quanta
let favorRadius = ``1/8`` - quanta
let favorDiameter = 2. * favorRadius

// TODO: pick better icons for health, costs, anima gain
let drawFortificationAbilities (fortification: Fortification) (i: ImageState) =
    let availableHeight = cardMidpoint - topTextBottom - textPadding
    [ "H", Some <| float fortification.Health, healthBlue
      "C", Option.map float fortification.Core.MainAbility.Metadata.TradeGain, tradeGold
      "S", Option.map float fortification.Core.MainAbility.Metadata.StrengthGain, strengthRed
      "E", Option.map float fortification.Core.MainAbility.Metadata.AnimaGain, animaGreen ]
    |> List.mapi (fun i (abbr, v, color) -> 
        match v with
        | Some value ->
            filledCircle color darkGray (``5/32`` + inset + abilityIconPadding) (topTextBottom + textPadding + availableHeight * (float i / 4.) + ``5/32``) ``5/32``
            >> (text largeSize TextAlignment.Center Center (``5/32`` + inset + abilityIconPadding + quanta) (topTextBottom + textPadding + availableHeight * (float i / 4.) + (``5/32`` - padding)) <| $"+{int value}{abbr}")
        | None -> id)
    |> List.iter (fun s -> s i |> ignore)
    i

// TODO: remove inset to startX and width here, and add them to parameters
let drawAbilities (startX: float<dot>) (top: float<dot>) (width: float<dot>) (bottom: float<dot>) card (i: ImageState) =
    let drawTextAtHeight abilityTop height text =
        line darkGray lineworkWidth startX (top + abilityTop) (startX + width) (top + abilityTop)
        >> captionText medSize (startX + textPadding) (top + abilityTop + padding) (width - 2. * (textPadding)) (height - 2. * padding) text
    let drawAbility abilityTop (height: float<dot>) icon text =
        line darkGray lineworkWidth startX (top + abilityTop) (startX + width) (top + abilityTop)
        >> match icon with 
           | Some i -> 
                outlinedCircle (startX + ``5/32`` + abilityIconPadding) (top + abilityTop + height / 2.) ``5/32``
                >> overlayImageAsCircle (startX + abilityIconPadding) (top + abilityTop + (height / 2. - ``5/32``)) ``5/16`` i
           | None -> id
        >> captionText medSize (startX + 2. * ``5/32`` + textPadding) (top + abilityTop + padding) (width - 2. * (textPadding + ``5/32``)) (height - 2. * padding) text
    let iconForAbility =
        function 
        | Plain _ -> None
        | Ally a -> a.Faction.Icon
        | Trash _ -> Some trashImage
        | Anima _ -> Some trashImage
    let cardAbilityHalfPoint = (bottom - top) / 2.
    let cardAbilityThirdPoint = (bottom - top) / 3.
    let cardAbilityTwoThirdPoint = (bottom - top) * 2. / 3.
    let { MainAbility = main; FlavorText = flavor }, secondary = 
        match card with
        | Human { Core = core; SecondaryAbility = ally } ->
            core, ally
        | Building { Core = core }
        | Fortification { Core = core }
        | Nomad { Core = core }
        | Creature { Core = core }
        | Relic { Core = core } 
        | God { Core = core } 
        | Settlement { Core = core } ->
            core, None
    i 
    |> match secondary, flavor with
       | Some at, Some flavor -> 
           drawTextAtHeight 0.<dot> cardAbilityThirdPoint main.Text
           >> drawAbility cardAbilityThirdPoint cardAbilityThirdPoint (iconForAbility at) at.Text
           >> drawTextAtHeight cardAbilityTwoThirdPoint cardAbilityThirdPoint flavor
       | Some at, None -> 
           drawTextAtHeight 0.<dot> cardAbilityHalfPoint main.Text
           >> drawAbility cardAbilityHalfPoint cardAbilityHalfPoint (iconForAbility at) at.Text
       | None, Some flavor -> 
           drawTextAtHeight 0.<dot> cardAbilityHalfPoint main.Text
           >> drawTextAtHeight cardAbilityHalfPoint cardAbilityHalfPoint flavor
       | None, None -> 
           drawTextAtHeight 0.<dot> (bottom - top) main.Text
    |> line darkGray lineworkWidth startX bottom (startX + width) bottom

// cost is going wrong direction
let drawCostAt boundaries (cost: CardCost) =
    let centerX = boundaries.Width - iconCostOffset
    let textCenterX = boundaries.Width - iconCostTextXOffset
    let circle fill from to' x = filledArc fill darkGray from to' x iconCostOffset iconCostRadius
    let strengthXCenterOffset = iconCostDiameter + 2. * padding
    let createIcon color val' iconCostTextXOffset =
        circle color 0 360 iconCostTextXOffset
        >> text extraExtraLargeSize TextAlignment.Center Center iconCostTextXOffset iconCostTextYOffset (string val')
    [cost.Trade |>> createIcon tradeGold; cost.Strength |>> createIcon strengthRed; cost.Anima |>> createIcon animaGreen]
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
        >> drawFortificationAbilities s
    | God { Core = c } ->
        let w = (godVerticalMidpoint - (inset + 2. * padding))
        overlayImage 
            (godVerticalMidpoint / 2. - w / 2. + inset)
            (inset + 2. * padding + ``1/2``) 
            w w c.Image
    | Settlement { Core = c } -> 
        let w = (settlementVerticalMidpoint - (inset + 2. * padding + ``1/2``))
        overlayImage 
            (settlementVerticalMidpoint / 2. - w / 2.)
            (inset + 2. * padding + ``1/2``) 
            w w c.Image

let drawFavor boundaries favor =
    match favor with
    | Some i -> 
        outlinedCircle (favorRadius + inset + 2. * padding) (boundaries.Height - favorRadius - inset - 2. * padding) favorRadius
        >> (string i |> text largeSize TextAlignment.Center Center (favorRadius + inset + 2. * padding + 1.<dot>) (boundaries.Height - favorRadius - inset - 3. * padding - 1.<dot>))
    | None -> id

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
    |> rectangle data.Faction.Primary inset (inset / 2.) (inset / 2.) (boundaries.Width - (inset / 2.)) (boundaries.Height - (inset / 2.))
    |> rectangle darkGray lineworkWidth inset inset (boundaries.Width - inset) (boundaries.Height - inset)
    // icon
    |> match data.Faction.Icon with
       | Some p ->
           outlinedCircle iconCostOffset iconCostOffset iconCostRadius
           >> overlayImageAsCircle (inset + padding + quanta) (inset + padding + quanta) iconCostDiameter p
       | None -> id
    // logo
    |> drawLogo card
    // cost
    |> drawCostAt boundaries data.Cost
    // favor
    |> drawFavor boundaries data.Favor 
    // ability area
    |> drawAbilities inset cardMidpoint (boundaries.Width - inset * 2.) cardAbilityBottomPoint card
    // name
    |> captionText largeSize (iconCostDiameter + inset + padding) inset (boundaries.Width - 2. * (iconCostDiameter + inset + padding)) ``3/8`` data.Name
    // faction-kind banner
    |> captionText smallSize (iconCostDiameter + inset + padding) (inset + ``3/8``) (boundaries.Width - 2. * (iconCostDiameter + inset + padding)) (fontToDot smallSize + 2. * padding) $"""{(if upgraded then "Upgraded " else "")}{data.Faction.Name} {cardKind card}"""
    // version
    |> text smallSize TextAlignment.Right Bottom (boundaries.Width - inset - padding) (boundaries.Height - inset - textPadding) version
    // count
    |> (if data.ShowCount then List.init (int data.Count) id else []
        |> List.fold (fun s i -> s >> filledCircle black darkGray (boundaries.Width - inset - 0.35<inch> * dpi - (float i) * (fontToDot smallSize + padding)) (boundaries.Height - inset - padding - (fontToDot smallSize / 2.)) (favorCircleSize / 2.)) id)

let drawGod boundaries (card: God) (i: ImageState) =
    let nameBottom = fontToDot largeSize + inset + 2. * textPadding
    //let leftRectangle = { boundaries with Width }
    i 
    |> rectangle card.Core.Faction.Primary inset (inset / 2.) (inset / 2.) (boundaries.Width - (inset / 2.)) (boundaries.Height - (inset / 2.))
    |> rectangle darkGray lineworkWidth inset inset (boundaries.Width - inset) (boundaries.Height - inset)
    |> line darkGray lineworkWidth godVerticalMidpoint inset godVerticalMidpoint (boundaries.Height - inset)
    // logo
    |> drawLogo (God card)
    // cost
    |> drawCostAt boundaries card.Core.Cost
    // favor
    |> drawFavor boundaries card.Core.Favor 
    // health bar, favor schedule, regular favor placement, cost icons, flavor text
    // ability area
    |> drawAbilities godVerticalMidpoint (inset + ``3/8`` + fontToDot smallSize + 2. * padding) (godVerticalMidpoint - inset) godAbilityBottomPoint (God card)
    // name
    |> captionText largeSize (``3/8`` + inset) inset (godVerticalMidpoint - 2. * (``3/8`` + inset)) ``3/8`` card.Core.Name
    // kind banner
    |> captionText smallSize (``3/8`` + inset) (inset + ``3/8``) (godVerticalMidpoint - 2. * (``3/8`` + inset)) (fontToDot smallSize + 2. * padding) (cardKind <| God card)
    // version
    |> text smallSize TextAlignment.Right Bottom (boundaries.Width - inset - padding) (boundaries.Height - inset - padding) version


let drawSettlement boundaries (card: Settlement) (i: ImageState) =
    let nameBottom = fontToDot largeSize + inset + 2. * textPadding
    //let leftRectangle = { boundaries with Width }
    i 
    |> rectangle card.Core.Faction.Primary inset (inset / 2.) (inset / 2.) (boundaries.Width - (inset / 2.)) (boundaries.Height - (inset / 2.))
    |> rectangle darkGray lineworkWidth inset inset (boundaries.Width - inset) (boundaries.Height - inset)
    |> line darkGray lineworkWidth settlementVerticalMidpoint inset settlementVerticalMidpoint (boundaries.Height - inset)
    // logo
    |> drawLogo (Settlement card)
    // cost
    |> drawCostAt boundaries card.Core.Cost
    // favor
    |> drawFavor boundaries card.Core.Favor
    // TODO: allow for three settlement abilities, with icons
    // health bar, favor schedule, regular favor placement, cost icons, flavor text
    // ability area
    |> drawAbilities settlementVerticalMidpoint settlementAbilityTopPoint (boundaries.Width - settlementVerticalMidpoint - inset) cardAbilityBottomPoint (Settlement card)
    // name
    |> captionText largeSize (``3/8`` + inset) inset (settlementVerticalMidpoint - 2. * (``3/8`` + inset)) ``3/8`` card.Core.Name
    // kind banner
    |> captionText smallSize (``3/8`` + inset) (inset + ``3/8``) (settlementVerticalMidpoint - 2. * (``3/8`` + inset)) (fontToDot smallSize + 2. * padding) (cardKind <| Settlement card)
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

let drawToFile (path: string) compositeCards (cards: Card list) =
    let boundaries = 
        match cards[0] with 
        | Settlement _  -> settlementBoundaries
        | God _  -> godBoundaries
        | _ -> cardBoundaries
    use image = 
        new MagickImage(MagickColors.White,
            (if compositeCards then 3 * int boundaries.XPixelCount + 4 else int boundaries.XPixelCount),
             if compositeCards then 3 * int boundaries.YPixelCount + 4 else int boundaries.YPixelCount)
    image.Density <- Density(float dpi, DensityUnit.PixelsPerInch)
    for (x, y, c) in cards |> List.mapi (fun i c -> (i / 3 * (int boundaries.XPixelCount) + 2 * (i / 3), i % 3 * (int boundaries.YPixelCount) + 2 * (i % 3), c)) do 
        drawAt c boundaries x y image
    let name = 
        if compositeCards then 
            Guid.NewGuid().ToString() 
        else 
            List.head cards |> name |> String.filter (fun c -> c <> ' ')
    image.Write $"{path}\{name}.png"

let drawAllCards = false
let compositeCards = false

let outputPath = System.IO.Path.Combine(basePath, GeneratedFolder)
if Directory.Exists outputPath then
    Directory.Delete(outputPath, true)
Directory.CreateDirectory outputPath |> ignore

try
    //let cards, errors = SpreadsheetLoader.load (basePath + @"Cards.ods")
    //File.WriteAllLines(Path.Combine(basePath, GeneratedFolder, "errors.txt"), errors)
    //cards 
    sampleCards
    |> List.groupBy (
        function 
        | Settlement _ -> 0
        | God _ -> 1
        | _ -> 2
    )
    |> List.collect (fun (_,c) -> 
        c
        |> List.collect (fun c -> 
            konst c 
            |> List.init (if drawAllCards then int (core c).Count else 1)
        )
        |> List.sortBy name
        |> List.chunkBySize (if compositeCards then 9 else 1)
    )
    |> PSeq.iter (drawToFile outputPath compositeCards)
finally
    for i in cardCache do
        i.Value.Dispose()