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

// card
let tradeStrengthDualCostOffset = ``1/16`` + 2.<dot>
let cardMidpoint = 2.<inch> * dpi
let cardBottomPoint = cardBoundaries.Height * dpi - 2. * (``1/8`` + inset + quanta)

let topTextBottom = inset + ``3/8`` + medSize + 2. * padding

// settlement
let settlementVerticalMidpoint = 2.5<inch> * dpi
let settlementAbilityTopPoint = 1.5<inch> * dpi

let iconCostRadius = ``3/16``
let iconCostDiameter = 2. * iconCostRadius
let iconCostYOffset = iconCostRadius + inset + padding + quanta
let iconCostTextXOffset = iconCostRadius + inset + padding
let iconCostTextYOffset = iconCostRadius + inset - quanta
let favorRadius = ``1/8`` - quanta
let favorDiameter = 2. * favorRadius

// TODO: pick better icons for health, costs, anima gain
let drawShieldAbilities (shield: Shield) (i: ImageState) =
    let availableHeight = cardMidpoint - topTextBottom - textPadding
    [ "H", Some <| float shield.Health, shieldBlue
      "C", Option.map float shield.Core.MainAbility.Metadata.TradeGain, tradeGold
      "S", Option.map float shield.Core.MainAbility.Metadata.StrengthGain, strengthRed
      "E", Option.map float shield.Core.MainAbility.Metadata.AnimaGain, animaGreen ]
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
    let drawMainTextAtHeight height text =
        captionText medSize (startX + inset + textPadding) (top + padding) (width - 2. * (inset + textPadding)) (height - 2. * padding) text
    let drawAbility abilityTop (height: float<dot>) icon text =
        captionText medSize (startX + inset + 2. * ``5/32`` + textPadding) (top + abilityTop + padding) (width - 2. * (inset + textPadding + ``5/32``)) (height - 2. * padding) text
        >> line darkGray lineworkWidth (startX + inset) (top + abilityTop) (startX + width - inset) (top + abilityTop)
        >> outlinedCircle (startX + ``5/32`` + inset + abilityIconPadding) (top + abilityTop + height / 2.) ``5/32``
        >> match icon with 
           | Some i -> overlayImage (startX + inset + abilityIconPadding) (top + abilityTop + (height / 2. - ``5/32``)) ``5/16`` ``5/16`` i
           | None -> id
    let iconForAbility =
        function 
        | Ally a -> a.Faction.Icon.Value
        | Trash a -> trashImage
        | Anima a -> trashImage
    let cardAbilityHalfPoint = (bottom - top) / 2.
    let cardAbilityThirdPoint = (bottom - top) / 3.
    let cardAbilityTwoThirdPoint = (bottom - top) * 2. / 3.
    let { MainAbility = main; Faction = faction; FlavorText = flavor }, ally = 
        match card with
        | Human { Core = core; SecondaryAbility = ally } 
        | Building { Core = core; SecondaryAbility = ally } ->
            core, ally
        | Shield { Core = core }
        | Nomad { Core = core }
        | Monster { Core = core }
        | Relic { Core = core } 
        | Settlement { Core = core } ->
            core, None
    i 
    |> line darkGray lineworkWidth (startX + inset) top (startX + width - inset) top
    |> line darkGray lineworkWidth (startX + inset) bottom (startX + width - inset) bottom
    |> match ally, flavor with
       | Some at, Some flavor -> 
           drawMainTextAtHeight cardAbilityThirdPoint main.Text
           >> drawAbility cardAbilityThirdPoint cardAbilityThirdPoint at .Icon.Value at.Text
           >> drawAbility cardAbilityTwoThirdPoint cardAbilityThirdPoint None flavor
       | Some at, None -> 
           drawMainTextAtHeight cardAbilityHalfPoint main.Text
           >> drawAbility cardAbilityHalfPoint cardAbilityHalfPoint faction.Icon.Value at.Text
       | None, Some flavor -> 
           drawMainTextAtHeight cardAbilityHalfPoint main.Text
           >> drawAbility cardAbilityHalfPoint cardAbilityHalfPoint None flavor
       | None, None -> 
           drawMainTextAtHeight (bottom - top) main.Text

let drawCostAt boundaries cost =
    let centerX, arcHalfLength = (boundaries.PixelWidth - iconCostYOffset), iconCostRadius / Math.Sqrt 2.
    let textCenterX = boundaries.PixelWidth - iconCostTextXOffset
    let circle fill from to' y = filledArc fill darkGray from to' centerX y iconCostRadius
    match cost with
    | Some (TradeOnly c) ->
        circle tradeGold 0 360 iconCostYOffset
        >> text extraExtraLargeSize TextAlignment.Center Center textCenterX iconCostTextYOffset (string c)
    | Some (StrengthOnly s) ->
        circle strengthRed 0 360 iconCostYOffset
        >> text extraExtraLargeSize TextAlignment.Center Center textCenterX iconCostTextYOffset (string s)
    | Some (TradeOrStrength (c,s)) ->
        circle tradeGold 135 315 iconCostYOffset
        >> circle strengthRed 315 135 iconCostYOffset
        >> text extraLargeSize TextAlignment.Center Center (textCenterX - tradeStrengthDualCostOffset) (iconCostTextYOffset - tradeStrengthDualCostOffset) (string c)
        >> text extraLargeSize TextAlignment.Center Center (textCenterX + tradeStrengthDualCostOffset) (iconCostTextYOffset + tradeStrengthDualCostOffset) (string s)
        >> line darkGray lineworkWidth (centerX - arcHalfLength) (iconCostYOffset + arcHalfLength) (centerX + arcHalfLength) (iconCostYOffset - arcHalfLength)
    // TODO
    | Some (TradeAndStrength (c,s)) ->
        let strengthYCenterOffset = iconCostDiameter + 2. * padding
        circle tradeGold 0 360 iconCostYOffset
        >> text extraExtraLargeSize TextAlignment.Center Center textCenterX iconCostTextYOffset (string c)
        >> circle strengthRed 0 360 (iconCostYOffset + strengthYCenterOffset)
        >> text extraExtraLargeSize TextAlignment.Center Center textCenterX (iconCostTextYOffset + strengthYCenterOffset) (string s)
    | None -> id

let drawCardCore boundaries (card: Card) (i: ImageState) =
    let data, upgraded = 
        match card with
        | Human { Core = core; Upgraded = upgraded }
        | Building { Core = core; Upgraded = upgraded } ->
            core, upgraded
        | Shield { Core = core; Upgraded = upgraded } ->
            core, upgraded
        | Nomad { Core = core }
        | Monster { Core = core }
        | Relic { Core = core } ->
            core, false
        | Settlement _ -> invalidOp "This function does not support drawing settlements"

    let drawLogo (i: ImageData) =
        let w = (cardMidpoint - (inset + 2. * padding + ``1/2``))
        overlayImage 
            (boundaries.PixelWidth / 2. - w / 2.)
            (inset + 2. * padding + ``1/2``) 
            w w i
    i 
    |> rectangle data.Faction.Primary inset (inset / 2.) (inset / 2.) (boundaries.PixelWidth - (inset / 2.)) (boundaries.PixelHeight - (inset / 2.))
    |> rectangle darkGray lineworkWidth inset inset (boundaries.PixelWidth - inset) (boundaries.PixelHeight - inset)
    // icon
    |> match data.Faction.Icon with
       | Some p ->
           outlinedCircle iconCostYOffset iconCostYOffset iconCostRadius
           >> overlayImage (inset + padding + quanta) (inset + padding + quanta) iconCostDiameter iconCostDiameter p
       | None -> id
    // logo
    |> match card with
       | Human _ -> drawLogo humanImage
       | Building _ -> drawLogo buildingImage
       | Shield s -> 
        drawLogo shieldImage
        >> drawShieldAbilities s
       | Nomad _ -> drawLogo buildingImage
       | Monster m -> spaceMonsterIcons[m.Core.Name.GetHashCode() |> Math.Abs |> (flip (%) spaceMonsterIcons.Length)] |> drawLogo
       | Relic _ -> drawLogo relicImage
    // cost
    |> drawCostAt boundaries data.Cost
    // favor
    |> match data.Favor with
       | Some i -> 
            outlinedCircle (favorRadius + inset + 2. * padding) (boundaries.PixelHeight - favorRadius - inset - 2. * padding) favorRadius
            >> (string i |> text largeSize TextAlignment.Center Center (favorRadius + inset + 2. * padding + 1.<dot>) (boundaries.PixelHeight - favorRadius - inset - 3. * padding - 1.<dot>))
       | None -> id
    // ability area
    |> drawAbilities 0.<dot> cardMidpoint boundaries.PixelWidth cardBottomPoint card
    // name
    |> captionText largeSize (iconCostDiameter + inset + padding) inset (boundaries.PixelWidth - 2. * (iconCostDiameter + inset + padding)) ``3/8`` data.Name
    // faction-kind banner
    |> captionText smallSize (iconCostDiameter + inset + padding) (inset + ``3/8``) (boundaries.PixelWidth - 2. * (iconCostDiameter + inset + padding)) (smallSize + 2. * padding) $"""{(if upgraded then "Upgraded " else "")}{data.Faction.Name} {cardKind card}"""
    // version
    |> text smallSize TextAlignment.Right Bottom (boundaries.PixelWidth - inset - padding) (boundaries.PixelHeight - inset - textPadding) version
    // count
    |> (if data.ShowCount then List.init (int data.Count) id else []
        |> List.fold (fun s i -> s >> filledCircle black darkGray (boundaries.PixelWidth - inset - 0.35<inch> * dpi  - (float i) * (smallSize + padding)) (boundaries.PixelHeight - inset - padding - smallSize / 2.) (favorCircleSize / 2.)) id)

let drawSettlement boundaries (card: Settlement) (i: ImageState) =
    let nameBottom = largeSize + inset + 2. * textPadding
    //let leftRectangle = { boundaries with Width }
    let drawLogo (i: ImageData) =
        let w = (settlementVerticalMidpoint - (inset + 2. * padding + ``1/2``))
        overlayImage 
            (settlementVerticalMidpoint / 2. - w / 2.)
            (inset + 2. * padding + ``1/2``) 
            w w i
    i 
    |> rectangle card.Core.Faction.Primary inset (inset / 2.) (inset / 2.) (boundaries.PixelWidth - (inset / 2.)) (boundaries.PixelHeight - (inset / 2.))
    |> rectangle darkGray lineworkWidth inset inset (boundaries.PixelWidth - inset) (boundaries.PixelHeight - inset)
    |> line darkGray lineworkWidth settlementVerticalMidpoint inset settlementVerticalMidpoint (boundaries.PixelHeight - inset)
    // logo
    |> drawLogo settlementImage
    // cost
    |> drawCostAt boundaries card.Core.Cost
    // favor
    //|> match card.Core.Favor with
    //   | Some i -> 
    //        outlinedCircle (``1/8``+ inset + padding) (boundaries.PixelHeight - ``1/8`` - inset - padding) ``1/8``
    //        >> (string i |> text extraLargeSize TextAlignment.Center Center (``1/8`` + inset + padding + one) (boundaries.PixelHeight - ``1/8`` - inset - padding - padding))
    //   | None -> id
    // TODO: allow for three settlement abilities, with icons
    // health bar, favor schedule, regular favor placement, cost icons, flavor text
    // ability area
    |> drawAbilities settlementVerticalMidpoint settlementAbilityTopPoint settlementVerticalMidpoint cardBottomPoint (Settlement card)
    // name
    |> captionText largeSize (``3/8`` + inset) inset (settlementVerticalMidpoint - 2. * (``3/8`` + inset)) ``3/8`` card.Core.Name
    // kind banner
    |> captionText smallSize (``3/8`` + inset) (inset + ``3/8``) (settlementVerticalMidpoint - 2. * (``3/8`` + inset)) (smallSize + 2. * padding) (cardKind <| Settlement card)
    // version
    |> text smallSize TextAlignment.Right Bottom (boundaries.PixelWidth - inset - padding) (boundaries.PixelHeight - inset - padding) version
    //|> (List.init (Option.defaultValue 0u card.Core.Count |> int) id
    //    |> List.fold (fun s i -> s >> filledCircle darkGray medGray (boundaries.PixelWidth - inset - 0.35<inch> * dpi - (float i) * (smallSize + padding)) (boundaries.PixelHeight - inset - padding - smallSize/2.) (circleSize / 2.)) id)

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
               | s -> drawCardCore boundaries s
            |> ignore
            drawable.Draw image
            image))
    
    printfn $"Drawing card {card} at position {(startX, startY)}"
    masterImage.Composite(image, startX, startY, CompositeOperator.Over)

let drawToFile (path: string) compositeCards cards =
    let boundaries = 
        match List.forall (function Settlement p -> true | _ -> false) cards with 
        | true -> settlementBoundaries
        | false -> cardBoundaries
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

let drawAllCards = true
let compositeCards = true

let outputPath = System.IO.Path.Combine(basePath, GeneratedFolder)
if Directory.Exists outputPath then
    Directory.Delete(outputPath, true)
Directory.CreateDirectory outputPath |> ignore

try
    let cards, errors = SpreadsheetLoader.load (basePath + @"Cards.ods")
    File.WriteAllLines(Path.Combine(basePath, GeneratedFolder, "errors.txt"), errors)
    cards 
    //sampleCards
    |> List.collect (fun c -> konst c |> List.init (if drawAllCards then int (core c).Count else 1))
    |> List.sortBy name
    |> List.chunkBySize (if compositeCards then 9 else 1)
    |> PSeq.iter (drawToFile outputPath compositeCards)
finally
    for i in cardCache do
        i.Value.Dispose()