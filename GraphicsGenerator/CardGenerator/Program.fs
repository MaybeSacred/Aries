open ImageMagick
open ImageMagick.Configuration
open System
open System.IO
open Microsoft.FSharp.Collections
open FSharp.Collections.ParallelSeq

open Types
open DrawingPrimitives

let creditStrengthDualCostOffset = ``1/16`` + 2.<dot>
let cardMidpoint = 2.<inch> * dpi
let cardBottomPoint = cardBoundaries.Height * dpi - 2. * (``1/8`` + inset + quanta)
let planetVerticalMidpoint = 2.<inch> * dpi
let abilityHalfPoint = (cardBottomPoint - cardMidpoint) / 2.
let abilityThirdPoint = (cardBottomPoint - cardMidpoint) / 3.
let abilityTwoThirdPoint = (cardBottomPoint - cardMidpoint) * 2. / 3.
let topTextBottom = inset + ``3/8`` + medSize + 2. * padding
let iconCostRadius = ``3/16``
let iconCostDiameter = 2. * iconCostRadius
let iconCostYOffset = iconCostRadius + inset + padding + quanta
let iconCostTextXOffset = iconCostRadius + inset + padding
let iconCostTextYOffset = iconCostRadius + inset - quanta
let rewardRadius = ``1/8`` - quanta
let rewardDiameter = 2. * rewardRadius

let drawShieldAbilities (shield: Shield) (i: ImageState) =
    let availableHeight = cardMidpoint - topTextBottom - textPadding
    [ Some <| float shield.Health, shieldBlue
      Option.map float shield.Core.MainAbility.Metadata.CreditGain, creditGold
      Option.map float shield.Core.MainAbility.Metadata.StrengthGain, strengthRed
      Option.map float shield.Core.MainAbility.Metadata.EnergyGain, energyGreen ]
    |> List.mapi (fun i (v, c) -> 
        match v with
        | Some s ->
            filledCircle c darkGray (``5/32`` + inset + abilityIconPadding) (topTextBottom + textPadding + availableHeight * (float i / 4.) + ``5/32``) ``5/32``
            >> (text extraLargeSize TextAlignment.Center Center (``5/32`` + inset + abilityIconPadding + quanta) (topTextBottom + textPadding + availableHeight * (float i / 4.) + (``5/32`` - padding)) <| $"+{int s}")
        | None -> id)
    |> List.iter (fun s -> s i |> ignore)
    i

let drawAbilities boundaries card (i: ImageState) =
    let drawMainTextAtHeight height text =
        captionTextCentered boundaries medSize (inset + textPadding) (cardMidpoint + padding) (height - 2. * padding) text
    let drawAbility top (height: float<dot>) icon text =
        captionText medSize (inset + 2. * ``5/32`` + textPadding) (cardMidpoint + top + padding) (boundaries.PixelWidth - 2. * (inset + textPadding + ``5/32``)) (height - 2. * padding) text
        >> line darkGray lineworkWidth inset (cardMidpoint + top) (boundaries.PixelWidth - inset) (cardMidpoint + top)
        >> outlinedCircle (``5/32`` + inset + abilityIconPadding) (cardMidpoint + top + height / 2.) ``5/32``
        >> overlayImage (inset + abilityIconPadding) (cardMidpoint + top + (height / 2. - ``5/32``)) ``5/16`` ``5/16`` icon

    let main, ally, scrap, faction = 
        match card with
        | Ship { Core = { MainAbility = main; Faction = faction }; AllyAbility = ally; TrashAbility = scrap } 
        | Fleet { Core = { MainAbility = main; Faction = faction }; AllyAbility = ally; TrashAbility = scrap } ->
            main, ally, scrap, faction
        | Shield { Core = { MainAbility = main; Faction = faction } } ->
            main, None, None, faction
    i 
    |> line darkGray lineworkWidth inset cardMidpoint (boundaries.PixelWidth - inset) cardMidpoint
    |> line darkGray lineworkWidth inset cardBottomPoint (boundaries.PixelWidth - inset) cardBottomPoint
    |> match ally, scrap with
        | Some at, Some st -> 
            drawMainTextAtHeight abilityThirdPoint main.Text
            >> drawAbility abilityThirdPoint abilityThirdPoint faction.Icon.Value at.Text
            >> drawAbility abilityTwoThirdPoint abilityThirdPoint trashImage st.Text
        | Some at, None -> 
            drawMainTextAtHeight abilityHalfPoint main.Text
            >> drawAbility abilityHalfPoint abilityHalfPoint faction.Icon.Value at.Text
        | None, Some st -> 
            drawMainTextAtHeight abilityHalfPoint main.Text
            >> drawAbility abilityHalfPoint abilityHalfPoint trashImage st.Text
        | None, None -> drawMainTextAtHeight (cardBottomPoint - cardMidpoint) main.Text

let drawCostAt boundaries cost =
    let centerX, arcHalfLength = (boundaries.PixelWidth - iconCostYOffset), iconCostRadius / Math.Sqrt 2.
    let textCenterX = boundaries.PixelWidth - iconCostTextXOffset
    let circle fill from to' y = filledArc fill darkGray from to' centerX y iconCostRadius
    match cost with
    | Some (CreditOnly c) ->
        circle creditGold 0 360 iconCostYOffset
        >> text extraExtraLargeSize TextAlignment.Center Center textCenterX iconCostTextYOffset (string c)
    | Some (StrengthOnly s) ->
        circle strengthRed 0 360 iconCostYOffset
        >> text extraExtraLargeSize TextAlignment.Center Center textCenterX iconCostTextYOffset (string s)
    | Some (CreditOrStrength (c,s)) ->
        circle creditGold 135 315 iconCostYOffset
        >> circle strengthRed 315 135 iconCostYOffset
        >> text extraLargeSize TextAlignment.Center Center (textCenterX - creditStrengthDualCostOffset) (iconCostTextYOffset - creditStrengthDualCostOffset) (string c)
        >> text extraLargeSize TextAlignment.Center Center (textCenterX + creditStrengthDualCostOffset) (iconCostTextYOffset + creditStrengthDualCostOffset) (string s)
        >> line darkGray lineworkWidth (centerX - arcHalfLength) (iconCostYOffset + arcHalfLength) (centerX + arcHalfLength) (iconCostYOffset - arcHalfLength)
    // TODO
    | Some (CreditAndStrength (c,s)) ->
        let strengthYCenterOffset = iconCostDiameter + 2. * padding
        circle creditGold 0 360 iconCostYOffset
        >> text extraExtraLargeSize TextAlignment.Center Center textCenterX iconCostTextYOffset (string c)
        >> circle strengthRed 0 360 (iconCostYOffset + strengthYCenterOffset)
        >> text extraExtraLargeSize TextAlignment.Center Center textCenterX (iconCostTextYOffset + strengthYCenterOffset) (string s)
    | None -> id

let drawCardCore boundaries (card: Card) (i: ImageState) =
    let data, upgraded = 
        match card with
        | Ship { Core = core; Upgraded = upgraded }
        | Fleet { Core = core; Upgraded = upgraded } ->
            core, upgraded
        | Shield { Core = core; Upgraded = upgraded } ->
            core, upgraded
        | Planet _ -> invalidOp "This function does not support drawing planets"

    let drawLogo (i: Icon) =
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
       | Ship _ -> drawLogo shipImage
       | Fleet _ -> drawLogo fleetImage
       | Shield s -> 
        drawLogo shieldImage
        >> drawShieldAbilities s
    // cost
    |> drawCostAt boundaries data.Cost
    // reward
    |> match data.Reward with
       | Some i -> 
            outlinedCircle (rewardRadius + inset + 2. * padding) (boundaries.PixelHeight - rewardRadius - inset - 2. * padding) rewardRadius
            >> (string i |> text largeSize TextAlignment.Center Center (rewardRadius + inset + 2. * padding + 1.<dot>) (boundaries.PixelHeight - rewardRadius - inset - 3. * padding - 1.<dot>))
       | None -> id
    // ability area
    |> drawAbilities boundaries card
    // name
    |> captionTextCentered boundaries largeSize (iconCostDiameter + inset + padding) inset ``3/8`` data.Name
    // faction-kind banner
    |> captionTextCentered boundaries medSize (iconCostDiameter + inset + padding) (inset + ``3/8``) (medSize + 2. * padding) $"""{(if upgraded then "Upgraded " else "")}{data.Faction.Name} {cardKind card}"""
    // version
    |> text smallSize TextAlignment.Right Bottom (boundaries.PixelWidth - inset - padding) (boundaries.PixelHeight - inset - padding) version
    |> (List.init (Option.defaultValue 0u data.Count |> int) id
        |> List.fold (fun s i -> s >> filledCircle black darkGray (boundaries.PixelWidth - inset - 0.35<inch> * dpi  - (float i) * (smallSize + padding)) (boundaries.PixelHeight - inset - padding - smallSize/2.) (circleSize / 2.)) id)

let drawPlanet boundaries (card: Planet) (i: ImageState) =
    let nameBottom = largeSize + inset + 2. * textPadding
    let drawLogo (i: Icon) =
        let w = (cardMidpoint - (inset + 2. * padding + ``1/2``))
        overlayImage 
            (boundaries.PixelWidth / 2. - w / 2.)
            (inset + 2. * padding + ``1/2``) 
            w w i
    i 
    |> rectangle card.Core.Faction.Primary (lineworkWidth * 2.) (inset - 3.<dot>) (inset - 3.<dot>) (boundaries.PixelWidth - inset + 3.<dot>) (boundaries.PixelHeight - inset + 3.<dot>)
    |> rectangle darkGray lineworkWidth inset inset (boundaries.PixelWidth - inset) (boundaries.PixelHeight - inset)
    // logo
    |> drawLogo planetImage
    // cost
    |> drawCostAt boundaries card.Core.Cost
    // reward
    //|> match card.Core.Reward with
    //   | Some i -> 
    //        outlinedCircle (``1/8``+ inset + padding) (boundaries.PixelHeight - ``1/8`` - inset - padding) ``1/8``
    //        >> (string i |> text extraLargeSize TextAlignment.Center Center (``1/8`` + inset + padding + one) (boundaries.PixelHeight - ``1/8`` - inset - padding - padding))
    //   | None -> id
    // ability area
    //|> drawAbilities boundaries card
    // name
    |> captionTextCentered boundaries largeSize (``3/8`` + inset) inset ``3/8`` card.Core.Name
    // kind banner
    |> captionTextCentered boundaries medSize (``3/8`` + inset) (inset + ``3/8``) (medSize + 2. * padding) (cardKind <| Planet card)
    // version
    |> text smallSize TextAlignment.Right Bottom (boundaries.PixelWidth - inset - padding) (boundaries.PixelHeight - inset - padding) version
    |> (List.init (Option.defaultValue 0u card.Core.Count |> int) id
        |> List.fold (fun s i -> s >> filledCircle darkGray medGray (boundaries.PixelWidth - inset - 0.35<inch> * dpi  - (float i) * (smallSize + padding)) (boundaries.PixelHeight - inset - padding - smallSize/2.) (circleSize / 2.)) id)

let draw (path: string) card =
    let boundaries = 
        match card with
        | Planet p -> planetBoundaries
        | s -> cardBoundaries
    use image = new MagickImage(MagickColors.White, int boundaries.XPixelCount, int boundaries.YPixelCount)
    printfn $"Drawing card {card} to path {path}"
    image.Density <- Density(float dpi, DensityUnit.PixelsPerInch)
    
    let drawable = Drawables()
    { Image = image; Drawables = drawable }
    |> match card with
       | Planet p -> drawPlanet boundaries p
       | s -> drawCardCore boundaries s
    |> ignore
    drawable.Draw image
    image.Write path

let outputPath = System.IO.Path.Combine(basePath, GeneratedFolder)
if Directory.Exists outputPath then
    Directory.Delete(outputPath, true)
Directory.CreateDirectory outputPath |> ignore

let cards, errors = SpreadsheetLoader.load (basePath + @"Cards.ods")
for i in errors do
    printfn $"%s{i}"
File.WriteAllLines(Path.Combine(basePath, GeneratedFolder, "errors.txt"), errors)
cards 
//sampleCards
|> PSeq.iter (fun s -> draw $"{outputPath}\{name s |> String.filter (fun c -> c <> ' ')}.png" s)

