open ImageMagick
open ImageMagick.Configuration
open System
open System.IO
open Microsoft.FSharp.Collections
open FSharp.Collections.ParallelSeq
open FSharpPlus

open Types
open DrawingPrimitives

// card
let creditStrengthDualCostOffset = ``1/16`` + 2.<dot>
let cardMidpoint = 2.<inch> * dpi
let cardBottomPoint = cardBoundaries.Height * dpi - 2. * (``1/8`` + inset + quanta)

let topTextBottom = inset + ``3/8`` + medSize + 2. * padding

// planet
let planetVerticalMidpoint = 2.5<inch> * dpi
let planetAbilityTopPoint = 1.5<inch> * dpi

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
    // TODO: remove inset to startX and width here, and add them to parameters
let drawAbilities (startX: float<dot>) (top: float<dot>) (width: float<dot>) (bottom: float<dot>) card (i: ImageState) =
    let drawMainTextAtHeight height text =
        captionText medSize (startX + inset + textPadding) (top + padding) (width - 2. * (inset + textPadding)) (height - 2. * padding) text
    let drawAbility abilityTop (height: float<dot>) icon text =
        captionText medSize (startX + inset + 2. * ``5/32`` + textPadding) (top + abilityTop + padding) (width - 2. * (inset + textPadding + ``5/32``)) (height - 2. * padding) text
        >> line darkGray lineworkWidth (startX + inset) (top + abilityTop) (startX + width - inset) (top + abilityTop)
        >> outlinedCircle (startX + ``5/32`` + inset + abilityIconPadding) (top + abilityTop + height / 2.) ``5/32``
        >> overlayImage (startX + inset + abilityIconPadding) (top + abilityTop + (height / 2. - ``5/32``)) ``5/16`` ``5/16`` icon
    let cardAbilityHalfPoint = (bottom - top) / 2.
    let cardAbilityThirdPoint = (bottom - top) / 3.
    let cardAbilityTwoThirdPoint = (bottom - top) * 2. / 3.
    let main, ally, scrap, faction = 
        match card with
        | Ship { Core = { MainAbility = main; Faction = faction }; AllyAbility = ally; TrashAbility = scrap } 
        | Fleet { Core = { MainAbility = main; Faction = faction }; AllyAbility = ally; TrashAbility = scrap } ->
            main, ally, scrap, faction
        | Shield { Core = { MainAbility = main; Faction = faction } }
        | Mercenary { Core = { MainAbility = main; Faction = faction } }
        | Monster { Core = { MainAbility = main; Faction = faction } }
        | Relic { Core = { MainAbility = main; Faction = faction } } 
        | Planet { Core = { MainAbility = main; Faction = faction } } ->
            main, None, None, faction
    i 
    |> line darkGray lineworkWidth (startX + inset) top (startX + width - inset) top
    |> line darkGray lineworkWidth (startX + inset) bottom (startX + width - inset) bottom
    |> match ally, scrap with
        | Some at, Some st -> 
            drawMainTextAtHeight cardAbilityThirdPoint main.Text
            >> drawAbility cardAbilityThirdPoint cardAbilityThirdPoint faction.Icon.Value at.Text
            >> drawAbility cardAbilityTwoThirdPoint cardAbilityThirdPoint trashImage st.Text
        | Some at, None -> 
            drawMainTextAtHeight cardAbilityHalfPoint main.Text
            >> drawAbility cardAbilityHalfPoint cardAbilityHalfPoint faction.Icon.Value at.Text
        | None, Some st -> 
            drawMainTextAtHeight cardAbilityHalfPoint main.Text
            >> drawAbility cardAbilityHalfPoint cardAbilityHalfPoint trashImage st.Text
        | None, None -> 
            drawMainTextAtHeight (bottom - top) main.Text

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
        | Mercenary { Core = core }
        | Monster { Core = core }
        | Relic { Core = core } ->
            core, false
        | Planet _ -> invalidOp "This function does not support drawing planets"

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
       | Ship _ -> drawLogo shipImage
       | Fleet _ -> drawLogo fleetImage
       | Shield s -> 
        drawLogo shieldImage
        >> drawShieldAbilities s
       | Mercenary _ -> drawLogo fleetImage
       | Monster m -> spaceMonsterIcons[m.Core.Name.GetHashCode() |> Math.Abs |> (flip (%) spaceMonsterIcons.Length)] |> drawLogo
       | Relic _ -> drawLogo relicImage
    // cost
    |> drawCostAt boundaries data.Cost
    // reward
    |> match data.Reward with
       | Some i -> 
            outlinedCircle (rewardRadius + inset + 2. * padding) (boundaries.PixelHeight - rewardRadius - inset - 2. * padding) rewardRadius
            >> (string i |> text largeSize TextAlignment.Center Center (rewardRadius + inset + 2. * padding + 1.<dot>) (boundaries.PixelHeight - rewardRadius - inset - 3. * padding - 1.<dot>))
       | None -> id
    // ability area
    |> drawAbilities 0.<dot> cardMidpoint boundaries.PixelWidth cardBottomPoint card
    // name
    |> captionText largeSize (iconCostDiameter + inset + padding) inset (boundaries.PixelWidth - 2. * (iconCostDiameter + inset + padding)) ``3/8`` data.Name
    // faction-kind banner
    |> captionText smallSize (iconCostDiameter + inset + padding) (inset + ``3/8``) (boundaries.PixelWidth - 2. * (iconCostDiameter + inset + padding)) (smallSize + 2. * padding) $"""{(if upgraded then "Upgraded " else "")}{data.Faction.Name} {cardKind card}"""
    // version
    |> text smallSize TextAlignment.Right Bottom (boundaries.PixelWidth - inset - padding) (boundaries.PixelHeight - inset - padding) version
    // count
    |> (List.init (Option.defaultValue 0u data.Count |> int) id
        |> List.fold (fun s i -> s >> filledCircle black darkGray (boundaries.PixelWidth - inset - 0.35<inch> * dpi  - (float i) * (smallSize + padding)) (boundaries.PixelHeight - inset - padding - smallSize/2.) (circleSize / 2.)) id)

let drawPlanet boundaries (card: Planet) (i: ImageState) =
    let nameBottom = largeSize + inset + 2. * textPadding
    //let leftRectangle = { boundaries with Width }
    let drawLogo (i: ImageData) =
        let w = (planetVerticalMidpoint - (inset + 2. * padding + ``1/2``))
        overlayImage 
            (planetVerticalMidpoint / 2. - w / 2.)
            (inset + 2. * padding + ``1/2``) 
            w w i
    i 
    |> rectangle card.Core.Faction.Primary (lineworkWidth * 2.) (inset - 3.<dot>) (inset - 3.<dot>) (boundaries.PixelWidth - inset + 3.<dot>) (boundaries.PixelHeight - inset + 3.<dot>)
    |> rectangle darkGray lineworkWidth inset inset (boundaries.PixelWidth - inset) (boundaries.PixelHeight - inset)
    |> line darkGray lineworkWidth planetVerticalMidpoint inset planetVerticalMidpoint (boundaries.PixelHeight - inset)
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
    // TODO: allow for three planet abilities, with icons
    // health bar, reward schedule, regular reward placement, cost icons, flavor text
    // ability area
    |> drawAbilities planetVerticalMidpoint planetAbilityTopPoint planetVerticalMidpoint cardBottomPoint (Planet card)
    // name
    |> captionText largeSize (``3/8`` + inset) inset (planetVerticalMidpoint - 2. * (``3/8`` + inset)) ``3/8`` card.Core.Name
    // kind banner
    |> captionText smallSize (``3/8`` + inset) (inset + ``3/8``) (planetVerticalMidpoint - 2. * (``3/8`` + inset)) (smallSize + 2. * padding) (cardKind <| Planet card)
    // version
    |> text smallSize TextAlignment.Right Bottom (boundaries.PixelWidth - inset - padding) (boundaries.PixelHeight - inset - padding) version
    //|> (List.init (Option.defaultValue 0u card.Core.Count |> int) id
    //    |> List.fold (fun s i -> s >> filledCircle darkGray medGray (boundaries.PixelWidth - inset - 0.35<inch> * dpi  - (float i) * (smallSize + padding)) (boundaries.PixelHeight - inset - padding - smallSize/2.) (circleSize / 2.)) id)

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
File.WriteAllLines(Path.Combine(basePath, GeneratedFolder, "errors.txt"), errors)
cards 
//sampleCards
|> PSeq.iter (fun s -> draw $"{outputPath}\{name s |> String.filter (fun c -> c <> ' ')}.png" s)

