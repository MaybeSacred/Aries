open ImageMagick
open ImageMagick.Configuration
open System
open System.IO

open Types

type Box = {
    X: float<dot>
    Y: float<dot>
    Width: float<dot>
    Height: float<dot>
}

let atPosition x y box = { box with X = x; Y = y }

[<Literal>]
let version = "v0.11"

type TextVerticalAlignment = Top | Center | Bottom

let colorWithOpacity (color: IMagickColor<byte>) opacity = MagickColor(color.R, color.G, color.B, opacity)

let darkGray = MagickColor(0x16uy, 0x1Fuy, 0x1Fuy)
let black = MagickColor(0x8uy, 0x8uy, 0x8uy)
let creditGold = colorWithOpacity MagickColors.Gold 0x60uy
let strengthRed = colorWithOpacity MagickColors.Red 0x60uy
let shieldBlue = colorWithOpacity MagickColors.CornflowerBlue 0x58uy
let energyGreen = colorWithOpacity MagickColors.LawnGreen 0x50uy

[<Literal>]
let dpi = 144.<dot/inch>
let widthInches = 2.<inch> + 7.<inch>/16.
let heightInches = 3.<inch> + 7.<inch>/16.
let width = dpi * widthInches - 1.<dot>
let height = dpi * heightInches - 1.<dot>
[<Literal>]
let inset = 4.<dot>
[<Literal>]
let circleSize = 9.<dot>
[<Literal>]
let smallSize = 12.<dot>
[<Literal>]
let medSize = 16.<dot>
[<Literal>]
let largeSize = 20.<dot>
[<Literal>]
let extraLargeSize = 24.<dot>
[<Literal>]
let extraExtraLargeSize = 40.<dot>
[<Literal>]
let lineworkWidth = 2.<dot>
[<Literal>]
let textPadding = 6.<dot>
[<Literal>]
let abilityIconPadding = 4.<dot>
[<Literal>]
let padding = 2.<dot>
[<Literal>]
let one = 1.<dot>

let ``1/16`` = (1.<inch> / 16.) * dpi
let ``3/32`` = (3.<inch> / 32.) * dpi
let ``1/8`` = (1.<inch> / 8.) * dpi
let ``5/32`` = (5.<inch> / 32.) * dpi
let ``3/16`` = (3.<inch> / 16.) * dpi
let ``1/4`` = dpi * 1.<inch> / 4.
let ``5/16`` = (5.<inch> / 16.) * dpi
let ``3/8`` = (3.<inch> / 8.) * dpi
let ``1/2`` = dpi * 1.<inch> / 2.
let creditStrengthDualCostOffset = ``1/16`` + 3.<dot>
let cardMidpoint = 2.<inch> * dpi
let cardBottomPoint = heightInches * dpi - 2. * (``1/8`` + inset + one)
let abilityHalfPoint = (cardBottomPoint - cardMidpoint) / 2.
let abilityThirdPoint = (cardBottomPoint - cardMidpoint) / 3.
let abilityTwoThirdPoint = (cardBottomPoint - cardMidpoint) * 2. / 3.
let topTextBottom = inset + ``3/8`` + medSize + 2. * padding

let basePath =
    Environment.GetCommandLineArgs() 
    |> Array.tryItem 1
    |> Option.defaultValue @"C:\Users\jtyso\Documents\Aries\"

let text (size: float<dot>) hAlignment vAlignment (startX: float<dot>) (startY: float<dot>) text (i: ImageState) =
    i.Drawables
     .PushGraphicContext()
     .Font("Verdana")
     .FillColor(black)
     .StrokeOpacity(Percentage 0.)
     .FillOpacity(Percentage 100.)
     .FontPointSize(float size)
     .TextAlignment(hAlignment)
     .Text(float startX, 
        float <| match vAlignment with | Top -> startY + size | Center -> startY + size/2. | Bottom -> startY
        , text)
     .PopGraphicContext()
    i

let filledArc (fillColor: IMagickColor<byte>) strokeColor from to' (x: float<dot>) (y: float<dot>) (radius: float<dot>) (i: ImageState) =
    i.Drawables
     .PushGraphicContext()
     .FillColor(fillColor)
     .FillOpacity(Percentage (float fillColor.A * 100. / 256.))
     .StrokeColor(strokeColor)
     .StrokeWidth(float lineworkWidth)
     .StrokeOpacity(Percentage 100.)
     .Ellipse(float x, float y, float radius, float radius, from, to')
     .PopGraphicContext()
    i

let filledCircle fillColor strokeColor (x: float<dot>) (y: float<dot>) (radius: float<dot>) (i: ImageState) =
    filledArc fillColor strokeColor 0. 360. x y radius i

let outlinedCircle (x: float<dot>) (y: float<dot>) (radius: float<dot>) (i: ImageState) =
    filledCircle MagickColors.None darkGray x y radius i

let rectangle strokeColor (strokeWidth: float<dot>) (startX: float<dot>) (startY: float<dot>) (endX: float<dot>) (endY: float<dot>) (i: ImageState) =
    i.Drawables
     .PushGraphicContext()
     .FillOpacity(Percentage 0.)
     .StrokeWidth(float strokeWidth)
     .StrokeColor(strokeColor)
     .Rectangle(float startX, float startY, float endX, float endY)
     .PopGraphicContext()
    i

let line color (width: float<dot>) (startX: float<dot>) (startY: float<dot>) (endX: float<dot>) (endY: float<dot>) (i: ImageState) =
    i.Drawables
     .PushGraphicContext()
     .FillOpacity(Percentage 0.)
     .StrokeOpacity(Percentage 100.)
     .StrokeColor(color)
     .StrokeWidth(float width)
     .Line(float startX, float startY, float endX, float endY)
     .PopGraphicContext()
    i

let overlayImage (startX: float<dot>) (startY: float<dot>) (width: float<dot>) (height: float<dot>) (scale: float) (path: string) (i: ImageState) =
    let settings = MagickReadSettings()
    //settings.FillColor <- black
    settings.BackgroundColor <- MagickColors.Transparent
    let scaledHalfWidth, scaledHalfHeight = int <| scale * width / 2., int <| scale * height / 2.
    let size = MagickGeometry(scaledHalfWidth * 2, scaledHalfHeight * 2)
    use ii = new MagickImage(Path.Combine(basePath, ImagesPath, path), settings)
    ii.Resize(size) // lol the calcs aren't wrong, but scaled images appear 1 pixel too far to the top left corner
    i.Image.Composite(ii, int startX + (if scale <> 1. then (int (width / 2.) - scaledHalfWidth + 1) else 0), int startY + (if scale <> 1. then (int (height / 2.) - scaledHalfHeight + 1) else 0), CompositeOperator.Over)
    i

let captionText (size: float<dot>) (startX: float<dot>) (startY: float<dot>) (width: float<dot>) (height: float<dot>) (text: string) (i: ImageState) =
    let settings = MagickReadSettings()
    settings.Font <- "Verdana"
    settings.FontPointsize <- float size
    settings.FillColor <- black
    settings.TextGravity <- Gravity.Center
    settings.BackgroundColor <- MagickColors.Transparent
    settings.Height <- int height // height of text box
    settings.Width <- int width // width of text box
    use ii = new MagickImage($"caption:{text}", settings)
    i.Image.Composite(ii, int startX, int startY, CompositeOperator.Over)
    i

let captionTextCentered size startX startY height (text: string) (i: ImageState) =
    captionText size startX startY (i.Width + 1.<dot> - 2. * startX) height text i

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
            >> (text extraLargeSize TextAlignment.Center Center (``5/32`` + inset + abilityIconPadding + one) (topTextBottom + textPadding + availableHeight * (float i / 4.) + (``5/32`` - padding)) <| $"+{int s}")
        | None -> id)
    |> List.iter (fun s -> s i |> ignore)
    i

let drawAbilities card (i: ImageState) =
    let drawMainTextAtHeight height text =
        captionTextCentered medSize (inset + textPadding) (cardMidpoint + padding) (height - 2. * padding) text
    let drawAbility top (height: float<dot>) icon text =
        captionText medSize (inset + 2. * ``5/32`` + textPadding) (cardMidpoint + top + padding) (width - 2. * (inset + textPadding + ``5/32``)) (height - 2. * padding) text
        >> line darkGray lineworkWidth inset (cardMidpoint + top) (width - inset) (cardMidpoint + top)
        >> outlinedCircle (``5/32`` + inset + abilityIconPadding) (cardMidpoint + top + height / 2.) ``5/32``
        >> overlayImage (inset + abilityIconPadding) (cardMidpoint + top + (height / 2. - ``5/32``)) ``5/16`` ``5/16`` icon.ScaleCorrection icon.Path

    let main, ally, scrap, faction = 
        match card with
        | Ship { Core = { MainAbility = main }; Faction = faction; AllyAbility = ally; ScrapAbility = scrap } 
        | Fleet { Core = { MainAbility = main }; Faction = faction; AllyAbility = ally; ScrapAbility = scrap } ->
            main, ally, scrap, faction
        | Shield { Core = { MainAbility = main }; Faction = faction; } ->
            main, None, None, faction
    i 
    |> line darkGray lineworkWidth inset cardMidpoint (width - inset) cardMidpoint
    |> line darkGray lineworkWidth inset cardBottomPoint (width - inset) cardBottomPoint
    |> match ally, scrap with
        | Some at, Some st -> 
            drawMainTextAtHeight abilityThirdPoint main.Text
            >> drawAbility abilityThirdPoint abilityThirdPoint faction.Icon.Value at.Text
            >> drawAbility abilityTwoThirdPoint abilityThirdPoint trashIcon st.Text
        | Some at, None -> 
            drawMainTextAtHeight abilityHalfPoint main.Text
            >> drawAbility abilityHalfPoint abilityHalfPoint faction.Icon.Value at.Text
        | None, Some st -> 
            drawMainTextAtHeight abilityHalfPoint main.Text
            >> drawAbility abilityHalfPoint abilityHalfPoint trashIcon st.Text
        | None, None -> drawMainTextAtHeight (cardBottomPoint - cardMidpoint) main.Text

let drawCardCore (card: Card) (i: ImageState) =
    let data = 
        match card with
        | Ship { Core = core; Faction = faction }
        | Fleet { Core = core; Faction = faction } -> 
            {| Faction = faction; Core = core |}
        | Shield { Core = core; Faction = faction } ->
            {| Faction = faction; Core = core |}
        | Planet _ -> invalidOp "This function does not support drawing planets"

    let drawLogo (i: Icon) =
        let w = (cardMidpoint - (inset + 2. * padding + ``1/2``))
        overlayImage 
            (width / 2. - w / 2.)
            (inset + 2. * padding + ``1/2``) 
            w w 
            i.ScaleCorrection i.Path
    i 
    |> rectangle data.Faction.Primary (lineworkWidth * 2.) (inset - 3.<dot>) (inset - 3.<dot>) (width - inset + 3.<dot>) (height - inset + 3.<dot>)
    |> rectangle darkGray lineworkWidth inset inset (width - inset) (height - inset)
    // icon
    |> match data.Faction.Icon with
       | Some p ->
           outlinedCircle (``1/4`` + inset + padding) (``1/4`` + inset + padding) ``1/4`` 
           >> overlayImage (inset + padding) (inset + padding) ``1/2`` ``1/2`` p.ScaleCorrection p.Path
       | None -> id
    // logo
    |> match card with
       | Ship _ -> drawLogo shipIcon
       | Fleet _ -> drawLogo fleetIcon
       | Shield s -> 
        drawLogo shieldIcon
        >> drawShieldAbilities s
    // cost
    |> (let centerX, centerY, arcHalfLength = (width - ``1/4`` - inset - padding), (``1/4`` + inset + padding), ``1/4`` / Math.Sqrt 2.
        let circle fill from to' = filledArc fill darkGray from to' centerX centerY ``1/4``
        match data.Core.Cost with
       | Some (CreditOnly c) ->
           circle creditGold 0. 360.
           >> text extraExtraLargeSize TextAlignment.Center Center (width - ``1/4`` - inset - one) (``1/4`` + inset - padding) (string c)
       | Some (StrengthOnly s) ->
           circle strengthRed 0. 360.
           >> text extraExtraLargeSize TextAlignment.Center Center (width - ``1/4`` - inset - one) (``1/4`` + inset - padding) (string s)
       | Some (CreditAndStrength (c,s)) ->
           circle creditGold 135. 315.
           >> circle strengthRed 315. 135.
           >> text extraLargeSize TextAlignment.Center Center (width - ``1/4`` - creditStrengthDualCostOffset - inset - one) (``1/4`` - creditStrengthDualCostOffset + inset - one) (string c)
           >> text extraLargeSize TextAlignment.Center Center (width - ``1/4`` + creditStrengthDualCostOffset - inset - one) (``1/4`` + creditStrengthDualCostOffset + inset - one) (string s)
           >> line darkGray lineworkWidth (centerX - arcHalfLength) (centerY + arcHalfLength) (centerX + arcHalfLength) (centerY - arcHalfLength)
       | None -> id)
    // clout
    |> match data.Core.Clout with
       | Some i -> 
            outlinedCircle (``1/8``+ inset + padding) (height - ``1/8`` - inset - padding) ``1/8``
            >> (string i |> text extraLargeSize TextAlignment.Center Center (``1/8`` + inset + padding + one) (height - ``1/8`` - inset - padding - padding))
       | None -> id
    // ability area
    |> drawAbilities card
    // name
    |> captionTextCentered largeSize (``3/8`` + inset) inset ``3/8`` data.Core.Name
    // faction-kind banner
    |> captionTextCentered medSize (``3/8`` + inset) (inset + ``3/8``) (medSize + 2. * padding) $"{data.Faction.Name} {cardKind card}"
    // version
    |> text smallSize TextAlignment.Right Bottom (width - inset - padding) (height - inset - padding) version
    |> (List.init (Option.defaultValue 0u data.Core.Count |> int) id
        |> List.fold (fun s i -> s >> filledCircle black darkGray (width - inset - 0.35<inch> * dpi  - (float i) * (smallSize + padding)) (height - inset - padding - smallSize/2.) (circleSize / 2.)) id)

let draw (path: string) card =
    use image = new MagickImage(MagickColors.White, int (width + 1.<dot>), int (height + 1.<dot>))
    printfn $"Drawing card {card} to path {path}"
    image.Density <- Density(float dpi, DensityUnit.PixelsPerInch)
    
    let drawable = Drawables()
    { Image = image; Drawables = drawable; Width = width; Height = height }
    |> drawCardCore card
    |> ignore
    drawable.Draw image
    image.Write path

let outputPath = System.IO.Path.Combine(basePath, CardPath)

sampleCards
|> List.iter (fun s -> draw $"{outputPath}\{name s |> String.filter (fun c -> c <> ' ')}.png" s)
