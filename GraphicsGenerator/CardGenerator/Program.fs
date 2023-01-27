open ImageMagick
open ImageMagick.Configuration
open System
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
let darkGray = MagickColors.DarkSlateGray
let black = MagickColor(0x10uy, 0x10uy, 0x10uy)

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
let extraLargeSize = 28.<dot>
[<Literal>]
let extraExtraLargeSize = 40.<dot>
[<Literal>]
let lineworkWidth = 2.<dot>
[<Literal>]
let textPadding = 6.<dot>
[<Literal>]
let abilityIconPadding = 3.<dot>
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
let ``3/8`` = (3.<inch> / 8.) * dpi
let ``1/2`` = dpi * 1.<inch> / 2.
let creditStrengthDualCostOffset = ``1/16``
let cardMidpoint = 2.<inch> * dpi
let cardBottomPoint = heightInches * dpi - 2. * (``1/8`` + inset + one)
let abilityHalfPoint = (cardBottomPoint - cardMidpoint) / 2.
let abilityThirdPoint = (cardBottomPoint - cardMidpoint) / 3.
let abilityTwoThirdPoint = (cardBottomPoint - cardMidpoint) * 2. / 3.

let text (size: float<dot>) hAlignment vAlignment (startX: float<dot>) (startY: float<dot>) text (i: ImageState) =
    i.Drawables.Font("Verdana")
     .FillColor(black)
     .StrokeOpacity(Percentage 0.)
     .FillOpacity(Percentage 100.)
     .FontPointSize(float size)
     .TextAlignment(hAlignment)
     .Text(float startX, 
        float <| match vAlignment with | Top -> startY + size | Center -> startY + size/2. | Bottom -> startY
        , text)
    i

let filledCircle (x: float<dot>) (y: float<dot>) (radius: float<dot>) (i: ImageState) =
    i.Drawables
     .FillColor(black)
     .FillOpacity(Percentage 100.)
     .StrokeOpacity(Percentage 100.)
     .Ellipse(float x, float y, float radius, float radius, 0., 360.)
    i

let circle (x: float<dot>) (y: float<dot>) (radius: float<dot>) (i: ImageState) =
    i.Drawables.FillOpacity(Percentage 0.)
     .StrokeOpacity(Percentage 100.)
     .Ellipse(float x, float y, float radius, float radius, 0., 360.)
    i

let line color (width: float<dot>) (startX: float<dot>) (startY: float<dot>) (endX: float<dot>) (endY: float<dot>) (i: ImageState) =
    i.Drawables.FillOpacity(Percentage 0.)
     .StrokeOpacity(Percentage 100.)
     .StrokeColor(color)
     .StrokeWidth(float width)
     .Line(float startX, float startY, float endX, float endY)
    i

let overlayImage (startX: float<dot>) (startY: float<dot>) (width: float<dot>) (height: float<dot>) (scale: float) (path: string) (i: ImageState) =
    let settings = MagickReadSettings()
    //settings.FillColor <- black
    settings.BackgroundColor <- MagickColors.Transparent
    let size = MagickGeometry(int <| scale * width, int <| scale * height)
    use ii = new MagickImage(path, settings)
    ii.Resize(size)
    i.Image.Composite(ii, int <| startX + (1. - scale) * width, int <| startY + (1. - scale) * height, CompositeOperator.Over)
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

let drawAbilities card (i: ImageState) =
    let drawMainTextAtHeight height text =
        captionTextCentered medSize (inset + textPadding) (cardMidpoint + padding) (height - 2. * padding) text
    let drawAbility top text =
        captionText medSize (inset + 2. * ``5/32`` + textPadding) (cardMidpoint + top + padding) (width - 2. * (inset + textPadding + ``5/32``)) (abilityThirdPoint - 2. * padding) text
        >> line darkGray lineworkWidth inset (cardMidpoint + top) (width - inset) (cardMidpoint + top)
        >> circle (``5/32`` + inset + abilityIconPadding) (cardMidpoint + top + abilityThirdPoint / 2.) ``5/32``

    let abilities = 
        match card with
        | Ship { Core = { MainAbility = main }; AllyAbility = ally; ScrapAbility = scrap } 
        | Fleet { Core = { MainAbility = main }; AllyAbility = ally; ScrapAbility = scrap } ->
            main, ally, scrap
        | Shield { Core = { MainAbility = main } } ->
            main, None, None
    i 
    |> line darkGray lineworkWidth inset cardMidpoint (width - inset) cardMidpoint
    |> line darkGray lineworkWidth inset cardBottomPoint (width - inset) cardBottomPoint
    |> match abilities with
        | main, Some at, Some st -> 
            drawMainTextAtHeight abilityThirdPoint main.Text
            >> drawAbility abilityThirdPoint at.Text
            >> drawAbility abilityTwoThirdPoint st.Text
        | main, Some at, None -> 
            drawMainTextAtHeight abilityHalfPoint main.Text
            >> drawAbility abilityHalfPoint at.Text
        | main, None, Some st -> 
            drawMainTextAtHeight abilityHalfPoint main.Text
            >> drawAbility abilityHalfPoint st.Text
        | main, None, None -> drawMainTextAtHeight (cardBottomPoint - cardMidpoint) main.Text

let drawCardCore (card: Card) (i: ImageState) =
    let data = 
        match card with
        | Ship { Core = core; Faction = faction; Count = count }
        | Fleet { Core = core; Faction = faction; Count = count } -> 
            {| Faction = faction; Core = core; Count = count |}
        | Shield { Core = core; Faction = faction; Count = count } ->
            {| Faction = faction; Core = core; Count = Some count |}
        | Planet _ -> invalidOp "This function does not support drawing planets"
    let drawLogo (i: Icon) =
        let w = (cardMidpoint - (inset + 2. * padding + ``1/2``))
        overlayImage 
            (width / 2. - w / 2.)
            (inset + 2. * padding + ``1/2``) 
            w w 
            i.ScaleCorrection i.Path
    i 
    // icon
    |> match data.Faction.Icon with
       | Some p ->
           circle (``1/4`` + inset + padding) (``1/4`` + inset + padding) ``1/4`` 
           >> overlayImage (inset + padding) (inset + padding) ``1/2`` ``1/2`` p.ScaleCorrection p.Path
       | None -> id
    // logo
    |> match card with
       | Ship _ -> drawLogo shipIcon
       | Fleet _ -> drawLogo fleetIcon
       | Shield _ -> drawLogo shieldIcon
    // cost
    |> (let circle = circle (width - ``1/4`` - inset - padding) (``1/4`` + inset + padding) ``1/4``
        match data.Core.Cost with
       | Some (CreditOnly c) ->
           circle
           >> text extraExtraLargeSize TextAlignment.Center Center (width - ``1/4`` - inset - one) (``1/4`` + inset - padding) (string c)
       | Some (StrengthOnly s) ->
           circle
           >> text extraExtraLargeSize TextAlignment.Center Center (width - ``1/4`` - inset - one) (``1/4`` + inset - padding) (string s)
       | Some (CreditAndStrength (c,s)) ->
           circle
           >> text extraLargeSize TextAlignment.Center Center (width - ``1/4`` - creditStrengthDualCostOffset - inset - one) (``1/4`` - creditStrengthDualCostOffset + inset - one) (string c)
           >> text extraLargeSize TextAlignment.Center Center (width - ``1/4`` + creditStrengthDualCostOffset - inset - one) (``1/4`` + creditStrengthDualCostOffset + inset - one) (string s)
       | None -> id)
    // clout
    |> match data.Core.Clout with
       | Some i -> 
            circle (``1/8``+ inset + padding) (height - ``1/8`` - inset - padding) ``1/8``
            >> (string i |> text largeSize TextAlignment.Center Center (``1/8`` + inset + padding + one) (height - ``1/8`` - inset - padding - one))
       | None -> id
    // ability area
    |> drawAbilities card
    // name
    |> captionTextCentered largeSize (``3/8`` + inset) inset ``3/8`` data.Core.Name
    |> captionTextCentered medSize (``3/8`` + inset) (inset + ``3/8``) (medSize + 2. * padding) $"{data.Faction.Name} {cardKind card}"
    // version
    |> text smallSize TextAlignment.Right Bottom (width - inset - padding) (height - inset - padding) version
    |> (List.init (Option.defaultValue 0u data.Count |> int) id
        |> List.fold (fun s i -> s >> filledCircle (width - inset - 0.325<inch> * dpi  - (float i) * (smallSize + padding)) (height - inset - padding - smallSize/2.) (circleSize / 2.)) id)

let draw (path: string) card =
    use image = new MagickImage(MagickColors.White, int (width + 1.<dot>), int (height + 1.<dot>))
    printfn $"Drawing card {card} to path {path}"
    image.Density <- Density(float dpi, DensityUnit.PixelsPerInch)
    let drawable = 
        Drawables()
         .FillOpacity(Percentage 0.)
         // border
         .StrokeWidth(float <| lineworkWidth * 2.)
         .StrokeColor(imperium.Primary)
         .Rectangle(float <| inset - 3.<dot>, float <| inset - 3.<dot>, float <| width - inset + 3.<dot>, float <| height - inset + 3.<dot>)
         .StrokeColor(darkGray)
         .StrokeWidth(float lineworkWidth)
         .Rectangle(float inset, float inset, float <| width - inset, float <| height - inset)

    { Image = image; Drawables = drawable; Width = width; Height = height }
    |> drawCardCore card
    |> ignore
    drawable.Draw image
    //image.Composite()
    image.Write path


printfn "Hello from F#"

sampleCards
|> List.iter (fun s -> draw $"..\..\..\..\..\..\Cards\{name s |> String.filter (fun c -> c <> ' ')}.png" s)
