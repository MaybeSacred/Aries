open ImageMagick
open ImageMagick.Configuration
open System

[<Measure>]
type dot

[<Measure>]
type inch

type Box = {
    X: float<dot>
    Y: float<dot>
    Width: float<dot>
    Height: float<dot>
}

let atPosition x y box = { box with X = x; Y = y }

type Icon = {
    Path: string 
    ScaleCorrection: float 
}

type Faction = {
    Primary: MagickColor
    Secondary: MagickColor
    Icon: Icon option
    Name: string
}

type CardKind = Shield | Fleet | Planet

type Card = {
    Faction: Faction
    Kind: CardKind
    Name: string
    MainText: string
    AllyText: string option
    ScrapText: string option
    Cost: uint
    Influence: uint option
    Count: uint option
}

type ImageState = {
    Image: MagickImage
    Drawables: IDrawables<byte>
    Width: float<dot>
    Height: float<dot>
}

let fleetIcon = {
    Path = @"C:\Users\jtyso\Documents\Interstellar Overdrive\Ship.webp"
    ScaleCorrection = 1.0
}

let shieldIcon = {
    Path = @"C:\Users\jtyso\Documents\Interstellar Overdrive\ShieldOutlineLogo.png"
    ScaleCorrection = 1.0
}

let unaligned = {
    Primary = new MagickColor(0xAAuy, 0x27uy, 0x04uy)
    Secondary = new MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = None
    Name = "Unaligned"
}

let imperium = {
    Primary = new MagickColor(0xAAuy, 0x27uy, 0x04uy)
    Secondary = new MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = Some {
        Path = @"C:\Users\jtyso\Documents\Interstellar Overdrive\ImperiumLogo.png"
        ScaleCorrection = 1.0
    }
    Name = "Imperium"
}

let stellarion = {
    Primary = new MagickColor(0xAAuy, 0x27uy, 0x04uy)
    Secondary = new MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = Some {
        Path = @"C:\Users\jtyso\Documents\Interstellar Overdrive\ImperiumLogo.png"
        ScaleCorrection = 1.0
    }
    Name = "Stellarion"
}

let botBrigade = {
    Primary = new MagickColor(0xAAuy, 0x27uy, 0x04uy)
    Secondary = new MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = Some {
        Path = @"C:\Users\jtyso\Documents\Interstellar Overdrive\BattleBotLogo.png"
        ScaleCorrection = 0.88
    }
    Name = "Bot Brigade"
}

let rogueAlliance = {
    Primary = new MagickColor(0xAAuy, 0x27uy, 0x04uy)
    Secondary = new MagickColor(0x52uy, 0x13uy, 0x02uy)
    Icon = Some {
        Path = @"C:\Users\jtyso\Documents\Interstellar Overdrive\RogueAllianceLogo.png"
        ScaleCorrection = 1.0
    }
    Name = "Rogue Alliance"
}

let sparky = {
    Faction = botBrigade
    Kind = Fleet
    Name = "Metallic Hydrogen Supplier"
    MainText = "Draw 1 card. Some really long text to see what happens"
    AllyText = Some "Draw 1 card. Some other really long text to see what happens"
    ScrapText = Some "Scrap this card. Gain 1 Strength"
    Cost = 88u
    Influence = Some 88u
    Count = Some 3u
}

[<Literal>]
let version = "v0.1"

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
let extraLargeSize = 32.<dot>
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

let ``3/16`` = (3.<inch> / 16.) * dpi
let ``5/32`` = (5.<inch> / 32.) * dpi
let ``3/8`` = (3.<inch> / 8.) * dpi
let ``1/4`` = dpi / 4.
let ``1/8`` = (1.<inch> / 8.) * dpi
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

let draw (path: string) card =
    //ImageMagick.IMagickImage
    use image = new MagickImage(MagickColors.White, int (width + 1.<dot>), int (height + 1.<dot>))
    image.Density <- Density(float dpi, DensityUnit.PixelsPerInch)
    printfn "Hello from F#: %A %f %f" image.Density.Units image.Density.X image.Density.Y
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
    // icon
    |> match card.Faction.Icon with
       | Some p ->
           circle (``3/16`` + inset + padding) (``3/16`` + inset + padding) ``3/16`` 
           >> overlayImage (inset + padding) (inset + padding) ``3/8`` ``3/8`` p.ScaleCorrection p.Path
       | None -> id
    |> match card.Kind with
       | Fleet -> overlayImage (width / 2. - (cardMidpoint - (inset + 2. * padding + ``3/8``)) / 2.) (inset + 2. * padding + ``3/8``) (cardMidpoint - (inset + 2. * padding + ``3/8``)) (cardMidpoint - (inset + 2. * padding + ``3/8``)) fleetIcon.ScaleCorrection fleetIcon.Path
       | Shield -> overlayImage (inset + padding) (inset + padding) (width - inset - padding) (width - inset - padding) shieldIcon.ScaleCorrection shieldIcon.Path
    // cost
    |> circle (width - ``3/16`` - inset - padding) (``3/16`` + inset + padding) ``3/16``
    |> text extraLargeSize TextAlignment.Center Center (width - ``3/16`` - inset - one) (``3/16`` + inset - one) (string card.Cost)
    // influence
    |> match card.Influence with
       | Some i -> 
            circle (``1/8``+ inset + padding) (height - ``1/8`` - inset - padding) ``1/8``
            >> (string i |> text medSize TextAlignment.Center Center (``1/8`` + inset + padding + one) (height - ``1/8`` - inset - padding - one))
       | None -> id
    // ability area
    |> line darkGray lineworkWidth inset cardMidpoint (width - inset) cardMidpoint
    |> line darkGray lineworkWidth inset cardBottomPoint (width - inset) cardBottomPoint
    |> match card.AllyText, card.ScrapText with
       | Some at, Some st -> 
            captionTextCentered medSize (inset + textPadding) (cardMidpoint + padding) (abilityThirdPoint - 2. * padding) card.MainText
            >> captionText medSize (inset + 2. * ``5/32`` + textPadding) (cardMidpoint + abilityThirdPoint + padding) (width - 2. * (inset + textPadding + ``5/32``)) (abilityThirdPoint - 2. * padding) at
            >> line darkGray lineworkWidth inset (cardMidpoint + abilityThirdPoint) (width - inset) (cardMidpoint + abilityThirdPoint)
            >> circle (``5/32`` + inset + abilityIconPadding) (cardMidpoint + abilityThirdPoint + abilityThirdPoint / 2.) ``5/32``
            >> captionText medSize (inset + 2. * ``5/32`` + textPadding) (cardMidpoint + abilityTwoThirdPoint + padding) (width - 2. * (inset + textPadding + ``5/32``)) (abilityThirdPoint - 2. * padding) st
            >> line darkGray lineworkWidth inset (cardMidpoint + abilityTwoThirdPoint) (width - inset) (cardMidpoint + abilityTwoThirdPoint)
            >> circle (``5/32`` + inset + abilityIconPadding) (cardMidpoint + abilityTwoThirdPoint + abilityThirdPoint / 2.) ``5/32``
       //| Some at, None -> 
       //     captionTextCentered medSize (inset + 2. * ``3/16ths`` + 6.) (cardMidpoint + abilityHalfPoint + padding) at
       //     >> line darkGray lineworkWidth inset (2.5 * dpi) (width - inset) (2.5 * dpi)
       //     >> circle (``3/16ths`` + inset + 4.) (``3/16ths`` + inset + 2.5 * dpi) ``3/16ths``
       //| None, Some st -> 
       //     text medSize TextAlignment.Left Top (inset + 2. * ``3/16ths`` + 5.) (inset + ``3/16ths`` + 2.5 * dpi) st
       //     >> line darkGray lineworkWidth inset (2.5 * dpi) (width - inset) (2.5 * dpi)
       //     >> circle (``3/16ths`` + inset + 4.) (``3/16ths`` + inset + 2.5 * dpi) ``3/16ths``
       | _ -> captionTextCentered medSize (inset + textPadding) (cardMidpoint + padding) (cardBottomPoint - cardMidpoint - 2. * padding) card.MainText
    // name
    |> captionTextCentered largeSize (``3/8`` + inset) inset ``3/8`` card.Name
    |> captionTextCentered medSize (``3/8`` + inset) (inset + ``3/8``) medSize $"{card.Faction.Name} {card.Kind}"
    // version
    |> text smallSize TextAlignment.Right Bottom (width - inset - padding) (height - inset - padding) version
    |> (List.init (Option.defaultValue 0u card.Count |> int) id
        |> List.fold (fun s i -> s >> filledCircle (width - inset - 0.375<inch> * dpi  - (float i) * (smallSize + padding)) (height - inset - padding - smallSize/2.) (circleSize / 2.)) id)
    |> ignore
    drawable.Draw image
    //image.Composite()
    image.Write path


printfn "Hello from F#"

draw "./one.png" sparky
