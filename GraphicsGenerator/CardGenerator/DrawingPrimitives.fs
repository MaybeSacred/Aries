module DrawingPrimitives

open ImageMagick
open System.IO
open System
open Types

[<Measure>]
type dot

[<Measure>]
type fontPoint

[<Measure>]
type inch

type PangoWrap = Word | Char | ``Word-Char``
// https://imagemagick.org/script/defines.php
// pango:single-paragraph
type PangoReadDefines() =
    // pango:wrap=word|char|word-char
    member val Wrap: PangoWrap option = None with get, set
    interface IReadDefines with
        member x.Format = MagickFormat.Pango
        member x.Defines = 
            seq {
                if (x.Wrap.IsSome) then
                    yield MagickDefine(MagickFormat.Pango, "wrap", 
                        match x.Wrap.Value with Word -> "word" | Char -> "char" | ``Word-Char`` -> "word-char")
            }

type DrawFrom = | TopLeft | TopRight | BottomLeft | BottomRight

type Box = {
    X: float<dot>
    Y: float<dot>
    Width: float<dot>
    Height: float<dot>
    DrawFrom: DrawFrom
}
//List.pairwise
let atPosition x y box = { box with X = x; Y = y }

[<Literal>]
let version = "v0.1"

[<Literal>]
let font = "Verdana"

type TextVerticalAlignment = Top | Center | Bottom

let colorWithOpacity (color: IMagickColor<byte>) opacity = MagickColor(color.R, color.G, color.B, opacity)

let darkGray = MagickColor(0x14uy, 0x1Duy, 0x1Duy)
let medGray = MagickColor(0x46uy, 0x58uy, 0x58uy)
let black = MagickColor(0x00uy, 0x00uy, 0x00uy)
let tradeGold = colorWithOpacity MagickColors.Gold 0x70uy
let strengthRed = colorWithOpacity MagickColors.Red 0x70uy
let healthBlue = colorWithOpacity MagickColors.CornflowerBlue 0x68uy
let animaGreen = colorWithOpacity MagickColors.LawnGreen 0x68uy

[<Literal>]
let dpi = 300.<dot/inch>



type Boundaries = {
    WidthInInches: float<inch>
    HeightInInches: float<inch>
    Width: float<dot>
    Height: float<dot>
    XPixelCount: float<dot>
    YPixelCount: float<dot>
}

type ImageState = {
    Image: MagickImage
    Drawables: IDrawables<byte>
}
//    Boxes: Box list

//let pushBox box i =
//    { i with Boxes = List.append i.Boxes [box] }

//let popBox box i =
//    { i with Boxes = List. List.append i.Boxes [box] }

// drawFrom TopRight TopLeft ?

//[<Literal>]
let dotsPerFontPoint = 300.<dot>/72.<fontPoint>

let fontToDot f = f * dotsPerFontPoint

let fromDimensions width height = {
    WidthInInches = width
    HeightInInches = height
    Width = dpi * width - 1.<dot>
    Height = dpi * height - 1.<dot>
    XPixelCount = dpi * width
    YPixelCount = dpi * height
}

// https://printninja.com/printing-resource-center/printing-options/custom-game-printing/card-dimensions/non-standard-sized-playing-cards/
let cardBoundaries = fromDimensions 2.45<inch> 3.45<inch>

let godBoundaries = fromDimensions 4.1<inch> 2.95<inch>

let settlementBoundaries = fromDimensions 5.5<inch> 3.75<inch>

let ``1/16`` = (1.<inch> / 16.) * dpi
let ``3/32`` = (3.<inch> / 32.) * dpi
let ``1/8`` = (1.<inch> / 8.) * dpi
let ``5/32`` = (5.<inch> / 32.) * dpi
let ``3/16`` = (3.<inch> / 16.) * dpi
let ``1/4`` = dpi * 1.<inch> / 4.
let ``5/16`` = (5.<inch> / 16.) * dpi
let ``3/8`` = (3.<inch> / 8.) * dpi
let ``1/2`` = dpi * 1.<inch> / 2.

[<Literal>]
let inset = 14.<dot>
[<Literal>]
let favorCircleSize = 18.<dot>
[<Literal>]
let smallSize = 6.<fontPoint>
[<Literal>]
let medSize = 8.<fontPoint>
[<Literal>]
let largeSize = 10.<fontPoint>
[<Literal>]
let extraLargeSize = 12.<fontPoint>
[<Literal>]
let extraExtraLargeSize = 16.<fontPoint>
[<Literal>]
let lineworkWidth = 5.<dot>
[<Literal>]
let textPadding = 12.<dot>
[<Literal>]
let abilityIconPadding = 8.<dot>
[<Literal>]
let padding = 4.<dot>
/// smallest unit we care about
[<Literal>]
let quanta = 2.<dot>

[<Literal>]
let ImagesFolder = @"Images"

[<Literal>]
let ProductionCardFolder = @"Cards"

[<Literal>]
let GeneratedFolder = @"Generated"

let basePath =
    Environment.GetCommandLineArgs() 
    |> Array.tryItem 1
    |> Option.defaultValue @"C:\Users\jtyso\Documents\Aries\"

let text (size: float<fontPoint>) hAlignment vAlignment (startX: float<dot>) (startY: float<dot>) text (i: ImageState) =
    i.Drawables
     .PushGraphicContext()
     .Font(font)
     .FillColor(black)
     .StrokeOpacity(Percentage 0.)
     .FillOpacity(Percentage 100.)
     .FontPointSize(float size)
     .TextAlignment(hAlignment)
     .Density(float dpi)
     .Text(float startX, 
        float <| match vAlignment with | Top -> startY + fontToDot size | Center -> startY + (fontToDot size / 2.) | Bottom -> startY
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

let overlayImage (startX: float<dot>) (startY: float<dot>) (width: float<dot>) (height: float<dot>) (icon: ImageData) (i: ImageState) =
    let settings = MagickReadSettings()
    //settings.FillColor <- black
    settings.BackgroundColor <- MagickColors.Transparent
    settings.ColorSpace <- ColorSpace.Gray
    let scaledHalfWidth, scaledHalfHeight = int <| icon.ScaleCorrection * width / 2., int <| icon.ScaleCorrection * height / 2.
    let size = MagickGeometry(scaledHalfWidth * 2, scaledHalfHeight * 2)
    use ii = new MagickImage(Path.Combine(basePath, ImagesFolder, icon.Path), settings)
    ii.Evaluate(Channels.Alpha, EvaluateOperator.Multiply, icon.Opacity)
    ii.Resize(size) // lol the calcs aren't wrong, but scaled images appear 1 pixel too far to the top left corner
    i.Image.Composite(ii, 
        int startX + (if icon.ScaleCorrection <> 1. then (int (width / 2.) - scaledHalfWidth + 1) else 0), 
        int startY + (if icon.ScaleCorrection <> 1. then (int (height / 2.) - scaledHalfHeight + 1) else 0), 
        CompositeOperator.Over)
    i

let overlayImageAsCircle (startX: float<dot>) (startY: float<dot>) (diameter: float<dot>) (icon: ImageData) (i: ImageState) =
    let scaledHalfWidth, scaledHalfHeight = int <| icon.ScaleCorrection * diameter / 2., int <| icon.ScaleCorrection * diameter / 2.
    let size = MagickGeometry(scaledHalfWidth * 2, scaledHalfHeight * 2)
    use circle = new MagickImage(MagickColors.Transparent, size.Width, size.Height)
    let drawable = Drawables()
    { Image = circle; Drawables = drawable }
    |> filledCircle MagickColors.White MagickColors.Transparent (float scaledHalfWidth * 1.<dot>) (float scaledHalfHeight * 1.<dot>) (diameter / 2.)
    drawable.Draw circle
    let settings = MagickReadSettings()
    use ii = new MagickImage(Path.Combine(basePath, ImagesFolder, icon.Path), settings)
    ii.HasAlpha <- true
    ii.BackgroundColor <- MagickColors.Transparent
    ii.Resize(size)
    ii.Composite(circle, CompositeOperator.DstIn)
    ii.Evaluate(Channels.Alpha, EvaluateOperator.Multiply, icon.Opacity)
    i.Image.Composite(ii, 
        int startX + (if icon.ScaleCorrection <> 1. then (int (diameter / 2.) - scaledHalfWidth + 1) else 0), 
        int startY + (if icon.ScaleCorrection <> 1. then (int (diameter / 2.) - scaledHalfHeight + 1) else 0), 
        CompositeOperator.Over)
    i

let captionText (size: float<fontPoint>) (startX: float<dot>) (startY: float<dot>) (width: float<dot>) (height: float<dot>) (text: string) (i: ImageState) =
    let settings = MagickReadSettings()
    settings.Font <- font
    settings.FontPointsize <- float size
    settings.FillColor <- black
    settings.TextGravity <- Gravity.Center
    settings.BackgroundColor <- MagickColors.Transparent
    settings.Density <- Density(float dpi)
    settings.Height <- int height // height of text box
    settings.Width <- int width // width of text box
    use ii = new MagickImage($"caption:{text}", settings)
    i.Image.Composite(ii, int startX, int startY, CompositeOperator.Over)
    i

let pangoText (size: float<fontPoint>) (startX: float<dot>) (startY: float<dot>) (width: float<dot>) (height: float<dot>) (text: string) (i: ImageState) =
    let settings = MagickReadSettings()
    settings.Font <- font
    settings.FontPointsize <- float size
    settings.FillColor <- black
    
    settings.TextGravity <- Gravity.Center
    let pango = PangoReadDefines()
    pango.Wrap <- Some Word
    settings.Defines <- pango
    settings.Debug <- true
    settings.BackgroundColor <- MagickColors.Transparent
    settings.Density <- Density(float dpi)
    settings.Verbose <- true
    settings.Height <- int height // height of text box
    settings.Width <- int width // width of text box
    use ii = new MagickImage($"pango:{text}", settings)
    i.Image.Composite(ii, int startX, int startY, CompositeOperator.Over)
    i

//let captionTextCentered boundaries size startX startY height (text: string) (i: ImageState) =
//    captionText size startX startY (boundaries.XPixelCount - 2. * startX) height text i

