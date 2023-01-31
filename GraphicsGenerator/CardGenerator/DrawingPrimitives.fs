module DrawingPrimitives

open ImageMagick
open System.IO
open System

[<Measure>]
type dot

[<Measure>]
type inch

type DrawFrom = | TopLeft | TopRight | BottomLeft | BottomRight

type Box = {
    X: float<dot>
    Y: float<dot>
    Width: float<dot>
    Height: float<dot>
    DrawFrom: DrawFrom
}

let atPosition x y box = { box with X = x; Y = y }

[<Literal>]
let version = "v0.1"

type TextVerticalAlignment = Top | Center | Bottom

let colorWithOpacity (color: IMagickColor<byte>) opacity = MagickColor(color.R, color.G, color.B, opacity)

let darkGray = MagickColor(0x18uy, 0x21uy, 0x21uy)
let medGray = MagickColor(0x46uy, 0x58uy, 0x58uy)
let black = MagickColor(0x8uy, 0x8uy, 0x8uy)
let creditGold = colorWithOpacity MagickColors.Gold 0x60uy
let strengthRed = colorWithOpacity MagickColors.Red 0x60uy
let shieldBlue = colorWithOpacity MagickColors.CornflowerBlue 0x58uy
let energyGreen = colorWithOpacity MagickColors.LawnGreen 0x50uy

[<Literal>]
let dpi = 144.<dot/inch>

type Boundaries = {
    Width: float<inch>
    Height: float<inch>
    PixelWidth: float<dot>
    PixelHeight: float<dot>
    XPixelCount: float<dot>
    YPixelCount: float<dot>
}

type ImageState = {
    Image: MagickImage
    Drawables: IDrawables<byte>
    Boxes: Box list
}

let zeroBox drawFrom width height = { X = 0.<dot>; Y = 0.<dot>; Width = width; Height = height; DrawFrom = drawFrom }

let pushBox box i =
    { i with Boxes = List.Cons (box, i.Boxes) }

let pushFromExistingBox drawFrom x y i = 
    let box = { X = x; Y = y; Width = i.Boxes[0].Width; Height = i.Boxes[0].Height; DrawFrom = drawFrom }
    { i with Boxes = List.Cons (box, i.Boxes) }

let popBox i =
    { i with Boxes = List.tail i.Boxes }

let translateWith x y boxes =
    boxes 
    |> List.fold (fun (x,y, last) b -> 
        match last.DrawFrom with
        | TopLeft -> (x + b.X, y + b.Y, b)
        | TopRight -> (b.X + b.Width - x, y + b.Y, b)
        | BottomLeft -> (x + b.X, b.Y + b.Height - y, b)
        | BottomRight -> (b.X + b.Width - x, b.Y + b.Height - y, b)
    ) (x,y, boxes.Head)

let fromDimensions width height = {
    Width = width
    Height = height
    PixelWidth = dpi * width - 1.<dot>
    PixelHeight = dpi * height - 1.<dot>
    XPixelCount = dpi * width
    YPixelCount = dpi * height
}

let cardBoundaries = fromDimensions (2.<inch> + 7.<inch>/16.) (3.<inch> + 7.<inch>/16.)

let planetBoundaries = fromDimensions (5.<inch>) (3.<inch>)

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

[<Literal>]
let ImagesPath = @"Images"

[<Literal>]
let CardPath = @"Cards"

let basePath =
    Environment.GetCommandLineArgs() 
    |> Array.tryItem 1
    |> Option.defaultValue @"C:\Users\jtyso\Documents\Aries\"

let text (size: float<dot>) hAlignment vAlignment (startX: float<dot>) (startY: float<dot>) text (i: ImageState) =
    let startX, startY = translateWith startX startY i.Boxes
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
    let x, y = translateWith x y i.Boxes
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
    let startX, startY = translateWith startX startY i.Boxes
    i.Drawables
     .PushGraphicContext()
     .FillOpacity(Percentage 0.)
     .StrokeWidth(float strokeWidth)
     .StrokeColor(strokeColor)
     .Rectangle(float startX, float startY, float endX, float endY)
     .PopGraphicContext()
    i

let line color (width: float<dot>) (startX: float<dot>) (startY: float<dot>) (endX: float<dot>) (endY: float<dot>) (i: ImageState) =
    let startX, startY = translateWith startX startY i.Boxes
    let endX, endY = translateWith endX endY i.Boxes
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
    let startX, startY = translateWith startX startY i.Boxes
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
    let startX, startY = translateWith startX startY i.Boxes
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

let captionTextCentered boundaries size startX startY height (text: string) (i: ImageState) =
    captionText size startX startY (boundaries.XPixelCount - 2. * startX) height text i

