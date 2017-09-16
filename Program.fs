open System
open System.Numerics
open System.Drawing
open System.Drawing.Imaging

let lerpNum (min1:float) (max1:float) (min2:float) (max2:float) (value:float) =
    (value-min1)/(max1-min1) * (max2-min2) + min2

let iterateComplexFunction iterations escapeValue func = 
    let rec iterate iters (z:Complex) =
        if (z.Imaginary+z.Real) > escapeValue then iters
        elif iters <= 0 then 0
        else iterate (iters-1) (func z)
    (iterate iterations Complex.Zero)

let iterateComplexFunctionWithDefault iterations escapeValue func z = 
    let rec iterate iters (z:Complex) =
        if (z.Imaginary+z.Real) > escapeValue then iters
        elif iters <= 0 then 0
        else iterate (iters-1) (func z)
    (iterate iterations z)

let drawGraphPoint (bitmap : Bitmap) x yMin yMax lastY value =
    let y = bitmap.Height - (int (lerpNum yMin yMax 0.0 (float bitmap.Height) value))
    use g = Graphics.FromImage(bitmap)
    if (x = 0 && y < bitmap.Height && y >= 0) then bitmap.SetPixel(x, y, Color.Black)
    elif ((y >= bitmap.Height || y < 0) && (lastY >= bitmap.Height || lastY < 0)) then ()
    else g.DrawLine(new Pen(Color.Black, 1.0f), x-1, lastY, x, y)
    y

let scatterGraphPoint (bitmap : Bitmap) x yMin yMax value =
    let y = bitmap.Height - (int (lerpNum yMin yMax 0.0 (float bitmap.Height) value))
    use g = Graphics.FromImage(bitmap)
    if (y < bitmap.Height && y >= 0) then bitmap.SetPixel(x, y, Color.Black)

let addGraphAxes (bitmap : Bitmap) xMin xMax yMin yMax =
    let xAxis = int (lerpNum yMin yMax 0.0 (float bitmap.Height) 0.0)
    let yAxis = int (lerpNum xMin xMax 0.0 (float bitmap.Width) 0.0)
    use g = Graphics.FromImage(bitmap)
    g.DrawLine(new Pen(Color.Black, 1.0f), 0, xAxis, bitmap.Width-1, xAxis)
    g.DrawLine(new Pen(Color.Black, 1.0f), yAxis, 0, yAxis, bitmap.Height-1)

let addIntegerLatice (bitmap : Bitmap) xMin xMax yMin yMax =
    use g = Graphics.FromImage(bitmap)
    let mutable i = int xMin
    let step = if int (log10 (xMax-xMin) - 1.0) > 0 then int (log10 (xMax-xMin) - 1.0) else 0
    let step2 = if int (log10 (yMax-yMin) - 1.0) > 0 then int (log10 (yMax-yMin) - 1.0) else 0
    while i <= (int xMax) do
        let xPos = int (lerpNum xMin xMax 0.0 (float bitmap.Width) (float i))
        g.DrawLine(new Pen(Color.LightGray, 1.0f), xPos, 0, xPos, bitmap.Width)
        i <- i+int (10.0**(float step))
    i <- int yMin
    while i <= (int yMax) do
        let yPos = int (lerpNum yMin yMax 0.0 (float bitmap.Width) (float i))
        g.DrawLine(new Pen(Color.LightGray, 1.0f), 0, yPos, bitmap.Height, yPos)
        i <- i+int (10.0**(float step2))

let addGraphLabels (bitmap : Bitmap) xMin xMax yMin yMax =
    let xAxis = int (lerpNum yMin yMax 0.0 (float bitmap.Height) 0.0)
    let yAxis = int (lerpNum xMin xMax 0.0 (float bitmap.Width) 0.0)
    use g = Graphics.FromImage(bitmap)
    let mutable i = int xMin
    let step = if int (log10 (xMax-xMin) - 1.0) > 0 then int (log10 (xMax-xMin) - 1.0) else 0
    let step2 = if int (log10 (yMax-yMin) - 1.0) > 0 then int (log10 (yMax-yMin) - 1.0) else 0
    while i <= (int xMax) do
        let xPos = int (lerpNum xMin xMax 0.0 (float bitmap.Width) (float i))
        g.DrawLine(new Pen(Color.Black, 1.0f), xPos, xAxis-2, xPos, xAxis+2)
        if i <> 0 then g.DrawString(i.ToString(), SystemFonts.DefaultFont, Brushes.Black, (float32 (xPos-4)), (float32 (xAxis+10)))
        i <- i+int (10.0**(float step))
    i <- int yMin
    while i <= (int yMax) do
        let yPos = int (lerpNum yMin yMax 0.0 (float bitmap.Width) (float i))
        g.DrawLine(new Pen(Color.Black, 1.0f), yAxis-2, yPos, yAxis+2, yPos)
        if i <> 0 then g.DrawString((-i).ToString(), SystemFonts.DefaultFont, Brushes.Black, (float32 (yAxis-14)), (float32 (yPos-6)))
        i <- i+int (10.0**(float step2))

let scatterPlotFunction xMin xMax yMin yMax (width:int) (height:int) func =
    let bitmap = new Bitmap(width, height)
    use g = Graphics.FromImage(bitmap)
    g.FillRectangle(Brushes.White, Rectangle(0, 0, bitmap.Width, bitmap.Height))
    addIntegerLatice bitmap xMin xMax yMin yMax
    for i = 0 to width-1 do
        (func (lerpNum 0.0 (float width) xMin xMax (float i))
         |> scatterGraphPoint bitmap i yMin yMax)
    addGraphAxes bitmap xMin xMax yMin yMax
    addGraphLabels bitmap xMin xMax yMin yMax
    bitmap

let plotFunction xMin xMax yMin yMax (width:int) (height:int) func =
    let bitmap = new Bitmap(width, height)
    let mutable lastY = 0
    use g = Graphics.FromImage(bitmap)
    g.FillRectangle(Brushes.White, Rectangle(0, 0, bitmap.Width, bitmap.Height))
    addIntegerLatice bitmap xMin xMax yMin yMax
    for i = 0 to width-1 do
        lastY <- (func (lerpNum 0.0 (float width) xMin xMax (float i))
         |> drawGraphPoint bitmap i yMin yMax lastY)
    addGraphAxes bitmap xMin xMax yMin yMax
    addGraphLabels bitmap xMin xMax yMin yMax
    bitmap

let plotComplexFunction rMin rMax iMin iMax (width:int) (height:int) func =
    let bitmap = new Bitmap(width, height)
    use g = Graphics.FromImage(bitmap)
    g.FillRectangle(Brushes.White, Rectangle(0, 0, bitmap.Width, bitmap.Height))
    addIntegerLatice bitmap rMin rMax iMin iMax
    for r = 0 to width-1 do
        for i = 0 to height-1 do
            if func (Complex(lerpNum 0.0 (float width) rMin rMax (float r), lerpNum 0.0 (float height) iMin iMax (float i))) then bitmap.SetPixel(r, i, Color.DarkGray)
    addGraphAxes bitmap rMin rMax iMin iMax
    addGraphLabels bitmap rMin rMax iMin iMax
    bitmap

[<EntryPoint>]
let main argv =
    //let bmp = plotFunction -6.5 6.5 -2.2 2.2 512 512 (fun x -> (sin x))
    let bmp = plotFunction -30.0 30.0 -30.0 30.0 1024 1024 (fun x -> (50.0/(x+1.0)))
    //let bmp = plotComplexFunction -2.0 2.0 -2.0 2.0 1024 1024 (fun x -> (iterateComplexFunction 500 2.0 (fun z -> z*z+x)) = 0)
    bmp.Save("plot.png", ImageFormat.Png)
    0
