namespace Generate
open System.Drawing
open System

module Shapes =
    type RectangularPrism = {
        NX: int
        NY: int
        NZ: int
        Points: Map<int*int*int,Color>
    }
    type Position = {
        X: int
        Y: int
        Z: int
        }

    type ShapeWithPosition = {
        Position: Position
        Shape: RectangularPrism
    }

    type GridType =
        | Diamond
        | Square
        | Basic

    type Vertex = 
        {
        PositionX: float
        PositionY: float
        PositionZ: float
        ColorR: float
        ColorG: float
        ColorB: float
        NormalX: float
        NormalY: float
        NormalZ: float
        }
    let createColor (r:float) (g:float) (b:float) =
        Color.FromArgb(255, int (r*255.0), int (g*255.0), int (b*255.0) )

    let colorToHsv (color:Color)=  
        let maxColor = max (max color.R color.G) color.B
        let minColor = min (min color.R color.G) color.B

        let hue = float (color.GetHue())
        let saturation = if maxColor = 0uy then 0.0 else 1.0 - (1.0 * (float minColor) / (float maxColor))
        let value = (float maxColor) / 255.0
        (hue,saturation,value)

    let colorFromHsv ((hue,saturation,value): (float*float*float)) =
        let hi = (int32 (floor (hue / 60.0))) % 6
        let f = hue / 60.0 - floor(hue / 60.0);

        let fullValue = value * 255.0;
        let v = int32 (fullValue);
        let p = int32(fullValue * (1.0 - saturation));
        let q = int32(fullValue * (1.0 - f * saturation));
        let t = int32(fullValue * (1.0 - (1.0 - f) * saturation));

        match hi with
        | 0 -> Color.FromArgb(255, v, t, p)
        | 1 -> Color.FromArgb(255, q, v, p)
        | 2 -> Color.FromArgb(255, p, v, t)
        | 3 ->  Color.FromArgb(255, p, q, v)
        | 4 -> Color.FromArgb(255, t, p, v)
        | _ -> Color.FromArgb(255, v, p, q)
        
    let randomizeValue (r:System.Random) (step:float) (color:Color)  =
        let (baseH,baseS,baseV) = colorToHsv color
        let dist = Math.Sqrt(-2.0*Math.Log(r.NextDouble())) * Math.Sin(2.0*Math.PI*r.NextDouble())
        let addedV = step*dist
        let newV = (baseV + addedV) |> min 1.0 |> max 0.0
        colorFromHsv (baseH,baseS,newV)

    let randomizeSaturation (r:System.Random) (step:float) (color:Color)  =
        let (baseH,baseS,baseV) = colorToHsv color
        let dist = Math.Sqrt(-2.0*Math.Log(r.NextDouble())) * Math.Sin(2.0*Math.PI*r.NextDouble())
        let addedV = step*dist
        let newV = (baseV + addedV) |> min 1.0 |> max 0.0
        colorFromHsv (baseH,baseS,newV)

    let averageColors (colors: Color seq) =
        let (totalHue,totalSaturation,totalValue,count) =
            colors
            |> Seq.map colorToHsv
            |> Seq.fold (fun (tH,tS,tV,count) (h,s,v) -> (tH+h,tS+s,tV+v,count+1)) (0.0,0.0,0.0,0)
        let floatCount = float count
        colorFromHsv (totalHue/floatCount,totalSaturation/floatCount,totalValue/floatCount)
  

    let generateAllPointsOnSurface (gridType:GridType) (nX:int) (nY:int) (nZ:int) (step:int) =
        let side (nA:int) (nB:int) = 
            match gridType with
            | Square ->
                seq {for a in [0..nA/step] do
                        let isEven = a % 2 = 0
                        let bChoices =  [0..(if isEven then nB/step/2 - 1 else nB/step/2)]
                        for b in bChoices do 
                            yield (a*step,(if isEven then b*step*2 + step else b*step*2) )}
            | Diamond -> 
                seq {for a in [0..(nA/(step*2)-1)] do 
                         for b in [0..(nB/(step*2)-1)] do
                             yield ((a*2*step+step),(b*2*step+step))}
            | Basic -> 
                seq {for a in [0..(nA/step)] do 
                         for b in [0..(nB/step)] do
                             yield (a*step,b*step)}
        seq [
            ((fun (a,b) -> (0,a,b)),nY,nZ)
            ((fun (a,b) -> (nX,a,b)),nY,nZ)
            ((fun (a,b) -> (a,0,b)),nX,nZ)
            ((fun (a,b) -> (a,nY,b)),nX,nZ)
            ((fun (a,b) -> (a,b,0)),nX,nY)
            ((fun (a,b) -> (a,b,nZ)),nX,nY)
            ]
        |> Seq.collect (fun (changer, nA, nB) -> Seq.map changer (side nA nB))
        |> Seq.distinct
        |> Seq.cache
        
    let isOnPlane nA a =
        a = 0 || a = nA

    let getNeighbors isDiamond nX nY nZ step ((x,y,z):(int*int*int)) =
        if isDiamond then
            if isOnPlane nX x then [(x,y-step,z-step);(x,y-step,z+step);(x,y+step,z-step);(x,y+step,z+step)]
            else if isOnPlane nY y then [(x-step,y,z-step);(x-step,y,z+step);(x+step,y,z-step);(x+step,y,z+step)]
            else if isOnPlane nZ z then [(x-step,y-step,z);(x+step,y-step,z);(x-step,y+step,z);(x+step,y+step,z)]
            else List.empty
        else
            seq [
                    (if isOnPlane nX x then [(x,y-step,z);(x,y+step,z);(x,y,z-step);(x,y,z+step)] else List.empty)
                    (if isOnPlane nY y then [(x-step,y,z);(x-step,y,z);(x,y,z-step);(x,y,z+step)] else List.empty)
                    (if isOnPlane nZ z then [(x-step,y,z);(x+step,y,z);(x,y-step,z);(x,y+step,z)] else List.empty)
                    ]
                |> List.concat

    let rec gcd (a:int) (b:int) = 
        if b = 0 then a else gcd b (a % b)

    let rec greatestPowerOfTwo powersSoFar a =
        if a % 2 = 1 then
            powersSoFar
        else greatestPowerOfTwo (powersSoFar*2) (a/2)

    let log2 a =
        let rec log2Inner current a =
            if a = 1 
            then current 
            else log2Inner (current + 1) (a/2)
        log2Inner 0 a

    let pow2 a =
        Seq.replicate a 2
        |> Seq.fold (*) 1

    let seqRandom (r:System.Random) (s: 'T seq) =
        let length = Seq.length s
        Seq.item (r.Next(length)) s

    let colorRectangularPrism (r:System.Random) (nX:int) (nY:int) (nZ:int) (baseColor: Color) : RectangularPrism =
        let baseRandomStep = 0.2
        let stepRandomStep = 0.05
        let initialGridStep = 
                [nX;nY;nZ]
                |> List.map (greatestPowerOfTwo 1)
                |> List.min
        let initial = 
            generateAllPointsOnSurface Basic nX nY nZ initialGridStep
            |> Seq.map (fun p -> (p,randomizeValue r baseRandomStep baseColor))
            |> Map.ofSeq
            
             
        let takeStep (isDiamond:bool) (nX:int) (nY:int) (nZ:int) (stepSize:int) (current:Map<(int*int*int),Color>) =
            let gridType = if isDiamond then Diamond else Square
            generateAllPointsOnSurface gridType nX nY nZ stepSize
            |> Seq.map 
                (fun point ->
                    let newColor =
                        getNeighbors isDiamond nX nY nZ stepSize point
                        |> Seq.choose (fun neighbor -> Map.tryFind neighbor current)
                        |> averageColors
                        |> randomizeValue r stepRandomStep
                    (point,newColor))
            |> Seq.fold (fun current (point,color) -> Map.add point color current) current
            
        let points =
            Seq.init (log2 initialGridStep) id
            |> Seq.scan (fun c i -> c*2) 1
            |> Seq.rev
            |> Seq.tail //up to this is to generate the step sizes
            |> Seq.collect (fun stepSize -> seq [(takeStep true nX nY nZ stepSize);(takeStep false nX nY nZ stepSize)])
            |> Seq.fold (fun c takeStep -> takeStep c) initial
        {Points = points; NX = nX; NY = nY; NZ = nZ}

module Clusters =
    open Shapes
    let spawnRectFromRect (r:System.Random) (maxSideSize:int) (color:Color) (s:ShapeWithPosition) =
        let rect = s.Shape
        let p = s.Position
        
        let logOldNx = log2 rect.NX
        let logOldNy = log2 rect.NY

        let nX = r.Next(logOldNx) + 1 |> pow2
        let nY = r.Next(logOldNy) + 1 |> pow2
        let nZ = r.Next(log2 maxSideSize) + 1 |> pow2
        let pX = p.X + r.Next(rect.NX-nX+1)
        let pY = p.Y + r.Next(rect.NY-nY+1)
        let pZ = p.Z + rect.NZ
        let shape = colorRectangularPrism r nX nY nZ color
        {Position = {X = pX;Y = pY; Z = pZ}; Shape = shape}

    let simpleStack (startingPosition:Position) (maxSideSize:int) (color:Color) =
        let r = new System.Random()
        let initial = 
            let nX = r.Next(log2 maxSideSize) + 1 |> pow2
            let nY = r.Next(log2 maxSideSize) + 1 |> pow2
            let nZ = r.Next(log2 maxSideSize) + 1 |> pow2
            let shape = colorRectangularPrism r nX nY nZ color
            {Position=startingPosition;Shape=shape}
        let second = spawnRectFromRect r maxSideSize Color.Red initial
        [initial;second]

module Vertices =
    open Shapes
    let shiftVertices (position:Position) (vertices: Vertex array) =
        vertices
        |> Array.map (fun v -> {v with PositionX = v.PositionX + (float position.X); PositionY = v.PositionY + (float position.Y); PositionZ = v.PositionZ + (float position.Z)})

    let shapeToTriangles (s:ShapeWithPosition) =
        let position = s.Position
        let rect = s.Shape 
        let r = new System.Random()
        let sideInfo =
            seq [
                (((fun (a,b) -> (0,a,b)),(-1.0,0.0,0.0)),rect.NY,rect.NZ)
                (((fun (a,b) -> (rect.NX,a,b)),(1.0,0.0,0.0)),rect.NY,rect.NZ)
                (((fun (a,b) -> (a,0,b)),(0.0,-1.0,0.0)),rect.NX,rect.NZ)
                (((fun (a,b) -> (a,rect.NY,b)),(0.0,1.0,0.0)),rect.NX,rect.NZ)
                (((fun (a,b) -> (a,b,0)),(0.0,0.0,-1.0)),rect.NX,rect.NY)
                (((fun (a,b) -> (a,b,rect.NZ)),(0.0,0.0,1.0)),rect.NX,rect.NY)
                ]
        let getTriangles (nA:int) (nB:int) ((sideMap,sideNormal):((int*int->int*int*int)*(float*float*float))) =
            seq {for a in [0..(nA-1)] do
                    for b in [0..(nB-1)] do 
                        for side in [true;false] do 
                            let points = 
                                seq [(a,b);(if side then (a,b+1) else (a+1,b));(a+1,b+1)]
                                |> Seq.map sideMap
                                |> Seq.cache
                            let color =
                                points
                                |> Seq.map (fun point -> Map.find point rect.Points)
                                |> seqRandom r
                            let fullPoints =
                                points
                                |> Seq.map (fun point -> (point,color,sideNormal))
                            yield fullPoints}
            |> Seq.collect id
            
        sideInfo
        |> Seq.collect (fun (side,nA,nB) -> getTriangles nA nB side)
        |> Seq.map (fun ((x,y,z),color,(nX,nY,nZ)) -> 
            {
                PositionX = float x
                PositionY = float y
                PositionZ = float z
                ColorR = (float color.R)/255.0
                ColorG = (float color.G)/255.0
                ColorB = (float color.B)/255.0
                NormalX = nX
                NormalY = nY
                NormalZ = nZ
            })
        |> Array.ofSeq
        |> shiftVertices position

    let shapesToTriangles (shapes: ShapeWithPosition list) = 
        shapes
        |> List.toArray
        |> Array.map shapeToTriangles