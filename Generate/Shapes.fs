namespace Generate
open System.Drawing
open System

module Shapes =

    type Cube = {
        N: int
        Points: Map<int*int*int,Color>
        }
    
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
        
    let randomizeColor (r:System.Random) (step:float) (color:Color)  =
        let (baseH,baseS,baseV) = colorToHsv color
        let angle = r.NextDouble()*360.0
        let dist = Math.Sqrt(-2.0*Math.Log(r.NextDouble())) * Math.Sin(2.0*Math.PI*r.NextDouble())
        let addedS = Math.Sin(angle)*step*dist
        let addedV = Math.Cos(angle)*step*dist
        let newS = (baseS + addedS) |> min 1.0 |> max 0.0
        let newV = (baseV + addedV) |> min 1.0 |> max 0.0
        colorFromHsv (baseH,newS,newV)

    let averageColors (colors: Color seq) =
        let (totalHue,totalSaturation,totalValue,count) =
            colors
            |> Seq.map colorToHsv
            |> Seq.fold (fun (tH,tS,tV,count) (h,s,v) -> (tH+h,tS+s,tV+v,count+1)) (0.0,0.0,0.0,0)
        let floatCount = float count
        colorFromHsv (totalHue/floatCount,totalSaturation/floatCount,totalValue/floatCount)
    
    let rec cartesianProduct LL = 
        match LL with
        | [] -> Seq.singleton []
        | L::Ls -> seq {for x in L do for xs in cartesianProduct Ls -> x::xs}

    let generateAllPointsOnSurface (isDiamond:bool) (n:int) (step:int) =
        let side = 
            if not isDiamond then
                seq {for x in [0..n/step] do
                        let isEven = x % 2 = 0
                        let yChoices =  [0..(if isEven then n/step/2 - 1 else n/step/2)]
                        for y in yChoices do 
                            yield (x*step,(if isEven then y*step*2 + step else y*step*2) )}
            else 
                seq {for x in [0..(n/(step*2)-1)] do 
                         for y in [0..(n/(step*2)-1)] do
                             yield ((x*2*step+step),(y*2*step+step))}
        seq [
            (fun (x,y) -> (0,x,y))
            (fun (x,y) -> (n,x,y))
            (fun (x,y) -> (x,0,y))
            (fun (x,y) -> (x,n,y))
            (fun (x,y) -> (x,y,0))
            (fun (x,y) -> (x,y,n))
            ]
        |> Seq.collect (fun changer -> Seq.map changer side)
        |> Seq.distinct
        |> Seq.cache
        
    let isOnPlane n a =
        a = 0 || a = n

    let getNeighbors isDiamond n step ((x,y,z):(int*int*int)) =
        if isDiamond then
            if isOnPlane n x then [(x,y-step,z-step);(x,y-step,z+step);(x,y+step,z-step);(x,y+step,z+step)]
            else if isOnPlane n y then [(x-step,y,z-step);(x-step,y,z+step);(x+step,y,z-step);(x+step,y,z+step)]
            else if isOnPlane n z then [(x-step,y-step,z);(x+step,y-step,z);(x-step,y+step,z);(x+step,y+step,z)]
            else List.empty
        else
            seq [
                    (if isOnPlane n x then [(x,y-step,z);(x,y+step,z);(x,y,z-step);(x,y,z+step)] else List.empty)
                    (if isOnPlane n y then [(x-step,y,z);(x-step,y,z);(x,y,z-step);(x,y,z+step)] else List.empty)
                    (if isOnPlane n z then [(x-step,y,z);(x+step,y,z);(x,y-step,z);(x,y+step,z)] else List.empty)
                    ]
                |> List.concat

    let colorCube (logN:int) (baseColor: Color) : Cube =
        let n = Seq.replicate logN 2 |> Seq.reduce (*)
        let baseRandomStep = 0.2
        let stepRandomStep = 0.05
        let r = new System.Random()
        let initial = 
            seq [
                (0,0,0)
                (0,0,n)
                (0,n,0)
                (0,n,n)
                (n,0,0)
                (n,0,n)
                (n,n,0)
                (n,n,n)
                ]
            |> Seq.map (fun p -> (p,randomizeColor r baseRandomStep baseColor))
            |> Map.ofSeq
            
             
        let takeStep (isDiamond:bool) (n:int) (stepSize:int) (current:Map<(int*int*int),Color>) =
            generateAllPointsOnSurface isDiamond n stepSize
            |> Seq.map 
                (fun point ->
                    let newColor =
                        getNeighbors isDiamond n stepSize point
                        |> Seq.choose (fun neighbor -> Map.tryFind neighbor current)
                        |> averageColors
                        |> randomizeColor r stepRandomStep
                    (point,newColor))
            |> Seq.fold (fun current (point,color) -> Map.add point color current) current
            
        let points =
            Seq.init logN id
            |> Seq.scan (fun c i -> c*2) 1
            |> Seq.rev
            |> Seq.tail //up to this is to generate the step sizes
            |> Seq.collect (fun stepSize -> seq [(takeStep true n stepSize);(takeStep false n stepSize)])
            |> Seq.fold (fun c takeStep -> takeStep c) initial
        {Points = points; N = n}

    let cubeToTriangles (cube:Cube) = 
        let n = cube.N
        let sideInfo =
            seq [
                ((fun (a,b) -> (0,a,b)),(-1.0,0.0,0.0))
                ((fun (a,b) -> (n,a,b)),(1.0,0.0,0.0))
                ((fun (a,b) -> (a,0,b)),(0.0,-1.0,0.0))
                ((fun (a,b) -> (a,n,b)),(0.0,1.0,0.0))
                ((fun (a,b) -> (a,b,0)),(0.0,0.0,-1.0))
                ((fun (a,b) -> (a,b,n)),(0.0,0.0,1.0))
                ]
        let getTriangles ((sideMap,sideNormal):((int*int->int*int*int)*(float*float*float))) =
            seq {for a in [0..(n-1)] do
                    for b in [0..(n-1)] do 
                        for side in [true;false] do 
                            let points = 
                                seq [(a,b);(if side then (a,b+1) else (a+1,b));(a+1,b+1)]
                                |> Seq.map sideMap
                                |> Seq.cache
                            let color =
                                points
                                |> Seq.map (fun point -> Map.find point cube.Points)
                                |> averageColors
                            let fullPoints =
                                points
                                |> Seq.map (fun point -> (point,color,sideNormal))
                            yield fullPoints}
            |> Seq.collect id
            
        sideInfo
        |> Seq.collect (fun side -> getTriangles side)
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