// Original auth r: Andy Sloane (https://github.com/a1k0n/)
// Writeup: https://www.a1k0n.net/2011/07/20/donut-math.html
// Actual C code:
//              k;double sin()
//          ,cos();main(){float A=
//        0,B=0,i,j,z[1760];char b[
//      1760];printf("\x1b[2J");for(;;
//   ){memset(b,32,1760);memset(z,0,7040)
//   ;for(j=0;6.28>j;j+=0.07)for(i=0;6.28
//  >i;i+=0.02){float c=sin(i),d=cos(j),e=
//  sin(A),f=sin(j),g=cos(A),h=d+2,D=1/(c*
//  h*e+f*g+5),l=cos      (i),m=cos(B),n=s\
// in(B),t=c*h*g-f*        e;int x=40+30*D*
// (l*h*m-t*n),y=            12+15*D*(l*h*n
// +t*m),o=x+80*y,          N=8*((f*e-c*d*g
//  )*m-c*d*e-f*g-l        *d*n);if(22>y&&
//  y>0&&x>0&&80>x&&D>z[o]){z[o]=D;;;b[o]=
//  ".,-~:;=!*#$@"[N>0?N:0];}}/*#****!!-*/
//   printf("\x1b[H");for(k=0;1761>k;k++)
//    putchar(k%80?b[k]:10);A+=0.04;B+=
//      0.02;}}/*****####*******!!=;:~
//        ~::==!!!**********!!!==::-
//          .,~~;;;========;;;:~-.
//              ..,--------,*/
//
// Port: Hideki A. Ikeda <HidekiAI@CodeMonkeyNinja.dev>
// 2021-July
//
// NOTE: all personal comments and notes are on my RUST (ascii_torust) side
//       personal joke is I've yet to encounter legible F# code that I can comprehend in a glance...
open System

[<EntryPoint>]
let main argv =
    let luminancePixels = ".,-~:;=!*#$@" // gets darker as surface normal vector gets longer

    let screenWidth = 80
    let screenHeight = 22
    let screenCenterX = screenWidth / 2
    let screenCenterY = screenHeight / 2
    let charSpace = ' '
    let charLineFeed = Convert.ToChar(10)
    let screenDim = (screenWidth * (screenHeight + 1))
    let thetaRotateSteps = 0.07
    let phiRotateSteps = 0.02
    //let inner_radius = 1.0
    //let outer_radius = 2.0
    let twoPI = 2.0 * (double Math.PI)
    let mutable mutA = 0.0
    let mutable mutB = 0.0
    let mutable mutPhi = 0.0
    let mutable mutTheta = 0.0
    let dimSize = screenDim + screenWidth + 2
    let mutable mutZBuff: double array = Array.zeroCreate dimSize
    let mutable mutRenderBuff: char array = Array.zeroCreate dimSize
    printfn ("\x1b[2J") // ASCII representations to CLS
    let mutable outerLoop = false
    let mutable innerLoop = false

    let rec recMyWork () =
        for buffIndex in 0 .. screenDim do
            mutRenderBuff.[buffIndex] <- charSpace
            mutZBuff.[buffIndex] <- 0.0

        let sinA = Math.Sin mutA
        let cosA = Math.Cos mutA
        let cosB = Math.Cos mutB
        let sinB = Math.Sin mutB
        outerLoop <- false
        mutTheta <- 0.0
        while not outerLoop do
            let cosTheta = Math.Cos mutTheta
            let sinTheta = Math.Sin mutTheta
            mutPhi <- 0.0
            innerLoop <- false

            while not innerLoop do
                let sinPhi = Math.Sin mutPhi
                let cosPhi = Math.Cos mutPhi
                let h = cosTheta + 2.0
                let t = sinPhi * h * cosA - sinTheta * sinA

                let zDepth =
                    1.0 / (sinPhi * h * sinA + sinTheta * cosA + 5.0)

                let t = sinPhi * h * cosA - sinTheta * sinA

                let screenX =
                    int (
                        (double screenCenterX
                         + 30.0 * zDepth * (cosPhi * h * cosB - t * sinB))
                    )

                let screenY =
                    int (
                        (double (screenCenterY + 1)
                         + 15.0 * zDepth * (cosPhi * h * sinB + t * cosB))
                    )

                let pixelIndex = screenX + screenWidth * screenY

                if pixelIndex >= mutRenderBuff.Length then
                    failwithf "Buffer overflow: Index for {%d},{%d} exceeds {%d}" screenX screenY mutRenderBuff.Length

                // OK, just one (and only one) comment: This ranges 0..11 (8*sqrt(2)=11.3) to calculate the luminance which index to array pixels[]
                let vecNormal =
                    8.0
                    * ((sinTheta * sinA - sinPhi * cosTheta * cosA)
                       * cosB
                       - sinPhi * cosTheta * sinA
                       - sinTheta * cosA
                       - cosPhi * cosTheta * sinB)
                // OK, I lied...  Only want to raycast if surface normal is not 0 (and positive)
                if (screenHeight > screenY
                   && screenWidth > screenX
                   && screenY > 0
                   && screenX > 0
                   && zDepth > mutZBuff.[pixelIndex]) then
                    mutZBuff.[pixelIndex] <- zDepth

                    let normalAsIndex =
                        if vecNormal > 0.0 then
                            int vecNormal
                        else
                            0

                    let pixel = luminancePixels.[normalAsIndex]
                    mutRenderBuff.[pixelIndex] <- pixel

                mutPhi <- mutPhi + phiRotateSteps

                if mutPhi > twoPI then
                    // we've fully rotated 360-degrees (2PI)
                    innerLoop <- true
                    mutPhi <- 0.0

            mutTheta <- mutTheta + thetaRotateSteps

            if mutTheta > twoPI then
                // we've fully rotated 360-degrees (2PI)
                outerLoop <- true
                mutTheta <- 0.0

        Console.Write(charLineFeed)
        printfn ("\x1b[H") // move cursor back to HOME position

        for screenXYPixelIndex in 0 .. (screenDim + 1) do
            let ch =
                match screenXYPixelIndex with
                | i when (screenXYPixelIndex % screenWidth) <> 0 -> mutRenderBuff.[screenXYPixelIndex]
                | _ -> charLineFeed

            Console.Write(ch)
            mutA <- mutA + 0.00004
            mutB <- mutB + 0.00002

        System.Threading.Thread.Sleep(100)

        recMyWork () // recurse

    recMyWork ()

    0
