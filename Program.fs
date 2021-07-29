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
// There is an optimized version which does not call math-libs, but for this
// port, it would be more straight-forward to use the one  with cos/sin libs
//
// NOTE: all personal comments and notes are on my RUST (ascii_torust) side
//       personal joke is I've yet to encounter legible F# code that I can comprehend in a glance...
open System

[<EntryPoint>]
let main argv =
    let pixels = ".,-~:;=!*#$@"

    let screen_width = 80.0
    let screen_height = 22.0
    let screen_center_x = screen_width / 2.0
    let screen_center_y = screen_height / 2.0
    let char_space = ' '
    let char_line_feed = Convert.ToChar(10)
    let screen_dim = (screen_width * (screen_height + 1.0))
    let torus_midpoint_z = 10.0
    let theta_rotate_steps = 0.07
    let phi_rotate_steps = 0.02
    let inner_radius = 1.0
    let outer_radius = 2.0
    let two_pi = 2.0 * 3.14159265
    let midpoint_screen_projection_z = torus_midpoint_z / 2.0
    let mutable mut_a = 0.0
    let mutable mut_b = 0.0
    let mutable mut_phi = 0.0
    let mutable mut_theta = 0.0
    let dim_size = screen_dim + (screen_width) + 2.0
    let mutable mut_z_buff: float array = Array.zeroCreate (int dim_size) //vec![0.0; dim_size]
    let mutable mut_render_buff: char array = Array.zeroCreate (int dim_size) //vec![char_space; dim_size]
    printfn ("\x1b[2J")
    let mutable outerLoop = false
    let mutable innerLoop = false

    let rec recMyWork () =
        for buff_index in 0 .. (int screen_dim) do
            mut_render_buff.[buff_index] <- char_space
            mut_z_buff.[buff_index] <- 0.0

        let sin_a = Math.Sin mut_a
        let cos_a = Math.Cos mut_a
        let cos_b = Math.Cos mut_b
        let sin_b = Math.Sin mut_b
        outerLoop <- false

        while outerLoop = false do
            let cos_theta = Math.Cos mut_theta
            let sin_theta = Math.Sin mut_theta
            mut_phi <- 0.0
            innerLoop <- false

            while innerLoop = false do
                let sin_phi = Math.Sin mut_phi
                let cos_phi = Math.Cos mut_phi
                let h1 = cos_theta + 2.0

                let h2 =
                    cos_theta * (outer_radius + inner_radius)

                let h = h1
                let _t = sin_phi * h * cos_a - sin_theta * sin_a

                let _D =
                    1.0
                    / (sin_phi * h * sin_a + sin_theta * cos_a + 5.0)

                let local_z =
                    sin_phi * h * sin_a
                    + sin_theta * cos_a
                    + midpoint_screen_projection_z

                let one_over_z = 1.0 / local_z
                let t = sin_phi * h * cos_a - sin_theta * sin_a

                let _x =
                    (40.0
                     + 30.0 * _D * (cos_phi * h * cos_b - _t * sin_b))

                let projection_x =
                    screen_center_x
                    + 30.0
                      * one_over_z
                      * (cos_phi * h * cos_b - t * sin_b)

                let _y =
                    (12.0
                     + 15.0 * _D * (cos_phi * h * sin_b + _t * cos_b))

                let projection_y =
                    screen_center_y
                    + 15.0
                      * one_over_z
                      * (cos_phi * h * sin_b + t * cos_b)

                let pos_xy =
                    projection_x + (screen_width * projection_y)

                let us_pos_xy = int pos_xy

                let luminance =
                    (sin_theta * sin_a - sin_phi * cos_theta * cos_a)
                    * cos_b
                    - sin_phi * cos_theta * sin_a
                    - sin_theta * cos_a
                    - cos_phi * cos_theta * sin_b

                let luminance_index = 8.0 * luminance
                let depth = mut_z_buff.[us_pos_xy]
                let _o = int (_x + 80.0 * _y)

                if _o >= mut_render_buff.Length then
                    failwithf "Index for {%f},{%f} exceeds {%d}" _x _y mut_render_buff.Length

                let _N =
                    8.0
                    * ((sin_theta * sin_a - sin_phi * cos_theta * cos_a)
                       * cos_b
                       - sin_phi * cos_theta * sin_a
                       - sin_theta * cos_a
                       - cos_phi * cos_theta * sin_b)

                if 22.0 > _y
                   && _y > 0.0
                   && _x > 0.0
                   && 80.0 > _x
                   && _D > mut_z_buff.[_o] then
                    mut_z_buff.[_o] <- _D
                    let _bi = if _N > 0.0 then int _N else 0
                    let _pixel = pixels.[_bi]
                    mut_render_buff.[_o] <- _pixel

                mut_phi <- mut_phi + phi_rotate_steps

                if mut_phi > two_pi then
                    innerLoop <- true

            mut_theta <- mut_theta + theta_rotate_steps

            if mut_theta > two_pi then
                outerLoop <- true

        Console.Write(char_line_feed)
        printfn ("\x1b[H")

        for screen_index in 0 .. (int screen_dim + 1) do
            let ch =
                match screen_index with
                | i when (screen_index % (int screen_width)) <> 0 -> mut_render_buff.[screen_index]
                | _ -> char_line_feed

            Console.Write(ch)
            mut_a <- mut_a + 0.00004
            mut_b <- mut_b + 0.00002

        System.Threading.Thread.Sleep(300)

        recMyWork ()    // recurse
    recMyWork()

    0
