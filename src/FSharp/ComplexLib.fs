module ComplexLib
//open Microsoft.FSharp.Math
open System
type Complex(real:float, imag:float) = 
    struct
        [<DefaultValue>] static val mutable private _symbol: char
        new(real) = Complex(real, 0.0)
        //Be super-fancy
        new(arg:string) = 
            if not (arg.EndsWith("i") || arg.EndsWith("j") || arg.EndsWith("I") || arg.EndsWith("J")) then failwith "Invalid complex number string"
            let nums = System.Text.RegularExpressions.Regex.Split(arg.Substring(0, arg.Length-1), @"\s*(\+|-)\s*")
            if nums.Length <> 3 then failwith "Invalid complex number string" //<> is so weird
            Complex(System.Double.Parse nums.[0], System.Double.Parse nums.[2])
        static do Complex._symbol <- 'i'
        
        member this.Real with get() = real// and set(value) = _real <- value
        member this.Imag with get() = imag// and set(value) = _imag <- value
        member this.Mag = sqrt(this.Real**2.0 + this.Imag**2.0)
        member this.Phase = atan2 this.Imag this.Real
        member this.Conjugate with get() = Complex(this.Real, -this.Imag)
        static member ImaginarySymbol with get() = Complex._symbol and set(value) = Complex._symbol <- value
        override this.ToString() = sprintf "%.4f%+.4f%c" real imag Complex._symbol
    
        static member FromPolar(mag:float, phase:float) = Complex(mag * cos phase, mag * sin phase)

        member this.MulI() = Complex(-this.Imag, this.Real)
        member this.DivI() = Complex(this.Imag, -this.Real)
        member this.Square() = Complex(this.Real**2.0 - this.Imag**2.0, 2.0*this.Real*this.Imag)

        static member One = Complex(1.0, 0.0)
        static member Zero = Complex(0.0, 0.0)
        static member I = Complex(0.0, 1.0)
    
        static member (+) (a:Complex, b:Complex) = Complex(a.Real + b.Real, a.Imag + b.Imag)
        static member (+) (a:Complex, b:float) = Complex(a.Real + b, a.Imag)
        static member (+) (a:float, b:Complex) = Complex(b.Real + a, b.Imag)
    
        static member (-) (a:Complex, b:Complex) = Complex(a.Real - b.Real, a.Imag - b.Imag)
        static member (-) (a:Complex, b:float) = Complex(a.Real - b, a.Imag)
        static member (-) (a:float, b:Complex) = -b + a
    
        static member ( * ) (a:Complex, b:Complex) = Complex.FromPolar(a.Mag * b.Mag, a.Phase + b.Phase) //*)
        static member ( * ) (a:Complex, b:float) = Complex(a.Real * b, a.Imag * b)//*)
        static member ( * ) (a:float, b:Complex) = Complex(a * b.Real, a * b.Imag)//*)
    
        static member (/) (a:Complex, b:Complex) = Complex.FromPolar(a.Mag / b.Mag, a.Phase - b.Phase)
        static member (/) (a:Complex, b:float) = Complex(a.Real / b, a.Imag / b)
        //No longer depends on a function
        static member (/) (a:float, b:Complex) = a * Complex(b.Real, -b.Imag)/(b.Real**2.0 + b.Imag**2.0)
        //WHY NO USE SIGNAUTURE FILE???
        static member Pow (a:Complex, b:Complex) = 
            let temp = Complex(log a.Mag, a.Phase) * b
            exp temp.Real * Complex(cos temp.Imag, sin temp.Imag)
        static member Pow (a:Complex, b:float) = Complex.FromPolar(a.Mag ** b, a.Phase * b)
        //Changed to not depend on explicit functions
        static member Pow (a:float, b:Complex) = 
            let temp = b * log a
            exp temp.Real * Complex(cos temp.Imag, sin temp.Imag)
        static member Sqrt (arg:Complex) = Complex(sqrt arg.Mag, arg.Phase/2.0)
        static member Sin (arg:Complex) = Complex(sin arg.Real * cosh arg.Imag, cos arg.Real * sinh arg.Imag)
        static member Cos (arg:Complex) = Complex(cos arg.Real * cosh arg.Imag, -(sin arg.Real) * (sinh arg.Imag))
        static member Tan (arg:Complex) = cos arg / sin arg
        static member Sinh (arg:Complex) = Complex(sinh arg.Real * cos arg.Imag, cosh arg.Real * sin arg.Imag)
        static member Cosh (arg:Complex) = Complex(cosh arg.Real * cos arg.Imag, sinh arg.Real * sin arg.Imag)
        static member Tanh (arg:Complex) = sinh arg / cosh arg
        static member Exp (arg:Complex) = exp arg.Real * Complex(cos arg.Imag, sin arg.Imag)
        static member Log (arg:Complex) = Complex(log arg.Mag, arg.Phase)
        static member Log10 (arg:Complex) = Complex(log10 arg.Mag, arg.Phase)

        static member Atan (arg:Complex) = log((arg.MulI() + 1.0)/(1.0 - arg.MulI()))/Complex(0.0, 2.0)
        static member Acot (arg:Complex) = log((arg + Complex.I)/(arg - Complex.I))/Complex(0.0,2.0)
        static member Asin (arg:Complex) = 
            let temp = 1.0 - arg.Square()
            (log(arg.MulI() + sqrt temp)).DivI()
        static member cacos (arg:Complex) = 
            let temp = 1.0 - arg.Square()
            (log(arg + (sqrt temp)*Complex.I)).DivI()

        static member Atanh (arg:Complex) = log((arg + 1.0)/(1.0 - arg))/2.0
        static member Acoth (arg:Complex) = log((arg + 1.0)/(arg - 1.0))/2.0
        static member Asinh (arg:Complex) = log(arg + sqrt(1.0 + arg.Square()))
        static member Acosh (arg:Complex) = log(arg - sqrt(arg.Square() - 1.0))

        static member (~-) (a:Complex) = Complex(-a.Real, -a.Imag)
    end

type Complex32(real:float32, imag:float32) = 
    struct
        [<DefaultValue>] static val mutable private _symbol: char
        new(real) = Complex32(real, 0.0f)
        //Be super-fancy
        new(arg:string) = 
            if not (arg.EndsWith("i") || arg.EndsWith("j") || arg.EndsWith("I") || arg.EndsWith("J")) then failwith "Invalid complex number string"
            let nums = System.Text.RegularExpressions.Regex.Split(arg.Substring(0, arg.Length-1), @"\s*(\+|-)\s*")
            if nums.Length <> 2 then failwith "Invalid complex number string" //<> is so weird
            Complex32(System.Single.Parse nums.[0], System.Single.Parse nums.[1])
        static do Complex32._symbol <- 'i'
        
        member this.Real with get() = real// and set(value) = _real <- value
        member this.Imag with get() = imag// and set(value) = _imag <- value
        member this.Mag = sqrt(this.Real**2.0f + this.Imag**2.0f)
        member this.Phase = atan2 this.Imag this.Real
        member this.Conjugate with get() = Complex32(this.Real, -this.Imag)
        static member ImaginarySymbol with get() = Complex32._symbol and set(value) = Complex32._symbol <- value
        override this.ToString() = sprintf "%.4f%+.4f%c" real imag Complex32._symbol
    
        static member FromPolar(mag:float32, phase:float32) = Complex32(mag * cos phase, mag * sin phase)

        member this.MulI() = Complex32(-this.Imag, this.Real)
        member this.DivI() = Complex32(this.Imag, -this.Real)
        member this.Square() = Complex32(this.Real**2.0f - this.Imag**2.0f, 2.0f*this.Real*this.Imag)

        static member One = Complex32(1.0f, 0.0f)
        static member Zero = Complex32(0.0f, 0.0f)
        static member I = Complex32(0.0f, 1.0f)
    
        static member (+) (a:Complex32, b:Complex32) = Complex32(a.Real + b.Real, a.Imag + b.Imag)
        static member (+) (a:Complex32, b:float32) = Complex32(a.Real + b, a.Imag)
        static member (+) (a:float32, b:Complex32) = Complex32(b.Real + a, b.Imag)
    
        static member (-) (a:Complex32, b:Complex32) = Complex32(a.Real - b.Real, a.Imag - b.Imag)
        static member (-) (a:Complex32, b:float32) = Complex32(a.Real - b, a.Imag)
        static member (-) (a:float32, b:Complex32) = -b + a
    
        static member ( * ) (a:Complex32, b:Complex32) = Complex32.FromPolar(a.Mag * b.Mag, a.Phase + b.Phase) //*)
        static member ( * ) (a:Complex32, b:float32) = Complex32(a.Real * b, a.Imag * b)//*)
        static member ( * ) (a:float32, b:Complex32) = Complex32(a * b.Real, a * b.Imag)//*)
    
        static member (/) (a:Complex32, b:Complex32) = Complex32.FromPolar(a.Mag / b.Mag, a.Phase - b.Phase)
        static member (/) (a:Complex32, b:float32) = Complex32(a.Real / b, a.Imag / b)
        //No longer depends on a function
        static member (/) (a:float32, b:Complex32) = a * Complex32(b.Real, -b.Imag)/(b.Real**2.0f + b.Imag**2.0f)
        //WHY NO USE SIGNAUTURE FILE???
        static member Pow (a:Complex32, b:Complex32) = 
            let temp = Complex32(log a.Mag, a.Phase) * b
            exp temp.Real * Complex32(cos temp.Imag, sin temp.Imag)
        static member Pow (a:Complex32, b:float32) = Complex32.FromPolar(a.Mag ** b, a.Phase * b)
        //Changed to not depend on explicit functions
        static member Pow (a:float32, b:Complex32) = 
            let temp = b * log a
            exp temp.Real * Complex32(cos temp.Imag, sin temp.Imag)
        static member Sqrt (arg: Complex32) = Complex32(sqrt arg.Mag, arg.Phase/2.0f)
        //Trig operators, for the built-in operators sin, cos, etc. to use.
        static member Cos (arg:Complex32) = Complex32(sin arg.Real * cosh arg.Imag, cos arg.Real * sinh arg.Imag)
        static member Sin (arg:Complex32) = Complex32(sin arg.Real * cosh arg.Imag, cos arg.Real * sinh arg.Imag)
        static member Tan (arg:Complex32) = sin arg / cos arg
        static member Sinh (arg:Complex32) = Complex32(sinh arg.Real * cos arg.Imag, cosh arg.Real * sin arg.Imag)
        static member Cosh (arg:Complex32) = Complex32(cosh arg.Real * cos arg.Imag, sinh arg.Real * sin arg.Imag)
        static member Tanh (arg:Complex32) = sin arg / cos arg
        static member Exp (arg:Complex32) = exp arg.Real * Complex32(cos arg.Imag, sin arg.Imag)
        static member Log (arg:Complex32) = Complex32(log arg.Mag, arg.Phase)
        static member Log10 (arg:Complex32) = Complex32(log10 arg.Mag, arg.Phase)

        static member Atan (arg:Complex32) = log((arg.MulI() + 1.0f)/(1.0f - arg.MulI()))/Complex32(0.0f, 2.0f)
        static member Acot (arg:Complex32) = log((arg + Complex32.I)/(arg - Complex32.I))/Complex32(0.0f,2.0f)
        static member Asin (arg:Complex32) = 
            let temp = 1.0f - arg.Square()
            (log(arg.MulI() + sqrt temp)).DivI()
        static member Acos (arg:Complex32) = 
            let temp = 1.0f - arg.Square()
            (log(arg + (sqrt temp)*Complex32.I)).DivI()

        static member Atanh (arg:Complex32) = log((arg + 1.0f)/(1.0f - arg))/2.0f
        static member Acoth (arg:Complex32) = log((arg + 1.0f)/(arg - 1.0f))/2.0f
        static member Asinh (arg:Complex32) = log(arg + sqrt(1.0f + arg.Square()))
        static member Acosh (arg:Complex32) = log(arg - sqrt(arg.Square() - 1.0f))

        static member (~-) (a:Complex32) = Complex32(-a.Real, -a.Imag)
    end


let conjugate (arg:Complex) = Complex(arg.Real, -arg.Imag)
let sqnorm (arg:Complex) = arg.Real**2.0 + arg.Imag**2.0
let inverse (arg:Complex) = conjugate arg / sqnorm arg
let phasor (mag:float) (phase:float) = Complex.FromPolar(mag, phase)
let magphase (arg:Complex) = (arg.Mag, arg.Phase)

let square (arg:Complex) = Complex(arg.Real**2.0 - arg.Imag**2.0, 2.0*arg.Real*arg.Imag)



//Complex gamma function, using the Lanczos approximation
let rec gamma (z:Complex) = 
    let g = 7
    let p = [0.99999999999980993; 676.5203681218851; -1259.1392167224028;
            771.32342877765313; -176.61502916214059; 12.507343278686905;
            -0.13857109526572012; 9.9843695780195716e-6; 1.5056327351493116e-7]
    if z.Real < 0.5 then
        Math.PI / (sin(Math.PI*z) + gamma(1.0-z))
    else
        let z2 = z - 1.0
        let x = List.mapi (fun i elem -> elem/(z2 + float i)) p |> List.sum
        let t = z2 + float g + 0.5
        sqrt(2.0*Math.PI) * t**(z2+0.5) * exp(-t) * x
//end gamma

let lngamma (z:Complex) = //Same approximation as before
    let inner = 1.0/(12.0*z - 1.0/(10.0*z))
    let ln2pi = 1.83787706640934548356065947281123527972
    (ln2pi - log z)/2.0 + z*(log inner - 1.0)

//Be SUPER FANCY
module NumericLiteralI = 
    let inline FromZero() = Complex.Zero
    let inline FromeOne() = Complex.I
    let inline FromInt32(n:int) = Complex(0.0, float n)
    let inline FromInt64(n:int64) = Complex(0.0, float n)
    let inline FromString(arg:string) = 
        let num = arg.Substring(0, arg.Length-1)
        Complex(0.0, float num)