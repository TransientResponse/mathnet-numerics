module ComplexLib
//open Microsoft.FSharp.Math

type Complex(real:float, imag:float) = 
    struct
        [<DefaultValue>] static val mutable private _symbol: char
        new(real) = Complex(real, 0.0)
        //Be super-fancy
        new(arg:string) = 
            if not (arg.EndsWith("i") || arg.EndsWith("j") || arg.EndsWith("I") || arg.EndsWith("J")) then failwith "Invalid complex number string"
            let nums = System.Text.RegularExpressions.Regex.Split(arg.Substring(0, arg.Length-1), @"\s*(\+|-)\s*")
            if nums.Length <> 2 then failwith "Invalid complex number string" //<> is so weird
            Complex(System.Double.Parse nums.[0], System.Double.Parse nums.[1])
        static do Complex._symbol <- 'i'
        
        member this.Real with get() = real// and set(value) = _real <- value
        member this.Imag with get() = imag// and set(value) = _imag <- value
        member this.Mag = sqrt(this.Real**2.0 + this.Imag**2.0)
        member this.Phase = atan2 this.Imag this.Real
        member this.Conjugate with get() = Complex(this.Real, -this.Imag)
        static member ImaginarySymbol with get() = Complex._symbol and set(value) = Complex._symbol <- value
        override this.ToString() = sprintf "%.4f%+.4f%c" real imag Complex._symbol
    
        static member FromPolar(mag:float, phase:float) = Complex(mag * cos phase, mag * sin phase)

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
        //The compiler doesn't seem to understand.
//        static member Pow (a:Complex, b:Complex) = a**b
//        static member Pow (a:Complex, b:float) = a ** b
//        static member Pow (a:float, b:Complex) = //a**b doesn't work for this one
//            let temp = b * log a                 //for no apparent reason
//            exp temp.Real * Complex(cos temp.Imag, sin temp.Imag)
        static member (~-) (a:Complex) = Complex(-a.Real, -a.Imag)
    end

///Quicker shortcut for multiply by I.
let mulI (arg:Complex) = Complex(-arg.Imag, arg.Real)
let divI (arg:Complex) = Complex(arg.Imag, -arg.Real)
    
let csin (arg:Complex): Complex = Complex(sin arg.Real * cosh arg.Imag, cos arg.Real * sinh arg.Imag)
let ccos (arg:Complex) = Complex(cos arg.Real * cosh arg.Imag, -(sin arg.Real) * (sinh arg.Imag))
let ctan (arg:Complex) = csin arg / ccos arg
let csinh (arg:Complex) = Complex(sinh arg.Real * cos arg.Imag, cosh arg.Real * sin arg.Imag)
let ccosh (arg:Complex) = Complex(cosh arg.Real * cos arg.Imag, sinh arg.Real * sin arg.Imag)
let ctanh (arg:Complex) = csinh arg / ccosh arg
let conjugate (arg:Complex) = Complex(arg.Real, -arg.Imag)
let sqnorm (arg:Complex) = arg.Real**2.0 + arg.Imag**2.0
let inverse (arg:Complex) = conjugate arg / sqnorm arg
let cexp (arg:Complex) = exp arg.Real * Complex(cos arg.Imag, sin arg.Imag)
let phasor (mag:float) (phase:float) = Complex.FromPolar(mag, phase)
let magphase (arg:Complex) = (arg.Mag, arg.Phase)

let clog (arg:Complex) = Complex(log arg.Mag, arg.Phase)
let clog10 (arg:Complex) = Complex(log10 arg.Mag, arg.Phase)

let csqrt (arg:Complex) = Complex.FromPolar(sqrt arg.Mag, arg.Phase/2.0)
let square (arg:Complex) = Complex(arg.Real**2.0 - arg.Imag**2.0, 2.0*arg.Real*arg.Imag)

let catan (arg:Complex) = clog((mulI arg + 1.0)/(1.0 - mulI arg))/Complex(0.0, 2.0)
let cacot (arg:Complex) = clog((arg + Complex.I)/(arg - Complex.I))/Complex(0.0,2.0)
let casin (arg:Complex) = 
    let temp = 1.0 - square arg
    divI (clog(mulI arg + csqrt temp))
let cacos (arg:Complex) = 
    let temp = 1.0 - square arg
    divI(clog(arg + mulI (csqrt temp)))

let catanh (arg:Complex) = clog((arg + 1.0)/(1.0 - arg))/2.0
let cacoth (arg:Complex) = clog((arg + 1.0)/(arg - 1.0))/2.0
let casinh (arg:Complex) = clog(arg + csqrt(1.0 + square(arg)))
let cacosh (arg:Complex) = clog(arg - csqrt(square(arg) - 1.0))

//Complex gamma function, using Gergő Nemes' approximation (based on the Stirling approximation)
let cgamma (z:Complex) = 
    let premult = csqrt(2.0*System.Math.PI/z)
    let inner = 1.0/(12.0*z - 1.0/(10.0*z))
    let inve = 0.3678794411714423215955237701615 //Don't rely on double-precision arithmetic
    premult * (inve * (z + inner))**z

let clngamma (z:Complex) = //Same approximation as before
    let inner = 1.0/(12.0*z - 1.0/(10.0*z))
    let ln2pi = 1.83787706640934548356065947281123527972
    (ln2pi - clog z)/2.0 + z*(clog inner - 1.0)

//Be SUPER FANCY
module NumericLiteralI = 
    let inline FromZero() = Complex.Zero
    let inline FromeOne() = Complex.I
    let inline FromInt32(n:int) = Complex(0.0, float n)
    let inline FromInt64(n:int64) = Complex(0.0, float n)
    let inline FromString(arg:string) = 
        let num = arg.Substring(0, arg.Length-1)
        Complex(0.0, float num)