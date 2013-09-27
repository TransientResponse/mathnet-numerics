module ComplexLib
//open Microsoft.FSharp.Math

type Complex(real: float, imag: float) = 
    struct
        member this.Real with get() = real// and set(value) = _real <- value
        member this.Imag with get() = imag// and set(value) = _imag <- value
        member this.Mag = sqrt(this.Real**2.0 + this.Imag**2.0)
        member this.Phase = atan2 this.Imag this.Real
        member this.Conjugate with get() = Complex(this.Real, -this.Imag)
        override this.ToString() = sprintf "%.4f%+.4fj" real imag
    
        static member FromPolar(mag:float, phase:float) = Complex(mag * cos phase, mag * sin phase)

        static member One = Complex(1.0, 0.0)
        static member Zero = Complex(0.0, 0.0)
        static member I = Complex(0.0, 1.0)
    
        static member (+) (a:Complex, b:Complex) = Complex(a.Real + b.Real, a.Imag + b.Imag)
        static member (+) (a:Complex, b:float) = Complex(a.Real + b, a.Imag)
        static member (+) (a:float, b:Complex) = Complex(b.Real + a, b.Imag)
    
        static member (-) (a:Complex, b:Complex) = Complex(a.Real - b.Real, a.Imag - b.Imag)
        static member (-) (a:Complex, b:float) = Complex(a.Real - b, a.Imag)
        static member (-) (a:float, b:Complex) = Complex(b.Real - a, b.Imag)
    
        static member ( * ) (a:Complex, b:Complex) = Complex.FromPolar(a.Mag * b.Mag, a.Phase + b.Phase) //*)
        static member ( * ) (a:Complex, b:float) = Complex(a.Real * b, a.Imag * b)//*)
        static member ( * ) (a:float, b:Complex) = Complex(a * b.Real, a * b.Imag)//*)
    
        static member (/) (a:Complex, b:Complex) = Complex.FromPolar(a.Mag / b.Mag, a.Phase - b.Phase)
        static member (/) (a:Complex, b:float) = Complex(a.Real / b, a.Imag / b)
        //No longer depends on a function
        static member (/) (a:float, b:Complex) = a * Complex(b.Real, -b.Imag)/(b.Real**2.0 + b.Imag**2.0)
        //WHY NO USE SIGNAUTURE FILE???
        static member ( ** ) (a:Complex, b:Complex) = 
            let temp = Complex(log a.Mag, a.Phase) * b
            exp temp.Real * Complex(cos temp.Imag, sin temp.Imag)
        static member ( ** ) (a:Complex, b:float) = Complex.FromPolar(a.Mag ** b, a.Phase * b)
        //Changed to not depend on explicit functions
        static member ( ** ) (a:float, b:Complex) = 
            let temp = b * log a
            exp temp.Real * Complex(cos temp.Imag, sin temp.Imag)
        static member (~-) (a:Complex) = Complex(-a.Real, -a.Imag)
    end

///Quicker shortcut for mulIply by I.
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

let clog (arg:Complex) = Complex.FromPolar(log arg.Mag, arg.Phase)
let clog10 (arg:Complex) = Complex.FromPolar(log10 arg.Mag, arg.Phase)

let csqrt (arg:Complex) = Complex.FromPolar(sqrt arg.Mag, arg.Phase/2.0)
let square (arg:Complex) = Complex(arg.Real**2.0 - arg.Imag**2.0, 2.0*arg.Real*arg.Imag)

let catan (arg:Complex) = clog((mulI arg + Complex.One)/(Complex.One - mulI arg))/Complex(0.0, 2.0)
let cacot (arg:Complex) = clog((arg + Complex.I)/(arg - Complex.I))/Complex(0.0,2.0)
let casin (arg:Complex) = 
    let temp = Complex.One - square arg
    clog(arg*Complex.I + csqrt temp)/Complex.I
let cacos (arg:Complex) = 
    let temp = Complex.One - square arg
    divI(clog(arg + mulI (csqrt temp)))