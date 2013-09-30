module ComplexLib
type Complex =
  struct
    new : real:float -> Complex
    new : real:float * imag:float -> Complex
    val mutable private _real: float
    val mutable private _imag: float
    override ToString : unit -> string
    member Conjugate : Complex
    member Imag : float
    member Mag : float
    member Phase : float
    member Real : float
    static val mutable private _symbol: char
    static member FromPolar : mag:float * phase:float -> Complex
    static member Pow : a:Complex * b:Complex -> Complex
    static member Pow : a:Complex * b:float -> Complex
    static member Pow : a:float * b:Complex -> Complex
    static member I : Complex
    static member ImaginarySymbol : char
    static member One : Complex
    static member Zero : Complex
    static member ( + ) : a:Complex * b:Complex -> Complex
    static member ( + ) : a:Complex * b:float -> Complex
    static member ( + ) : a:float * b:Complex -> Complex
    static member ( / ) : a:Complex * b:Complex -> Complex
    static member ( / ) : a:Complex * b:float -> Complex
    static member ( / ) : a:float * b:Complex -> Complex
    static member ( * ) : a:Complex * b:Complex -> Complex
    static member ( * ) : a:Complex * b:float -> Complex
    static member ( * ) : a:float * b:Complex -> Complex
    static member ( - ) : a:Complex * b:Complex -> Complex
    static member ( - ) : a:Complex * b:float -> Complex
    static member ( - ) : a:float * b:Complex -> Complex
    static member ( ~- ) : a:Complex -> Complex
    static member ImaginarySymbol : char with set
  end
val mulI : arg:Complex -> Complex
val divI : arg:Complex -> Complex
val csin : arg:Complex -> Complex
val ccos : arg:Complex -> Complex
val ctan : arg:Complex -> Complex
val csinh : arg:Complex -> Complex
val ccosh : arg:Complex -> Complex
val ctanh : arg:Complex -> Complex
val conjugate : arg:Complex -> Complex
val sqnorm : arg:Complex -> float
val inverse : arg:Complex -> Complex
val cexp : arg:Complex -> Complex
val phasor : mag:float -> phase:float -> Complex
val magphase : arg:Complex -> float * float
val clog : arg:Complex -> Complex
val clog10 : arg:Complex -> Complex
val csqrt : arg:Complex -> Complex
val square : arg:Complex -> Complex
val catan : arg:Complex -> Complex
val cacot : arg:Complex -> Complex
val casin : arg:Complex -> Complex
val cacos : arg:Complex -> Complex
val catanh : arg:Complex -> Complex
val cacoth : arg:Complex -> Complex
val casinh : arg:Complex -> Complex
val cacosh : arg:Complex -> Complex
val cgamma : z:Complex -> Complex
val clngamma : z:Complex -> Complex

