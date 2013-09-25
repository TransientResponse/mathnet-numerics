module ComplexLib
type Complex =
  struct
    new : real:float * imag:float -> Complex
    override ToString : unit -> string
    member Conjugate : Complex
    member Imag : float
    member Mag : float
    member Phase : float
    member Real : float
    static member FromPolar : mag:float * phase:float -> Complex
    static member ( + ) : a:Complex * b:Complex -> Complex
    static member ( + ) : a:Complex * b:float -> Complex
    static member ( + ) : a:float * b:Complex -> Complex
    static member ( / ) : a:Complex * b:Complex -> Complex
    static member ( / ) : a:Complex * b:float -> Complex
    static member ( / ) : a:float * b:Complex -> Complex
    static member ( ** ) : a:Complex * b:Complex -> Complex
    static member ( ** ) : a:Complex * b:float -> Complex
    static member ( ** ) : a:float * b:Complex -> Complex
    static member ( * ) : a:Complex * b:Complex -> Complex
    static member ( * ) : a:Complex * b:float -> Complex
    static member ( * ) : a:float * b:Complex -> Complex
    static member ( - ) : a:Complex * b:Complex -> Complex
    static member ( - ) : a:Complex * b:float -> Complex
    static member ( - ) : a:float * b:Complex -> Complex
  end
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

