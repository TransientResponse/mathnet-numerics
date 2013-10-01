module ComplexLib
type Complex =
  struct
    new : real:float -> Complex
    new : arg:string -> Complex
    new : real:float * imag:float -> Complex
    member DivI : unit -> Complex
    member MulI : unit -> Complex
    member Square : unit -> Complex
    override ToString : unit -> string
    member Conjugate : Complex
    member Imag : float
    member Mag : float
    member Phase : float
    member Real : float
    static val mutable private _symbol: char
    static member Acosh : arg:Complex -> Complex
    static member Acot : arg:Complex -> Complex
    static member Acoth : arg:Complex -> Complex
    static member Asin : arg:Complex -> Complex
    static member Asinh : arg:Complex -> Complex
    static member Atan : arg:Complex -> Complex
    static member Atanh : arg:Complex -> Complex
    static member Cos : arg:Complex -> Complex
    static member Cosh : arg:Complex -> Complex
    static member Exp : arg:Complex -> Complex
    static member FromPolar : mag:float * phase:float -> Complex
    static member Log : arg:Complex -> Complex
    static member Log10 : arg:Complex -> Complex
    static member Pow : a:Complex * b:Complex -> Complex
    static member Pow : a:Complex * b:float -> Complex
    static member Pow : a:float * b:Complex -> Complex
    static member Sin : arg:Complex -> Complex
    static member Sinh : arg:Complex -> Complex
    static member Sqrt : arg:Complex -> Complex
    static member Tan : arg:Complex -> Complex
    static member Tanh : arg:Complex -> Complex
    static member cacos : arg:Complex -> Complex
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
type Complex32 =
  struct
    new : real:float32 -> Complex32
    new : arg:string -> Complex32
    new : real:float32 * imag:float32 -> Complex32
    member DivI : unit -> Complex32
    member MulI : unit -> Complex32
    member Square : unit -> Complex32
    override ToString : unit -> string
    member Conjugate : Complex32
    member Imag : float32
    member Mag : float32
    member Phase : float32
    member Real : float32
    static val mutable private _symbol: char
    static member Acos : arg:Complex32 -> Complex32
    static member Acosh : arg:Complex32 -> Complex32
    static member Acot : arg:Complex32 -> Complex32
    static member Acoth : arg:Complex32 -> Complex32
    static member Asin : arg:Complex32 -> Complex32
    static member Asinh : arg:Complex32 -> Complex32
    static member Atan : arg:Complex32 -> Complex32
    static member Atanh : arg:Complex32 -> Complex32
    static member Cos : arg:Complex32 -> Complex32
    static member Cosh : arg:Complex32 -> Complex32
    static member Exp : arg:Complex32 -> Complex32
    static member FromPolar : mag:float32 * phase:float32 -> Complex32
    static member Log : arg:Complex32 -> Complex32
    static member Log10 : arg:Complex32 -> Complex32
    static member Pow : a:Complex32 * b:Complex32 -> Complex32
    static member Pow : a:Complex32 * b:float32 -> Complex32
    static member Pow : a:float32 * b:Complex32 -> Complex32
    static member Sin : arg:Complex32 -> Complex32
    static member Sinh : arg:Complex32 -> Complex32
    static member Sqrt : arg:Complex32 -> Complex32
    static member Tan : arg:Complex32 -> Complex32
    static member Tanh : arg:Complex32 -> Complex32
    static member I : Complex32
    static member ImaginarySymbol : char
    static member One : Complex32
    static member Zero : Complex32
    static member ( + ) : a:Complex32 * b:Complex32 -> Complex32
    static member ( + ) : a:Complex32 * b:float32 -> Complex32
    static member ( + ) : a:float32 * b:Complex32 -> Complex32
    static member ( / ) : a:Complex32 * b:Complex32 -> Complex32
    static member ( / ) : a:Complex32 * b:float32 -> Complex32
    static member ( / ) : a:float32 * b:Complex32 -> Complex32
    static member ( * ) : a:Complex32 * b:Complex32 -> Complex32
    static member ( * ) : a:Complex32 * b:float32 -> Complex32
    static member ( * ) : a:float32 * b:Complex32 -> Complex32
    static member ( - ) : a:Complex32 * b:Complex32 -> Complex32
    static member ( - ) : a:Complex32 * b:float32 -> Complex32
    static member ( - ) : a:float32 * b:Complex32 -> Complex32
    static member ( ~- ) : a:Complex32 -> Complex32
    static member ImaginarySymbol : char with set
  end
val conjugate : arg:Complex -> Complex
val sqnorm : arg:Complex -> float
val inverse : arg:Complex -> Complex
val cexp : arg:Complex -> Complex
val phasor : mag:float -> phase:float -> Complex
val magphase : arg:Complex -> float * float
val square : arg:Complex -> Complex
val gamma : z:Complex -> Complex
val lngamma : z:Complex -> Complex
module NumericLiteralI = begin
  val inline FromZero : unit -> Complex
  val inline FromeOne : unit -> Complex
  val inline FromInt32 : n:int -> Complex
  val inline FromInt64 : n:int64 -> Complex
  val inline FromString : arg:string -> Complex
end

