{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  Math functions                                                                                  }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing        }
{  rights and limitations under the License.                                                       }
{                                                                                                  }
{  The Original Code is: YclMath.pas.                                                              }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2003-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// For history see end of file

{$I Ycl.inc}

{$IFDEF CPUI386}
  {$DEFINE USEFPUx87}
{$ENDIF CPUI386}

unit YclMath;

interface
uses
  YclBase;

// Trigonometric constants
const
                                                                        //                80x87 FPU
  MathE        = 2.71828182845904523536028747135266249775724709369995;  // e
  MathLog2E    = 1.4426950408889634073599246810018921;                  // Log2(e)         FLDL2E
  MathLog210   = 3.32192809488736;                                      // Log2(10)        FLDL2T
  MathLog10E   = 0.4342944819032518276511289189166051;                  // Log10(e)
  MathLog102   = 0.301029995663981;                                     // Log10(2)        FLDLG2
  MathLn2      = 0.6931471805599453094172321214581766;                  // Ln(2)           FLDLN2
  MathLn10     = 2.3025850929940456840179914546843642;                  // Ln(10)

  MathPi       = 3.1415926535897932384626433832795028841971693993751;   // Pi
  MathPi_2     = 1.5707963267948966192313216916397514420985846996876;   // Pi/2
  MathPi_4     = 0.7853981633974483096156608458198757210492923498438;   // Pi/4
  Math1_Pi     = 0.3183098861837906715377675267450287;                  // 1/Pi
  Math2_Pi     = 0.6366197723675813430755350534900574;                  // 2/Pi

  Math1_SqrtPi = 0.5641895835477562869480794515607726;                  // 1/Sqrt(Pi)
  Math2_SqrtPi = 1.1283791670955125738961589031215452;                  // 2/Sqrt(Pi)

  MathSqrt2    = 1.41421356237309504880168872420969807856967187537694;  // Sqrt(2)
  MathSqrt3    = 1.7320508075688772935274463415059;                     // Sqrt(3)
  MathSqrt1_2  = 0.7071067811865475244008443621048490;                  // Sqrt(1/2) = 1/Sqrt(2)

  MathGamma    = 0.57721566490153286060651209008240243104215933593992;  // Euler constant
  MathZeta3    = 1.20205690315959428539973816151144999076498629234049;  // Apery's constant

  // LibC Names
  M_E        = MathE;
  M_LOG2E    = MathLog2E;
  M_LOG10E   = MathLog10E;
  M_LN2      = MathLn2;
  M_LN10     = MathLn10;

  M_PI       = MathPi;
  M_PI_2     = MathPi_2;
  M_PI_4     = MathPi_4;
  M_1_PI     = Math1_Pi;
  M_2_PI     = Math2_Pi;

  M_1_SQRTPI = Math1_SqrtPi;
  M_2_SQRTPI = Math2_SqrtPi;

  M_SQRT2    = MathSqrt2;
  M_SQRT1_2  = MathSqrt1_2;

const
  Sin00  = 0.0;
  Sin30  = 0.5;                                // Sin(30) = Sqrt(1 / 4)
  Sin45  = MathSqrt2 * 0.5;                    // Sin(45) = Sqrt(2 / 4)
  Sin60  = MathSqrt3 * 0.5;                    // Sin(60) = Sqrt(3 / 4)
  Sin90  = 1.0;                                // Sin(90) = Sqrt(4 / 4)
  Cos00 = Sin90;
  Cos30 = Sin60;
  Cos45 = Sin45;
  Cos60 = Sin30;
  Cos90 = Sin00;

  Sin000 =  Sin00;   //  0.0
  Sin030 =  Sin30;   //  0.5
  Sin045 =  Sin45;   //  0.707..
  Sin060 =  Sin60;   //  0.866..
  Sin090 =  Sin90;   //  1.0
  Sin120 =  Sin60;   //  0.866..
  Sin135 =  Sin45;   //  0.707..
  Sin150 =  Sin30;   //  0.5
  Sin180 =  Sin00;   //  0.0
  Sin210 = -Sin30;   // -0.5
  Sin225 = -Sin45;   // -0.707..
  Sin240 = -Sin60;   // -0.866..
  Sin270 = -Sin90;   // -1.0
  Sin300 = -Sin60;   // -0.866..
  Sin315 = -Sin45;   // -0.707..
  Sin330 = -Sin30;   // -0.5
  Sin360 =  Sin00;   //  0.0

  Cos000 =  Sin90;   //  1.0
  Cos030 =  Sin60;   //  0.866..
  Cos045 =  Sin45;   //  0.707..
  Cos060 =  Sin30;   //  0.5
  Cos090 =  Sin00;   //  0.0
  Cos120 = -Sin30;   // -0.5
  Cos135 = -Sin45;   // -0.707..
  Cos150 = -Sin60;   // -0.866..
  Cos180 = -Sin90;   // -1.0
  Cos210 = -Sin60;   // -0.866..
  Cos225 = -Sin45;   // -0.707..
  Cos240 = -Sin30;   // -0.5
  Cos270 =  Sin00;   //  0.0
  Cos300 =  Sin30;   //  0.5
  Cos315 =  Sin45;   //  0.707..
  Cos330 =  Sin60;   //  0.866..
  Cos360 =  Sin90;   //  1.0

// Konstanten für Winkel in Bogenmaß
const
  Rad000 =   0.0 * Pi / 180;
  Rad045 =  45.0 * Pi / 180;
  Rad090 =  90.0 * Pi / 180;
  Rad135 = 135.0 * Pi / 180;
  Rad180 = 180.0 * Pi / 180;
  Rad225 = 225.0 * Pi / 180;
  Rad270 = 270.0 * Pi / 180;
  Rad315 = 315.0 * Pi / 180;
  Rad360 = 360.0 * Pi / 180;


// Classification functions

{ TODO : implementieren }
{
// ****************  classify real floating type
// Classify given number
function FPClassify(Value: Single): Integer;
function FPClassify(Value: Double): Integer;
function FPClassify(Value: Extended): Integer;
}

// Test for finite value
// Return True if Value is not +-Inf or NaN
function IsFinite(const Value: Single): Boolean; overload;
function IsFinite(const Value: Double): Boolean; overload;
function IsFinite(const Value: Extended): Boolean; overload;

// Test for infinity
// +Inf: +1
// -Inf: -1
// Finite or NaN: 0
function IsInf(const Value: Single): Integer; overload;
function IsInf(const Value: Double): Integer; overload;
function IsInf(const Value: Extended): Integer; overload;

// Test for NaN
// Return True, if Value is NaN
function IsNaN(const Value: Single): Boolean; overload;
function IsNaN(const Value: Double): Boolean; overload;
function IsNaN(const Value: Extended): Boolean; overload;

const
  NaN           =  0/0;
  PlusInfinity  =  1/0;
  MinusInfinity = -1/0;

function SingleMaxNormalValue: Single;
function SingleMinNormalValue: Single;
function SingleMaxSubnormalValue: Single;
function SingleMinSubnormalValue: Single;

function DoubleMaxNormalValue: Double;
function DoubleMinNormalValue: Double;
function DoubleMaxSubnormalValue: Double;
function DoubleMinSubnormalValue: Double;

function DoubleExtendedMaxNormalValue: Extended;
function DoubleExtendedMinNormalValue: Extended;
function DoubleExtendedMaxSubnormalValue: Extended;
function DoubleExtendedMinSubnormalValue: Extended;


// ********************************  Trigonometric functions  ********************************

// ****************  sine function
// in: |Value| <= 2^63 radians 
{ unit System
function Sin(const x: Single): Single; overload;
function Sin(const x: Double): Double; overload;
function Sin(const x: Extended): Extended; overload;
}

// ****************  cosine function
// in: |Value| <= 2^63 radians
{ unit System
function Cos(const x: Single): Single; overload;
function Cos(const x: Double): Double; overload;
function Cos(const x: Extended): Extended; overload;
}

// ****************  sine and cosine function
// SinCos is faster than executing the Sin and Cos functions in succession
// in: |Value| <= 2^63 radians
procedure SinCos(const x: Single; out Sin, Cos: Single); overload;
procedure SinCos(const x: Double; out Sin, Cos: Double); overload;
procedure SinCos(const x: Extended; out Sin, Cos: Extended); overload;

// ****************  tangent function
// in: |Value| <= 2^63 radians
// Single and Double have similar cpu cycles
function Tan(const x: Single): Single; overload;
function Tan(const x: Double): Double; overload;
function Tan(const x: Extended): Extended; overload;

// ****************  cotangent function
// in: 0 < |Value| <= 2^63 radians
function Cotan(const x: Single): Single; overload;
function Cotan(const x: Double): Double; overload;
function Cotan(const x: Extended): Extended; overload;

// Inverse trigonometric functions

// ****************  arc sine function
// in: |Value| <= 1  out: [-PI/2..PI/2] radians
function ASin(const x: Single): Single; overload;
function ASin(const x: Double): Double; overload;
function ASin(const x: Extended): Extended; overload;

// ****************  arc cosine functions
// in: |Value| <= 1  out: [0..PI] radians
function ACos(const x: Single): Single; overload;
function ACos(const x: Double): Double; overload;
function ACos(const x: Extended): Extended; overload;

// ****************  arc tangent function
// out: [-PI/2..PI/2] radians
function ATan(const x: Single): Single; overload;
function ATan(const x: Double): Double; overload;
function ATan(const x: Extended): Extended; overload;

// ATan2 calculates ATan(Y/X), and returns an angle in the correct quadrant.
// out: [-PI..PI] radians
function ATan2(const y, x: Single): Single; overload;
function ATan2(const y, x: Double): Double; overload;
function ATan2(const y, x: Extended): Extended; overload;


// *********************************  Hyperbolic functions  **********************************

// ****************  hyperbolic sine functions
function SinH(const x: Single): Single; overload;
function SinH(const x: Double): Double; overload;
function SinH(const x: Extended): Extended; overload;

// ****************  hyperbolic cosine functions
function CosH(const x: Single): Single; overload;
function CosH(const x: Double): Double; overload;
function CosH(const x: Extended): Extended; overload;

// ****************  hyperbolic tangent functions
function TanH(const x: Single): Single; overload;
function TanH(const x: Double): Double; overload;
function TanH(const x: Extended): Extended; overload;


// *****************************  Inverse hyperbolic functions  ******************************

// ****************  inverse hyperbolic sine functions
function ASinH(const x: Single): Single; overload;
function ASinH(const x: Double): Double; overload;
function ASinH(const x: Extended): Extended; overload;

// ****************  inverse hyperbolic cosine functions
function ACosH(const x: Single): Single; overload;
function ACosH(const x: Double): Double; overload;
function ACosH(const x: Extended): Extended; overload;

// ****************  inverse hyperbolic tangent functions
function ATanH(const x: Single): Single; overload;
function ATanH(const x: Double): Double; overload;
function ATanH(const x: Extended): Extended; overload;


// ****************************  Angle unit conversion routines  *****************************

function DegToRad(const Value: Single): Single; overload;
function DegToRad(const Value: Double): Double; overload;
function DegToRad(const Value: Extended): Extended; overload;

function RadToDeg(const Value: Single): Single; overload;
function RadToDeg(const Value: Double): Double; overload;
function RadToDeg(const Value: Extended): Extended; overload;

// -Pi < Result <= Pi
function NormalizeSignedRad(const Value: Single): Single; overload;
function NormalizeSignedRad(const Value: Double): Double; overload;
function NormalizeSignedRad(const Value: Extended): Extended; overload;

// 0 <= Result < 2*Pi
function NormalizeUnsignedRad(const Value: Single): Single; overload;
function NormalizeUnsignedRad(const Value: Double): Double; overload;
function NormalizeUnsignedRad(const Value: Extended): Extended; overload;


// ***************************  Power functions (Exponentiation)  ****************************

// ****************  power function           y = Base^Exponent, Exponent = Integer
function IntPower(Base: Extended; const Exponent: Integer): Extended;

// ****************  power function           y = Base^Exponent
function Pow(const Base, Exponent: Single): Single; overload;
function Pow(const Base, Exponent: Double): Double; overload;
function Pow(const Base, Exponent: Extended): Extended; overload;


// ************************************  Root functions  *************************************

// ****************  square root function     y = x^(1/2)
{ unit System
function Sqrt(const Value: Single): Single; overload;
function Sqrt(const Value: Double): Double; overload;
function Sqrt(const Value: Extended): Extended; overload;
}

// ****************  cube root functions      y = x^(1/3)
function Cbrt(const x: Single): Single; overload;
function Cbrt(const x: Double): Double; overload;
function Cbrt(const x: Extended): Extended; overload;


// ****************************  Exponential functions (y = b^x)  ****************************

// ****************  exponential function     y = e^x
{ unit System
function Exp(const Value: Single): Single; overload;
function Exp(const Value: Double): Double; overload;
function Exp(const Value: Extended): Extended; overload;
}

// ****************  exponential function     y = e^x -1
function ExpM1(const x: Single): Single; overload;
function ExpM1(const x: Double): Double; overload;
function ExpM1(const x: Extended): Extended; overload;

// ****************  exponential function to base two    y = 2^x
// sometimes named Exp2
function Pow2(const x: Single): Single; overload;
function Pow2(const x: Double): Double; overload;
function Pow2(const x: Extended): Extended; overload;

// ****************  exponential function to base ten    y = 10^x
// sometimes named Exp10
function Pow10(const x: Single): Single; overload;
function Pow10(const x: Double): Double; overload;
function Pow10(const x: Extended): Extended; overload;


// *********************************  Logarithmic functions  *********************************

// ****************  natural logarithm function          y = ln(x)
function Log(const x: Single): Single; overload;
function Log(const x: Double): Double; overload;
function Log(const x: Extended): Extended; overload;

// ****************  natural logarithm function          y = ln(1+x)
function Log1P(const x: Single): Single; overload;
function Log1P(const x: Double): Double; overload;
function Log1P(const x: Extended): Extended; overload;

// ****************  base 2 logarithm function
function Log2(const x: Single): Single; overload;
function Log2(const x: Double): Double; overload;
function Log2(const x: Extended): Extended; overload;

// ****************  base 10 logarithm function
function Log10(const x: Single): Single; overload;
function Log10(const x: Double): Double; overload;
function Log10(const x: Extended): Extended; overload;

// ****************  base n logarithm function
function LogN(const Base, x: Single): Single; overload;
function LogN(const Base, x: Double): Double; overload;
function LogN(const Base, x: Extended): Extended; overload;


// *******************************  Absolute value functions  ********************************

// ****************  absolute value function
function FAbs(const x: Single): Single; overload;
function FAbs(const x: Double): Double; overload;
function FAbs(const x: Extended): Extended; overload;

function LAbs(const x: LongInt): LongInt;

// ****************  number manipulation function
// return the x parameter with the same sign as the y parameter
{ TODO : check for a correct definition, if x or y = 0 }
function CopySign(const x, y: Single): Single; overload;
function CopySign(const x, y: Double): Double; overload;
function CopySign(const x, y: Extended): Extended; overload;

// ****************  test sign
// Test for negative number
function SignBit(const x: Single): Boolean; overload;
function SignBit(const x: Double): Boolean; overload;
function SignBit(const x: Extended): Boolean; overload;

// return -1, if x < 0; 0 if x = 0; +1 if x > 0 
function Sign(const x: Single): Integer; overload;
function Sign(const x: Double): Integer; overload;
function Sign(const x: Extended): Integer; overload;


// *******************************  Nearest integer functions  *******************************

type
  // Note: direct used for the RC field; do'nt change
  TRoundingMode = (
    rmNearest,       // Rounded result is the closest to the infinitely precise results.
                     // If two values are equally close, the result is the even value
                     // (that is, the one with the least-significant bit of zero). Default
    rmDown,          // Rounded result is closest to but no greater than the infinitely
                     // precise result.
    rmUp,            // Rounded result is closest to but no less than the infinitely
                     // precise result.
    rmTowardZero     // Rounded result is closest to but no greater in absolute value than
                     // the infinitely precise result.
  );
const
  rmEven     = rmNearest;
  rmTruncate = rmTowardZero;

// ****************  ceiling value function
function Ceil(const Value: Extended): Extended;
function LCeil(const Value: Extended): LongInt;
function LLCeil(const Value: Extended): Int64;

// ****************  floor function
function Floor(const Value: Extended): Extended;
function LFloor(const Value: Extended): LongInt;
function LLFloor(const Value: Extended): Int64;

// ****************  "banker's round" function
// Round Value to nearest integral value, rounding halfway cases away from zero
function RoundToNearest(const Value: Extended): Extended;
function LRoundToNearest(const Value: Extended): LongInt;
function LLRoundToNearest(const Value: Extended): Int64;

// ****************  round to truncated integer value
// renamed from Trunc, LTrunc, LLTrunc to avoid problems with the Pascal function Trunc
// Round Value to the integral value in floating-point format nearest but not larger in magnitude
function RoundTowardZero(const Value: Extended): Extended;
function LRoundTowardZero(const Value: Extended): Integer;
function LLRoundTowardZero(const Value: Extended): Int64;

// ****************  round to nearest integer value using specific rounding direction
// Round Value to nearest integral value according to rounding direction in Mode
function RInt(const Value: Extended; Mode: TRoundingMode = rmNearest): Extended;
function LRInt(const Value: Extended; Mode: TRoundingMode = rmNearest): LongInt;
function LLRInt(const Value: Extended; Mode: TRoundingMode = rmNearest): Int64; 

// ****************  
// Example: Fraction = 0.25 => Results are ..., -0.25, 0.0, 0.25, 0.5, 0.75, 1.0, ...
function RoundToFraction(const Value, Fraction: Single; Mode: TRoundingMode = rmNearest): Single; overload;
function RoundToFraction(const Value, Fraction: Double; Mode: TRoundingMode = rmNearest): Double; overload;
function RoundToFraction(const Value, Fraction: Extended; Mode: TRoundingMode = rmNearest): Extended; overload;

{ TODO : implementieren }
(*
// ****************  next representable floating-point number
// Return X + epsilon if X < Y, X - epsilon if X > Y
function NextAfter(const x, y: Single): Single; overload;
function NextAfter(const x, y: Double): Double; overload;
function NextAfter(const x, y: Extended): Extended; overload;

// ****************  next representable floating-point number
function NextToward(const x, y: Single): Single; overload;
function NextToward(const x, y: Double): Double; overload;
function NextToward(const x, y: Extended): Extended; overload;

// ****************  floating-point rounding functions
// Round X to integral value in floating-point format using current rounding direction, but do
// not raise inexact exception
function NearByInt(const x: Single): Single; overload;
function NearByInt(const x: Double): Double; overload;
function NearByInt(const x: Extended): Extended; overload;
*)


// **********************************  Remainder functions  **********************************

// ****************  floating-point remainder value function
// Partial Remainder, ANSI C
function FMod(const x, y: Single): Single; overload;
function FMod(const x, y: Double): Double; overload;
function FMod(const x, y: Extended): Extended; overload;

// ****************  remainder function
// Partial Remainder, IEEE-754
// sometimes names DRem
// Return the remainder of integer divison X / Y with infinite precision
function Remainder(const x, y: Single): Single; overload;
function Remainder(const x, y: Double): Double; overload;
function Remainder(const x, y: Extended): Extended; overload;

// ****************  remainder functions
// Compute remainder of X and Y and put in *QUO a value with sign of x/y and magnitude congruent
// 'mod 2^n' to the magnitude of the integral quotient x/y, with n >= 3
{ TODO : implementieren }
{
function RemQuo(const x, y: Single; out quo: Integer): Single; overload;
function RemQuo(const x, y: Double; out quo: Integer): Double; overload;
function RemQuo(const x, y: Extended; out quo: Integer): Extended; overload;
}

// ****************************  Floating point number functions  ****************************

// ****************  extract mantissa and exponent from a floating-point number
function FrExp(const Value: Single; out Exponent: Integer): Single; overload;
function FrExp(const Value: Double; out Exponent: Integer): Double; overload;
function FrExp(const Value: Extended; out Exponent: Integer): Extended; overload;

// ****************  load exponent of a floating-point number
function LdExp(const Fraction: Single; const Exponent: Integer): Single; overload;
function LdExp(const Fraction: Double; const Exponent: Integer): Double; overload;
function LdExp(const Fraction: Extended; const Exponent: Integer): Extended; overload;

// ****************  decompose a floating-point number
// The modf function breaks down the floating-point value Value into fractional and integer parts,
// each of which has the same sign as Value. The signed fractional portion of Value is returned.
// The integer portion is stored in IPart.
function ModF(const Value: Single; out IPart: Single): Single; overload;
function ModF(const Value: Double; out IPart: Double): Double; overload;
function ModF(const Value: Extended; out IPart: Extended): Extended; overload;

{ TODO : Beschreibung und implementieren }
// These functions shall compute the exponent of x, which is the integral part of logr |x|, as
// a signed floating-point value, for non-zero x, where r is the radix of the machine's
// floating-point arithmetic, which is the value of FLT_RADIX defined in the <float.h> header.
{
// ****************  radix-independent exponent
function LogB(const Base, x: Single): Single;
function LogB(const Base, x: Double): Double;
function LogB(const Base, x: Extended): Extended;

// ****************  return an unbiased exponent
// Return the binary exponent of X, which must be nonzero
function ILogB(const x: Single): Integer;
function ILogB(const x: Double): Integer;
function ILogB(const x: Extended): Integer;
}
{
// ****************  load exponent of a radix-independent floating-point number
// Return X times (2 to the Nth power)
function ScalB(const x, n: Single): Single;
function ScalB(const x, n: Double): Double;
function ScalB(const x, n: Extended): Extended;

// ****************  compute exponent using FLT_RADIX
// Return X times (2 to the Nth power)
function ScalBN(const x, n: Single): Single;
function ScalBN(const x, n: Double): Double;
function ScalBN(const x, n: Extended): Extended;

// ****************  compute exponent using FLT_RADIX
// Return X times (2 to the Nth power)
function ScalBLN(const x: Single; n: Integer): Single;
function ScalBLN(const x: Double; n: Integer): Double;
function ScalBLN(const x: Extended; n: Integer): Extended;
}
{
// Return the fractional part of X after dividing out 'ilogb (X)'
function Significand(const x: Single): Single; overload;
function Significand(const x: Double): Double; overload;
function Significand(const x: Extended): Extended; overload;
}

// ******************  Maximum, minimum, and positive difference functions  ******************

// ****************  determine maximum numeric value of two floating-point numbers
function FMax(const x, y: Single): Single; overload;
function FMax(const x, y: Double): Double; overload;
function FMax(const x, y: Extended): Extended; overload;

function Max(const x, y: Integer): Integer; overload;
function Max(const x, y: Int64): Int64; overload;

// ****************  determine minimum numeric value of two floating-point numbers
function FMin(const x, y: Single): Single; overload;
function FMin(const x, y: Double): Double; overload;
function FMin(const x, y: Extended): Extended; overload;

function Min(const x, y: Integer): Integer; overload;
function Min(const x, y: Int64): Int64; overload;

// ****************  return positive difference between X and Y
function FDim(const x, y: Single): Single; overload;
function FDim(const x, y: Double): Double; overload;
function FDim(const x, y: Extended): Extended; overload;

// ****************  floating-point multiply-add
// (x * y) + z
function Fma(const x, y, z: Single): Single; overload;
function Fma(const x, y, z: Double): Double; overload;
function Fma(const x, y, z: Extended): Extended; overload;

(*
// ****************  test if x greater than y
isgreater                            -
// ****************  test if x is greater than or equal to y
isgreaterequal                       -
// ****************  test if x is less than y
isless                               -
// ****************  test if x is less than or equal to y
islessequal                          -
// ****************  test if x is less than or greater than y
islessgreater                        -
// ****************  test for a normal value
isnormal                             -
// ****************  test if arguments are unordered
isunordered
*)                        

function IsSameValue(const Value1, Value2: Single; const Epsilon: Single): Boolean; overload;
function IsSameValue(const Value1, Value2: Double; const Epsilon: Double): Boolean; overload;
function IsSameValue(const Value1, Value2: Extended; const Epsilon: Extended): Boolean; overload;


// ***********************************  Special functions  ***********************************

// ****************  euclidean distance function
function Hypot(const x, y: Single): Single; overload;
function Hypot(const x, y: Double): Double; overload;
function Hypot(const x, y: Extended): Extended; overload;

{ TODO : implementieren }
{
// ****************  error function
function Erf(const x: Single): Single; overload;
function Erf(const x: Double): Double; overload;
function Erf(const x: Extended): Extended; overload;

// ****************  complementary error function
function ErfC(const x: Single): Single; overload;
function ErfC(const x: Double): Double; overload;
function ErfC(const x: Extended): Extended; overload;
}
(*
// Bessel functions
// ****************  Bessel functions of the first kind
function J0(x: Single): Single; overload;
function J0(x: Double): Double; overload;
function J0(x: Extended): Extended; overload;

function J1(x: Single): Single; overload;
function J1(x: Double): Double; overload;
function J1(x: Extended): Extended; overload;

function JN(x: Single; n: Integer): Single; overload;
function JN(x: Double; n: Integer): Double; overload;
function JN(x: Extended; n: Integer): Extended; overload;

// ****************  Bessel functions of the second kind
function Y0(x: Single): Single; overload;
function Y0(x: Double): Double; overload;
function Y0(x: Extended): Extended; overload;

function Y1(x: Single): Single; overload;
function Y1(x: Double): Double; overload;
function Y1(x: Extended): Extended; overload;

function YN(x: Single; n: Integer): Single; overload;
function YN(x: Double; n: Integer): Double; overload;
function YN(x: Extended; n: Integer): Extended; overload;


// ****************  log gamma function
// previous named Gamma
function LGamma(x: Single): Single; overload;
function LGamma(x: Double): Double; overload;
function LGamma(x: Extended): Extended; overload;

// ****************  compute gamma() function
// True gamma function
function TGamma(x: Single): Single; overload;
function TGamma(x: Double): Double; overload;
function TGamma(x: Extended): Extended; overload;

// Reentrant version of lgamma.  This function uses the global variable 'signgam'.  The reentrant
// version instead takes a pointer and stores the value through it.
{ TODO : out or var }
function LGamma_r(x: Single; out signgamp: Integer): Single; overload;
function LGamma_r(x: Double; out signgamp: Integer): Double; overload;
function LGamma_r(x: Extended; out signgamp: Integer): Extended; overload;
*)


// ***********************************  Array Funktionen  ************************************

function ArrayGetMaxIndex(const Values: array of Single): Integer;
function ArrayGetMinIndex(const Values: array of Single): Integer;

// Result 0..Count-1
function NormalizePeriodicValue(Value, Count: Integer): Integer;

implementation

// *******************************  Classification functions  ********************************

// Sign Exponent(e)     Fraction(f)     Value
//   0  00..00          00..00          +0
//   0  00..00          00..01  11..11  Positive Denormalized Real  0.f x 2^(-b+1)
//   0  00..01  11..10  XX..XX          Positive Normalized Real    1.f x 2^(e-b)
//   0  11..11          00..00          +Infinity
//   0  11..11          00..01  01..11  SNaN
//   0  11..11          10..00  11..11  QNaN
//   1  00..00          00..00          -0
//   1  00..00          00..01  11..11  Negative Denormalized Real  -0.f x 2^(-b+1)
//   1  00..01  11..10  XX..XX          Negative Normalized Real    -1.f x 2^(e-b)
//   1  11..11          00..00          -Infinity
//   1  11..11          00..01  01..11  SNaN
//   1  11..11          10..00  11..11  QNaN

type
  TSingleRec = packed record
    V: LongWord;
  end;
  {$IFDEF ENDIAN_LITTLE}
  TDoubleRec = packed record            // Little Endian
  case Integer of
    0: (V: UInt64);
    1: (V0: LongWord;
        V1: LongWord);
  end;
  TDoubleExtendedRec = packed record    // Little Endian
  case Integer of
    0: (Significand: UInt64;
        SignExponent: Word);
    1: (Significand0: LongWord;
        Significand1: LongWord);
  end;
  {$ELSE ENDIAN_LITTLE~}
  TDoubleRec = packed record            // Big Endian
  case Integer of
    0: (V: UInt64);
    1: (V1: LongWord;
        V0: LongWord);
  end;
  TDoubleExtendedRec = packed record    // Big Endian
    SignExponent: Word;
  case Integer of
    0: (Significand: UInt64);
    1: (Significand1: LongWord;
        Significand0: LongWord);
  end;
  {$ENDIF ~ENDIAN_LITTLE}

const
  // Mask for Single precision                         TSingleRec
  MaskSingleSignBit              = $80000000;          // V
  MaskSingleExponent             = $7F800000;          // V
  MaskSingleFraction             = $007FFFFF;          // V
                                 
  // Mask for Double precision                         TDoubleRec
  MaskDoubleSignBit              = $80000000;          // V1
  MaskDoubleExponent             = $7FF00000;          // V1
  MaskDoubleFraction             = $000FFFFF;          // V1
//MaskDoubleFraction64           = $000FFFFFFFFFFFFF;  // V

  // Mask for Double-Extended precision                TDoubleExtendedRec
  MaskDoubleExtendedSignBit      = $8000;              // SignExponent
  MaskDoubleExtendedExponent     = $7FFF;              // SignExponent
  MaskDoubleExtendedIntegerBit   = $80000000;          // Significand1
//MaskDoubleExtendedIntegerBit64 = $8000000000000000;  // Significand
//MaskDoubleExtendedFraction     = $7FFFFFFF;          // Significand1
//MaskDoubleExtendedFraction64   = $7FFFFFFFFFFFFFFF;  // Significand

//SingleExponentBias             = 127;
//DoubleExponentBias             = 1023;
//DoubleExtendedExponentBias     = 16383;

// Test for finite value
// Return True if X is not +-Inf or NaN
function IsFinite(const Value: Single): Boolean;
begin
  // Exponent <> MaxExponent
  Byte(Result) := ((TSingleRec(Value).V and MaskSingleExponent) - MaskSingleExponent) shr 31;
end;

function IsFinite(const Value: Double): Boolean;
begin
  // Exponent <> MaxExponent
  Byte(Result) := ((TDoubleRec(Value).V1 and MaskDoubleExponent) - MaskDoubleExponent) shr 31;
end;

function IsFinite(const Value: Extended): Boolean;
begin
  // Exponent <> MaxExponent
  Byte(Result) := ((TDoubleExtendedRec(Value).SignExponent and MaskDoubleExtendedExponent)
                   - MaskDoubleExtendedExponent) shr 15;
end;

// Test for infinity
// +Inf: +1
// -Inf: -1
// Finite or NaN: 0
function IsInf(const Value: Single): Integer;
// +Inf: 7F800000
// -Inf: FF800000
begin
  if (TSingleRec(Value).V and (MaskSingleExponent or MaskSingleFraction)) = MaskSingleExponent then begin
    if (TSingleRec(Value).V and MaskSingleSignBit) <> 0 then
      Result := -1
    else
      Result := 1;
  end
  else
    Result := 0;
end;

function IsInf(const Value: Double): Integer;
// +Inf: 7FF00000 00000000
// -Inf: FFF00000 00000000
begin
  if (TDoubleRec(Value).V0 = 0) and
     ((TDoubleRec(Value).V1 and (MaskDoubleExponent or MaskDoubleFraction)) = MaskDoubleExponent) then begin
    if (TDoubleRec(Value).V1 and MaskDoubleSignBit) <> 0 then
      Result := -1
    else
      Result := 1;
  end
  else
    Result := 0;
end;

function IsInf(const Value: Extended): Integer;
// +Inf: 7FFF 00000000 00000000
// -Inf: FFF0 00000000 00000000
begin
  {$IFDEF CPU68k}
  // ignore Integer Bit, Integer Bit = 0 mean Pseudo-Infinity
  if (TDoubleExtendedRec(Value).Significand0 = 0) and
     ((TDoubleExtendedRec(Value).Significand1 and MaskDoubleExtendedFraction) = 0) and
     ((TDoubleExtendedRec(Value).SignExponent and MaskDoubleExtendedExponent) = MaskDoubleExtendedExponent) then begin
    if (TDoubleExtendedRec(Value).SignExponent and MaskDoubleExtendedSignBit) <> 0 then
      Result := -1
    else
      Result := 1;
  end
  else
    Result := 0;
  {$ELSE CPU68k~}
  // heed Integer Bit, Integer Bit = 0 mean Pseudo-Infinity
  if (TDoubleExtendedRec(Value).Significand0 = 0) and
     (TDoubleExtendedRec(Value).Significand1 = MaskDoubleExtendedIntegerBit) and
     ((TDoubleExtendedRec(Value).SignExponent and MaskDoubleExtendedExponent) = MaskDoubleExtendedExponent) then begin
    if (TDoubleExtendedRec(Value).SignExponent and MaskDoubleExtendedSignBit) <> 0 then
      Result := -1
    else
      Result := 1;
  end
  else
    Result := 0;
  {$ENDIF ~CPU68k}
end;

function IsNaN(const Value: Single): Boolean;
// QNaN: 7FFFFFFF (7FC00000..7FFFFFFF, FFC00000..FFFFFFFF)
// SNaN: 7F800001 (7F800001..7FBFFFFF, FF800001..FFBFFFFF)
begin
  Result := ((TSingleRec(Value).V and MaskSingleFraction) <> 0) and
            ((TSingleRec(Value).V and MaskSingleExponent) = MaskSingleExponent);
end;

function IsNaN(const Value: Double): Boolean;
// QNaN: 7FFFFFFF FFFFFFFF (7FF80000 00000000..7FFFFFFF FFFFFFFF,
//                          FFF80000 00000000..FFFFFFFF FFFFFFFF)
// SNaN: 7FF00000 00000001 (7FF00000 00000001..7FF7FFFF FFFFFFFF,
//                          FFF00000 00000001..FFF7FFFF FFFFFFFF)
begin
  Result := ((TDoubleRec(Value).V0 <> 0) or
             ((TDoubleRec(Value).V1 and MaskDoubleFraction) <> 0)) and
            ((TDoubleRec(Value).V1 and MaskDoubleExponent) = MaskDoubleExponent);
end;

function IsNaN(const Value: Extended): Boolean;
// QNaN: 7FFF C0000000 00000000 (7FFF C0000000 00000000..7FFF FFFFFFFF FFFFFFFF,
//                               FFFF C0000000 00000000..FFFF FFFFFFFF FFFFFFFF)
// SNaN: 7FFF 80000000 00000001 (7FFF 80000000 00000001..7FFF BFFFFFFF FFFFFFFF,
//                               FFFF 80000000 00000001..FFFF BFFFFFFF FFFFFFFF)
begin
  {$IFDEF CPU68k}
  // ignore Integer Bit, Integer Bit = 0 mean Pseudo-NaN
  Result := ((TDoubleExtendedRec(Value).Significand0 <> 0) or
             ((TDoubleExtendedRec(Value).Significand1 and $7FFFFFFF) <> 0)) and
            ((TDoubleExtendedRec(Value).SignExponent and $7FFF) = $7FFF);
  {$ELSE CPU68k~}
  // heed Integer Bit, Integer Bit = 0 mean Pseudo-NaN
  Result := ((TDoubleExtendedRec(Value).Significand0 <> 0) or
             ((TDoubleExtendedRec(Value).Significand1 and $7FFFFFFF) <> 0)) and
            ((TDoubleExtendedRec(Value).Significand1 and $80000000) = $80000000) and
            ((TDoubleExtendedRec(Value).SignExponent and $7FFF) = $7FFF);
  {$ENDIF ~CPU68k}
end;


function SingleMaxNormalValue: Single;
begin
  TSingleRec(Result).V := $7F7FFFFF;
end;                       

function SingleMinNormalValue: Single;
begin
  TSingleRec(Result).V := $00800000;
end;                       

function SingleMaxSubnormalValue: Single;
begin
  TSingleRec(Result).V := $007FFFFF;
end;

function SingleMinSubnormalValue: Single;
begin
  TSingleRec(Result).V := $00000001;
end;                       


function DoubleMaxNormalValue: Double;
begin
  TDoubleRec(Result).V0 := $FFFFFFFF;
  TDoubleRec(Result).V1 := $7FEFFFFF;
end;                        

function DoubleMinNormalValue: Double;
begin
  TDoubleRec(Result).V0 := $00000000;
  TDoubleRec(Result).V1 := $00100000;
end;

function DoubleMaxSubnormalValue: Double;
begin
  TDoubleRec(Result).V0 := $FFFFFFFF;
  TDoubleRec(Result).V1 := $000FFFFF;
end;                        

function DoubleMinSubnormalValue: Double;
begin
  TDoubleRec(Result).V0 := $00000001;
  TDoubleRec(Result).V1 := $00000000;
end;


function DoubleExtendedMaxNormalValue: Extended;
begin
  TDoubleExtendedRec(Result).Significand0 := $FFFFFFFF;
  TDoubleExtendedRec(Result).Significand1 := $FFFFFFFF;
  TDoubleExtendedRec(Result).SignExponent := $7FFE;
end;

function DoubleExtendedMinNormalValue: Extended;
begin
  TDoubleExtendedRec(Result).Significand0 := $00000000;
  TDoubleExtendedRec(Result).Significand1 := $80000000;
  TDoubleExtendedRec(Result).SignExponent := $0001;
end;

function DoubleExtendedMaxSubnormalValue: Extended;
begin                                       
  TDoubleExtendedRec(Result).Significand0 := $FFFFFFFF;
  TDoubleExtendedRec(Result).Significand1 := $7FFFFFFF;
  TDoubleExtendedRec(Result).SignExponent := $0000;
end;

function DoubleExtendedMinSubnormalValue: Extended;
begin
  TDoubleExtendedRec(Result).Significand0 := $00000001;
  TDoubleExtendedRec(Result).Significand1 := $00000000;
  TDoubleExtendedRec(Result).SignExponent := $0000;
end;

// ********************************  Trigonometric functions  ********************************

// ************************************  sine and cosine  ************************************

// SinCos is faster than executing the Sin and Cos functions in succession
// in: |Value| <= 2^63 radians
{$IFDEF USEFPUx87}
procedure SinCos(const x: Single; out Sin, Cos: Single); register; assembler;
asm
        FLD     x
        FSINCOS
        FSTP    DWORD PTR [edx]    // Cos
        FSTP    DWORD PTR [eax]    // Sin
        FWAIT
end;
{$ELSE USEFPUx87~}
procedure SinCos(const x: Single; out Sin, Cos: Single);
begin
  Sin := System.Sin(x);
  Cos := System.Cos(x);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
procedure SinCos(const x: Double; out Sin, Cos: Double); register; assembler;
asm
        FLD     x
        FSINCOS
        FSTP    QWORD PTR [edx]    // Cos
        FSTP    QWORD PTR [eax]    // Sin
        FWAIT
end;
{$ELSE USEFPUx87~}
procedure SinCos(const x: Double; out Sin, Cos: Double);
begin
  Sin := System.Sin(x);
  Cos := System.Cos(x);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
procedure SinCos(const x: Extended; out Sin, Cos: Extended); register; assembler;
asm
        FLD     x
        FSINCOS
        FSTP    TBYTE PTR [edx]    // Cos
        FSTP    TBYTE PTR [eax]    // Sin
        FWAIT
end;
{$ELSE USEFPUx87~}
procedure SinCos(const x: Extended; out Sin, Cos: Extended);
begin
  Sin := System.Sin(x);
  Cos := System.Cos(x);
end;
{$ENDIF ~USEFPUx87}

// ****************************************  tangent  ****************************************

{$IFDEF USEFPUx87}
// in: |Value| <= 2^63 radians
procedure InternalTan; register; assembler;
asm
        FPTAN              // Tan(Value) -> ST(1), 1.0 -> ST(0)
        FSTP    ST(0)      // pop (1.0)
        FWAIT
end;
{$ENDIF USEFPUx87}

// in: |Value| <= 2^63 radians
{$IFDEF USEFPUx87}
function Tan(const x: Single): Single; register; assembler;
asm
        FLD     x
        CALL    InternalTan
end;
{$ELSE USEFPUx87~}
function Tan(const x: Single): Single;
begin
  Result := System.Sin(x) / System.Cos(x);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function Tan(const x: Double): Double; register; assembler;
asm
        FLD     x
        CALL    InternalTan
end;
{$ELSE USEFPUx87~}
function Tan(const x: Double): Double;
begin
  Result := System.Sin(x) / System.Cos(x);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function Tan(const x: Extended): Extended; register; assembler;
asm
        FLD     x
        CALL    InternalTan
end;
{$ELSE USEFPUx87~}
function Tan(const x: Extended): Extended;
begin
  Result := System.Sin(x) / System.Cos(x);
end;
{$ENDIF ~USEFPUx87}

// ***************************************  cotangent  ***************************************

{$IFDEF USEFPUx87}
procedure InternalCotan; register; assembler;
asm
        FPTAN                  // Tan(Value) -> ST(1), 1.0 -> ST(0)
        FDIVRP  ST(1), ST(0)   // 1.0 / Tan(Value)
        FWAIT
end;
{$ENDIF USEFPUx87}

// in: 0 < |Value| <= 2^63 radians
{$IFDEF USEFPUx87}
function Cotan(const x: Single): Single; register; assembler;
asm
        FLD     x
        CALL    InternalCotan
end;
{$ELSE USEFPUx87~}
function Cotan(const x: Single): Single;
begin
  Result := System.Cos(x) / System.Sin(x);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function Cotan(const x: Double): Double; register; assembler;
asm
        FLD     x
        CALL    InternalCotan
end;
{$ELSE USEFPUx87~}
function Cotan(const x: Double): Double;
begin
  Result := System.Cos(x) / System.Sin(x);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function Cotan(const x: Extended): Extended; register; assembler;
asm
        FLD     x
        CALL    InternalCotan
end;
{$ELSE USEFPUx87~}
function Cotan(const x: Extended): Extended;
begin
  Result := System.Cos(x) / System.Sin(x);
end;
{$ENDIF ~USEFPUx87}

// ****************************  Inverse trigonometric functions  ****************************

// ****************  arc sine function
// in: |Value| <= 1  out: [-PI/2..PI/2] radians

// Sin(Result) = Value
// Sin(Result)^2 + Cos(Result)^2 = 1.0
// => Cos(Result) = Sqrt(1.0 - Value^2)
// Tan(Result) = Sin(Result) / Cos(Result)
// => Tan(Result) = Value / Sqrt(1.0 - Value^2)

{$IFDEF USEFPUx87}
procedure InternalASin; register; assembler;
//  Result := ATan2(Value, Sqrt(1.0 - Sqr(Value)));
asm
        FLD1                         // Push 1.0
        FLD     ST(1)                // Push Value
        FMUL    ST(0), ST(0)         // Value²
        FSUBP   ST(1), ST(0)         // 1.0 - Value²
        FSQRT
        FPATAN                       // ArcTan2(Value, 1.0 - Value²)
        FWAIT
end;
{$ENDIF USEFPUx87}

{$IFDEF USEFPUx87}
function ASin(const x: Single): Single; register; assembler;
asm
        FLD     x                // Push Value
        CALL    InternalASin;
end;
{$ELSE USEFPUx87~}
function ASin(const x: Single): Single;
begin
  Result := ATan2(x, System.Sqrt(1.0 - Sqr(x)));
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function ASin(const x: Double): Double; register; assembler;
asm
        FLD     x                // Push Value
        CALL    InternalASin;
end;
{$ELSE USEFPUx87~}
function ASin(const x: Double): Double;
begin
  Result := ATan2(x, System.Sqrt(1.0 - Sqr(x)));
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function ASin(const x: Extended): Extended; register; assembler;
asm
        FLD     x                // Push Value
        CALL    InternalASin;
end;
{$ELSE USEFPUx87~}
function ASin(const x: Extended): Extended;
begin
  Result := ATan2(x, System.Sqrt(1.0 - Sqr(x)));
end;
{$ENDIF ~USEFPUx87}


// ****************  arc cosine functions
// in: |Value| <= 1  out: [0..PI] radians

{$IFDEF USEFPUx87}
procedure InternalACos; register; assembler;
//  Result := ATan2(Sqrt(1.0 - Sqr(Value)), Value);
asm
        FLD1                         // Push 1.0
        FLD     ST(1)                // Push Value
        FMUL    ST(0), ST(0)         // Value²
        FSUBP   ST(1), ST(0)         // 1.0 - Value²
        FSQRT
        FXCH    ST(1)
        FPATAN                       // ArcTan2(1.0 - Value², Value)
        FWAIT
end;
{$ENDIF USEFPUx87}

// in: |Value| <= 1  out: [0..PI] radians
// Algorithm see ArcSin
{$IFDEF USEFPUx87}
function ACos(const x: Single): Single; register; assembler;
asm
        FLD     x                // Push Value
        CALL    InternalACos
end;
{$ELSE USEFPUx87~}
function ACos(const x: Single): Single;
begin
  Result := ATan2(System.Sqrt(1.0 - Sqr(x)), x);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function ACos(const x: Double): Double; register; assembler;
asm
        FLD     x                // Push Value
        CALL    InternalACos
end;
{$ELSE USEFPUx87~}
function ACos(const x: Double): Double;
begin
  Result := ATan2(System.Sqrt(1.0 - Sqr(x)), x);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function ACos(const x: Extended): Extended; register; assembler;
asm
        FLD     x                // Push Value
        CALL    InternalACos
end;
{$ELSE USEFPUx87~}
function ACos(const x: Extended): Extended;
begin
  Result := ATan2(System.Sqrt(1.0 - Sqr(x)), x);
end;
{$ENDIF ~USEFPUx87}


// ****************  arc tangent function
// out: [-PI/2..PI/2] radians
function ATan(const x: Single): Single;
begin
  Result := System.ArcTan(x);
end;

function ATan(const x: Double): Double;
begin
  Result := System.ArcTan(x);
end;

function ATan(const x: Extended): Extended;
begin
  Result := System.ArcTan(x);
end;


// ****************  arc tangent function

// ATan2 calculates ATan(Y/X), and returns an angle in the correct quadrant.
// out: [-PI..PI] radians
{$IFDEF USEFPUx87}
function ATan2(const y, x: Single): Single; register;
asm
        FLD     y          // Y -> ST(1)
        FLD     x          // X -> ST(0)
        FPATAN
        FWAIT
end;
{$ELSE USEFPUx87~}
function ATan2(const y, x: Single): Single; register;
var
  y2, x2: Extended;
begin
  y2 := y;
  x2 := x;
  Result := ATan2(y2, x2);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function ATan2(const y, x: Double): Double; register;
asm
        FLD     y
        FLD     x
        FPATAN
        FWAIT
end;
{$ELSE USEFPUx87~}
function ATan2(const y, x: Double): Double; register;
var
  y2, x2: Extended;
begin
  y2 := y;
  x2 := x;
  Result := ATan2(y2, x2);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function ATan2(const y, x: Extended): Extended; register;
asm
        FLD     y
        FLD     x
        FPATAN
        FWAIT
end;
{$ELSE USEFPUx87~}
function ATan2(const y, x: Extended): Extended; register;
begin
  if x = 0 then begin
    if y > 0 then       // x = 0, y > 0
      Result := 0.5 * Pi
    else if y < 0 then  // x = 0, y < 0
      Result := -0.5 * Pi
    else                // x = 0, y = 0
      Result := 0;
  end
  else begin  // x <> 0
    Result := System.ArcTan(y / x);  // -Pi/2 .. +Pi/2
    if x < 0 then begin
      if y >= 0 then                 // x < 0, y >= 0, Result -Pi/2..0 -> Pi/2..Pi
        Result := Result + Pi
      else                           // x < 0, y < 0, Result 0..Pi/2 -> -Pi..-Pi/2
        Result := Result - Pi;
    end;
  end;
end;
{$ENDIF ~USEFPUx87}

// *********************************  Hyperbolic functions  **********************************

// ****************  hyperbolic sine functions

// SinH(x) = (e^x - e^(-x)) / 2 = (e^x - 1 / e^x) / 2
function SinH(const x: Single): Single;
var
  Temp: Extended;
begin
  Temp := System.Exp(x);
  Result := (Temp - 1 / Temp) * 0.5;
end;

function SinH(const x: Double): Double;
var
  Temp: Extended;
begin
  Temp := System.Exp(x);
  Result := (Temp - 1 / Temp) * 0.5;
end;

function SinH(const x: Extended): Extended;
var
  Temp: Extended;
begin
  Temp := System.Exp(x);
  Result := (Temp - 1 / Temp) * 0.5;
end;


// ****************  hyperbolic cosine functions

// CosH(x) = (e^x + e^(-x)) / 2 = (e^x + 1 / e^x) / 2
function CosH(const x: Single): Single;
var
  Temp: Extended;
begin
  Temp := System.Exp(x);
  Result := (Temp + 1 / Temp) * 0.5;
end;

function CosH(const x: Double): Double;
var
  Temp: Extended;
begin
  Temp := System.Exp(x);
  Result := (Temp + 1 / Temp) * 0.5;
end;

function CosH(const x: Extended): Extended;
var
  Temp: Extended;
begin
  Temp := System.Exp(x);
  Result := (Temp + 1 / Temp) * 0.5;
end;


// ****************  hyperbolic tangent functions

// TanH(x) = SinH(x) / CosH(x)
// TanH(x) = (e^x - 1 / e^x) / 2       e^x    e^2*x - 1
//           -------------------     * ---  = ---------
//           (e^x + 1 / e^x) / 2       e^x    e^2*x + 1
function TanH(const x: Single): Single;
var
  Temp: Extended;
begin
  { TODO : Genauigkeit verbessern, dann Konstante anpassen }
  if Abs(x) < 20.0 then begin
    Temp := System.Exp(2 * x);
    Result := (Temp - 1) / (Temp + 1);
  end
  else if x < 0 then
    Result := -1.0
  else
    Result := 1.0;
end;

function TanH(const x: Double): Double;
var
  Temp: Extended;
begin
  { TODO : Genauigkeit verbessern, dann Konstante anpassen }
  if Abs(x) < 20.0 then begin
    Temp := System.Exp(2 * x);
    Result := (Temp - 1) / (Temp + 1);
  end
  else if x < 0 then
    Result := -1.0
  else
    Result := 1.0;
end;

function TanH(const x: Extended): Extended;
var
  Temp: Extended;
begin
  { TODO : Genauigkeit verbessern, dann Konstante anpassen }
  if Abs(x) < 20.0 then begin
    Temp := System.Exp(2 * x);
    Result := (Temp - 1) / (Temp + 1);
  end
  else if x < 0 then
    Result := -1.0
  else
    Result := 1.0;
end;


// *****************************  Inverse hyperbolic functions  ******************************

// ****************  inverse hyperbolic sine functions

// ASinH(x) = ln(x + Sqrt(x^2 + 1))
function ASinH(const x: Single): Single;
begin
  Result := System.Ln(x + System.Sqrt(Sqr(x) + 1));
end;

function ASinH(const x: Double): Double;
begin
  Result := System.Ln(x + System.Sqrt(Sqr(x) + 1));
end;

function ASinH(const x: Extended): Extended;
begin
  Result := System.Ln(x + System.Sqrt(Sqr(x) + 1));
end;


// ****************  inverse hyperbolic cosine functions

// ACosH(x) = +/- ln(x + Sqrt(x^2 - 1))
function ACosH(const x: Single): Single;
begin
  Result := System.Ln(x + System.Sqrt(Sqr(x) - 1));
end;

function ACosH(const x: Double): Double;
begin
  Result := System.Ln(x + System.Sqrt(Sqr(x) - 1));
end;

function ACosH(const x: Extended): Extended;
begin
  Result := System.Ln(x + System.Sqrt(Sqr(x) - 1));
end;


// ****************  inverse hyperbolic tangent functions

// ATanH(x) = 1/2 * ln((1+x)/1-x))
function ATanH(const x: Single): Single;
begin
  Result := System.Ln((1 + x) / (1 - x)) * 0.5;
end;

function ATanH(const x: Double): Double;
begin
  Result := System.Ln((1 + x) / (1 - x)) * 0.5;
end;

function ATanH(const x: Extended): Extended;
begin
  Result := System.Ln((1 + x) / (1 - x)) * 0.5;
end;


// ****************************  Angle unit conversion routines  *****************************

function DegToRad(const Value: Single): Single;
begin
  Result := Value * (Pi / 180);
end;

function DegToRad(const Value: Double): Double;
begin
  Result := Value * (Pi / 180);
end;

function DegToRad(const Value: Extended): Extended;
begin
  Result := Value * (Pi / 180);
end;

function RadToDeg(const Value: Single): Single;
begin
  Result := Value * (180 / Pi);
end;

function RadToDeg(const Value: Double): Double;
begin
  Result := Value * (180 / Pi);
end;

function RadToDeg(const Value: Extended): Extended;
begin
  Result := Value * (180 / Pi);
end;

// -Pi < Result <= Pi
function NormalizeSignedRad(const Value: Single): Single;
var
  Temp: Extended;
begin
  Temp := (Value / (2 * Pi)) + 0.5;
  Temp := Temp - Floor(Temp);
  if Temp = 0 then
    Result := Pi
  else
    Result := (Temp - 0.5) * (2 * Pi);
end;

function NormalizeSignedRad(const Value: Double): Double;
var
  Temp: Extended;
begin
  Temp := (Value / (2 * Pi)) + 0.5;
  Temp := Temp - Floor(Temp);
  if Temp = 0 then
    Result := Pi
  else
    Result := (Temp - 0.5) * (2 * Pi);
end;

function NormalizeSignedRad(const Value: Extended): Extended;
var
  Temp: Extended;
begin
  Temp := (Value / (2 * Pi)) + 0.5;
  Temp := Temp - Floor(Temp);
  if Temp = 0 then
    Result := Pi
  else
    Result := (Temp - 0.5) * (2 * Pi);
end;

// 0 <= Result < 2*Pi
function NormalizeUnsignedRad(const Value: Single): Single;
var
  Temp: Extended;
begin
  Temp := Value / (2 * Pi);
  Temp := Temp - Floor(Temp);
  Result := Temp * (2 * Pi);
end;

function NormalizeUnsignedRad(const Value: Double): Double;
var
  Temp: Extended;
begin
  Temp := Value / (2 * Pi);
  Temp := Temp - Floor(Temp);
  Result := Temp * (2 * Pi);
end;

function NormalizeUnsignedRad(const Value: Extended): Extended;
var
  Temp: Extended;
begin
  Temp := Value / (2 * Pi);
  Temp := Temp - Floor(Temp);
  Result := Temp * (2 * Pi);
end;


// ***************************  Power functions (Exponentiation)  ****************************

// y = b^x  <==>  x = logb(y)
// x = log2(y) / log2(b)
// log2(y) = x * log2(b)  <==> y = 2^(x * log2(b))
// k = x * log2(b)
// y = 2^k

{$IFDEF USEFPUx87}
// Calculation with 80x87 FPU
// F2XM1 need -1 <= ST(0) <= 1
// FSCALE use the integer part of ST(1)
// k = k1 + k2, -1 <= k1 <= 1, k2 = Integer

// y = 2^(k1 + k2)
// y = 2^k1 * 2^k2

// In:  ST(0), ST(1) x resp. exponent; log2(b) resp. log2(base); order not important
// Out: ST(0) b^x
procedure InternalExp; register; assembler;       // y = b^x
asm
        // calculate k = x * log2(b)
        FMULP   ST(1), ST(0)          // log2(b) * x
        FLD     ST(0)                 // duplicate k
        // split k in integer and fraction part
        FRNDINT                       // k2 := Int(k), the rounding mode is not important
        FSUB    ST(1), ST(0)          // k1 := k - k2 (fraction part)
        FXCH    ST(1)
        // calculate 2^k1
        F2XM1                         // 2^k1 - 1
        FLD1                          // +1
        FADDP   ST(1), ST(0)
        // calculate 2^k1 * 2^k2
        FSCALE
        FSTP    ST(1)                 // clear Stack (remove k2)
        FWAIT
end;
{$ENDIF USEFPUx87}


// ****************  power function           y = Base^Exponent, Exponent = Integer
function IntPower(Base: Extended; const Exponent: Integer): Extended;
var
  i: Cardinal;
begin
  Result := 1.0;
  if Exponent < 0 then
    Base := 1 / Base;
  i := Abs(Exponent);
  while i <> 0 do begin
    while not Odd(i) do begin
      i := i shr 1;
      Base := Sqr(Base);
    end;
    i := i - 1;
    Result := Result * Base;
  end;
end;

// ****************  power function           y = Base^Exponent

function Pow(const Base, Exponent: Single): Single;
var
  B, E: Extended;
begin
  B := Base;
  E := Exponent;
  Result := Pow(B, E);
end;

function Pow(const Base, Exponent: Double): Double;
var
  B, E: Extended;
begin
  B := Base;
  E := Exponent;
  Result := Pow(B, E);
end;

{$IFDEF USEFPUx87}
function PowAsm(const Base, Exponent: Extended): Extended; register; assembler;
asm
        FLD     Exponent
        FLD1                          // 1
        FLD     Base
        FYL2X                         // 1 * log2(base)
        CALL    InternalExp
end;
{$ENDIF USEFPUx87}

function Pow(const Base, Exponent: Extended): Extended;
begin
  if Exponent = 0.0 then         // a^0 = 1
    Result := 1.0
  else if Exponent = 1.0 then
    Result := Base
  else if Base = 0.0 then begin
    if Exponent > 0.0 then
      Result := 0.0              // 0^n = 0, n <> 0
    else
      Result := PlusInfinity;    // 0^n = Inf, n < 0
  end
  else if (Frac(Exponent) = 0.0) and (Abs(Exponent) <= High(Integer)) then
    Result := IntPower(Base, Trunc(Exponent))
  else
    {$IFDEF USEFPUx87}
    Result := PowAsm(Base, Exponent);
    {$ELSE USEFPUx87~}
    Result := System.Exp(Exponent * System.Ln(Base));
    {$ENDIF ~USEFPUx87}
end;


// ************************************  Root functions  *************************************

// ****************  cube root functions      y = x^(1/3)
{$IFDEF USEFPUx87}
const
  C1_3: Extended = 1.0 / 3.0;

// In: ST(0): x
procedure InternalCbrt; register; assembler;
asm
        FTST                          // test x < 0
        FSTSW   AX                    // Status -> AX
        FABS
        FLD1                          // 1
        FXCH    ST(1)
        FYL2X                         // 1 * log2(x)
        FLD     C1_3                  // exponent = 1/3
        CALL    InternalExp
        SAHF
        JNB     @@01                  // x >= 0
        FCHS                          // Result := -Result
        FWAIT
@@01:
end;
{$ENDIF USEFPUx87}

{$IFDEF USEFPUx87}
function Cbrt(const x: Single): Single; register; assembler;
asm
        FLD     x
        CALL    InternalCbrt
end;
{$ELSE USEFPUx87~}
function Cbrt(const x: Single): Single;
begin
  Result := System.Exp(1/3 * System.Ln(Abs(x)));
  if x < 0 then
    Result := -Result;
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function Cbrt(const x: Double): Double; register; assembler;
asm
        FLD     x
        CALL    InternalCbrt
end;
{$ELSE USEFPUx87~}
function Cbrt(const x: Double): Double;
begin
  Result := System.Exp(1/3 * System.Ln(Abs(x)));
  if x < 0 then
    Result := -Result;
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function Cbrt(const x: Extended): Extended; register; assembler;
asm
        FLD     x
        CALL    InternalCbrt
end;
{$ELSE USEFPUx87~}
function Cbrt(const x: Extended): Extended;
begin
  Result := System.Exp(1/3 * System.Ln(Abs(x)));
  if x < 0 then
    Result := -Result;
end;
{$ENDIF ~USEFPUx87}


// ****************************  Exponential functions (y = b^x)  ****************************

{ TODO : Präzision verbessern }
(*
// ****************  exponential function     y = e^x
{$IFDEF USEFPUx87}
function Exp(const x: Single): Single; register; assembler;
asm
        FLD     x
        FLDL2E                // log2(e)
        CALL    InternalExp
end;
{$ELSE USEFPUx87~}
function Exp(const x: Single): Single;
begin
  Result := System.Exp(x);
end;
{$ENDIF USEFPUx87}

{$IFDEF USEFPUx87}
function Exp(const x: Double): Double; register; assembler;
asm
        FLD     x
        FLDL2E                // log2(e)
        CALL    InternalExp
end;
{$ELSE USEFPUx87~}
function Exp(const x: Double): Double;
begin
  Result := System.Exp(x);
end;
{$ENDIF USEFPUx87}

{$IFDEF USEFPUx87}
function Exp(const x: Extended): Extended; register; assembler;
asm
        FLD     x
        FLDL2E                // log2(e)
        CALL    InternalExp
end;
{$ELSE USEFPUx87~}
function Exp(const x: Extended): Extended;
begin
  Result := System.Exp(x);
end;
{$ENDIF USEFPUx87}
*)

// ****************  exponential function     y = e^x -1
function ExpM1(const x: Single): Single;
begin
  { TODO : optimieren, so macht die Funktion keinen Sinn }
  Result := System.Exp(x) - 1.0;
end;

function ExpM1(const x: Double): Double;
begin
  { TODO : optimieren, so macht die Funktion keinen Sinn }
  Result := System.Exp(x) - 1.0;
end;

function ExpM1(const x: Extended): Extended;
begin
  { TODO : optimieren, so macht die Funktion keinen Sinn }
  Result := System.Exp(x) - 1.0;
end;

// ****************  exponential function to base two    y = 2^x

{$IFDEF USEFPUx87}
function Pow2(const x: Single): Single; register; assembler;
asm
        FLD     x
        FLD1                  // log2(2) = 1
        CALL    InternalExp
end;
{$ELSE USEFPUx87~}
function Pow2(const x: Single): Single;
begin
  Result := Pow(2.0, x);
end;
{$ENDIF USEFPUx87}

{$IFDEF USEFPUx87}
function Pow2(const x: Double): Double; register; assembler;
asm
        FLD     x
        FLD1                  // log2(2) = 1
        CALL    InternalExp
end;
{$ELSE USEFPUx87~}
function Pow2(const x: Double): Double;
begin
  Result := Pow(2.0, x);
end;
{$ENDIF USEFPUx87}

{$IFDEF USEFPUx87}
function Pow2(const x: Extended): Extended; register; assembler;
asm
        FLD     x
        FLD1                  // log2(2) = 1
        CALL    InternalExp
end;
{$ELSE USEFPUx87~}
function Pow2(const x: Extended): Extended;
begin
  Result := Pow(2.0, x);
end;
{$ENDIF USEFPUx87}

// ****************  exponential function to base ten    y = 10^x
{$IFDEF USEFPUx87}
function Pow10(const x: Single): Single; register; assembler;
asm
        FLD     x
        FLDL2T                // log2(10)
        CALL    InternalExp
end;
{$ELSE USEFPUx87~}
function Pow10(const x: Single): Single;
begin
  Result := Pow(10.0, x);
end;
{$ENDIF USEFPUx87}

{$IFDEF USEFPUx87}
function Pow10(const x: Double): Double; register; assembler;
asm
        FLD     x
        FLDL2T                // log2(10)
        CALL    InternalExp
end;
{$ELSE USEFPUx87~}
function Pow10(const x: Double): Double;
begin
  Result := Pow(10.0, x);
end;
{$ENDIF USEFPUx87}

{$IFDEF USEFPUx87}
function Pow10(const x: Extended): Extended; register; assembler;
asm
        FLD     x
        FLDL2T                // log2(10)
        CALL    InternalExp
end;
{$ELSE USEFPUx87~}
function Pow10(const x: Extended): Extended;
begin
  Result := Pow(10.0, x);
end;
{$ENDIF USEFPUx87}


// *********************************  Logarithmic functions  *********************************

// Logarithm knowledge
// x = logb(y) => x = logn(y) / logn(b)
// logb(n) = 1 / logn(b) => x = logn(y) * logb(n)

// ****************  natural logarithm function          y = ln(x)
function Log(const x: Single): Single;
begin
  Result := System.Ln(x);
end;

function Log(const x: Double): Double;
begin
  Result := System.Ln(x);
end;

function Log(const x: Extended): Extended;
begin
  Result := System.Ln(x);
end;

// ****************  natural logarithm function          y = ln(1+x)
function Log1P(const x: Single): Single;
begin
  { TODO : optimieren, so macht die Funktion u.U. keinen Sinn }
  Result := System.Ln(1 + x);
end;

function Log1P(const x: Double): Double;
begin
  { TODO : optimieren, so macht die Funktion u.U. keinen Sinn }
  Result := System.Ln(1 + x);
end;

function Log1P(const x: Extended): Extended;
begin
  { TODO : optimieren, so macht die Funktion u.U. keinen Sinn }
  Result := System.Ln(1 + x);
end;

// ****************  base 2 logarithm function
{$IFDEF USEFPUx87}
// log2(x) = log2(x) * 1
function Log2(const x: Single): Single; register; assembler;
asm
        FLD1             // 1
        FLD     x
        FYL2X            // 1 * log2(Value)
        FWAIT
end;
{$ELSE USEFPUx87~}
// log2(x) = ln(x) / ln(2) = ln(x) * log2(e)
function Log2(const x: Single): Single;
begin
  Result := System.Ln(x) * MathLog2E;
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function Log2(const x: Double): Double; register; assembler;
asm
        FLD1             // 1
        FLD     x
        FYL2X            // 1 * log2(Value)
        FWAIT
end;
{$ELSE USEFPUx87~}
function Log2(const x: Double): Double;
begin
  Result := System.Ln(x) * MathLog2E;
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function Log2(const x: Extended): Extended; register; assembler;
asm
        FLD1             // 1
        FLD     x
        FYL2X            // 1 * log2(Value)
        FWAIT
end;
{$ELSE USEFPUx87~}
function Log2(const x: Extended): Extended;
begin
  Result := System.Ln(x) * MathLog2E;
end;
{$ENDIF ~USEFPUx87}


// ****************  base 10 logarithm function

{$IFDEF USEFPUx87}
// log10(x) = log2(x) / log2(10) = log2(x) * log10(2)
function Log10(const x: Single): Single; register; assembler;
asm
        FLDLG2           // log10(2)
        FLD     x
        FYL2X            // log10(2) * log2(Value)
        FWAIT
end;
{$ELSE USEFPUx87~}
// log10(x) = ln(x) / ln(10) = ln(x) * log10(e)
function Log10(const x: Single): Single;
begin
  Result := System.Ln(x) * MathLog10E;
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function Log10(const x: Double): Double; register; assembler;
asm
        FLDLG2           // log10(2)
        FLD     x
        FYL2X            // log10(2) * log2(Value)
        FWAIT
end;
{$ELSE USEFPUx87~}
function Log10(const x: Double): Double;
begin
  Result := System.Ln(x) * MathLog10E;
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function Log10(const x: Extended): Extended; register; assembler;
asm
        FLDLG2           // log10(2)
        FLD     x
        FYL2X            // log10(2) * log2(Value)
        FWAIT
end;
{$ELSE USEFPUx87~}
function Log10(const x: Extended): Extended;
begin
  Result := System.Ln(x) * MathLog10E;
end;
{$ENDIF ~USEFPUx87}


// ****************  base n logarithm function

{$IFDEF USEFPUx87}
// logn(x) = log2(x) / log2(n)
function LogN(const Base, x: Single): Single; register; assembler;
asm
        // 1 * log2(x)
        FLD1
        FLD     x
        FYL2X
        // 1 * log2(Base)
        FLD1
        FLD     Base
        FYL2X
        // log2(x) / log2(Base)
        FDIVP   ST(1), ST(0)
        FWAIT
end;
{$ELSE USEFPUx87~}
// logn(x) = ln(x) / ln(n)
function LogN(const Base, x: Single): Single;
begin
  Result := System.Ln(x) / System.Ln(Base);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function LogN(const Base, x: Double): Double; register; assembler;
asm
        // 1 * log2(x)
        FLD1
        FLD     x
        FYL2X
        // 1 * log2(Base)
        FLD1
        FLD     Base
        FYL2X
        // log2(x) / log2(Base)
        FDIVP   ST(1), ST(0)
        FWAIT
end;
{$ELSE USEFPUx87~}
function LogN(const Base, x: Double): Double;
begin
  Result := System.Ln(x) / System.Ln(Base);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function LogN(const Base, x: Extended): Extended; register; assembler;
asm
        // 1 * log2(x)
        FLD1
        FLD     x
        FYL2X
        // 1 * log2(Base)
        FLD1
        FLD     Base
        FYL2X
        // log2(x) / log2(Base)
        FDIVP   ST(1), ST(0)
        FWAIT
end;
{$ELSE USEFPUx87~}
function LogN(const Base, x: Extended): Extended;
begin
  Result := System.Ln(x) / System.Ln(Base);
end;
{$ENDIF ~USEFPUx87}


// ***********************************  Special functions  ***********************************

// ****************  Euclidean distance function

// Sqrt(X^2 + Y^2)

// |x| <= |y| -> C := |x|, D := |y|  => C <= D
// |x| >  |y| -> C := |y|, D := |x|
// Result := Sqrt((C^2 + D^2) * D^2 / D^2)
// Result := Sqrt(D^2 * (C^2 / D^2 + D^2 / D^2))
// Result := Sqrt(D^2) * Sqrt((C / D)^2 + 1)
// Result := |D| * Sqrt((C / D)^2 + 1)
function Hypot(const x, Y: Single): Single;
var
  C, D, T: Single;
begin
  C := Abs(x);
  D := Abs(y);
  if C > D then begin
    T := C;
    C := D;
    D := T;
  end;
  if D = 0 then
    Result := 0   // C <= D => C = 0, D = 0, Sqrt(0^2 + 0^2) = 0
  else
    Result := D * System.Sqrt(Sqr(C / D) + 1);
end;

function Hypot(const x, y: Double): Double;
var
  C, D, T: Double;
begin
  C := Abs(x);
  D := Abs(y);
  if C > D then begin
    T := C;
    C := D;
    D := T;
  end;
  if D = 0 then
    Result := 0   // C <= D => C = 0, D = 0, Sqrt(0^2 + 0^2) = 0
  else
    Result := D * System.Sqrt(Sqr(C / D) + 1);
end;

function Hypot(const x, y: Extended): Extended;
var
  C, D, T: Extended;
begin
  C := Abs(x);
  D := Abs(y);
  if C > D then begin
    T := C;
    C := D;
    D := T;
  end;
  if D = 0 then
    Result := 0   // C <= D => C = 0, D = 0, Sqrt(0^2 + 0^2) = 0
  else
    Result := D * System.Sqrt(Sqr(C / D) + 1);
end;

// ****************  error function
(*
function Erf(const x: Single): Single;
begin
  { TODO : implementieren }
end;

function Erf(const x: Double): Double;
begin
  { TODO : implementieren }
end;

function Erf(const x: Extended): Extended;
begin
  { TODO : implementieren }
end;

// ****************  complementary error function

function ErfC(const x: Single): Single;
begin
  Result := 1.0 - Erf(x);
end;

function ErfC(const x: Double): Double;
begin
  Result := 1.0 - Erf(x);
end;

function ErfC(const x: Extended): Extended;
begin
  Result := 1.0 - Erf(x);
end;
*)

// *********************************  Value sign functions  **********************************

// ****************  absolute value function

function FAbs(const x: Single): Single;
begin
  Result := System.Abs(x);
end;

function FAbs(const x: Double): Double;
begin
  Result := System.Abs(x);
end;

function FAbs(const x: Extended): Extended;
begin
  Result := System.Abs(x);
end;

function LAbs(const x: LongInt): LongInt;
begin
  Result := System.Abs(x);
end;

// ****************  number manipulation function

// return the x parameter with the same sign as the y parameter
function CopySign(const x, y: Single): Single;
begin
  if y >= 0 then
    Result := Abs(x)
  else if x < 0 then   // y < 0, x < 0
    Result := x
  else                 // y < 0, x >= 0
    Result := -x;
{  Result := x;
  TSingleRec(Result).V := (TSingleRec(Result).V and not MaskSingleSignBit) or
                          (TSingleRec(y).V and MaskSingleSignBit); }
end;

function CopySign(const x, y: Double): Double;
begin
  if y >= 0 then
    Result := Abs(x)
  else if x < 0 then   // y < 0, x < 0
    Result := x
  else                 // y < 0, x >= 0
    Result := -x;
{  Result := x;
  TDoubleRec(Result).V1 := (TDoubleRec(Result).V1 and not MaskDoubleSignBit) or
                           (TDoubleRec(y).V1 and MaskDoubleSignBit); }
end;

function CopySign(const x, y: Extended): Extended;
begin
  if y >= 0 then
    Result := Abs(x)
  else if x < 0 then   // y < 0, x < 0
    Result := x
  else                 // y < 0, x >= 0
    Result := -x;
{  Result := x;
  TDoubleExtendedRec(Result).SignExponent :=
      (TDoubleExtendedRec(Result).SignExponent and not MaskDoubleExtendedSignBit) or
      (TDoubleExtendedRec(y).SignExponent and MaskDoubleExtendedSignBit); }
end;


// ****************  test sign

// Test for negative number
function SignBit(const x: Single): Boolean; overload;
begin
  Byte(Result) := (TSingleRec(x).V and MaskSingleSignBit) shr 31;
end;

function SignBit(const x: Double): Boolean; overload;
begin
  Byte(Result) := (TDoubleRec(x).V1 and MaskDoubleSignBit) shr 31;
end;

function SignBit(const x: Extended): Boolean; overload;
begin
  Byte(Result) := (TDoubleExtendedRec(x).SignExponent and MaskDoubleExtendedSignBit) shr 15;
end;


// ****************  sign function

// return -1, if x < 0; 0 if x = 0; +1 if x > 0
function Sign(const x: Single): Integer;
begin
  if x = 0 then
    Result := 0
  else if x < 0 then
    Result := -1
  else
    Result := 1;
end;

function Sign(const x: Double): Integer;
begin
  if x = 0 then
    Result := 0
  else if x < 0 then
    Result := -1
  else
    Result := 1;
end;

function Sign(const x: Extended): Integer;
begin
  if x = 0 then
    Result := 0
  else if x < 0 then
    Result := -1
  else
    Result := 1;
end;


// *****************************  Rounding - Internal functions  *****************************

{$IFDEF USEFPUx87}
const
  // FPU Control Word

  // Bit     12: Infinity = 1
  // Bit 11..10: RC Rounding Control
  // Bit  9.. 8: PC Precision Control
  // Bit      5: PM Precision Exception Mask
  // Bit      4: UM Underflow Exception Mask
  // Bit      3: OM Overflow Exception Mask
  // Bit      2: ZM Zero-Divide Exception Mask
  // Bit      1: DM Denormalized Exception Mask
  // Bit      0: IM Invalid Operation Exception Mask

  // Round to nearest
  // RC = 00 (Round to nearest)
  // PC = 11 (Double-Extended Precision)
  // PM, UM, ZM
  FPUControlWordRoundToNearest = $1332;

  // Round down
  // RC = 01 (Round down)
  // PC = 11 (Double-Extended Precision)
  // PM, UM, ZM
  FPUControlWordRoundDown = $1732;

  // Round up
  // RC = 10 (Round up)
  // PC = 11 (Double-Extended Precision)
  // PM, UM, ZM
  FPUControlWordRoundUp = $1B32;

  // Round toward zero
  // RC = 11 (Round toward zero)
  // PC = 11 (Double-Extended Precision)
  // PM, UM, ZM
  FPUControlWordRoundTowardZero = $1F32;

// ST(0): Value
// EAX: ControlWord
// return rounded value in ST(0)
procedure InternalRoundFloat{ControlWord: LongWord}; register; assembler;
asm
        SUB     ESP, 4
        FWAIT
        FNSTCW  [ESP]            // save ControlWord
        MOV     [ESP + 2], AX    // set new ControlWord
        FWAIT
        FLDCW   [ESP + 2]
        FRNDINT                  // round the value
        FLDCW   [ESP]            // load old ControlWord
        FWAIT
        ADD     ESP, 4
        FWAIT
end;

// ST(0): Value
// EAX: ControlWord
// return rounded value in EAX
procedure InternalRoundInt32{ControlWord: LongWord}; register; assembler;
asm
        SUB     ESP, 8
        FWAIT
        FNSTCW  [ESP]                // save ControlWord
        MOV     [ESP + 2], AX        // set new ControlWord
        FWAIT
        FLDCW   [ESP + 2]
        FISTP   DWORD PTR [ESP + 4]  // round the value
        FLDCW   [ESP]                // load old ControlWord
        FWAIT
        MOV     EAX, [ESP + 4]       // result -> EAX
        ADD     ESP, 8
end;

// ST(0): Value
// EAX: ControlWord
// return rounded value in EDX:EAX
procedure InternalRoundInt64{ControlWord: LongWord}; register; assembler;
asm
        SUB     ESP, 12
        FWAIT
        FNSTCW  [ESP]                // save ControlWord
        MOV     [ESP + 2], AX        // set new ControlWord
        FWAIT
        FLDCW   [ESP + 2]
        FISTP   QWORD PTR [ESP + 4]  // round the value
        FLDCW   [ESP]                // load old ControlWord
        FWAIT
        MOV     EAX, [ESP + 4]       // result -> EDX:EAX
        MOV     EDX, [ESP + 8]
        ADD     ESP, 12
end;
{$ENDIF USEFPUx87}

// *******************************  Nearest integer functions  *******************************

// ****************  ceiling value function

{$IFDEF USEFPUx87}
function Ceil(const Value: Extended): Extended; register; assembler;
asm
        FLD     Value
        MOV     EAX, FPUControlWordRoundUp
        CALL    InternalRoundFloat
end;
{$ELSE USEFPUx87~}
function Ceil(const Value: Extended): Extended;
begin
  Result := System.Int(Value);
  if Value > Result then
    Result := Result + 1;
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function LCeil(const Value: Extended): LongInt; register; assembler;
asm
        FLD     Value
        MOV     EAX, FPUControlWordRoundUp
        CALL    InternalRoundInt32
end;
{$ELSE USEFPUx87~}
function LCeil(const Value: Extended): LongInt;
begin
  Result := Trunc(Value);
  if System.Frac(Value) > 0 then
    System.Inc(Result);
end;
{$ENDIF ~USEFPUx87}


{$IFDEF USEFPUx87}
function LLCeil(const Value: Extended): Int64; register; assembler;
asm
        FLD     Value
        MOV     EAX, FPUControlWordRoundUp
        CALL    InternalRoundInt64
end;
{$ELSE USEFPUx87~}
function LLCeil(const Value: Extended): Int64;
begin
  Result := Trunc(Value);
  if System.Frac(Value) > 0 then
    Inc(Result);
end;
{$ENDIF ~USEFPUx87}


// ****************  floor function

{$IFDEF USEFPUx87}
function Floor(const Value: Extended): Extended; register; assembler;
asm
        FLD     Value
        MOV     EAX, FPUControlWordRoundDown
        CALL    InternalRoundFloat
end;
{$ELSE USEFPUx87~}
function Floor(const Value: Extended): Extended;
begin
  Result := System.Int(Value);
  if Value < Result then
    Result := Result - 1;
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function LFloor(const Value: Extended): LongInt; register; assembler;
asm
        FLD     Value
        MOV     EAX, FPUControlWordRoundDown
        CALL    InternalRoundInt32
end;
{$ELSE USEFPUx87~}
function LFloor(const Value: Extended): LongInt;
begin
  Result := Trunc(Value);
  if System.Frac(Value) < 0 then
    Dec(Result);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function LLFloor(const Value: Extended): Int64; register; assembler;
asm
        FLD     Value
        MOV     EAX, FPUControlWordRoundDown
        CALL    InternalRoundInt64
end;
{$ELSE USEFPUx87~}
function LLFloor(const Value: Extended): Int64;
begin
  Result := Trunc(Value);
  if System.Frac(Value) < 0 then
    Dec(Result);
end;
{$ENDIF ~USEFPUx87}


// ****************  "banker's round" function
// Round Value to nearest integral value, rounding halfway cases away from zero
{$IFDEF USEFPUx87}
function RoundToNearest(const Value: Extended): Extended; register; assembler;
asm
        FLD     Value
        MOV     EAX, FPUControlWordRoundToNearest
        CALL    InternalRoundFloat
end;
{$ELSE USEFPUx87~}
function RoundToNearest(const Value: Extended): Extended;
var
  V: Extended;
begin
  V := Abs(Value) + 0.5;
  Result := System.Int(V);
  if Result = V then begin  // x.5
    if Odd(Trunc(Result)) then
      Result := Result - 1.0;
  end;
  if Value < 0 then
    Result := -Result;
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function LRoundToNearest(const Value: Extended): Integer; register; assembler;
asm
        FLD     Value
        MOV     EAX, FPUControlWordRoundToNearest
        CALL    InternalRoundInt32
end;
{$ELSE USEFPUx87~}
function LRoundToNearest(const Value: Extended): Integer;
var
  V: Extended;
begin
  V := Abs(Value) + 0.5;
  Result := Trunc(V);
  if Result = V then begin  // x.5
    if Odd(Result) then
      Dec(Result);
  end;
  if Value < 0 then
    Result := -Result;
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function LLRoundToNearest(const Value: Extended): Int64; register; assembler;
asm
        FLD     Value
        MOV     EAX, FPUControlWordRoundToNearest
        CALL    InternalRoundInt64
end;
{$ELSE USEFPUx87~}
function LLRoundToNearest(const Value: Extended): Int64;
var
  V: Extended;
begin
  V := Abs(Value) + 0.5;
  Result := Trunc(V);
  if Result = V then begin  // x.5
    if Odd(Result) then
      Dec(Result);
  end;
  if Value < 0 then
    Result := -Result;
end;
{$ENDIF ~USEFPUx87}


// ****************  round to truncated integer value
// renamed from Trunc, LTrunc, LLTrunc to avoid problems with the Pascal function Trunc
// Round Value to the integral value in floating-point format nearest but not larger in magnitude
{$IFDEF USEFPUx87}
function RoundTowardZero(const Value: Extended): Extended; register; assembler;
asm
        FLD     Value
        MOV     EAX, FPUControlWordRoundTowardZero
        CALL    InternalRoundFloat
end;
{$ELSE USEFPUx87~}
function RoundTowardZero(const Value: Extended): Extended;
begin
  Result := System.Int(Value);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function LRoundTowardZero(const Value: Extended): Integer; register; assembler;
asm
        FLD     Value
        MOV     EAX, FPUControlWordRoundTowardZero
        CALL    InternalRoundInt32
end;
{$ELSE USEFPUx87~}
function LRoundTowardZero(const Value: Extended): Integer;
begin
  Result := Trunc(Value);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function LLRoundTowardZero(const Value: Extended): Int64; register; assembler;
asm
        FLD     Value
        MOV     EAX, FPUControlWordRoundTowardZero
        CALL    InternalRoundInt64
end;
{$ELSE USEFPUx87~}
function LLRoundTowardZero(const Value: Extended): Int64;
begin
  Result := Trunc(Value);
end;
{$ENDIF ~USEFPUx87}


// ****************  round to nearest integer value using specific rounding direction
// Round Value to nearest integral value according to rounding direction in Mode
{$IFDEF USEFPUx87}
function RInt(const Value: Extended; Mode: TRoundingMode = rmNearest): Extended; register; assembler;
asm
        SHL     EAX, 10          // rounding mode -> Bit 11..10
        AND     EAX, $0C00       // mask rounding-control (RC) field
        OR      EAX, FPUControlWordRoundToNearest
        FLD     Value
        CALL    InternalRoundFloat
end;
{$ELSE USEFPUx87~}
function RInt(const Value: Extended; Mode: TRoundingMode = rmNearest): Extended;
begin
  case Mode of
    rmNearest:
      Result := RoundToNearest(Value);
    rmDown:
      Result := Floor(Value);
    rmUp:
      Result := Ceil(Value);
  else
    Result := RoundTowardZero(Value);
  end;
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function LRInt(const Value: Extended; Mode: TRoundingMode = rmNearest): LongInt; register; assembler;
asm
        SHL     EAX, 10          // rounding mode -> Bit 11..10
        AND     EAX, $0C00       // mask rounding-control (RC) field
        OR      EAX, FPUControlWordRoundToNearest
        FLD     Value
        CALL    InternalRoundInt32
end;
{$ELSE USEFPUx87~}
function LRInt(const Value: Extended; Mode: TRoundingMode = rmNearest): LongInt;
begin
  case Mode of
    rmNearest:
      Result := LRoundToNearest(Value);
    rmDown:
      Result := LFloor(Value);
    rmUp:
      Result := LCeil(Value);
  else
    Result := LRoundTowardZero(Value);
  end;
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function LLRInt(const Value: Extended; Mode: TRoundingMode = rmNearest): Int64; register; assembler;
asm
        SHL     EAX, 10          // rounding mode -> Bit 11..10
        AND     EAX, $0C00       // mask rounding-control (RC) field
        OR      EAX, FPUControlWordRoundToNearest
        FLD     Value
        CALL    InternalRoundInt64
end;
{$ELSE USEFPUx87~}
function LLRInt(const Value: Extended; Mode: TRoundingMode = rmNearest): Int64;
begin
  case Mode of
    rmNearest:
      Result := LLRoundToNearest(Value);
    rmDown:
      Result := LLFloor(Value);
    rmUp:
      Result := LLCeil(Value);
  else
    Result := LLRoundTowardZero(Value);
  end;
end;
{$ENDIF ~USEFPUx87}


// ****************
// Example: Fraction = 0.25 => Results are ..., -0.25, 0.0, 0.25, 0.5, 0.75, 1.0, ...

// ST(0): Value
// ST(1): Fraction
// EAX: rounding mode
{$IFDEF USEFPUx87}
procedure InternalRoundToFraction; register; assembler;
asm
        FDIV    ST(0), ST(1)     // Result := Value / Fraction
        SHL     EAX, 10          // rounding mode -> Bit 11..10
        AND     EAX, $0C00       // mask rounding-control (RC) field
        OR      EAX, FPUControlWordRoundToNearest
        SUB     ESP, 4
        FWAIT
        FNSTCW  [ESP]            // save ControlWord
        MOV     [ESP + 2], AX    // set new ControlWord
        FWAIT
        FLDCW   [ESP + 2]
        FRNDINT                  // round the value
        FWAIT
        FLDCW   [ESP]            // load old ControlWord
        FWAIT
        ADD     ESP, 4
        FMULP   ST(1), ST(0)     // Result := Result * Fraction
        FWAIT
end;
{$ENDIF USEFPUx87}

{$IFDEF USEFPUx87}
function RoundToFraction(const Value, Fraction: Single; Mode: TRoundingMode = rmNearest): Single; register; assembler;
// EAX: fraction
// EDX: rounding mode
asm
        FLD     Fraction                 // Value -> ST(1)
        FLD     Value                    // Value -> ST(0)
        CALL    InternalRoundToFraction
end;
{$ELSE USEFPUx87~}
function RoundToFraction(const Value, Fraction: Single; Mode: TRoundingMode = rmNearest): Single;
var
  V, F: Extended;
begin
  V := Value;
  F := Fraction;
  Result := RoundToFraction(V, F, Mode);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function RoundToFraction(const Value, Fraction: Double; Mode: TRoundingMode = rmNearest): Double; register; assembler;
// EAX: fraction
// EDX: rounding mode
asm
        FLD     Fraction                 // Value -> ST(1)
        FLD     Value                    // Value -> ST(0)
        CALL    InternalRoundToFraction
end;
{$ELSE USEFPUx87~}
function RoundToFraction(const Value, Fraction: Double; Mode: TRoundingMode = rmNearest): Double;
var
  V, F: Extended;
begin
  V := Value;
  F := Fraction;
  Result := RoundToFraction(V, F, Mode);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function RoundToFraction(const Value, Fraction: Extended; Mode: TRoundingMode = rmNearest): Extended; register; assembler;
// EAX: fraction
// EDX: rounding mode
asm
        FLD     Fraction                 // Value -> ST(1)
        FLD     Value                    // Value -> ST(0)
        CALL    InternalRoundToFraction
end;
{$ELSE USEFPUx87~}
function RoundToFraction(const Value, Fraction: Extended; Mode: TRoundingMode = rmNearest): Extended;
begin
  Result := Value / Fraction;
  case Mode of
    rmNearest:
      Result := RoundToNearest(Result);
    rmDown:
      Result := Floor(Result);
    rmUp:
      Result := Ceil(Result);
  else
    Result := RoundTowardZero(Result);
  end;
  Result := Result * Fraction;
end;
{$ENDIF ~USEFPUx87}


// **********************************  Remainder functions  **********************************

// ****************  floating-point remainder value function

// Partial Remainder, ANSI C
{$IFDEF USEFPUx87}
function FMod(const x, y: Single): Single; register; assembler;
asm
        FLD     y
        FLD     x
        FPREM
        FFREE   ST(1)
        FWAIT
end;
{$ELSE USEFPUx87~}
function FMod(const x, y: Single): Single;
var
  v: Extended;
begin
  v := System.Int(x / y);
  Result := x - (v * y);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function FMod(const x, y: Double): Double; register; assembler;
asm
        FLD     y
        FLD     x
        FPREM
        FFREE   ST(1)
        FWAIT
end;
{$ELSE USEFPUx87~}
function FMod(const x, y: Double): Double;
var
  v: Extended;
begin
  v := System.Int(x / y);
  Result := x - (v * y);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function FMod(const x, y: Extended): Extended; register; assembler;
asm
        FLD     y
        FLD     x
        FPREM
        FFREE   ST(1)
        FWAIT
end;
{$ELSE USEFPUx87~}
function FMod(const x, y: Extended): Extended;
var
  v: Extended;
begin
  v := System.Int(x / y);
  Result := x - (v * y);
end;
{$ENDIF ~USEFPUx87}

// Partial Remainder, IEEE-754
{$IFDEF USEFPUx87}
function Remainder(const x, y: Single): Single; register; assembler;
asm
        FLD     y
        FLD     x
        FPREM1
        FFREE   ST(1)
        FWAIT
end;
{$ELSE USEFPUx87~}
function Remainder(const x, y: Single): Single;
var
  v: Extended;
begin
  v := RoundToNearest(x / y);
  Result := x - (v * y);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function Remainder(const x, y: Double): Double; register; assembler;
asm
        FLD     y
        FLD     x
        FPREM1
        FFREE   ST(1)
        FWAIT
end;
{$ELSE USEFPUx87~}
function Remainder(const x, y: Double): Double;
var
  v: Extended;
begin
  v := RoundToNearest(x / y);
  Result := x - (v * y);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function Remainder(const x, y: Extended): Extended; register; assembler;
asm
        FLD     y
        FLD     x
        FPREM1
        FFREE   ST(1)
        FWAIT
end;
{$ELSE USEFPUx87~}
function Remainder(const x, y: Extended): Extended;
var
  v: Extended;
begin
  v := RoundToNearest(x / y);
  Result := x - (v * y);
end;
{$ENDIF ~USEFPUx87}

// ****************  remainder functions
// Compute remainder of X and Y and put in *QUO a value with sign of x/y and magnitude congruent
// 'mod 2^n' to the magnitude of the integral quotient x/y, with n >= 3
{ TODO : implementieren }
{
function RemQuo(const x, y: Single; out quo: Integer): Single; overload;
function RemQuo(const x, y: Double; out quo: Integer): Double; overload;
function RemQuo(const x, y: Extended; out quo: Integer): Extended; overload;
}

// ****************************  Floating point number functions  ****************************

// ****************  extract mantissa and exponent from a floating-point number

{$IFDEF USEFPUx87}
// ST(0): Value
// EAX: Pointer to Exponent
procedure InternalFrExp; register; assembler;
asm
        MOV     EDX, EAX
        MOV     DWORD PTR [EDX], 0    // if X = 0, return 0
        FTST                          // test for Value = 0
        FSTSW   AX
        FWAIT
        SAHF
        JZ      @@01                  // Value = 0

        FXTRACT                       // significand 1.xxxE0 (fraction part * 2) -> ST(0)
                                      // exponent (exponent part - 1) -> ST(1)
        FLD1                          // load -1
        FCHS
        FXCH    ST(1)
        FSCALE                        // scale fraction part (-> 0.1xxxE0 resp. 1.xxxE-1)
        FXCH    ST(2)
        FISTP   DWORD PTR [EDX]       // store exponent part - 1
        FWAIT
        INC     DWORD PTR [EDX]       // scale exponent part
        FSTP    ST(0)                 // pop -1
@@01:   FWAIT
end;
{$ENDIF USEFPUx87}

{$IFDEF USEFPUx87}
function FrExp(const Value: Single; out Exponent: Integer): Single; register; assembler;
asm
        FLD     Value
        CALL    InternalFrExp
end;
{$ELSE USEFPUx87~}
function FrExp(const Value: Single; out Exponent: Integer): Single;
var
  V: Extended;
begin
  V := Value;
  Result := FrExp(V, Exponent);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function FrExp(const Value: Double; out Exponent: Integer): Double; register; assembler;
asm
        FLD     Value
        CALL    InternalFrExp
end;
{$ELSE USEFPUx87~}
function FrExp(const Value: Double; out Exponent: Integer): Double;
var
  V: Extended;
begin
  V := Value;
  Result := FrExp(V, Exponent);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function FrExp(const Value: Extended; out Exponent: Integer): Extended; register; assembler;
asm
        FLD     Value
        CALL    InternalFrExp
end;
{$ELSE USEFPUx87~}
function FrExp(const Value: Extended; out Exponent: Integer): Extended;
begin
  Exponent := 0;
  Result := Abs(Value);
  if Result > 0 then begin      // Value = 0 return 0 and Exponent = 0
    if Result < 0.5 then begin
      repeat
        Result := Result * 2;
        Exponent := Exponent - 1;
      until Result >= 0.5;
    end
    else begin  // Result >= 0.5
      while Result >= 1.0 do begin
        Result := Result * 0.5;
        Exponent := Exponent + 1;
      end;
    end;
    if Value < 0 then
      Result := -Result;
  end;
end;
{$ENDIF ~USEFPUx87}


// ****************  load exponent of a floating-point number

{$IFDEF USEFPUx87}
function LdExp(const Fraction: Single; const Exponent: LongInt): Single; register; assembler;
asm
        PUSH    EAX               // Exponent -> memory
        FWAIT
        FILD    DWORD PTR [ESP]   // load Exponent
        FWAIT
        POP     EAX
        FLD     Fraction
        FSCALE                    // Value * 2^Exponent
        FSTP    ST(1)
        FWAIT
end;
{$ELSE USEFPUx87~}
function LdExp(const Fraction: Single; const Exponent: LongInt): Single;
begin
  Result := Fraction * IntPower(2.0, Exponent);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function LdExp(const Fraction: Double; const Exponent: LongInt): Double; register; assembler;
asm
        PUSH    EAX               // Exponent -> memory
        FWAIT
        FILD    DWORD PTR [ESP]   // load Exponent
        FWAIT
        POP     EAX
        FLD     Fraction
        FSCALE                    // Value * 2^Exponent
        FSTP    ST(1)
        FWAIT
end;
{$ELSE USEFPUx87~}
function LdExp(const Fraction: Double; const Exponent: LongInt): Double;
begin
  Result := Fraction * IntPower(2.0, Exponent);
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function LdExp(const Fraction: Extended; const Exponent: LongInt): Extended; register; assembler;
asm
        PUSH    EAX               // Exponent -> memory
        FWAIT
        FILD    DWORD PTR [ESP]   // load Exponent
        FWAIT
        POP     EAX
        FLD     Fraction
        FSCALE                    // Value * 2^Exponent
        FSTP    ST(1)
        FWAIT
end;
{$ELSE USEFPUx87~}
function LdExp(const Fraction: Extended; const Exponent: LongInt): Extended;
begin
  Result := Fraction * IntPower(2.0, Exponent);
end;
{$ENDIF ~USEFPUx87}


// ****************  decompose a floating-point number

{$IFDEF USEFPUx87}
// ST(0): Value
// return ST(0) Integer part
//        ST(1) Fraction
procedure InternalModF; register; assembler;
asm
        MOV     EDX, EAX
        FLD     ST(0)                // duplicate Value
        MOV     EAX, FPUControlWordRoundTowardZero
        FWAIT
        CALL    InternalRoundFloat   // ST(0) Integer part
        FSUB    ST(1), ST(0)         // ST(1) Fraction
end;
{$ENDIF USEFPUx87}

{$IFDEF USEFPUx87}
function ModF(const Value: Single; out IPart: Single): Single; register; assembler;
asm
        FLD     Value
        CALL    InternalModF
        FSTP    DWORD PTR [EDX]
        FWAIT
end;
{$ELSE USEFPUx87~}
function ModF(const Value: Single; out IPart: Single): Single;
begin
  IPart := Int(Value);
  Result := Value - IPart;
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function ModF(const Value: Double; out IPart: Double): Double; register; assembler;
asm
        FLD     Value
        CALL    InternalModF
        FSTP    QWORD PTR [EDX]
        FWAIT
end;
{$ELSE USEFPUx87~}
function ModF(const Value: Double; out IPart: Double): Double;
begin
  IPart := Int(Value);
  Result := Value - IPart;
end;
{$ENDIF ~USEFPUx87}

{$IFDEF USEFPUx87}
function ModF(const Value: Extended; out IPart: Extended): Extended; register; assembler;
asm
        FLD     Value
        CALL    InternalModF
        FSTP    TBYTE PTR [EDX]
        FWAIT
end;
{$ELSE USEFPUx87~}
function ModF(const Value: Extended; out IPart: Extended): Extended;
begin
  IPart := Int(Value);
  Result := Value - IPart;
end;
{$ENDIF ~USEFPUx87}


// ******************  Maximum, minimum, and positive difference functions  ******************

// ****************  determine maximum numeric value of two floating-point numbers

function FMax(const x, y: Single): Single;
begin
  if x > y then
    Result := x
  else
    Result := y;
end;

function FMax(const x, y: Double): Double;
begin
  if x > y then
    Result := x
  else
    Result := y;
end;

function FMax(const x, y: Extended): Extended;
begin
  if x > y then
    Result := x
  else
    Result := y;
end;

function Max(const x, y: Integer): Integer;
begin
  if x > y then
    Result := x
  else
    Result := y;
end;

function Max(const x, y: Int64): Int64;
begin
  if x > y then
    Result := x
  else
    Result := y;
end;


// ****************  determine minimum numeric value of two floating-point numbers

function FMin(const x, y: Single): Single;
begin
  if x < y then
    Result := x
  else
    Result := y;
end;

function FMin(const x, y: Double): Double;
begin
  if x < y then
    Result := x
  else
    Result := y;
end;

function FMin(const x, y: Extended): Extended;
begin
  if x < y then
    Result := x
  else
    Result := y;
end;

function Min(const x, y: Integer): Integer;
begin
  if x < y then
    Result := x
  else
    Result := y;
end;

function Min(const x, y: Int64): Int64; 
begin
  if x < y then
    Result := x
  else
    Result := y;
end;


// ****************  return positive difference between x and y
function FDim(const x, y: Single): Single;
begin
  if x > y then
    Result := x - y
  else
    Result := 0.0;
end;

function FDim(const x, y: Double): Double;
begin
  if x > y then
    Result := x - y
  else
    Result := 0.0;
end;

function FDim(const x, y: Extended): Extended;
begin
  if x > y then
    Result := x - y
  else
    Result := 0.0;
end;

// ****************  floating-point multiply-add
// (x * y) + z
function Fma(const x, y, z: Single): Single;
begin
  Result := (x * y) + z;
end;

function Fma(const x, y, z: Double): Double;
begin
  Result := (x * y) + z;
end;

function Fma(const x, y, z: Extended): Extended;
begin
  Result := (x * y) + z;
end;


function IsSameValue(const Value1, Value2: Single; const Epsilon: Single): Boolean;
begin
  Result := Abs(Value1 - Value2) <= Epsilon;
end;

function IsSameValue(const Value1, Value2: Double; const Epsilon: Double): Boolean;
begin
  Result := Abs(Value1 - Value2) <= Epsilon;
end;

function IsSameValue(const Value1, Value2: Extended; const Epsilon: Extended): Boolean;
begin
  Result := Abs(Value1 - Value2) <= Epsilon;
end;

// ***********************************  Array Funktionen  ************************************

function ArrayGetMinIndex(const Values: array of Single): Integer;
var
  i: Integer;
  V, Min: Single;
begin
  Result := Low(Values);
  Min := Values[Low(Values)];
  for i := Low(Values) + 1 to High(Values) do begin
    V := Values[i];
    if Min > V then begin
      Min := V;
      Result := i;
    end;
  end;
end;

function ArrayGetMaxIndex(const Values: array of Single): Integer;
var
  i: Integer;
  V, Max: Single;
begin
  Result := Low(Values);
  Max := Values[Low(Values)];
  for i := Low(Values) + 1 to High(Values) do begin
    V := Values[i];
    if Max < V then begin
      Max := V;
      Result := i;
    end;
  end;
end;

// Result 0..Count-1
function NormalizePeriodicValue(Value, Count: Integer): Integer;
begin
  Result := Value mod Count;
  if Result < 0 then
    Result := Result + Count;
end;

// *******************************************************************************************

//  History:
//  2005-07-27, Peter J. Haas
//   - bugfix IsNan (Double)
//
//  2005-04-03, Peter J. Haas
//   - some modifications to remove FPC hints

end.
