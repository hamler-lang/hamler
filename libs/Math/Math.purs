-- | Wraps the math functions and constants from Javascript's built-in `Math` object.
-- | See [Math Reference at MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math).
module Math where

-- | An alias to make types in this module more explicit.
type Radians = Number

-- | Returns the absolute value of the argument.
foreign import abs :: Number -> Number

-- | Returns the inverse cosine of the argument.
foreign import acos :: Number -> Radians

-- | Returns the inverse sine of the argument.
foreign import asin :: Number -> Radians

-- | Returns the inverse tangent of the argument.
foreign import atan :: Number -> Radians

-- | Four-quadrant tangent inverse. Given the arguments `y` and `x`, returns
-- | the inverse tangent of `y / x`, where the signs of both arguments are used
-- | to determine the sign of the result.
-- | If the first argument is negative, the result will be negative.
-- | The result is the angle between the positive x axis and  a point `(x, y)`.
foreign import atan2 :: Number -> Number -> Radians

-- | Returns the smallest integer not smaller than the argument.
foreign import ceil :: Number -> Number

-- | Returns the cosine of the argument.
foreign import cos :: Radians -> Number

-- | Returns `e` exponentiated to the power of the argument.
foreign import exp :: Number -> Number

-- | Returns the largest integer not larger than the argument.
foreign import floor :: Number -> Number

-- | Returns the natural logarithm of a number.
foreign import log :: Number -> Number

-- | Returns the largest of two numbers.
foreign import max :: Number -> Number -> Number

-- | Returns the smallest of two numbers.
foreign import min :: Number -> Number -> Number

-- | Return  the first argument exponentiated to the power of the second argument.
foreign import pow :: Number -> Number -> Number

-- | Returns the integer closest to the argument.
foreign import round :: Number -> Number

-- | Returns the sine of the argument.
foreign import sin :: Radians -> Number

-- | Returns the square root of the argument.
foreign import sqrt :: Number -> Number

-- | Returns the tangent of the argument.
foreign import tan :: Radians -> Number

-- | Truncates the decimal portion of a number. Equivalent to `floor` if the
-- | number is positive, and `ceil` if the number is negative.
foreign import trunc :: Number -> Number

-- | Computes the remainder after division, wrapping Javascript's `%` operator.
foreign import remainder :: Number -> Number -> Number

infixl 7 remainder as %

-- | The base of natural logarithms, *e*, around 2.71828.
foreign import e :: Number

-- | The natural logarithm of 2, around 0.6931.
foreign import ln2 :: Number

-- | The natural logarithm of 10, around 2.3025.
foreign import ln10 :: Number

-- | The base 2 logarithm of `e`, around 1.4426.
foreign import log2e :: Number

-- | Base 10 logarithm of `e`, around 0.43429.
foreign import log10e :: Number

-- | The ratio of the circumference of a circle to its diameter, around 3.14159.
foreign import pi :: Number

-- | The ratio of the circumference of a circle to its radius, around 6.283185.
foreign import tau :: Number

-- | The Square root of one half, around 0.707107.
foreign import sqrt1_2 :: Number

-- | The square root of two, around 1.41421.
foreign import sqrt2 :: Number
