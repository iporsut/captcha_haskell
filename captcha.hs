module CaptchaModule where
import Data.List

data Captcha leftOperand operator rightOperand = Captcha leftOperand operator rightOperand

instance (Show l, Show o, Show r) => Show (Captcha l o r) where
	show (Captcha l o r) = show l ++ " " ++ show o ++ " " ++ show r

answerCaptcha :: (Operand l, Operator o, Operand r,  Num a) => (Captcha l o r) -> a
answerCaptcha (Captcha leftOperand operator rightOperand) = (apply operator) leftOperand rightOperand 

class Operand a where
	toInt :: (Num b) => a -> b

data TextOperand = TextZero |
			TextOne |
			TextTwo |
			TextThree |
			TextFour |
			TextFive |
			TextSix |
			TextSeven |
			TextEight |
			TextNine
			deriving (Enum, Bounded)

instance Show TextOperand where
	show TextZero = "Zero"
	show TextOne = "One"
	show TextTwo = "Two"
	show TextThree = "Three"
	show TextFour = "Four"
	show TextFive = "Five"
	show TextSix = "Six"
	show TextSeven = "Seven"
	show TextEight = "Eight"
	show TextNine = "Nine"

instance Operand TextOperand where
	toInt TextZero = 0
	toInt TextOne = 1
	toInt TextTwo = 2
	toInt TextThree = 3 
	toInt TextFour = 4
	toInt TextFive = 5
	toInt TextSix = 6
	toInt TextSeven = 7
	toInt TextEight = 8
	toInt TextNine = 9

data NumberOperand = NumberZero |
			NumberOne |
			NumberTwo |
			NumberThree |
			NumberFour |
			NumberFive |
			NumberSix |
			NumberSeven |
			NumberEight |
			NumberNine
			deriving (Enum, Bounded)

instance Show NumberOperand where
	show NumberZero = "0"
	show NumberOne = "1"
	show NumberTwo = "2"
	show NumberThree = "3"
	show NumberFour = "4"
	show NumberFive = "5"
	show NumberSix = "6"
	show NumberSeven = "7"
	show NumberEight = "8"
	show NumberNine = "9"

instance Operand NumberOperand where
	toInt NumberZero = 0
	toInt NumberOne = 1
	toInt NumberTwo = 2
	toInt NumberThree = 3 
	toInt NumberFour = 4
	toInt NumberFive = 5
	toInt NumberSix = 6
	toInt NumberSeven = 7
	toInt NumberEight = 8
	toInt NumberNine = 9

class Operator o where
	realOp :: (Num a) => o -> (a -> a -> a)
	apply :: (Operand a, Operand b, Num c) => o -> a -> b -> c

data TextOperator = Add | Sub | Mul deriving (Enum, Bounded)

instance Show TextOperator where
	show Add = "+"
	show Sub = "-"
	show Mul = "x"

instance Operator TextOperator where
	realOp Add = (+)
	realOp Sub = (-)
	realOp Mul = (*)
	apply operator a b = (realOp operator) (toInt a) (toInt b)
