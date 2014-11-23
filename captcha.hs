module CaptchaModule where

data Operand = TextOperand Int | NumberOperand Int
		deriving (Show)

showOperand:: Operand -> String
showOperand (TextOperand 0) = "Zero"
showOperand (TextOperand 1) = "One"
showOperand (TextOperand 2) = "Two"
showOperand (TextOperand 3) = "Three"
showOperand (TextOperand 4) = "Four"
showOperand (TextOperand 5) = "Five"
showOperand (TextOperand 6) = "Six"
showOperand (TextOperand 7) = "Seven"
showOperand (TextOperand 8) = "Eight"
showOperand (TextOperand 9) = "Nine"
showOperand (TextOperand _) = error "Invalid Text Operand"

showOperand (NumberOperand 0) = "0"
showOperand (NumberOperand 1) = "1"
showOperand (NumberOperand 2) = "2"
showOperand (NumberOperand 3) = "3"
showOperand (NumberOperand 4) = "4"
showOperand (NumberOperand 5) = "5"
showOperand (NumberOperand 6) = "6"
showOperand (NumberOperand 7) = "7"
showOperand (NumberOperand 8) = "8"
showOperand (NumberOperand 9) = "9"
showOperand (NumberOperand _) = error "Invalid Number Operand"

data Operator = Add | Sub | Mul
		deriving (Show)

showOperator :: Operator -> String
showOperator Add = "+"
showOperator Sub = "-"
showOperator Mul = "x"

realOperator :: Operator -> Int -> Int -> Int
realOperator Add = (+)
realOperator Sub = (-)
realOperator Mul = (*)

data Captcha = Captcha Operand Operator Operand
		deriving (Show)

showCaptcha :: Captcha -> String
showCaptcha (Captcha leftOperand operator rightOperand) = 
	let 
		showLeft  = showOperand(leftOperand)
		showOp    = showOperator(operator)
		showRight = showOperand(rightOperand)
	in
		showLeft ++ " " ++ showOp ++ " " ++ showRight

answerCaptcha :: Captcha -> Int
answerCaptcha captcha = case captcha of
	Captcha (NumberOperand left) op (TextOperand right) ->
		(realOperator op) left right 
	Captcha (TextOperand left) op (NumberOperand right) ->
		(realOperator op) left right 
