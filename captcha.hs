import System.Random
import Control.Monad

data CaptchaType = TextNumber | NumberText deriving(Enum)
data Operand = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving(Enum, Show)
data Operator = Add | Sub | Mul deriving(Enum)
data Captcha  = Captcha CaptchaType Operand Operator Operand

toCaptchaType n = toEnum n :: CaptchaType
toOperand n = toEnum n :: Operand
toOperator n = toEnum n :: Operator

instance Show Operator where
  show op = case op of
    Add -> "+"
    Sub -> "-"
    Mul -> "*"

instance Show Captcha where
  show (Captcha TextNumber a operator b) = (show a) ++ " " ++ (show operator) ++ " " ++ (show $ fromEnum b)
  show (Captcha NumberText a operator b) = (show $ fromEnum a) ++ " " ++ (show operator) ++ " " ++ (show b)

result :: Captcha -> Int
result (Captcha _ l Add r) = fromEnum l + fromEnum r
result (Captcha _ l Sub r) = fromEnum l - fromEnum r
result (Captcha _ l Mul r) = fromEnum l * fromEnum r

randomCaptchaType = liftM toCaptchaType $ randomRIO (0,1)
randomOperand = liftM toOperand $ randomRIO (0,9)
randomOperator = liftM toOperator $ randomRIO (0,2)

generateCaptcha :: IO Captcha
generateCaptcha = do
                    captchaType <- randomCaptchaType
                    left <- randomOperand
                    operator <- randomOperator
                    right <- randomOperand

                    return $ Captcha captchaType left operator right
