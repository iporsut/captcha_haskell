import System.Random
import Control.Monad

data CaptchaType = TextNumber | NumberText deriving(Enum)

data Operand = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving(Enum, Show)

data Operator = Add | Sub | Mul deriving(Enum)

instance Show Operator where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"

data Captcha  = Captcha CaptchaType Operand Operator Operand

instance Show Captcha where
  show (Captcha TextNumber a operator b) = (show a) ++ " " ++ (show operator) ++ " " ++ (show $ fromEnum b)
  show (Captcha NumberText a operator b) = (show $ fromEnum a) ++ " " ++ (show operator) ++ " " ++ (show b)

result :: Captcha -> Int
result (Captcha _ l Add r) = fromEnum l + fromEnum r
result (Captcha _ l Sub r) = fromEnum l - fromEnum r
result (Captcha _ l Mul r) = fromEnum l * fromEnum r

generateCaptcha :: IO Captcha
generateCaptcha = do
                    captchaType <- liftM (\n -> toEnum n :: CaptchaType) $ randomRIO (0,1)
                    operator <- liftM (\n -> toEnum n :: Operator) $ randomRIO (0,2)
                    left <- liftM (\n -> toEnum n :: Operand) $ randomRIO (0,9)
                    right <- liftM (\n -> toEnum n :: Operand) $ randomRIO (0,9)

                    return $ Captcha captchaType left operator right
