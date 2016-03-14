import System.Random

data TextOperand = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving(Enum, Show)

data Operator = Add | Sub | Mul deriving(Enum)

instance Show Operator where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"

data Captcha a b = Captcha a Operator b

instance (Show a, Show b) => Show (Captcha a b) where
  show (Captcha a operator b) = (show a) ++ " " ++ (show operator) ++ " " ++ (show b)


apply :: Int -> Operator -> Int -> Int
apply l Add r = l + r
apply l Sub r = l - r
apply l Mul r = l * r

generateCaptcha :: IO (String, Int)
generateCaptcha = do
                    captchaPattern <- randomRIO (0,1) :: IO Int
                    operator <- randomRIO (0,2) :: IO Int
                    operand1 <- randomRIO (0,9) :: IO Int
                    operand2 <- randomRIO (0,9) :: IO Int

                    let
                        op = toEnum operator :: Operator
                        textOperand = toEnum operand1 :: TextOperand
                        numOperand = operand2

                    if captchaPattern == 1 then
                      return (show $ Captcha textOperand op  numOperand, apply operand1 op operand2)
                      else
                      return (show $ Captcha numOperand op textOperand, apply operand2 op operand1)
