import System.Random

data NumberOperand = NumberOperand Int deriving(Show)

data TextOperand = TextOperand Int deriving(Show)

data Operator = Add | Sub | Mul

data Captcha =
    TextNumberCaptcha TextOperand Operator NumberOperand |
    NumberTextCaptcha NumberOperand Operator TextOperand 

class Operand operand where
    text :: operand -> [Char]
    value :: operand -> Int

instance Operand NumberOperand where
    text (NumberOperand n) = show n
    value (NumberOperand n) = n

instance Operand TextOperand where
    value (TextOperand n) = n
    text (TextOperand 0) = "Zero"
    text (TextOperand 1) = "One"
    text (TextOperand 2) = "Two"
    text (TextOperand 3) = "Three"
    text (TextOperand 4) = "Four"
    text (TextOperand 5) = "Five"
    text (TextOperand 6) = "Six"
    text (TextOperand 7) = "Seven"
    text (TextOperand 8) = "Eight"
    text (TextOperand 9) = "Nine"

instance Show Captcha where
    show (TextNumberCaptcha left operator right) = "Captcha " ++ (text left) ++ " " ++ (show operator) ++ " " ++ (text right)
    show (NumberTextCaptcha left operator right) = "Captcha " ++ (text left) ++ " " ++ (show operator) ++ " " ++ (text right)

instance Show Operator where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"

apply :: (Operand l,Operand r) => Operator -> l -> r -> Int
apply Add l r = (value l) + (value r)
apply Sub l r = (value l) - (value r)
apply Mul l r = (value l) * (value r)

selectOperatorByNumber :: Int -> Operator
selectOperatorByNumber 1 = Add
selectOperatorByNumber 2 = Sub
selectOperatorByNumber 3 = Mul

resultCaptcha :: Captcha -> Int
resultCaptcha (TextNumberCaptcha  left operator right) = apply operator left right
resultCaptcha (NumberTextCaptcha  left operator right) = apply operator left right

generateCaptcha :: IO (Captcha)
generateCaptcha = do
                    captchaPattern <- randomRIO (0,1) :: IO Int
                    textOperandPattern <- randomRIO (0,9) :: IO Int
                    numberOperandPattern <- randomRIO (0,9) :: IO Int
                    operatorPattern <- randomRIO (1,3) :: IO Int

                    let 
                        op = selectOperatorByNumber operatorPattern
                        textOperand = TextOperand textOperandPattern
                        numberOperand = NumberOperand numberOperandPattern in

                        if captchaPattern == 1 then
                            return (TextNumberCaptcha textOperand op  numberOperand)
                        else
                            return (NumberTextCaptcha numberOperand op textOperand)
