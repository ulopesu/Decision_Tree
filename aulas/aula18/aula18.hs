data Nat = Zero | Suc Nat
    deriving Show

natForInt :: Nat -> Int
natForInt Zero = 0;
natForInt (Suc n) = 1 + natForInt n

intForNat :: Int -> Nat
intForNat 0 = Zero;
intForNat x = Suc (intForNat (x-1))

sumNat :: Nat -> Nat -> Nat
sumNat n1 n2 = intForNat ((natForInt n1) + (natForInt n2))

sumNat2 :: Nat -> Nat -> Nat
sumNat2 Zero Zero = Zero
sumNat2 Zero (Suc n) = Suc (sumNat2 Zero n)
sumNat2 (Suc n) Zero = Suc (sumNat2 n Zero)
sumNat2 (Suc n1) (Suc n2) =  (Suc (Suc (sumNat2 n1 n2)))

sumNat3 :: Nat -> Nat -> Nat
sumNat3 Zero n = n
sumNat3 (Suc n1) n2 = Suc (sumNat3 n1 n2)

sumNat4 :: Nat -> Nat -> Nat
sumNat4 Zero n2 = n2
sumNat4 (Suc n1) n2 = sumNat4 n1 (Suc n2)



data Expr = Val Int | Add Expr Expr | Mul Expr Expr
    deriving Show

teste = eval (Add (Val 1) (Mul (Val 2) (Val 3)))
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y