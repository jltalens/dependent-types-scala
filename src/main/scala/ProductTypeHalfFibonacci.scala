/*
This is intented to be be used with ProvingGround:
```
git clone https://github.com/siddhartha-gadgil/ProvingGround.git
cd ProvingGround
sbt mantle/test:console
```
this standalone will not even compile
 */
object ProductTypeHalfFibonacci {
  import TLImplicits._
  import shapeless._
  val Nat = "Nat" :: Type
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros
  val n = "n" :: Nat
  val one = succ(zero)
  val two = succ(one)
  val three = succ(two)
  val four = succ(three)
  val five = succ(four)
  val six = succ(five)
  val recNProdNN = NatInd.rec(ProdTyp(Nat, Nat))
  val pair = "(half(n), half(n+1))" :: ProdTyp(Nat, Nat)
  val halfpair = recNProdNN(PairTerm(zero, one))(n :-> (pair :-> PairTerm(pair.second, succ(pair.first))  ))
  val half = n :-> halfpair(n).first
  half(six).fansi
}
