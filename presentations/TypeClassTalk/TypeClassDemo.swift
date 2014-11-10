
class K { }

// instance Eq K
extension K : Equatable { }

func == (lhs: K, rhs: K) -> Bool { return false }
func != (lhs: K, rhs: K) -> Bool { return false }

let o = K()

println(( o == o, o != o, o === o )) // (false, false, true)

// instance Ord K
extension K : Comparable { }

func < (lhs: K, rhs: K) -> Bool { return true }

let k = K()

println(( o < k, k < o )) // (true, true)


// DIY Monad

// class Monad t
protocol Monad { typealias T; func bind (ƒ: T -> Self) -> Self }

// infixl 1 >>=
infix operator >>= { associativity left precedence 100 }

func >>= <M: Monad> ($: M, ƒ: M.T -> M) -> M { return $.bind(ƒ) }

// instance Monad Array
extension Array : Monad {

  func bind (ƒ: T -> Array) -> Array { return reduce([]) { z, x in z + ƒ(x) } }
}

let ra = [1,2,3]

let rb = ra.bind { t in [t * t] }

let rc = ra >>= { t in [t * t * t] }

println(( ra, rb, rc )) // ([1, 2, 3], [1, 4, 9], [1, 8, 27])
