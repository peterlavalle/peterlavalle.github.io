
module monadish

data Mo: Type -> Type where
	Free: a -> Mo a

Functor Mo where
	map f (Free a) = Free $ f a

Applicative Mo where
	pure = Free
	(Free f) <*> (Free a) = Free $ f a

Monad Mo where
	(>>=) (Free a) f = f a
