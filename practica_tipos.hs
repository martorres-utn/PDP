p n = (head . filter n)

f x y = (x.fst.head) y > (x.snd.head) y

g (_ , c, _) = c

h :: (Eq y) => y -> [(x, y, z)] -> (x, y, z)
h nom = head.filter ((nom == ) . g)