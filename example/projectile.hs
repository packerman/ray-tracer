import RayTracer.Tuple

data Projectile = Projectile
    {   position :: Point
    ,   velocity :: Vector
    }

data Environment = Environment
    {   gravity :: Vector
    ,   wind :: Vector
    }

tick :: Environment -> Projectile -> Projectile
tick env proj = Projectile
    {   position = position proj + velocity proj
    ,   velocity = velocity proj + gravity env + wind env
    }

p = Projectile (point 0 1 0) (normalize $ vector 1 1 0)

e = Environment (vector 0 (-0.1) 0) (vector (-0.01) 0 0)

simulate env = takeWhile (\p -> (position p) ^. _y > 0) . iterate (tick env) 

main = mapM_ print $ map position $ simulate e p
