import RayTracer.Tuple
import RayTracer.Canvas

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

pointToPixel :: Int -> Point -> (Int, Int, Color)
pointToPixel height (V4 x y _ _) = (round x, height - 1 - round y, red)
    where
        red = color 1 0 0

p = Projectile (point 0 1 0) ((normalize $ vector 1 1.8 0) ^* 11.25)

e = Environment (vector 0 (-0.1) 0) (vector (-0.01) 0 0)

simulate :: Environment -> Projectile -> [Projectile]
simulate env = takeWhile (\p -> (position p) ^. _y > 0) . iterate (tick env) 

width = 900
height = 550

main = saveCanvas "projectile.png" $ 
        createCanvas width height $ pointToPixel height <$> position <$> simulate e p
