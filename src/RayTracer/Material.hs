module RayTracer.Material where

import RayTracer.Tuple as Tuple
import RayTracer.Light
import RayTracer.Types as Types

defaultMaterial :: Material
defaultMaterial = Material (Tuple.color 1 1 1) 0.1 0.9 0.9 200

lighting :: Material -> Light -> Point -> Vector -> Vector -> Color
lighting material light point eye normal =
    let effectiveColor = (Types.color material) * (intensity light)
        lightVector = normalize $ (position light) - point
        ambientColor = effectiveColor ^* (ambient material)
        lightDotNormal = lightVector `dot` normal
    in if lightDotNormal < 0
        then ambientColor
        else let diffuseColor = effectiveColor ^* (diffuse material * lightDotNormal)
                 reflectVector = reflect (- lightVector) normal
                 reflectDotEye = reflectVector `dot` eye
                in if reflectDotEye < 0
                    then ambientColor + diffuseColor
                    else let factor = reflectDotEye ** (shininess material)
                             specularColor = (intensity light) ^* (specular material * factor)
                          in ambientColor + diffuseColor + specularColor
