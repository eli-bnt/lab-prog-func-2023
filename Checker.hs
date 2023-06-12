----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de chequeo
--
-- Se debe implementar la funcion checkProgram que, dado un AST
-- que representa un programa, retorna Ok en caso de no encontrar errores, 
-- o la lista de errores encontrados en otro caso.   
----------------------------------------------------------------------------


module Checker where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario

import Data.List
import Data.Maybe

-- CHECKER

data Checked = Ok | Wrong [Error]

data Error = Duplicated      Name
           | Undefined       Name
           | ArgNumDef       Name Int Int
           | ArgNumApp       Name Int Int
           | Expected        Type Type
            
instance Show Error where
 show (Duplicated      n)  = "Duplicated declaration: " ++ n
 show (Undefined       n)  = "Undefined: " ++ n
 show (ArgNumDef   n s d)
   = "The number of parameters in the definition of "++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (ArgNumApp   n s d)
   = "The number of arguments in the application of: " ++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (Expected    ty ty')
   = "Expected: " ++ show ty ++ " Actual: " ++ show ty'



retornarRepetidos:: [Name] -> [Name]
retornarRepetidos  [] = []
retornarRepetidos (x:xs)
  | x `elem` xs = x: retornarRepetidos xs
  | otherwise = retornarRepetidos xs

listarNombreAux:: FunDef -> Name
listarNombreAux (FunDef (name,_) _ _ ) = name

listarNombresFunc:: [FunDef] -> [Name]
listarNombresFunc = map listarNombreAux

listarParamAux:: FunDef -> [Name]
listarParamAux (FunDef (_,_) names _ ) = retornarRepetidos names

listarNombreParams:: [FunDef] -> [Name]
listarNombreParams = concatMap listarParamAux

retornarError:: [Name] -> [Error]
retornarError = map retornarErrorAux

retornarErrorAux::Name -> Error
retornarErrorAux = Duplicated

checkearNombresRepetidos:: [FunDef]-> [Error]
checkearNombresRepetidos =  retornarError . retornarRepetidos . listarNombresFunc

checkearParametrosRepetidos:: [FunDef] -> [Error]
checkearParametrosRepetidos = retornarError . listarNombreParams

checkear2_1:: [FunDef]-> Checked
checkear2_1 defs =  
  let nombresRepetidos = checkearNombresRepetidos defs
      parametrosRepetidos = checkearParametrosRepetidos defs
      errores = nombresRepetidos ++ parametrosRepetidos
  in if null errores
       then Ok
       else Wrong errores

checkearCantParam :: FunDef -> [Error]
checkearCantParam (FunDef (name,Sig tipos tipo) params _ ) =
  let cantEsperada = length (tipos)
      cantReal = length params
  in if cantEsperada /= cantReal
      then  ([retornarErrorArgNumDef name cantReal cantEsperada])
      else []
retornarErrorArgNumDef:: Name -> Int -> Int -> Error
retornarErrorArgNumDef = ArgNumDef 

checkearCantParams::[FunDef]-> Checked
checkearCantParams def = 
  
  let cantParams= concatMap checkearCantParam def
  in if null cantParams
       then Ok
       else Wrong cantParams





checkProgram :: Program -> Checked
checkProgram (Program defs expr) = checkear2_1 defs
 


