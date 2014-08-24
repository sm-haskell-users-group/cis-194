module Main where

import JoinListBuffer
import Editor

main = runEditor editor buffer
                 where s = unlines $ reverse [ "This buffer is for notes you don't want to save, and for"
                                                 , "evaluation of steam valve coefficients."
                                                 , "To load a different file, type the character L followed"
                                                 , "by the name of the file."
                                             ]
                       buffer = s2jlb s
