module Qamar where

import           Text.Megaparsec (ParseErrorBundle, eof, parseTest, runParser)

import Qamar.Parser

runTest :: IO ()                                                     
runTest = parseTest (pExpr <* eof) "2 * (3 + 2) - (-3 + 1)"          
                                                                     
parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr        
parseExpr = runParser (pExpr <* eof) "local"                         
                                                                     
parse12 :: Either (ParseErrorBundle Text Void) Expr                  
parse12 = runParser (pExpr <* eof) "local" "2 * (3 + 2) - (-3 + 1)"  
                                                                     
pushNegExpression :: Text                                            
pushNegExpression = "(8 + (-(1 + 2)))"
