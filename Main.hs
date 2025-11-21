{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Main where

import Web.Scotty
import qualified Data.Text.Lazy as T
import qualified Data.Aeson as A
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Control.Exception (try, evaluate, SomeException)
import Control.Monad.IO.Class (liftIO)
import GHC.Float (isNaN, isInfinite)

import Expr (Expr)
import Parse (parseExpr)
import Eval (eval)
import Integrate (integrateDefiniteAuto, integrateSymbolic)
import Pretty (pretty)
import Differentiate (differentiateSymbolic)
import Simplify (simplify)
import LLM (llmExplainSteps)

rescueText :: ActionM a -> (SomeException -> ActionM a) -> ActionM a
rescueText = rescue

getPort :: IO Int
getPort = do
  mp <- lookupEnv "PORT"
  pure $ maybe 3000 id (mp >>= readMaybe)

main :: IO ()
main = do
  port <- getPort
  scotty port $ do
    get "/" $
      showPage Nothing Nothing Nothing Nothing Nothing Nothing

    -- Results for the page
    post "/" $ do
      exprTxt <- formParam "expr" :: ActionM T.Text
      aTxt    <- formParam "a"    :: ActionM T.Text
      bTxt    <- formParam "b"    :: ActionM T.Text

      let exprStr = T.unpack exprTxt
          maybeA  = readMaybe (T.unpack aTxt) :: Maybe Double
          maybeB  = readMaybe (T.unpack bTxt) :: Maybe Double

      case parseExpr exprStr of
        Left perr ->
          showPage (Just exprStr) (Just ("Error: " <> perr)) Nothing Nothing Nothing Nothing

        Right expr0 ->
          case (maybeA, maybeB) of
            (Just a, Just b) -> do
              let expr       = simplify expr0
                  f x        = eval expr x
              symbolicE <- symbolicEither expr

              let numeric    = integrateDefiniteAuto expr a b 1e-5
                  derivative = simplify (differentiateSymbolic expr)
                  xsRaw      = grid a b 200
                  ysRaw      = map f xsRaw
                  ys'Der     = map (eval derivative) xsRaw
                  triplesRaw = zip3 xsRaw ysRaw ys'Der
                  triples    = filter sane triplesRaw
              showPage (Just exprStr)
                       (Just (renderResults expr symbolicE numeric a b derivative))
                       (Just numeric)
                       (Just (a, b))
                       (Just (pretty derivative))
                       (Just triples)
            _ ->
              showPage (Just exprStr) (Just "Error: invalid bounds") Nothing Nothing Nothing Nothing

    -- JSON API
    post "/api/llm-explain" $ do
      exprTxt <- rescueText (formParam "expr" :: ActionM T.Text) (\_ -> pure "sin(x)")
      aTxt    <- rescueText (formParam "a"    :: ActionM T.Text) (\_ -> pure "")
      bTxt    <- rescueText (formParam "b"    :: ActionM T.Text) (\_ -> pure "")
      let ma = readMaybe (T.unpack aTxt) :: Maybe Double
          mb = readMaybe (T.unpack bTxt) :: Maybe Double
          mBounds = case (ma,mb) of
                      (Just a, Just b) -> Just (a,b)
                      _                -> Nothing
      r <- liftIO $ llmExplainSteps (T.unpack exprTxt) mBounds
      case r of
        Left err    -> json (A.object ["ok" A..= False, "error" A..= err])
        Right steps -> json (A.object ["ok" A..= True,  "steps" A..= steps])

-- helpers

symbolicEither :: Expr -> ActionM (Either String Expr)
symbolicEither e = do
  res <- liftIO (try (evaluate (integrateSymbolic e)) :: IO (Either SomeException Expr))
  pure $ either (Left . show) Right res

simplifySafe :: Either String Expr -> String
simplifySafe (Right e)  = pretty (simplify e)
simplifySafe (Left msg) = "N/A (" ++ msg ++ ")"

grid :: Double -> Double -> Int -> [Double]
grid a b n =
  let step = (b - a) / fromIntegral n
  in [a, a + step .. b]

sane :: (Double,Double,Double) -> Bool
sane (_, y1, y2) =
  all isFinite [y1, y2]
  where
    isFinite v = not (isNaN v) && not (isInfinite v)

renderResults :: Expr -> Either String Expr -> Double -> Double -> Double -> Expr -> String
renderResults expr mSym numeric a b diff =
  let symText = simplifySafe mSym
  in "\\[f(x) = " ++ pretty expr ++ "\\]"
   ++ "\\[f'(x) = " ++ pretty diff ++ "\\]"
   ++ "\\[\\int_{" ++ show a ++ "}^{" ++ show b ++ "} f(x)\\,dx = "
   ++ symText ++ " + C \\\\ \\approx " ++ showRounded 10 numeric ++ "\\]"

showRounded :: Int -> Double -> String
showRounded n x =
  let factor  = 10 ^^ n :: Double
      rounded = fromIntegral (round (x * factor) :: Integer) / factor
  in show rounded

showPage
  :: Maybe String
  -> Maybe String
  -> Maybe Double
  -> Maybe (Double, Double)
  -> Maybe String
  -> Maybe [(Double, Double, Double)]
  -> ActionM ()
showPage mExpr mSymOrErr _ mBounds _ mData = html $ mconcat
  [ "<!DOCTYPE html><html><head><meta charset='UTF-8'>"
  , "<title>Integral & Derivative Calculator</title>"
  , mathjaxScript
  , plotlyScript
  , "<style>"
  , "body{font-family:'Inter',sans-serif;background:#f4f6fa;color:#222;"
  , "display:flex;justify-content:center;align-items:flex-start;min-height:100vh;padding:40px;}"
  , ".card{background:#fff;padding:32px 38px;border-radius:16px;box-shadow:0 8px 24px rgba(0,0,0,0.1);"
  , "max-width:900px;width:100%;transition:box-shadow 0.3s ease;}"
  , ".card:hover{box-shadow:0 10px 30px rgba(0,0,0,0.15);}"
  , "h1{text-align:center;margin-bottom:5px;font-size:2em;color:#4a90e2;}"
  , "p.subtitle{text-align:center;color:#666;margin-bottom:25px;}"
  , "input,button{font-size:1em;border-radius:8px;padding:8px 10px;border:1px solid #ccc;}"
  , "input[type=text]{width:260px;margin-bottom:10px;}"
  , "input[type=range]{width:240px;}"
  , "input[type=submit],button{background:#4a90e2;color:white;border:none;cursor:pointer;margin:8px 4px;transition:background 0.25s;}"
  , "input[type=submit]:hover,button:hover{background:#357ABD;}"
  , ".flex{display:flex;flex-wrap:wrap;gap:20px;justify-content:space-between;align-items:center;}"
  , ".bounds{margin-top:10px;}"
  , "#plot{width:100%;height:460px;margin-top:30px;border-radius:10px;}"
  , ".error{color:#d9534f;margin-top:10px;}"
  , ".result-box{background:#f8f9fb;border-radius:10px;padding:15px 20px;margin-top:20px;}"
  , ".help{color:#777;font-size:0.9em;margin-top:15px;text-align:center;}"
  , "</style></head><body>"
  , "<div class='card'>"
  , "<h1>Haskell Integral & Derivative Calculator</h1>"
  , "<p class='subtitle'>Symbolic + numeric calculus with visualization</p>"
  , "<form id='calcForm' action='/' method='post'>"
  , "<div class='flex'>"
  , "<div>"
  , "<label>Expression:</label><br/>"
  , "<input name='expr' placeholder='sin(x)*cos(x)' value='", T.pack (maybe "sin(x)" id mExpr), "'/>"
  , "</div>"
  , "<div class='bounds'>"
  , "<label>a:</label><br/>"
  , "<input type='range' id='aSlider' min='-10' max='10' step='0.1' value='", T.pack (show (maybe 0 fst mBounds)), "' oninput='updateA(this.value)'/>"
  , "<input type='text' id='aInput' name='a' value='", maybe "0" (T.pack . show . fst) mBounds, "'/>"
  , "</div>"
  , "<div class='bounds'>"
  , "<label>b:</label><br/>"
  , "<input type='range' id='bSlider' min='-10' max='10' step='0.1' value='", T.pack (show (maybe 3.14 snd mBounds)), "' oninput='updateB(this.value)'/>"
  , "<input type='text' id='bInput' name='b' value='", maybe "3.14" (T.pack . show . snd) mBounds, "'/>"
  , "</div>"
  , "</div>"
  , "<div style='text-align:center;margin-top:10px;'>"
  , "<input type='submit' value='Compute'/>"
  , "<button type='button' onclick='replotOnly()'>Replot</button>"
  , "</div>"
  , "</form>"

  -- Results box
  , maybe "" (const "<div class='result-box'><h2>Results</h2>") mSymOrErr
  , maybe "" T.pack mSymOrErr
  , maybe "" (const "</div>") mSymOrErr

  , "<div id='llm-section' class='result-box' style='margin-top:20px;'>"
  , "<h2>LLM explanation</h2>"
  , "<button type='button' onclick='requestLLM()'>Explain current integral</button>"
  , "<div id='llm-status' class='help'></div>"
  , "<pre id='llm-text' style='white-space:pre-wrap;margin-top:10px;'></pre>"
  , "</div>"

  , renderPlot mData
  , "<div class='help'>Supported: sin, cos, tan, atan/arctan, exp, log/ln, sinh, cosh, tanh, +, -, *, /, ^."
  , " Use <code>x</code> as the variable. Examples: <code>cos x^2</code>, <code>sin 2x</code>, <code>(cos x)^2</code>.</div>"
  , "</div>"
  , sliderScript
  , "</body></html>"
  ]

plotlyScript :: T.Text
plotlyScript = "<script src='https://cdn.plot.ly/plotly-latest.min.js'></script>"

mathjaxScript :: T.Text
mathjaxScript = "<script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>"

renderPlot :: Maybe [(Double, Double, Double)] -> T.Text
renderPlot Nothing = ""
renderPlot (Just triples) =
  let xs   = [x  | (x,_,_) <- triples]
      ys   = [y  | (_,y,_) <- triples]
      ys'  = [y' | (_,_,y') <- triples]
      js   = mconcat
        [ "<div id='plot'></div>"
        , "<script>"
        , "var xs=", show xs, ";"
        , "var ys=", show ys, ";"
        , "var ys2=", show ys', ";"
        , "var trace1={x:xs,y:ys,mode:'lines',name:'f(x)'};"
        , "var trace2={x:xs,y:ys2,mode:'lines',name:\"f'(x)\"};"
        , "var layout={margin:{t:10},xaxis:{zeroline:false},yaxis:{zeroline:false}};"
        , "Plotly.newPlot('plot',[trace1,trace2],layout);"
        , "</script>"
        ]
  in T.pack js

sliderScript :: T.Text
sliderScript = T.pack $ mconcat
  [ "<script>"
  , "function updateA(v){"
  , "  document.getElementById('aInput').value=v;"
  , "  document.getElementById('aSlider').value=v;"
  , "}"
  , "function updateB(v){"
  , "  document.getElementById('bInput').value=v;"
  , "  document.getElementById('bSlider').value=v;"
  , "}"
  , "function replotOnly(){"
  , "  document.getElementById('calcForm').submit();"
  , "}"
  , "async function requestLLM(){"
  , "  const expr = document.querySelector(\"input[name='expr']\").value;"
  , "  const a    = document.getElementById('aInput').value;"
  , "  const b    = document.getElementById('bInput').value;"
  , "  const status = document.getElementById('llm-status');"
  , "  const out    = document.getElementById('llm-text');"
  , "  status.textContent = 'Contacting LLM...';"
  , "  out.textContent = '';"
  , "  const params = new URLSearchParams();"
  , "  params.append('expr', expr);"
  , "  params.append('a', a);"
  , "  params.append('b', b);"
  , "  try {"
  , "    const res = await fetch('/api/llm-explain', {"
  , "      method: 'POST',"
  , "      headers: {'Content-Type':'application/x-www-form-urlencoded'},"
  , "      body: params.toString()"
  , "    });"
  , "    const data = await res.json();"
  , "    if (data.ok) {"
  , "      status.textContent = '';"
  , "      out.textContent = data.steps;"
  , "      if (window.MathJax && MathJax.typesetPromise) {"
  , "        MathJax.typesetPromise();"
  , "      }"
  , "    } else {"
  , "      status.textContent = data.error || 'LLM error';"
  , "    }"
  , "  } catch (e) {"
  , "    status.textContent = 'Network error contacting LLM';"
  , "  }"
  , "}"
  , "</script>"
  ]
