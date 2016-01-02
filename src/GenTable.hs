{-# LANGUAGE QuasiQuotes #-}

module GenTable where

import Text.Blaze.Renderer.String (renderMarkup)
import Text.Hamlet (shamlet)

import Quote

getAnalysisAndQuotesAsHtml :: [(String, Int)] -> [Quote] -> String
getAnalysisAndQuotesAsHtml authors quotes = renderMarkup $
  [shamlet|
    <html>
      <head>
        <meta charset="utf-8" />
        <link rel="stylesheet" href="https://cdn.jsdelivr.net/foundation/6.1.0/foundation.min.css">
        <style>
          body { text-align: center; background-color: #D5DADC; }
          h2 { margin-top: 2em; }
          table { border: .1em solid gray; margin: 5em auto; }
          .bar { background-color: #074E68; padding-left: 0.3em; color: white; min-height: 1.4em; }
          .lefttd { width: 15em; text-align: right; }
          .righttd { width: 40em; }
          .enabledtd { border: .03em solid gray; }
          .disabledtd { border: 0; color: gray;}
      <body .bg-fblue>
        <h2>Simple quote analysis.
        <small>The real question is what to analyze here... so here are the most frequent punchline speakers:
        <table>
          $forall (author, num) <- take 10 authors
            <tr>
              <td .lefttd>
                #{author}
              <td .righttd>
                <div .bar style="width: #{div (40 * num) maxNum}em;">
                  <small>
                    #{num}
        <h2>Quotes in full text.
        <small>Find any iconsistencies? This incident should be reported.
        $forall (Quote qps) <- quotes
          <table>
            $forall qp <- qps
              <tr>
                $case qp
                  $of (QSpeech speaker attributes spoken)
                    <td .lefttd .enabledtd>
                      #{speaker}
                      $if not $ null attributes
                        <br />
                        <small>
                          #{attributes}
                    <td .righttd .enabledtd>
                      #{spoken}
                  $of (QOther other)
                    <td .lefttd .disabledtd>
                    <td .righttd .disabledtd>
                      #{other}
  |]
  where maxNum = maximum $ map snd authors
