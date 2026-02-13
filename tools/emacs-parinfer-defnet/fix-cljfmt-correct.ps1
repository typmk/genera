# Script to fix cljfmt Maven coordinates in deps.edn with correct group ID
$depsFile = "C:\Users\Apollo\.clojure\deps.edn"

$newContent = @'
{
  :aliases {
    ;; Code formatting with cljfmt
    :cljfmt
    {:extra-deps {dev.weavejester/cljfmt {:mvn/version "0.13.1"}}
     :main-opts ["-m" "cljfmt.main"]}

    ;; Format fix - formats and fixes all files in src and test directories
    :cljfmt-fix
    {:extra-deps {dev.weavejester/cljfmt {:mvn/version "0.13.1"}}
     :main-opts ["-m" "cljfmt.main" "fix"]}

    ;; Format check - check if files need formatting (useful for CI)
    :cljfmt-check
    {:extra-deps {dev.weavejester/cljfmt {:mvn/version "0.13.1"}}
     :main-opts ["-m" "cljfmt.main" "check"]}

    ;; REPL with useful development dependencies
    :dev
    {:extra-deps {nrepl/nrepl {:mvn/version "1.3.0"}
                  cider/cider-nrepl {:mvn/version "0.49.3"}}}

    ;; Test runner (uses cognitect test-runner)
    :test
    {:extra-paths ["test"]
     :extra-deps {io.github.cognitect-labs/test-runner
                  {:git/tag "v0.5.1" :git/sha "dfb30dd"}}
     :main-opts ["-m" "cognitect.test-runner"]
     :exec-fn cognitect.test-runner.api/test}
  }
}
'@

Set-Content -Path $depsFile -Value $newContent
Write-Host "Successfully updated cljfmt to correct coordinates in deps.edn"
