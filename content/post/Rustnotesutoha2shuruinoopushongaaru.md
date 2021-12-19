---
categories: [Rust, Advent Calendar, Advent Calendar 2021, Rust Advent Calendar]
date: 2021-12-20T01:57:28+09:00
title: "Rustのテストは2種類のオプションがある"
---
このエントリは[Rust Advent Calendar](https://qiita.com/advent-calendar/2021/rust)の1日目の記事です。

空いてる日を埋める担当のκeenです。1日目が空いてたので遡って記事を投稿します。
Rustでテストをするときに渡すオプションが2種類あるという話について。

<!--more-->

テストを実行するときに `cargo test` を使うかと思います。この `cargo test` のヘルプメッセージは以下のようになっています。

<details>
<pre class="chroma"><code class="language-text" data-lang="text">cargo-test
Execute all unit and integration tests and build examples of a local package

USAGE:
    cargo test [OPTIONS] [TESTNAME] [-- <args>...]

OPTIONS:
    -q, --quiet                      Display one character per test instead of one line
        --lib                        Test only this package's library unit tests
        --bin <NAME>...              Test only the specified binary
        --bins                       Test all binaries
        --example <NAME>...          Test only the specified example
        --examples                   Test all examples
        --test <NAME>...             Test only the specified test target
        --tests                      Test all tests
        --bench <NAME>...            Test only the specified bench target
        --benches                    Test all benches
        --all-targets                Test all targets
        --doc                        Test only this library's documentation
        --no-run                     Compile, but don't run tests
        --no-fail-fast               Run all tests regardless of failure
    -p, --package <SPEC>...          Package to run tests for
        --workspace                  Test all packages in the workspace
        --exclude <SPEC>...          Exclude packages from the test
        --all                        Alias for --workspace (deprecated)
    -j, --jobs <N>                   Number of parallel jobs, defaults to # of CPUs
        --release                    Build artifacts in release mode, with optimizations
        --profile <PROFILE-NAME>     Build artifacts with the specified profile
        --features <FEATURES>...     Space or comma separated list of features to activate
        --all-features               Activate all available features
        --no-default-features        Do not activate the `default` feature
        --target <TRIPLE>...         Build for the target triple
        --target-dir <DIRECTORY>     Directory for all generated artifacts
        --manifest-path <PATH>       Path to Cargo.toml
        --ignore-rust-version        Ignore `rust-version` specification in packages
        --message-format <FMT>...    Error format
        --unit-graph                 Output build graph in JSON (unstable)
        --future-incompat-report     Outputs a future incompatibility report at the end of the build (unstable)
    -v, --verbose                    Use verbose output (-vv very verbose/build.rs output)
        --color <WHEN>               Coloring: auto, always, never
        --frozen                     Require Cargo.lock and cache are up to date
        --locked                     Require Cargo.lock is up to date
        --offline                    Run without accessing the network
        --config <KEY=VALUE>...      Override a configuration value (unstable)
    -Z <FLAG>...                     Unstable (nightly-only) flags to Cargo, see 'cargo -Z help' for details
    -h, --help                       Prints help information

ARGS:
    <TESTNAME>    If specified, only run tests containing this string in their names
    <args>...     Arguments for the test binary

Run `cargo help test` for more detailed information.
</code></pre>

</details>

何やら色々オプションが並んでいますね。多くは `cargo build` などにもありますが、例えば `--exclude` や `--no-fail-fast` などはテスト専用のオプションに見えます。

この影響でここにリストアップされているものが全てのような気がしますが、実はテストのオプションはまだまだあります。上記はCargo側のオプションで、コンパイルされたテスト側にもまだオプションはあるのです。テストバイナリ側のヘルプメッセージは `cargo test -- --help` と `--` を挟んでからオプションを渡してあげると見れます。

<details>

<pre class="chroma"><code class="language-text" data-lang="text">$ cargo test -- --help
Usage: --help [OPTIONS] [FILTERS...]

Options:
        --include-ignored 
                        Run ignored and not ignored tests
        --ignored       Run only ignored tests
        --force-run-in-process 
                        Forces tests to run in-process when panic=abort
        --exclude-should-panic 
                        Excludes tests marked as should_panic
        --test          Run tests and not benchmarks
        --bench         Run benchmarks instead of tests
        --list          List all tests and benchmarks
    -h, --help          Display this message
        --logfile PATH  Write logs to the specified file
        --nocapture     don't capture stdout/stderr of each task, allow
                        printing directly
        --test-threads n_threads
                        Number of threads used for running tests in parallel
        --skip FILTER   Skip tests whose names contain FILTER (this flag can
                        be used multiple times)
    -q, --quiet         Display one character per test instead of one line.
                        Alias to --format=terse
        --exact         Exactly match filters rather than by substring
        --color auto|always|never
                        Configure coloring of output:
                        auto = colorize if stdout is a tty and tests are run
                        on serially (default);
                        always = always colorize output;
                        never = never colorize output;
        --format pretty|terse|json|junit
                        Configure formatting of output:
                        pretty = Print verbose output;
                        terse = Display one character per test;
                        json = Output a json document;
                        junit = Output a JUnit document
        --show-output   Show captured stdout of successful tests
    -Z unstable-options Enable nightly-only flags:
                        unstable-options = Allow use of experimental features
        --report-time [plain|colored]
                        Show execution time of each test. Available values:
                        plain = do not colorize the execution time (default);
                        colored = colorize output according to the `color`
                        parameter value;
                        Threshold values for colorized output can be
                        configured via
                        `RUST_TEST_TIME_UNIT`, `RUST_TEST_TIME_INTEGRATION`
                        and
                        `RUST_TEST_TIME_DOCTEST` environment variables.
                        Expected format of environment variable is
                        `VARIABLE=WARN_TIME,CRITICAL_TIME`.
                        Durations must be specified in milliseconds, e.g.
                        `500,2000` means that the warn time
                        is 0.5 seconds, and the critical time is 2 seconds.
                        Not available for --format=terse
        --ensure-time   Treat excess of the test execution time limit as
                        error.
                        Threshold values for this option can be configured via
                        `RUST_TEST_TIME_UNIT`, `RUST_TEST_TIME_INTEGRATION`
                        and
                        `RUST_TEST_TIME_DOCTEST` environment variables.
                        Expected format of environment variable is
                        `VARIABLE=WARN_TIME,CRITICAL_TIME`.
                        `CRITICAL_TIME` here means the limit that should not
                        be exceeded by test.
        --shuffle       Run tests in random order
        --shuffle-seed SEED
                        Run tests in random order; seed the random number
                        generator with SEED


The FILTER string is tested against the name of all tests, and only those
tests whose names contain the filter are run. Multiple filter strings may
be passed, which will run all tests matching any of the filters.

By default, all tests are run in parallel. This can be altered with the
--test-threads flag or the RUST_TEST_THREADS environment variable when running
tests (set it to 1).

By default, the tests are run in alphabetical order. Use --shuffle or set
RUST_TEST_SHUFFLE to run the tests in random order. Pass the generated
"shuffle seed" to --shuffle-seed (or set RUST_TEST_SHUFFLE_SEED) to run the
tests in the same order again. Note that --shuffle and --shuffle-seed do not
affect whether the tests are run in parallel.

All tests have their standard output and standard error captured by default.
This can be overridden with the --nocapture flag or setting RUST_TEST_NOCAPTURE
environment variable to a value other than "0". Logging is not captured by default.

Test Attributes:

    `#[test]`        - Indicates a function is a test to be run. This function
                       takes no arguments.
    `#[bench]`       - Indicates a function is a benchmark to be run. This
                       function takes one argument (test::Bencher).
    `#[should_panic]` - This function (also labeled with `#[test]`) will only pass if
                        the code causes a panic (an assertion failure or panic!)
                        A message may be provided, which the failure string must
                        contain: #[should_panic(expected = "foo")].
    `#[ignore]`       - When applied to a function which is already attributed as a
                        test, then the test runner will ignore these tests during
                        normal test runs. Running with --ignored or --include-ignored will run
                        these tests.
</code></pre>
</details>

こっちのオプションを使うとより精細にテストの実行をコントロールできます。

例えば `--no-capture` オプションを使えばテストが吐いた出力を見ることができます。

```rust
#[test]
fn foo() {
    println!("foo test");
}

#[test]
fn foo2() {
    println!("foo2 test");
}
```

```text
$ cargo test -- --nocapture
cargo test -- --nocapture
    Finished test [unoptimized + debuginfo] target(s) in 0.00s
     Running unittests (target/debug/deps/test_example-87b3befdea076842)

running 0 tests

test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

     Running tests/tests.rs (target/debug/deps/tests-ed358733425f347a)

running 2 tests
foo test
foo2 test
test foo ... ok
test foo2 ... ok

test result: ok. 2 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s
```


あるいは正確に `foo` だけ実行したい場合に `--exact` フラグも有効です。
普通のテストの引数に名前を渡すものだと部分一致していればそのままテストされるので `foo` と `foo2` や `foo` と `foo_fail` のようなテストが並んでいるときに `foo` だけを実行したくてもできません。

```text
$ cargo test foo
    Finished test [unoptimized + debuginfo] target(s) in 0.00s
     Running unittests (target/debug/deps/test_example-87b3befdea076842)

running 0 tests

test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

     Running tests/tests.rs (target/debug/deps/tests-ed358733425f347a)

running 2 tests
test foo ... ok
test foo2 ... ok

test result: ok. 2 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

```

そこでテストバイナリ側にある `--exact` フラグを使うと望んだものだけを実行できます。

```text
$ cargo test -- --exact foo
    Finished test [unoptimized + debuginfo] target(s) in 0.00s
     Running unittests (target/debug/deps/test_example-87b3befdea076842)

running 0 tests

test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s

     Running tests/tests.rs (target/debug/deps/tests-ed358733425f347a)

running 1 test
test foo ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 1 filtered out; finished in 0.00s
```


他にも実行スレッド数を制御したりといくつか必要になりそうな機能があるのでテストの実行のしかたを変えたいという人は一度バイナリ側のヘルプも読んでみて下さい。
