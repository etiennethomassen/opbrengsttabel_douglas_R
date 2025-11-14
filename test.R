# Generate docs and run tests

cat("📄 Generating documentation...\n")
devtools::document()

cat("📦 Loading package...\n")
devtools::load_all()

cat("🧪 Running tests...\n")
testthat::test_package("douglasJansen")

cat("✅ Done!\n")

