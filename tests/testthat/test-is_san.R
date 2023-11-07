test_that("is_san works", {
  # Test valid SAN strings
  expect_true(is_san("1. e4! {best by test} e5 \n 2. Nf3!! {black resigns in shame} 1-0"))
  expect_true(is_san("1. e4 c5 2. Nf3 d6 3. d4 cxd4 4. Nxd4 Nf6 5. Nc3 a6 6. Be3 e5 7. Nb3 Be6 8. f3 Be7 9. Qd2 O-O 10. O-O-O Nbd7 1/2-1/2"))

  # Test invalid SAN strings
  expect_false(is_san("e2e4 e7e5 g1f3"))
  expect_false(is_san("The game began 1. e4 e5"))

  # Test non-string inputs
  expect_error(is_san(123))
  expect_error(is_san(TRUE))
})
