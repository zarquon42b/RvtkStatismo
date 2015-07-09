context("pPCA statismoBuildModel comparison")

test_that("pPCA statismoBuildModel behaves", {
              require(Morpho)
              data(boneData)
              align <- rigidAlign(boneLM)$rotated
              smod <- statismoBuildModel(align,sigma=1)
              pmod <- pPCA(align,align=F,sigma=1)
              
              expect_equal(lapply(pmod@PCA,abs),lapply(smod@PCA,abs), tol=1e-5,check.attributes=TRUE)

          })
