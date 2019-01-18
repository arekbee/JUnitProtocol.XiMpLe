

printJUnitProtocol.XiMpLe <- function(testData,fileName = "") {
    errInfo <- RUnit::getErrors(testData)
    xml.testsuites <- XiMpLe::XMLNode("testsuites", attrs = list(
                                   errors = errInfo$nErr,
                                   failures = errInfo$nFail,
                                   tests = errInfo$nTestFunc
                                   ))

    for (tsName in names(testData)) {
        xml.testsuite <- XiMpLe::XMLNode("testsuite", attrs = c(
                                  errors = testData[[tsName]]$nErr,
                                  failures = testData[[tsName]]$nFail,
                                  name = tsName,
                                  tests = testData[[tsName]]$nTestFunc
                                ))
        if (testData[[tsName]]$nErr + testData[[tsName]]$nFail >= 0) {
            srcFileRes <- testData[[tsName]][["sourceFileResults"]]
            for (i in seq_along(srcFileRes)) {
                testFuncNames <- names(srcFileRes[[i]])
                for (j in seq_along(testFuncNames)) {
                    funcList <- srcFileRes[[i]][[testFuncNames[j]]]
                    xml.testcase <- XiMpLe::XMLNode("testcase", attrs = list(name = testFuncNames[j], time = funcList$time[['elapsed']]))

                    if (funcList$kind == "success") {
                    } else if (funcList$kind == "error") {
                        XiMpLe::XMLChildren(xml.testcase) <- XiMpLe::XMLNode("error", attrs = list("message" = funcList$msg, "type" = "ERROR"))
                    }
                    else if (funcList$kind == "failure") {
                        XiMpLe::XMLChildren(xml.testcase) <- XiMpLe::XMLNode("failure", attrs = list("message" = funcList$msg, "type" = "FAILURE"))
                    }
                    else if (funcList$kind == "deactivated") {
                        XiMpLe::XMLChildren(xml.testcase) <- XiMpLe::XMLNode("skipped")
                    }
                    XiMpLe::XMLChildren(xml.testsuite) <- c(XiMpLe::XMLChildren(xml.testsuite), xml.testcase)
                }
            }
        }
        XiMpLe::XMLChildren(xml.testsuites) <- c(XiMpLe::XMLChildren(xml.testsuites),  xml.testsuite)
    }

    xml <- XiMpLe::pasteXML(xml.testsuites)
    if (fileName == "") {
        cat(xml , file= stdout())
    } else {
        dir.create(dirname(fileName), showWarnings = FALSE, recursive = TRUE)
        cat(xml, file= fileName)
    }
    return(invisible(TRUE))
}
