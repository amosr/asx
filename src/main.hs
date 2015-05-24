import Asx.CompanyList.IO as CL
import Asx.Training.IO    as TR

import System.Environment

main
 = do args <- getArgs
      run args

 where
  run ["grab"]
    = CL.grabAllCompanies False
  run ["grab-force"]
    = CL.grabAllCompanies True

  run ("training":quants:codes)
    = TR.training 5 quants TrainPrint codes
  run ("train-into":quants:into:codes)
    = TR.training 5 quants (TrainInto into) codes

  run ("training-s":stride:quants:codes)
    = TR.training (read stride) quants TrainPrint codes
  run ("train-into-s":stride:quants:into:codes)
    = TR.training (read stride) quants (TrainInto into) codes

  run ("train-labels-into":stride:quants:into:codes)
    = TR.training (read stride) quants (TrainLabels into) codes

  run ("predict":quants:codes)
    = TR.predict quants Nothing codes
  run ("predict-into":quants:into:codes)
    = TR.predict quants (Just into) codes

  run ("quantiles":stride:codes)
    = TR.printQuantiles (read stride) codes
  run ("quantiles-into":stride:into:codes)
    = TR.writeQuantiles (read stride) into codes

  run _
   = usage

usage
 = putStrLn
 $ unlines
 [ "asx shit"
 , ""
 , "asx grab"
 , "    get company data off yahoo"
 , ""
 , "asx training stride quants [code ...]"
 , "    get training data for given codes"
 , "    use quantile data in quants file"
 , ""
 , "asx train-into stride quants into [code ...]"
 , "    write training data for given codes"
 , "    use quantile data in quants file"
 , ""
 , "asx predict quants [code ...]"
 , "asx predict-into quants into [code ...]"
 , "    write predictions (training data without labels) for given codes"
 , "    use quantile data in quants file"
 , ""
 , "asx quantiles stride [code ...]"
 , "    get quantiles for codes"
 , "    stride is how many records to skip in between used records"
 , ""
 , "asx quantiles-into stride into [code ...]"
 , "    get quantiles for codes and then store in file"
 , "" ]
