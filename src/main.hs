import Asx.CompanyList.IO as CL
import Asx.Training.IO    as TR
import Asx.Analysis.IO    as AN

import System.Environment

main
 = do args <- getArgs
      run args

 where
  run ["grab"]
    = CL.grabAllCompanies False
  run ["grab-force"]
    = CL.grabAllCompanies True

  run ("training":stride:future:quants:codes)
    = TR.training (read future) (read stride) quants TrainPrint codes
  run ("train-into":stride:future:quants:into:codes)
    = TR.training (read future) (read stride) quants (TrainInto into) codes

  run ("train-labels-into":stride:future:quants:into:codes)
    = TR.training (read future) (read stride) quants (TrainLabels into) codes

  run ("predict":quants:codes)
    = TR.predict quants Nothing codes
  run ("predict-into":quants:into:codes)
    = TR.predict quants (Just into) codes

  run ("quantiles":stride:codes)
    = TR.printQuantiles (read stride) codes
  run ("quantiles-into":stride:into:codes)
    = TR.writeQuantiles (read stride) into codes

  run ("export-ivory":stride:[])
    = TR.exportAsIvory 0

  run ("analyse":into:predictions)
    = AN.analyse into predictions

  run _
   = usage

usage
 = putStrLn
 $ unlines
 [ "asx things"
 , ""
 , "asx grab"
 , "    get company data off yahoo"
 , ""
 , "asx training stride future quants [code ...]"
 , "    get training data for given codes"
 , "    use quantile data in quants file"
 , ""
 , "asx train-into stride future quants into [code ...]"
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
 , ""
 , "asx analyse into prediction-files"
 , "    read different predictions on same things, average or whatever"
 , "" ]
