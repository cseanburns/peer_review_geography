dec4 <- select(dec4, firstAuthGeog, corrAuthGeog, submitAuthGeog,
               seniorAuthGeog, handlingEditorGeog, meanReviewScore,
               meanReviewerDaysRespond, meanReviewerDaysReview,
               propReviewersResponding, propReviewersAgreeing,
               sentForReview, paperRejected, noReviewsObtained,
               noReviewersResponded, timeToDecision, submitYear)

simple <- select(dec4, firstAuthGeog, corrAuthGeog, submitAuthGeog,
                 seniorAuthGeog, handlingEditorGeog, meanReviewScore,
                 meanReviewerDaysRespond, meanReviewerDaysReview,
                 propReviewersResponding, propReviewersAgreeing,
                 noReviewsObtained, noReviewersResponded, timeToDecision,
                 paperRejected)

set.seed(23)
imputed <- complete(mice(simple))

dec4$firstAuthGeog           <- imputed$firstAuthGeog
dec4$corrAuthGeog            <- imputed$corrAuthGeog
dec4$submitAuthGeog          <- imputed$submitAuthGeog
dec4$seniorAuthGeog          <- imputed$seniorAuthGeog
dec4$handlingEditorGeog      <- imputed$handlingEditorGeog
dec4$meanReviewScore         <- imputed$meanReviewScore
dec4$meanReviewerDaysRespond <- imputed$meanReviewerDaysRespond
dec4$meanReviewerDaysReview  <- imputed$meanReviewerDaysReview
dec4$propReviewersResponding <- imputed$propReviewersResponding
dec4$noReviewsObtained       <- imputed$noReviewsObtained
dec4$noReviewersResponded    <- imputed$noReviewersResponded
dec4$timeToDecision          <- imputed$timeToDecision
dec4$paperRejected           <- imputed$paperRejected
