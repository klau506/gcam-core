/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 *
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 *
 * Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
 * Distributed as open-source under the terms of the Educational Community
 * License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
 *
 * For further details, see: http://www.globalchange.umd.edu/models/gcam/
 *
 */


/*!
 * \file pass_through_technology.cpp
 * \ingroup Objects
 * \brief AgStorageTechnology class source file.
 * \author Ellie Lochner
 */

// Standard Library headers
#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include "technologies/include/ag_storage_technology.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/market_dependency_finder.h"
#include "technologies/include/iproduction_state.h"
#include "technologies/include/ioutput.h"
#include "technologies/include/production_state_factory.h"
#include "technologies/include/marginal_profit_calculator.h"


using namespace std;

extern Scenario* scenario;

AgStorageTechnology::AgStorageTechnology()
{
}

AgStorageTechnology::AgStorageTechnology( const string& aName, const int aYear ):
Technology(aName, aYear )
{
}

AgStorageTechnology::~AgStorageTechnology() {
}

AgStorageTechnology* AgStorageTechnology::clone() const {
    AgStorageTechnology* clone = new AgStorageTechnology( mName, mYear );
    clone->copy( *this );
    return clone;
}

void AgStorageTechnology::copy( const AgStorageTechnology& aOther ) {
    Technology::copy( aOther );
    
    mLogitExponent = aOther.mLogitExponent;
    mLossCoefficient = aOther.mLossCoefficient;
}

const string& AgStorageTechnology::getXMLNameStatic() {
    const static string XML_NAME = "food-storage-technology";

    return XML_NAME;
}

const string& AgStorageTechnology::getXMLName() const {
    return getXMLNameStatic();
}

void AgStorageTechnology::completeInit(const string& aRegionName,
    const string& aSectorName,
    const string& aSubsectorName,
    const IInfo* aSubsectorInfo,
    ILandAllocator* aLandAllocator)
{

    Technology::completeInit(aRegionName, aSectorName, aSubsectorName, aSubsectorInfo, aLandAllocator);

    // The AgStorageTechnology should not have any vintaging.  All vintaging should be
    // in the associated pass-through sector
    const Modeltime* modeltime = scenario->getModeltime();
    
    if (mYear < modeltime->getEndYear()) {
        const int period = modeltime->getyr_to_per(mYear);
        int minLifetime = modeltime->gettimestep(period + 1); //check this
        int maxLifetime = modeltime->gettimestep(period + 1) + modeltime->gettimestep(period + 2); //check this
  
    }

}

void AgStorageTechnology::setProductionState(const int aPeriod) {
    // Check that the state for this period has not already been initialized.
    // Note that this is the case when the same scenario is run multiple times
    // for instance when doing the policy cost calculation.  In which case
    // we must delete the memory to avoid a memory leak.
    if (mProductionState[aPeriod]) {
        delete mProductionState[aPeriod];
    }

    double initialOutput = 0;
    const Modeltime* modeltime = scenario->getModeltime();

    // initialOutput is only used when technology is "vintaged" and is ignored when it's a new invest
    if (aPeriod <= modeltime->getFinalCalibrationPeriod()) {
        initialOutput = mOpeningStock; // calibrated opening-stock
    }
    else {
        initialOutput = mStoredValue * mLossCoefficient; //stored from last*loss
    }

    mProductionState[aPeriod] =
        ProductionStateFactory::create(mYear, mLifetimeYears, mFixedOutput,
            initialOutput, aPeriod).release();
}


void AgStorageTechnology::initCalc(const string& aRegionName,
    const string& aSectorName,
    const IInfo* aSubsectorInfo,
    const Demographic* aDemographics,
    PreviousPeriodInfo& aPrevPeriodInfo,
    const int aPeriod) {

    Technology::initCalc(aRegionName, aSectorName, aSubsectorInfo, aDemographics, aPrevPeriodInfo, aPeriod);

    if (aPeriod > 0) {
        //mExpectedPrice = scenario->getMarketplace()->getPrice(aSectorName, aRegionName, aPeriod-1);
        mAdjExpectedPrice = (scenario->getMarketplace()->getPrice(aSectorName, aRegionName, aPeriod - 1) - mStorageCost) * mLossCoefficient;
    }
    else {
        mAdjExpectedPrice = 1;
    }
}

double AgStorageTechnology::getFixedOutput(const string& aRegionName,
    const string& aSectorName,
    const bool aHasRequiredInput,
    const string& aRequiredInput,
    const double aMarginalRevenue,
    const int aPeriod) const
{
    return 0;
}

void AgStorageTechnology::production(const string& aRegionName,
    const string& aSectorName,
    double aVariableDemand,
    double aFixedOutputScaleFactor,
    const int aPeriod)
{
    int OutputPosition;
    
    if (mProductionState[aPeriod]->isNewInvestment()) {

        OutputPosition = 0;

        const Modeltime* modeltime = scenario->getModeltime();
        mConsumption = aVariableDemand; // doesn't need to be member variable just here to write to debug xml for now
        if (!util::isValidNumber(mConsumption)) {
            mConsumption = 1;
        }
        if (aPeriod <= modeltime->getFinalCalibrationPeriod()) {  
            if (mClosingStock == 0) {
                mShareWeight = 0;
            }
            else if (mConsumption == 0) {
                mShareWeight = 0;
                ILogger& mainLog = ILogger::getLogger("main_log");
                mainLog.setLevel(ILogger::WARNING);
                mainLog << "No consumption and positive closing stock in " << aRegionName << " " << aSectorName << endl;
            }
            else {
                mShareWeight = mClosingStock / ((pow(mAdjExpectedPrice / mInputs[0]->getPrice(aRegionName, aPeriod), mLogitExponent)) * mConsumption);
            }
         }

        mStoredValue = mShareWeight * (pow(mAdjExpectedPrice / mInputs[0]->getPrice(aRegionName, aPeriod), mLogitExponent))* mConsumption;
        double total = mStoredValue + mConsumption;
        double totalToVariableRatio = total == 0?1.0 : total / mConsumption;
        mInputs[0]->setCoefficient(totalToVariableRatio, aPeriod);

        mTotal = total;
       
    }
    else if(mProductionState[aPeriod]->isOperating()){
        OutputPosition = 1;
        mInputs[0]->setCoefficient(0, aPeriod);
        
    }

    // Early exit optimization to avoid running through the demand function and
    // emissions calculations for non-operating technologies.
    if (!mProductionState[aPeriod]->isOperating()) {
        return;
    }

    // Construct a marginal profit calculator. This allows the calculation of 
    // marginal profits to be lazy.
    MarginalProfitCalculator marginalProfitCalc(this);

    // Use the production state to determine output.
    double primaryOutput =
        mProductionState[aPeriod]->calcProduction(aRegionName,
            aSectorName,
            aVariableDemand,
            &marginalProfitCalc,
            aFixedOutputScaleFactor,
            mShutdownDeciders,
            aPeriod);

    // Calculate input demand.
    mProductionFunction->calcDemand(mInputs, primaryOutput, aRegionName, aSectorName,
        1, aPeriod, 0, 1);

        mOutputs[OutputPosition]->setPhysicalOutput(primaryOutput, aRegionName, mCaptureComponent, aPeriod);
    
}

/* agTechnologies are not shared on cost, so this calCost method is overwritten
   by a calculation of technology profit which is passed to the land allocator
   where it is used for sharing land.  */

void AgStorageTechnology::calcCost(const string& aRegionName,
    const string& aSectorName,
    const int aPeriod)
{
    Technology::calcCost(aRegionName, aSectorName, aPeriod);
    mCosts[aPeriod] /= mInputs[0]->getCoefficient(aPeriod);
}

//! write object to xml output stream
void AgStorageTechnology::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteElement(mStoredValue, "stored-value", aOut, aTabs);
    XMLWriteElement(mAdjExpectedPrice, "expected-price", aOut, aTabs);
    XMLWriteElement(mConsumption, "consumption", aOut, aTabs);
    XMLWriteElement(mClosingStock, "closing-stock", aOut, aTabs);
    XMLWriteElement(mTotal, "total-supply", aOut, aTabs);
    XMLWriteElement(mShareWeight, "share-weight", aOut, aTabs);
    XMLWriteElement(mOpeningStock, "opening-stock", aOut, aTabs);
    XMLWriteElement(mStorageCost, "storage-cost", aOut, aTabs);
}

