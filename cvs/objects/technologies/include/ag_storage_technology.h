#ifndef _AG_STORAGE_TECHNOLOGY_H_
#define _AG_STORAGE_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

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
 * \file AG_STORAGE_TECHNOLOGY.H
 * \ingroup Objects
 * \brief The AgStorageTechnology class header file.
 * \author Ellie Lochner
 */
class IVisitor;

#include "technologies/include/technology.h"

/*!
 * \ingroup Objects
 * \brief A technology meant only to pass demands on to a pass-through sector.
 * \details This technology will look up how much fixed output there was in
 *          it's corresponding pass-though sector and report that as fixed
 *          to it's containing sector.  It will then pass along the total demand
 *          to the pass-through sector.
 *
 * \author Pralit Patel
 */
class AgStorageTechnology: public Technology
{
   friend class XMLDBOutputter;
public:
    AgStorageTechnology();
    AgStorageTechnology( const std::string& aName, const int aYear );
    virtual ~AgStorageTechnology();

    static const std::string& getXMLNameStatic();

    // ITechnology methods
    virtual const std::string& getXMLName() const;

    virtual AgStorageTechnology* clone() const;

    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const std::string& aSubsectorName,
                               const IInfo* aSubsectorIInfo,
                               ILandAllocator* aLandAllocator );

    virtual void initCalc(const std::string& aRegionName,
        const std::string& aSectorName,
        const IInfo* aSubsectorInfo,
        const Demographic* aDemographics,
        PreviousPeriodInfo& aPrevPeriodInfo,
        const int aPeriod);

    virtual double getFixedOutput(const std::string& aRegionName,
        const std::string& aSectorName,
        const bool aHasRequiredInput,
        const std::string& aRequiredInput,
        const double aMarginalRevenue,
        const int aPeriod) const;

    virtual void production( const std::string& aRegionName,
                             const std::string& aSectorName,
                             double aVariableDemand,
                             double aFixedOutputScaleFactor,
                             const int aPeriod );

    virtual void calcCost(const std::string& aRegionName,
                          const std::string& aSectorName,
                          const int aPeriod);


protected:
    virtual void toDebugXMLDerived( const int aPeriod, std::ostream& aout, Tabs* aTabs ) const;

    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        Technology,

        //! Amount carried forward from one model period to the next 
        DEFINE_VARIABLE(SIMPLE, "opening-stock", mOpeningStock, Value),
        // Expected price of food crop
        DEFINE_VARIABLE(SIMPLE | NOT_PARSABLE, "expected-price", mAdjExpectedPrice, Value),
        //initial amount of crop in storage
        DEFINE_VARIABLE(SIMPLE, "closing-stock", mClosingStock, Value),
        //
        DEFINE_VARIABLE(SIMPLE | STATE | NOT_PARSABLE, "stored-value", mStoredValue, Value),
        //
        DEFINE_VARIABLE(SIMPLE, "logit-exponent", mLogitExponent, Value), 
        //
        DEFINE_VARIABLE(SIMPLE, "loss-coefficient", mLossCoefficient, Value),
    
        DEFINE_VARIABLE(SIMPLE | STATE | NOT_PARSABLE, "consumption", mConsumption, Value),

        DEFINE_VARIABLE(SIMPLE | STATE | NOT_PARSABLE, "total", mTotal, Value),

        // Storage Cost 
        DEFINE_VARIABLE(SIMPLE, "storage-cost", mStorageCost, Value)
    )
    
    void copy( const AgStorageTechnology& aOther );
    virtual void setProductionState(const int aPeriod);

};

#endif // _AG_STORAGE_TECHNOLOGY_H_

