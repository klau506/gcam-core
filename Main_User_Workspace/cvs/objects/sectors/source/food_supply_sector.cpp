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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
* \file food_supply_sector.cpp
* \ingroup Objects
* \brief FoodSupplySector class source file.
* \author James Blackwood
*/

#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/xml_helper.h"

#include "sectors/include/food_supply_sector.h"
#include "sectors/include/food_supply_subsector.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "containers/include/scenario.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Default constructor.
* \author James Blackwood
*/
FoodSupplySector::FoodSupplySector( std::string& regionName ): SupplySector( regionName ) {
    calPrice = -1.0;
    mSectorType = "Agriculture"; //Default sector type for ag production sectors
}

//! Default destructor
FoodSupplySector::~FoodSupplySector( ) {
}

/*! \brief Parses any attributes specific to derived classes
* \author Josh Lurz, James Blackwood
* \param nodeName The name of the curr node. 
* \param curr pointer to the current node in the XML input tree
*/
bool FoodSupplySector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ){
    if ( nodeName == FoodSupplySubsector::getXMLNameStatic() ) {
        parseContainerNode( curr, subsec, subSectorNameMap, new FoodSupplySubsector( regionName, name ) );
    }
    else if ( nodeName == "calPrice" ) {
        calPrice = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "market" ){
        mMarketName = XMLHelper<string>::getValue( curr );
    }
    else if( !SupplySector::XMLDerivedClassParse( nodeName, curr ) ) {
        return false;
    }
    return true;
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
*
* \author Steve Smith, Josh Lurz
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void FoodSupplySector::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    SupplySector::toInputXMLDerived( out, tabs );
    XMLWriteElementCheckDefault( CVRT90*scenario->getMarketplace()->getPrice( name, regionName, 1, true ), "calPrice", out, tabs, -1.0 );
    XMLWriteElementCheckDefault( mMarketName, "market", out, tabs, string( "" ) );
}	

void FoodSupplySector::toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    SupplySector::toDebugXMLDerived( period, out, tabs );
    XMLWriteElement( CVRT90*scenario->getMarketplace()->getPrice( name, regionName, 1, true ), "calPrice", out, tabs );
    XMLWriteElement( mMarketName, "market", out, tabs );
}

/*! \brief Perform any initializations specific to this sector
*
* Is called AFTER sector, subsector, and technology initializations
*
* \author James Blackwood
*/
void FoodSupplySector::completeInit( const IInfo* aRegionInfo,
                                    DependencyFinder* aDependencyFinder,
                                    ILandAllocator* aLandAllocator,
                                    const GlobalTechnologyDatabase* aGlobalTechDB )
{
    if ( !aLandAllocator ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "LandAllocator not read in." << endl;
    }

    if( mMarketName.empty() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Market name for sector " << name << " was not set. Defaulting to regional market." << endl;
        mMarketName = regionName;
    }
    SupplySector::completeInit( aRegionInfo, aDependencyFinder, aLandAllocator, aGlobalTechDB );
}

/*! \brief Calculate the sector price.
* \details FoodSupplySectors are solved markets, so this function is overridden
*          to use the market price instead of an average subsector price.
* \param aGDP Regional GDP container.
* \param aPeriod model period.
* \return The sector price.
*/
double FoodSupplySector::getPrice( const GDP* aGDP, const int aPeriod ) const {
    return scenario->getMarketplace()->getPrice( name, regionName, aPeriod, true );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& FoodSupplySector::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& FoodSupplySector::getXMLNameStatic() {
    const static string XML_NAME = "FoodSupplySector";
    return XML_NAME;
}

void FoodSupplySector::supply( const GDP* aGDP, const int aPeriod ) {
    // The demand value passed to setOutput does not matter as the 
    // supply and demand will be made equal by the market.
    for( unsigned int i = 0; i < subsec.size(); ++i ){
        // set subsector output from Sector demand
        subsec[ i ]->setOutput( 1, 1, aGDP, aPeriod );
    }  
}

//! Create markets
void FoodSupplySector::setMarket() {
    Marketplace* marketplace = scenario->getMarketplace();
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    // name is resource name
    if ( marketplace->createMarket( regionName, mMarketName, name, IMarketType::NORMAL ) ) {
        // Set price and output units for period 0 market info
        IInfo* marketInfo = marketplace->getMarketInfo( name, regionName, 0, true );
        marketInfo->setString( "price-unit", mPriceUnit );
        marketInfo->setString( "output-unit", mOutputUnit );

        marketplace->setPrice( name, regionName, calPrice / CVRT90, 0, true );

        for( int per = 1; per < modeltime->getmaxper(); ++per ){
            marketplace->setMarketToSolve( name, regionName, per );
        }
        // Don't set calPrice if not valid
        if ( calPrice > 0 ) {
            for( int per = 0; per < modeltime->getmaxper(); ++per ){
                marketplace->getMarketInfo( name, regionName, per, true )->setDouble( "calPrice", calPrice );
            }
        }
    }
}