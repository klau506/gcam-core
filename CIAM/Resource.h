#ifndef _RESOURCE_H_
#define _RESOURCE_H_
#pragma once

#include "subrsrc.h"
#include <xercesc/dom/DOM.hpp>

using namespace xercesc;

/*! 
* \ingroup CIAM
* \brief An abstract class which defines a Resource object, which is a container for multiple Subresource objects.
* \author Sonny Kim
* \date $ Date $
* \version $ Revision $
*/

class Resource
{
protected:
	string name; //!< Resource name
	string market; //!< regional market
	int nosubrsrc; //!< number of subsectors for each Resource
	vector<subrsrc*> depsubrsrc; //!< subsector objects for each Resource
	vector<double> rscprc; //!< Resource price
	vector<double> available; //!< total Resource available
	vector<double> annualprod; //!< annual production rate of Resource
	vector<double> cummprod; //!< cummulative production of Resource
public:
	Resource(); // default construtor
	virtual ~Resource();
	virtual string getType() const = 0;
	void clear();
	void XMLParse( const DOMNode* node );
	void toXML( ostream& out ) const;
	void toDebugXML( const int period, ostream &out ) const;
	string getName() const; 
	void setMarket( const string& regionName );
	double price(int per); 
	void cummsupply(double prc,int per); // calculative cummulative supply from supply curve
	double showcummprod(int per); // returns cummulative supply
	// calculates annual supply or production
	void annualsupply(int per,double gnp1,double gnp2,double price1,double price2);
	double showannualprod(int per); // returns annnual production of Resource
	double showavailable(int per); // returns total available Resource
	double showsubavail( const string& subResourceName, const int per); // returns total available subResource
	int shownosubrsrc(void); // returns total number of subsectors
	void show(void); // shows Resource name and subResources
	// MiniCAM style output to database table
	void MCoutput( const string& regname ); 
	// output to file
	void outputfile( const string& regname ); 
};

/*! 
* \ingroup CIAM
* \brief A class which defines a DepletableResource object, which is a container for multiple Subresource objects.
* \author Josh Lurz
* \date $ Date $
* \version $ Revision $
*/
class DepletableResource: public Resource {
public: 
	virtual string getType() const;
};

/*! 
* \ingroup CIAM
* \brief A class which defines a FixedResource object, which is a container for multiple Subresource objects.
* \author Josh Lurz
* \date $ Date $
* \version $ Revision $
*/
class FixedResource: public Resource {
public: 
	virtual string getType() const;
};

/*! 
* \ingroup CIAM
* \brief A class which defines a RenewableResource object, which is a container for multiple Subresource objects.
* \author Josh Lurz
* \date $ Date $
* \version $ Revision $
*/
class RenewableResource: public Resource {
public: 
	virtual string getType() const;
};

#endif // _RESOURCE_H_