#ifndef HVACFourPipeBeam_hh_INCLUDED
#define HVACFourPipeBeam_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HVACFourPipeBeam {


	// Data
	// MODULE PARAMETER DEFINITIONS:

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern Array1D_bool CheckEquipName;


	extern int NumFourPipeBeams;

	// SUBROUTINE SPECIFICATIONS FOR MODULE HVACFourPipeBeam:

	// Types

	struct FourPipeBeamData
	{
		// Members
		// input data
		std::string Name; // name of unit
		std::string UnitType; // type of unit = AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam

		int AirAvailSchedNum; // index to schedule for pimary air availability
		int CoolingAvailSchedNum; // index to schedule for cooling availability
		int HeatingAvailSchedNum; // index to schedule for heating availability
		Real64 MaxAirVolFlow; // Design primary air volume flow rate m3/s (autosizable)
		Real64 MaxAirMassFlow; // Design primary air mass flow rate mkg/s
		Real64 MaxCoolWaterVolFlow; // m3/s
		Real64 MaxCoolWaterMassFlow; // kg/s
		Real64 MaxHotWaterVolFlow; // m3/s
		Real64 MaxHotWaterMassFlow; // kg/s
		int AirInNodeNum; // unit air inlet node number
		int AirOutNodeNum; // unit air outlet node number

		int ADUNum; // index of corresponding air distribution unit
		Real64 NumBeams; // number of beams in the zone
		Real64 BeamLength; // length of individual beam [m]
		Real64 DesInletWaterTemp; // design inlet water temperature [C]
		Real64 DesOutletWaterTemp; // design outlet water Temperature [c]

		Real64 EnthWaterOut; // current outlet water enthalpy [J/kg]
		Real64 BeamFlow; // supply air flow per beam [m3/s]
		Real64 CoolWaterMassFlow; // chilled water mass flow rate [kg/s]
		Real64 BeamCoolingEnergy; // beam cooling energy of all beams in the zone [J]
		Real64 BeamCoolingRate; // beam cooling rate of all beams in the zone [W]
		Real64 SupAirCoolingEnergy; // Total cooling energy from supply air [J]
		Real64 SupAirCoolingRate; // Total cooling rate from supply air [W]
		Real64 SupAirHeatingEnergy; // Total cooling energy from supply air [J]
		Real64 SupAirHeatingRate; // Total cooling rate from supply air [W]

		Real64 CWTempIn; // current inlet chilled water temperature [C]
		Real64 CWTempOut; // current outlet chilled water temperature [C]
		int CWInNodeNum; // chilled water inlet node
		int CWOutNodeNum; // chilled water outlet nod
		int CWLoopNum; // cooling water plant loop index number
		int CWLoopSideNum; // cooling water plant loop side index
		int CWBranchNum; // cooling water plant loop branch index
		int CWCompNum; // cooling water plant loop component index

		Real64 HWTempIn; // current inlet hot water temperature [C]
		Real64 HWTempOut; // current outlet hot water temperature [C]
		int HWInNodeNum; // hot water inlet node
		int HWOutNodeNum; // hot water outlet node
		int HWLoopNum; // cooling water plant loop index number
		int HWLoopSideNum; // cooling water plant loop side index
		int HWBranchNum; // cooling water plant loop branch index
		int HWCompNum; // cooling water plant loop component index

		int CBLoadReSimIndex;
		int CBMassFlowReSimIndex;
		int CBWaterOutletTempReSimIndex;

		// Default Constructor
		FourPipeBeamData() :
			UnitType_Num( 0 ),
			CBType_Num( 0 ),
			SchedPtr( 0 ),
			MaxAirVolFlow( 0.0 ),
			MaxAirMassFlow( 0.0 ),
			MaxCoolWaterVolFlow( 0.0 ),
			MaxCoolWaterMassFlow( 0.0 ),
			AirInNode( 0 ),
			AirOutNode( 0 ),
			CWInNode( 0 ),
			CWOutNode( 0 ),
			ADUNum( 0 ),
			NumBeams( 0.0 ),
			BeamLength( 0.0 ),
			DesInletWaterTemp( 0.0 ),
			DesOutletWaterTemp( 0.0 ),
			CoilArea( 0.0 ),
			a( 0.0 ),
			n1( 0.0 ),
			n2( 0.0 ),
			n3( 0.0 ),
			a0( 0.0 ),
			K1( 0.0 ),
			n( 0.0 ),
			Kin( 0.0 ),
			InDiam( 0.0 ),
			TWIn( 0.0 ),
			TWOut( 0.0 ),
			EnthWaterOut( 0.0 ),
			BeamFlow( 0.0 ),
			CoolWaterMassFlow( 0.0 ),
			BeamCoolingEnergy( 0.0 ),
			BeamCoolingRate( 0.0 ),
			SupAirCoolingEnergy( 0.0 ),
			SupAirCoolingRate( 0.0 ),
			SupAirHeatingEnergy( 0.0 ),
			SupAirHeatingRate( 0.0 ),
			CWLoopNum( 0 ),
			CWLoopSideNum( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			CBLoadReSimIndex( 0 ),
			CBMassFlowReSimIndex( 0 ),
			CBWaterOutletTempReSimIndex( 0 )
		{}

	};

	// Object Data
	extern Array1D< FourPipeBeamData > FourPipeBeam;

	// Functions

	void
	SimFourPipeBeam(
		std::string const & CompName, // name of the beam unit
		bool const FirstHVACIteration, // TRUE if first HVAC iteration in time step
		int const ZoneNum, // index of zone served by the unit
		int const ZoneNodeNum, // zone node number of zone served by the unit
		int & CompIndex, // which beam unit in data structure
		Real64 & NonAirSysOutput // convective cooling by the beam system [W]
	);

	void
	GetFourPipeBeams();

	void
	InitFourPipeBeam(
		int const CBNum, // number of the current beam unit being simulated
		bool const FirstHVACIteration // TRUE if first air loop solution this HVAC step
	);

	void
	SizeFourPipeBeam( int const CBNum );

	void
	ControlFourPipeBeam(
		int const CBNum, // number of the current unit being simulated
		int const ZoneNum, // number of zone being served
		int const ZoneNodeNum, // zone node number
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & NonAirSysOutput // convective cooling by the beam system [W]
	);

	void
	CalcFourPipeBeam(
		int const CBNum, // Unit index
		int const ZoneNode, // zone node number
		Real64 const CWFlow, // cold water flow [kg/s]
		Real64 & LoadMet, // load met by unit [W]
		Real64 & TWOut // chilled water outlet temperature [C]
	);

	Real64
	FourPipeBeamResidual(
		Real64 const CWFlow, // cold water flow rate in kg/s
		Array1< Real64 > const & Par
	);

	void
	UpdateFourPipeBeam( int const CBNum );

	void
	ReportFourPipeBeam( int const CBNum );

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // HVACFourPipeBeam

} // EnergyPlus

#endif
