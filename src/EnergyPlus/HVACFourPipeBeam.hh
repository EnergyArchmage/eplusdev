#ifndef HVACFourPipeBeam_hh_INCLUDED
#define HVACFourPipeBeam_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HVACFourPipeBeam {


	extern Array1D_bool CheckEquipName; // input error checking 
	extern int NumFourPipeBeams; // count of four pipe beam terminal units in entire model

	struct FourPipeBeamData
	{
		std::string Name; // name of unit
		std::string UnitType; // type of unit = AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam
		int ADUNum; // index of this unit in the corresponding air distribution unit structure
		int AirAvailSchedNum; // index to schedule for pimary air availability
		bool AirAvailable; // true if primary air is available
		int CoolingAvailSchedNum; // index to schedule for cooling availability
		bool CoolingAvailable; // true if beam cooling is available
		int HeatingAvailSchedNum; // index to schedule for heating availability
		bool HeatingAvailable; // true if beam heating is available
		Real64 VdotDesignPrimAir; // Design primary air volume flow rate m3/s (autosizable)
		bool VdotDesignPrimAirWasAutosized; // true if user input for design air flow was autsized on input
		Real64 MdotDesignPrimAir; // Design primary air mass flow rate kg/s
		int AirInNodeNum; // unit air inlet system node number
		int AirOutNodeNum; // unit air outlet system node number
		Real64 TotBeamLength; // length of all the beams in the zone (autosizable) (m)
		bool TotBeamLengthWasAutosized; // true if beam length was autosized on input
		Real64 VdotNormRatedPrimAir; // normalized primary air volume flow rate at rating point (m3/s-m)
		Real64 MdotNormRatedPrimAir; // normalized primary air mass flow rate at rating point (kg/s-m)
		//cooling
		bool BeamCoolingPresent;  // true if chilled water system is connected to beam
		Real64 VdotDesignCW; // Design chilled water volume flow rate (autosizable) (m3/s)
		bool VdotDesignCWWasAutosized; // true if use input for chilled water flow was autosized
		Real64 MdotDesignCW; // Design chilled water mass flow rate (kg/s)
		Real64 QdotNormRatedCooling; // normalized cooling capacity at rating point (W/m)
		Real64 DeltaTempRatedCooling; // temperature difference between zone air and entering chilled water at rating point (delta C)
		Real64 VdotNormRatedCW; // normalized chilled water volume flow rate at rating point (m3/s-m)
		Real64 MdotNormRatedCW; // normalized chilled water mass flow rate at rating point (kg/s-m)
		int ModCoolingQdotDeltaTFuncNum; // index to curve or table modifying cooling capacity as a function of delta T ratio
		int ModCoolingQdotAirFlowFuncNum; // index to curve or table modifying cooling capacity as a function of air flow ratio
		int ModCoolingQdotCWFlowFuncNum; // index to curve or table modifying cooling capacity as a function of chilled water flow ratio
		Real64 MdotCW; // current chilled water mass flow rate (kg/s)
		Real64 CWTempIn; // current inlet chilled water temperature [C]
		Real64 CWTempOut; // current outlet chilled water temperature [C]
		int CWInNodeNum; // chilled water inlet node
		int CWOutNodeNum; // chilled water outlet nod
		int CWLoopNum; // cooling water plant loop index number
		int CWLoopSideNum; // cooling water plant loop side index
		int CWBranchNum; // cooling water plant loop branch index
		int CWCompNum; // cooling water plant loop component index
		//heating
		bool BeamHeatingPresent; // true if hot water system is connected to beam
		Real64 VdotDesignHW; // Design hot water volume flow rate (autosizable) (m3/s)
		bool VdotDesignHWWasAutosized; // true if user input for hot water flow was autosized
		Real64 MdotDesignHW; // Design hot water mass flow rate (kg/s)
		Real64 QdotNormRatedHeating; // normalized heating capacity at rating point (W/m)
		Real64 DeltaTempRatedHeating; // temperature difference between zone air and entering hot water at rating point (delta C)
		Real64 VdotNormRatedHW; // normalized hot water volume flow rate at rating point (m3/s-m)
		Real64 MdotNormRatedHW; // normalized hot water mass flow rate at rating point (kg/s-m)
		int ModHeatingQdotDeltaTFuncNum; // index to curve or table modifying heating capacity as a function of delta T ratio
		int ModHeatingQdotAirFlowFuncNum; // index to curve or table modifying heating capacity as a function of air flow ratio
		int ModHeatingQdotHWFlowFuncNum; // index to curve or table modifying heating capacity as a function of chilled water flow ratio
		Real64 MdotHW; // current hot water mass flow rate (kg/s)
		Real64 HWTempIn; // current inlet hot water temperature (C)
		Real64 HWTempOut; // current outlet hot water temperature (C)
		int HWInNodeNum; // hot water inlet node
		int HWOutNodeNum; // hot water outlet node
		int HWLoopNum; // cooling water plant loop index number
		int HWLoopSideNum; // cooling water plant loop side index
		int HWBranchNum; // cooling water plant loop branch index
		int HWCompNum; // cooling water plant loop component index
		// simulation iteration controls
		int BeamLoadReSimIndex; 
		int BeamCWMassFlowReSimIndex;
		int BeamCWOutletTempReSimIndex;
		int BeamHWMassFlowReSimIndex;
		int BeamHWOutletTempReSimIndex;
		// output variables
		Real64 BeamCoolingEnergy; // beam sensible cooling energy of all beams in the zone [J]
		Real64 BeamCoolingRate; // beam sensible cooling rate of all beams in the zone [W]
		Real64 BeamHeatingEnergy; // beam heating energy of all beams in the zone [J]
		Real64 BeamHeatingRate; // beam heating rate of all beams in the zone [W]
		Real64 SupAirCoolingEnergy; // Total cooling energy from supply air [J]
		Real64 SupAirCoolingRate; // Total cooling rate from supply air [W]
		Real64 SupAirHeatingEnergy; // Total cooling energy from supply air [J]
		Real64 SupAirHeatingRate; // Total cooling rate from supply air [W]
		Real64 PrimAirFlow; // supply air flow per zone [m3/s]

		// Default Constructor
		FourPipeBeamData() :
			Name ( '' ), // name of unit
			UnitType( 'AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam' ), // type of unit
			ADUNum( 0 ), // index of this unit in the corresponding air distribution unit structure
			AirAvailSchedNum( 0 ), // index to schedule for pimary air availability
			AirAvailable( false ),
			CoolingAvailSchedNum( 0 ), // index to schedule for cooling availability
			CoolingAvailable( false ),
			HeatingAvailSchedNum( 0 ), // index to schedule for heating availability
			HeatingAvailable( false ),
			VdotDesignPrimAir( 0.0 ), // Design primary air volume flow rate m3/s (autosizable)
			MdotDesignPrimAir( 0.0 ), // Design primary air mass flow rate kg/s
			AirInNodeNum( 0 ), // unit air inlet system node number
			AirOutNodeNum( 0 ), // unit air outlet system node number
			TotBeamLength( 0.0 ), // length of all the beams in the zone (autosizable) (m)
			VdotNormRatedPrimAir( 0.0 ), // normalized primary air volume flow rate at rating point (m3/s-m)
			MdotNormRatedPrimAir( 0.0 ), // normalized primary air mass flow rate at rating point (kg/s-m)
		//cooling
			BeamCoolingPresent( false ),  // true if chilled water system is connected to beam
			VdotDesignCW( 0.0 ), // Design chilled water volume flow rate (autosizable) (m3/s)
			MdotDesignCW( 0.0 ), // Design chilled water mass flow rate (kg/s)
			QdotNormRatedCooling( 0.0 ), // normalized cooling capacity at rating point (W/m)
			DeltaTempRatedCooling( 0.0 ), // temperature difference between zone air and entering chilled water at rating point (delta C)
			VdotNormRatedCW( 0.0 ), // normalized chilled water volume flow rate at rating point (m3/s-m)
			MdotNormRatedCW( 0.0 ), // normalized chilled water mass flow rate at rating point (kg/s-m)
			ModCoolingQdotDeltaTFuncNum( 0 ), // index to curve or table modifying cooling capacity as a function of delta T ratio
			ModCoolingQdotAirFlowFuncNum( 0 ), // index to curve or table modifying cooling capacity as a function of air flow ratio
			ModCoolingQdotCWFlowFuncNum( 0 ), // index to curve or table modifying cooling capacity as a function of chilled water flow ratio
			MdotCW( 0.0 ), // current chilled water mass flow rate (kg/s)
			CWTempIn( 0.0 ), // current inlet chilled water temperature [C]
			CWTempOut( 0.0 ), // current outlet chilled water temperature [C]
			CWInNodeNum( 0 ), // chilled water inlet node
			CWOutNodeNum( 0 ), // chilled water outlet nod
			CWLoopNum( 0 ), // cooling water plant loop index number
			CWLoopSideNum( 0 ), // cooling water plant loop side index
			CWBranchNum( 0 ), // cooling water plant loop branch index
			CWCompNum( 0 ), // cooling water plant loop component index
			//heating
			BeamHeatingPresent( false ), // true if hot water system is connected to beam
			VdotDesignHW( 0.0 ), // Design hot water volume flow rate (autosizable) (m3/s)
			MdotDesignHW( 0.0 ), // Design hot water mass flow rate (kg/s)
			QdotNormRatedHeating( 0.0 ), // normalized heating capacity at rating point (W/m)
			DeltaTempRatedHeating( 0.0 ), // temperature difference between zone air and entering hot water at rating point (delta C)
			VdotNormRatedHW( 0.0 ), // normalized hot water volume flow rate at rating point (m3/s-m)
			MdotNormRatedHW( 0.0 ), // normalized hot water mass flow rate at rating point (kg/s-m)
			ModHeatingQdotDeltaTFuncNum( 0 ), // index to curve or table modifying heating capacity as a function of delta T ratio
			ModHeatingQdotAirFlowFuncNum( 0 ), // index to curve or table modifying heating capacity as a function of air flow ratio
			ModHeatingQdotHWFlowFuncNum( 0 ), // index to curve or table modifying heating capacity as a function of chilled water flow ratio
			MdotHW( 0.0 ), // current hot water mass flow rate (kg/s)
			HWTempIn( 0.0 ), // current inlet hot water temperature (C)
			HWTempOut( 0.0 ), // current outlet hot water temperature (C)
			HWInNodeNum( 0 ), // hot water inlet node
			HWOutNodeNum( 0 ), // hot water outlet node
			HWLoopNum( 0 ), // cooling water plant loop index number
			HWLoopSideNum( 0 ), // cooling water plant loop side index
			HWBranchNum( 0 ), // cooling water plant loop branch index
			HWCompNum( 0 ), // cooling water plant loop component index
			// simulation iteration controls
			BeamLoadReSimIndex( 0 ), 
			BeamCWMassFlowReSimIndex( 0 ),
			BeamCWOutletTempReSimIndex( 0 ),
			BeamHWMassFlowReSimIndex( 0 ),
			BeamHWOutletTempReSimIndex( 0 ),
			// output variables
			BeamCoolingEnergy( 0.0 ), // beam sensible cooling energy of all beams in the zone [J]
			BeamCoolingRate( 0.0 ), // beam sensible cooling rate of all beams in the zone [W]
			BeamHeatingEnergy( 0.0 ), // beam heating energy of all beams in the zone [J]
			BeamHeatingRate( 0.0 ), // beam heating rate of all beams in the zone [W]
			SupAirCoolingEnergy( 0.0 ), // Total cooling energy from supply air [J]
			SupAirCoolingRate( 0.0 ), // Total cooling rate from supply air [W]
			SupAirHeatingEnergy( 0.0 ), // Total cooling energy from supply air [J]
			SupAirHeatingRate; // Total cooling rate from supply air [W]
			PrimAirFlow; // supply air flow per zone [m3/s]
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
		int const BeamNum, // number of the current beam unit being simulated
		bool const FirstHVACIteration // TRUE if first air loop solution this HVAC step
	);

	void
	SizeFourPipeBeam( int const CBNum );

	void
	ControlFourPipeBeam(
		int const BeamNum, // number of the current unit being simulated
		int const ZoneNum, // number of zone being served
		int const ZoneNodeNum, // zone node number
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & NonAirSysOutput // convective cooling by the beam system [W]
	);

	void
	CalcFourPipeBeam(
		int const BeamNum, // Unit index
		int const ZoneNode, // zone node number
		Real64 const CWFlow, // chilled water flow [kg/s]
		Real64 const HWFlow, // hot water flow [kg/s]
		Real64 & LoadMet, // load met by unit [W]
		Real64 & CWTempOut // chilled water outlet temperature [C]
		Real64 & HWTempOut, // hot water outlet temperature [C]
	);

	Real64 FourPipeBeamResidual(
		Real64 const WaterFlow, // water flow rate in kg/s
		Array1< Real64 > const & Par
	);

	void
	UpdateFourPipeBeam( int const BeamNum );

	void
	ReportFourPipeBeam( int const BeamNum );

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
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
