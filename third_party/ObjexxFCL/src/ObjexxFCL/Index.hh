#ifndef ObjexxFCL_Index_hh_INCLUDED
#define ObjexxFCL_Index_hh_INCLUDED

// Index: Index Class
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.0.0
//
// Language: C++
//
// Copyright (c) 2000-2015 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/Omit.hh>

// C++ Headers
#include <cassert>
#include <iosfwd>
#include <type_traits>
#include <utility>

namespace ObjexxFCL {

// Index: Index Class
class Index
{

public: // Creation

	// Default Constructor
	inline
	Index() :
	 init_( false ),
	 i_( 0 )
	{}

	// Copy Constructor
	inline
	Index( Index const & I ) :
	 init_( I.init_ ),
	 i_( I.i_ )
	{}

	// Index Constructor
	inline
	Index( int const i ) :
	 init_( true ),
	 i_( i )
	{}

	// Omit Constructor
	inline
	Index( Omit ) :
	 init_( false ),
	 i_( 0 )
	{}

	// Destructor
	inline
	~Index()
	{}

public: // Assignment

	// Scalar Assignment
	inline
	Index &
	operator =( int const i )
	{
		init_ = true;
		i_ = i;
		return *this;
	}

public: // Conversion

	// int Conversion
	inline
	operator int() const
	{
		assert( init_ );
		return i_;
	}

public: // Predicate

	// Initialized?
	inline
	bool
	initialized() const
	{
		return init_;
	}

public: // Inspector

	// Index
	inline
	int
	i() const
	{
		assert( init_ );
		return i_;
	}

public: // Modifier

	// Clear
	inline
	void
	clear()
	{
		init_ = false;
		i_ = 0;
	}

	// Index Set
	inline
	Index &
	i( int const i )
	{
		init_ = true;
		i_ = i;
		return *this;
	}

	// Swap
	inline
	void
	swap( Index & I )
	{
		std::swap( init_, I.init_ );
		std::swap( i_, I.i_ );
	}

private: // Data

	bool init_; // Index initialized?
	int i_; // Index

}; // Index

// Functions

// Swap
inline
void
swap( Index & a, Index & b )
{
	a.swap( b );
}

// Comparison

// Index == Index
inline
bool
operator ==( Index const & a, Index const & b )
{
	return ( a.initialized() && b.initialized() ? ( a.i() == b.i() ) : ! ( a.initialized() || b.initialized() ) );
}

// Index != Index
inline
bool
operator !=( Index const & a, Index const & b )
{
	return !( a == b );
}

// Index == int
inline
bool
operator ==( Index const & a, int const b )
{
	return ( a.initialized() && ( a.i() == b ) );
}

// Index != int
inline
bool
operator !=( Index const & a, int const b )
{
	return !( a == b );
}

// int == Index
inline
bool
operator ==( int const a, Index const & b )
{
	return ( b.initialized() && ( a == b.i() ) );
}

// int != Index
inline
bool
operator !=( int const a, Index const & b )
{
	return !( a == b );
}

// I/O

// Stream >> Index
std::istream &
operator >>( std::istream & stream, Index & a );

// Stream << Index
std::ostream &
operator <<( std::ostream & stream, Index const & a );

} // ObjexxFCL

#endif // ObjexxFCL_Index_hh_INCLUDED
