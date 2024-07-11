#include <Rcpp.h>
using namespace Rcpp;

// Slightly reduce awfulness of C(++)
#define FOR0( var, from, to) for( int var=(from); var<(to); var++)

// [[Rcpp::export]]
LogicalVector ismis_dotlist( List dots) {
  // Avoids Pairlist, in case of cRankiness

  int nargl = dots. size();
  LogicalVector vismis( nargl);
  FOR0( i, 0, nargl){
    vismis[ i] =  dots[i] == R_MissingArg;
  }
return vismis;
};

// Index-checking macros
#define check_booli do{ \
  if( Rf_length( dots[ i]) != dimx[ i]){ \
stop( "Wrong-length logical index in dim " + \
    std::to_string( i+1)); \
  }; \
} while( 0)

#define badrange_strict( iv, maxrange) \
  ((iv <= 0) + (iv > dimx[ i]))
  
#define badrange_loose( iv, maxrange) \
  ((iv ==0) + (abs( iv) > dimx[ i]))

#define check_OOBNA( Rtype, Ctype, strict_or_loose) do{ \
  /* Loop not sugar, coz don't need actual vectors */ \
  /* Don't try for early exit, coz failure is rare */ \
  int failed = 0; \
  FOR0( j, 0, indi.size()){ \
    Ctype indj = indi[ j]; \
    failed += Rtype##Vector::is_na( indj) + \
      badrange_##strict_or_loose( indj, dimx[ i]); \
  }; \
         \
  if( failed){ \
stop( "NA/OOB index not allowed in dim " +  \
    std::to_string( i+1)); \
  }; \
} while( 0)

// Debugging
// #define printo( ...) Rprintf( __VA_ARGS__)
#define printo( ...) do {} while( 0)

// [[Rcpp::export]]
List indfix(
  List dots,
  LogicalVector dotmiss,
  IntegerVector dimx,
  IntegerVector offs,
  LogicalVector slice,
  LogicalVector nooff
){
  /* Return list with $inds and $newoffs */
  int ndots = dots. size(); // guaranteed same as offs, slice, nooff
  List inds( ndots); // mebbe efficienter to shallow-copy, for NA-offset case
  IntegerVector newoffs = clone( offs); // small
  
  FOR0( i, 0, ndots){
    int conseci = 0;
    if( dotmiss[ i]){
      conseci = 1; // no need to check
      IntegerVector indi = seq_len( dimx[ i]);
      inds[ i] = indi;
    } else if( !dotmiss[ i] && IntegerVector::is_na( offs[ i])){
      // Treat like standard R index
      // except NAs and 0's are banned (but -ve OK)
      int itype = TYPEOF( dots[ i]);
      if( itype == LGLSXP){
        check_booli;
      } else if( itype == INTSXP){
        IntegerVector indi = IntegerVector( dots[ i]);
        check_OOBNA( Integer, int, loose);
      } else if( itype == REALSXP){
        NumericVector indi = NumericVector( dots[ i]);
        check_OOBNA( Numeric, double, loose);
      };
      // else (char or illegal) no check; 
      // let R catch user-errors later
      inds[ i] = dots[ i]; // ?could avoid via shallow-copy?
    } else { // normal offarray offset
    
      // Could use switch() but its syntax is bloody awful
      int itype = TYPEOF( dots[ i]);
      if( itype == LGLSXP){
        check_booli;
        LogicalVector booli = LogicalVector( dots[ i]);
        
        // Equiv which()
        int nsel = 0;
        int firstel = dimx[ i];
        FOR0( j, 0, dimx[ i]){ // fucken 0-index grrr
          int troo = (booli[ j] == TRUE);
          firstel = (troo & (firstel > j)) ? j : firstel;
          nsel += troo;
        };

        // All-false (length 0) should be OK..?
        IntegerVector indi( nsel);
        int nset = 0;
        // Here I will for once make use of a crazy C floop
        for( int j=firstel; nset < nsel; j++){
          if( booli[ j] == TRUE){
            nset++;
            indi[ nset] = j;
          };
        }; // equiv which()
        
        inds[ i] = indi;
      } else if( itype == INTSXP){
        IntegerVector indi = IntegerVector( dots[ i]) - (offs[ i]-1);
        check_OOBNA( Integer, int, strict);
        inds[ i] = indi;
      } else if( itype == REALSXP){
        // Let R handle real-to-int
        NumericVector indi = NumericVector( dots[ i]) - (offs[ i]-1);
        check_OOBNA( Numeric, double, strict);
        inds[ i] = indi;
      } else {
stop( "Only numeric or logical index allowed for dim " +
          std::to_string( i+1));
      }; // type of index
    }; // type of dim (missing/NA-offset/normal offset)
    
    // listo[el].size() doesn't work...
    if( Rf_length( inds[i])== 0){ // corner case
      // Signal back to R (see R side for response)
return List( 0);
    };

    // No NAs, and no corner-cases :)
    if( !slice[ i]){ 
      if( !conseci && !IntegerVector::is_na( offs[ i])){ 
        // Check consecutivity
        IntegerVector indi = inds[ i];
        conseci = true;
        FOR0( j, 1, indi.size()){
          conseci &= (indi[ j]-indi[ j-1] == 1);
        };
        if( conseci){
          newoffs[ i] += indi[ 0]-1; // NB already shifted!
        } else {
          newoffs[ i] = NA_INTEGER;
          if( !nooff[ i]){
            warning( "Non-consec indices butnooff not set, for dim " + 
              std::to_string( i+1));
          };
        }; // if conseci
      }; // if need to check conseci
    } else if( Rf_length( inds[ i]) != 1) {
      // told to slice, but...
stop( "SLICE requested but >1 indices, for dim " +
        std::to_string( i+1));
    };
  }; // for i(dim)
  
  List ret = List::create( 
      Named( "inds")= inds,
      Named( "newoffs") = newoffs
    );
return( ret);
}; // indfix

