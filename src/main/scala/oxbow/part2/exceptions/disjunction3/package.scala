package oxbow.exceptions

package object disjunction3 {
 /*
 * Exploration of what it means for the "disjunction-ness" to start permeating your method signatures
 *
 * You can easily avoid `Err1 \/ (Err2 \/ (Err3 \/ Res))` by using flatMap
 *
 * Let's look at a real example; reading in FX rates from a file. Some observations
 *
 *  1. You have to be careful; easy to miss Java calls which throw exceptions. You must get used to using \/.fromTryCatchNonFatal into Java as a FFI
 *
 * But we have ended up with the fact we are handling errors as the only source of returns. What if we wish to accumulate state
 */
}
